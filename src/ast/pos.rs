use std::ops::{BitOr, BitOrAssign, Range};
use std::{fmt, str};

use bit_vec::BitVec;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CodeRange {
    pub start: usize,
    pub end: usize,
}
impl CodeRange {
    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }
    pub fn is_dummy(&self) -> bool {
        self.start > self.end
    }
}
impl fmt::Debug for CodeRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if *self == DUMMY_RANGE {
            write!(f, "DUMMY_RANGE")
        } else {
            f.debug_struct("CodeRange")
                .field("start", &self.start)
                .field("end", &self.end)
                .finish()
        }
    }
}
impl From<Range<usize>> for CodeRange {
    fn from(range: Range<usize>) -> Self {
        CodeRange {
            start: range.start,
            end: range.end,
        }
    }
}
impl From<CodeRange> for Range<usize> {
    fn from(range: CodeRange) -> Self {
        range.start..range.end
    }
}
pub const DUMMY_RANGE: CodeRange = CodeRange { start: 1, end: 0 };

impl BitOr for CodeRange {
    type Output = CodeRange;
    fn bitor(self, rhs: Self) -> Self::Output {
        if rhs.is_dummy() {
            self
        } else if self.is_dummy() {
            rhs
        } else {
            CodeRange {
                start: self.start.min(rhs.start),
                end: self.end.max(rhs.end),
            }
        }
    }
}
impl BitOrAssign for CodeRange {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

// For testing
pub fn pos_in<T, U>(all_text: T, sub_text: U, mut idx: usize) -> CodeRange
where
    T: AsRef<[u8]>,
    U: AsRef<[u8]>,
{
    let all_text: &[u8] = all_text.as_ref();
    let sub_text: &[u8] = sub_text.as_ref();
    // TODO: consider KMP
    let mut i = 0;
    while i + sub_text.len() <= all_text.len() {
        if all_text[i..i + sub_text.len()] == *sub_text {
            if idx == 0 {
                return CodeRange {
                    start: i,
                    end: i + sub_text.len(),
                };
            } else {
                idx -= 1;
            }
        }
        i += 1;
    }
    DUMMY_RANGE
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PositionIndex {
    entries: Vec<PositionEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct PositionEntry {
    pos: usize,
    rc: (usize, usize),
    skip: BitVec,
}

impl PositionIndex {
    pub fn new(s: &[u8]) -> Self {
        Self::with_chunk_size(s, 32)
    }

    pub fn with_chunk_size(s: &[u8], chunk_size: usize) -> Self {
        let mut entries = Vec::<PositionEntry>::new();
        let mut start_rc = (0_usize, 0_usize);
        let mut start_pos = 0_usize;
        let mut rc = (0_usize, 0_usize);
        let mut pos = 0_usize;
        let mut skip = BitVec::new();
        while pos < s.len() {
            let len = match s[pos] {
                0x00..=0x7F => 1,
                0xC0..=0xDF => 2,
                0xE0..=0xEF => 3,
                0xF0..=0xF7 => 4,
                _ => 1,
            };
            let len = len.min(s.len() - pos);
            let len = if str::from_utf8(&s[pos..pos + len]).is_ok() {
                len
            } else {
                1
            };

            let need_early_flush = len > 1 && (pos - start_pos) - skip.len() > chunk_size;
            if need_early_flush {
                entries.push(PositionEntry {
                    pos: start_pos,
                    rc: start_rc,
                    skip: skip.clone(),
                });
                start_rc = rc;
                start_pos = pos;
                skip.truncate(0);
            }

            if len > 1 {
                while skip.len() < pos - start_pos {
                    skip.push(false);
                }
            }
            match len {
                1 => {
                    // Defer until multi-byte character appears
                }
                2 => {
                    skip.push(true);
                    skip.push(false);
                }
                3 => {
                    skip.push(true);
                    skip.push(true);
                    skip.push(false);
                }
                4 => {
                    // Count as two characters for UTF-16 position encoding
                    skip.push(true);
                    skip.push(false);
                    skip.push(true);
                    skip.push(false);
                }
                _ => unreachable!(),
            }

            let need_flush = s[pos] == b'\n' || skip.len() > chunk_size;

            if s[pos] == b'\n' {
                rc.0 += 1;
                rc.1 = 0;
            } else if len == 4 {
                rc.1 += 2;
            } else {
                rc.1 += 1;
            }
            pos += len;

            if need_flush {
                entries.push(PositionEntry {
                    pos: start_pos,
                    rc: start_rc,
                    skip: skip.clone(),
                });
                start_rc = rc;
                start_pos = pos;
                skip.truncate(0);
            }
        }
        {
            // flush
            entries.push(PositionEntry {
                pos: start_pos,
                rc: start_rc,
                skip: skip.clone(),
            });
        }
        Self { entries }
    }

    pub fn rc_of(&self, pos: usize) -> (usize, usize) {
        let entry_index = self.entries.partition_point(|entry| entry.pos <= pos);
        let entry = &self.entries[entry_index.max(1) - 1];
        let relpos = pos - entry.pos;
        if entry.skip.is_empty() {
            // Fast path for ASCII case
            return (entry.rc.0, entry.rc.1 + relpos);
        }

        let relcol = if relpos < entry.skip.len() {
            (0..relpos).filter(|&i| !entry.skip[i]).count()
        } else {
            entry.skip.iter().filter(|&b| !b).count() + (relpos - entry.skip.len())
        };
        (entry.rc.0, entry.rc.1 + relcol)
    }

    pub fn pos_of(&self, rc: (usize, usize)) -> usize {
        let entry_index = self.entries.partition_point(|entry| entry.rc <= rc);
        let entry = &self.entries[entry_index.max(1) - 1];
        // Use saturation just in case (it should not happen though)
        let relcol = rc.1.saturating_sub(entry.rc.1);
        if entry.skip.is_empty() {
            // Fast path for ASCII case
            return entry.pos + relcol;
        }

        let mut relpos = 0;
        let mut relcol_left = relcol;
        while relpos < entry.skip.len() && relcol_left > 0 {
            if !entry.skip[relpos] {
                relcol_left -= 1;
            }
            relpos += 1;
        }
        entry.pos + relpos + relcol_left
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rc_of_ascii() {
        helper_test_rc_of(
            "foo bar\nbaz\nfoo bar foo bar foo bar\n".as_bytes(),
            vec![
                (0, (0, 0)),   // 'f'
                (1, (0, 1)),   // 'o'
                (2, (0, 2)),   // 'o'
                (3, (0, 3)),   // ' '
                (4, (0, 4)),   // 'b'
                (5, (0, 5)),   // 'a'
                (6, (0, 6)),   // 'r'
                (7, (0, 7)),   // '\n'
                (8, (1, 0)),   // 'b'
                (9, (1, 1)),   // 'a'
                (10, (1, 2)),  // 'z'
                (11, (1, 3)),  // '\n'
                (12, (2, 0)),  // 'f'
                (13, (2, 1)),  // 'o'
                (14, (2, 2)),  // 'o'
                (15, (2, 3)),  // ' '
                (16, (2, 4)),  // 'b'
                (17, (2, 5)),  // 'a'
                (18, (2, 6)),  // 'r'
                (19, (2, 7)),  // ' '
                (20, (2, 8)),  // 'f'
                (21, (2, 9)),  // 'o'
                (22, (2, 10)), // 'o'
                (23, (2, 11)), // ' '
                (24, (2, 12)), // 'b'
                (25, (2, 13)), // 'a'
                (26, (2, 14)), // 'r'
                (27, (2, 15)), // ' '
                (28, (2, 16)), // 'f'
                (29, (2, 17)), // 'o'
                (30, (2, 18)), // 'o'
                (31, (2, 19)), // ' '
                (32, (2, 20)), // 'b'
                (33, (2, 21)), // 'a'
                (34, (2, 22)), // 'r'
                (35, (2, 23)), // '\n'
                (36, (3, 0)),  // EOF
            ],
        );
    }

    #[test]
    fn test_rc_of_non_ascii() {
        helper_test_rc_of(
            "ωん🍺\nαβγδζηιあいうえおかきくけこ🍺🍻🍼🍽\n".as_bytes(),
            vec![
                (0, (0, 0)),   // 'ω'
                (2, (0, 1)),   // 'ん'
                (5, (0, 2)),   // '🍺'
                (9, (0, 4)),   // '\n'
                (10, (1, 0)),  // 'α'
                (12, (1, 1)),  // 'β'
                (14, (1, 2)),  // 'γ'
                (16, (1, 3)),  // 'δ'
                (18, (1, 4)),  // 'ζ'
                (20, (1, 5)),  // 'η'
                (22, (1, 6)),  // 'ι'
                (24, (1, 7)),  // 'あ'
                (27, (1, 8)),  // 'い'
                (30, (1, 9)),  // 'う'
                (33, (1, 10)), // 'え'
                (36, (1, 11)), // 'お'
                (39, (1, 12)), // 'か'
                (42, (1, 13)), // 'き'
                (45, (1, 14)), // 'く'
                (48, (1, 15)), // 'け'
                (51, (1, 16)), // 'こ'
                (54, (1, 17)), // '🍺'
                (58, (1, 19)), // '🍻'
                (62, (1, 21)), // '🍼'
                (66, (1, 23)), // '🍽'
                (70, (1, 25)), // '\n'
                (71, (2, 0)),  // EOF
            ],
        );
    }

    fn helper_test_rc_of(s: &[u8], expected: Vec<(usize, (usize, usize))>) {
        let index = PositionIndex::new(s);
        let actual = expected
            .iter()
            .map(|&(pos, _)| {
                let rc = index.rc_of(pos);
                (pos, rc)
            })
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_pos_of_ascii() {
        helper_test_pos_of(
            "foo bar\nbaz\nfoo bar foo bar foo bar\n".as_bytes(),
            vec![
                (0, (0, 0)),   // 'f'
                (1, (0, 1)),   // 'o'
                (2, (0, 2)),   // 'o'
                (3, (0, 3)),   // ' '
                (4, (0, 4)),   // 'b'
                (5, (0, 5)),   // 'a'
                (6, (0, 6)),   // 'r'
                (7, (0, 7)),   // '\n'
                (8, (1, 0)),   // 'b'
                (9, (1, 1)),   // 'a'
                (10, (1, 2)),  // 'z'
                (11, (1, 3)),  // '\n'
                (12, (2, 0)),  // 'f'
                (13, (2, 1)),  // 'o'
                (14, (2, 2)),  // 'o'
                (15, (2, 3)),  // ' '
                (16, (2, 4)),  // 'b'
                (17, (2, 5)),  // 'a'
                (18, (2, 6)),  // 'r'
                (19, (2, 7)),  // ' '
                (20, (2, 8)),  // 'f'
                (21, (2, 9)),  // 'o'
                (22, (2, 10)), // 'o'
                (23, (2, 11)), // ' '
                (24, (2, 12)), // 'b'
                (25, (2, 13)), // 'a'
                (26, (2, 14)), // 'r'
                (27, (2, 15)), // ' '
                (28, (2, 16)), // 'f'
                (29, (2, 17)), // 'o'
                (30, (2, 18)), // 'o'
                (31, (2, 19)), // ' '
                (32, (2, 20)), // 'b'
                (33, (2, 21)), // 'a'
                (34, (2, 22)), // 'r'
                (35, (2, 23)), // '\n'
                (36, (3, 0)),  // EOF
            ],
        );
    }

    #[test]
    fn test_pos_of_non_ascii() {
        helper_test_pos_of(
            "ωん🍺\nαβγδζηιあいうえおかきくけこ🍺🍻🍼🍽\n".as_bytes(),
            vec![
                (0, (0, 0)),   // 'ω'
                (2, (0, 1)),   // 'ん'
                (5, (0, 2)),   // '🍺'
                (9, (0, 4)),   // '\n'
                (10, (1, 0)),  // 'α'
                (12, (1, 1)),  // 'β'
                (14, (1, 2)),  // 'γ'
                (16, (1, 3)),  // 'δ'
                (18, (1, 4)),  // 'ζ'
                (20, (1, 5)),  // 'η'
                (22, (1, 6)),  // 'ι'
                (24, (1, 7)),  // 'あ'
                (27, (1, 8)),  // 'い'
                (30, (1, 9)),  // 'う'
                (33, (1, 10)), // 'え'
                (36, (1, 11)), // 'お'
                (39, (1, 12)), // 'か'
                (42, (1, 13)), // 'き'
                (45, (1, 14)), // 'く'
                (48, (1, 15)), // 'け'
                (51, (1, 16)), // 'こ'
                (54, (1, 17)), // '🍺'
                (58, (1, 19)), // '🍻'
                (62, (1, 21)), // '🍼'
                (66, (1, 23)), // '🍽'
                (70, (1, 25)), // '\n'
                (71, (2, 0)),  // EOF
            ],
        );
    }

    fn helper_test_pos_of(s: &[u8], expected: Vec<(usize, (usize, usize))>) {
        let index = PositionIndex::new(s);
        let actual = expected
            .iter()
            .map(|&(_, rc)| {
                let pos = index.pos_of(rc);
                (pos, rc)
            })
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }
}
