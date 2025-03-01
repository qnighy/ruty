use crate::encoding::{
    is_jisx0208_const_starter, is_unicode_const_starter, CharPlus, EncNext, EncodingImpl,
    EncodingState,
};

#[derive(Debug)]
pub(in crate::encoding) struct ShiftJisImpl;

impl EncodingImpl for ShiftJisImpl {
    fn is_stateless(&self) -> bool {
        true
    }
    fn is_ascii_compatible(&self) -> bool {
        true
    }
    fn next_char(&self, bytes: &[u8], _state: EncodingState) -> EncNext {
        match bytes[0] {
            b0 @ 0x00..0x80 => EncNext::Valid {
                len: 1,
                unicode: Some(b0 as char),
            },
            0x81..=0x9F | 0xE0..=0xFC => match *bytes.get(1).unwrap_or(&0) {
                0x40..=0x7E | 0x80..=0xFC => EncNext::Valid {
                    len: 2,
                    unicode: None,
                },
                _ => INVALID,
            },
            0xA1..=0xDF => EncNext::Valid {
                len: 1,
                unicode: None,
            },
            0x80 | _ => INVALID,
        }
    }
    fn is_const_starter(&self, ch: CharPlus) -> bool {
        match ch {
            CharPlus::Unicode(ch) => is_unicode_const_starter(ch),
            CharPlus::NonUnicode(bytes) => match *bytes {
                [b0, b1] => {
                    let (b0, b1) = to_jisx0208(b0, b1);
                    is_jisx0208_const_starter(b0, b1)
                }
                _ => false,
            },
            _ => false,
        }
    }
}

const INVALID: EncNext = EncNext::Invalid { len: 1 };

fn to_jisx0208(b0: u8, b1: u8) -> (u8, u8) {
    let shift_hi = if b0 < 0xE0 {
        b0 - 0x81
    } else {
        b0 - (0xE0 - 0x1F)
    };
    let shift_lo = b1 - 0x40 - (b1 >= 0x7F) as u8;
    let hi = shift_hi * 2 + shift_lo / 94;
    let lo = shift_lo % 94;
    (hi + 0xA1, lo + 0xA1)
}
