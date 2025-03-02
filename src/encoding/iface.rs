use crate::encoding::{CharPlus, EncodingState};

pub(in crate::encoding) trait EncodingImpl: std::fmt::Debug {
    fn is_stateless(&self) -> bool;
    /// Returns true if any valid string either contains non-ASCII characters or
    /// has the same meaning as ASCII.
    fn is_ascii_compatible(&self) -> bool;
    /// Returns true if any substring of a valid string either contains non-ASCII
    /// characters or has the same meaning as ASCII.
    fn is_ascii_substring_compatible(&self) -> bool;
    fn next_char(&self, bytes: &[u8], state: EncodingState) -> EncNext;
    fn is_const_starter(&self, char: CharPlus) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::encoding) enum EncNext {
    Valid {
        len: usize,
        unicode: Option<char>,
    },
    Invalid {
        len: usize,
    },
    Shift {
        len: usize,
        next_state: EncodingState,
    },
}
