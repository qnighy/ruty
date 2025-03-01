use crate::encoding::{is_unicode_const_starter, CharPlus, EncNext, EncodingImpl, EncodingState};

#[derive(Debug)]
pub(in crate::encoding) struct UsAsciiImpl;

impl EncodingImpl for UsAsciiImpl {
    fn is_stateless(&self) -> bool {
        true
    }
    fn is_ascii_compatible(&self) -> bool {
        true
    }
    fn next_char(&self, bytes: &[u8], _state: EncodingState) -> EncNext {
        if bytes[0] < 0x80 {
            EncNext::Valid {
                len: 1,
                unicode: Some(bytes[0] as char),
            }
        } else {
            EncNext::Invalid { len: 1 }
        }
    }
    fn is_const_starter(&self, ch: CharPlus) -> bool {
        match ch {
            CharPlus::Unicode(ch) => is_unicode_const_starter(ch),
            _ => false,
        }
    }
}
