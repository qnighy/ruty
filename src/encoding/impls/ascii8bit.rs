use crate::encoding::{is_unicode_const_starter, CharPlus, EncNext, EncodingImpl, EncodingState};

pub(in crate::encoding) struct Ascii8bitImpl;

impl EncodingImpl for Ascii8bitImpl {
    fn is_stateless(&self) -> bool {
        true
    }
    fn next_char(&self, bytes: &[u8], _state: EncodingState) -> EncNext {
        EncNext::Valid {
            len: 1,
            unicode: if bytes[0] < 0x80 {
                Some(bytes[0] as char)
            } else {
                None
            },
        }
    }
    fn is_const_starter(&self, ch: CharPlus) -> bool {
        match ch {
            CharPlus::Unicode(ch) => is_unicode_const_starter(ch),
            _ => false,
        }
    }
}
