use crate::encoding::{EncNext, EncodingImpl, EncodingState};

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
}
