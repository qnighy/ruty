use crate::encoding::{EncNext, EncodingImpl, EncodingState};

pub(in crate::encoding) struct UsAsciiImpl;

impl EncodingImpl for UsAsciiImpl {
    fn is_stateless(&self) -> bool {
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
}
