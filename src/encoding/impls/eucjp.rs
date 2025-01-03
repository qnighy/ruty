use crate::encoding::{is_unicode_const_starter, CharPlus, EncNext, EncodingImpl, EncodingState};

pub(in crate::encoding) struct EucJpImpl;

impl EncodingImpl for EucJpImpl {
    fn is_stateless(&self) -> bool {
        true
    }
    fn next_char(&self, bytes: &[u8], _state: EncodingState) -> EncNext {
        match bytes[0] {
            b0 @ 0x00..0x80 => EncNext::Valid {
                len: 1,
                unicode: Some(b0 as char),
            },
            0x8E | 0xA1..=0xFE => match *bytes.get(1).unwrap_or(&0) {
                0xA1..=0xFE => EncNext::Valid {
                    len: 2,
                    unicode: None,
                },
                _ => INVALID,
            },
            0x8F => match *bytes.get(1).unwrap_or(&0) {
                0xA1..=0xFE => match *bytes.get(2).unwrap_or(&0) {
                    0xA1..=0xFE => EncNext::Valid {
                        len: 3,
                        unicode: None,
                    },
                    _ => INVALID,
                },
                _ => INVALID,
            },
            _ => INVALID,
        }
    }
    fn is_const_starter(&self, ch: CharPlus) -> bool {
        match ch {
            CharPlus::Unicode(ch) => is_unicode_const_starter(ch),
            _ => false,
        }
    }
}

const INVALID: EncNext = EncNext::Invalid { len: 1 };
