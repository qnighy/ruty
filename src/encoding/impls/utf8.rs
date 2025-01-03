use crate::encoding::{is_unicode_const_starter, CharPlus, EncNext, EncodingImpl, EncodingState};

pub(in crate::encoding) struct Utf8Impl;

impl EncodingImpl for Utf8Impl {
    fn is_stateless(&self) -> bool {
        true
    }
    fn next_char(&self, bytes: &[u8], _state: EncodingState) -> EncNext {
        let has_cont = |len: usize| len <= bytes.len() && bytes[1..len].iter().all(|&b| is_cont(b));
        let b0 = bytes[0];
        match b0 {
            0x00..0x80 => valid(1, b0 as u32),
            0xC0..0xE0 => {
                if !has_cont(2) {
                    return INVALID;
                }
                let b1 = bytes[1];
                let unicode = ((b0 & 0x1F) as u32) << 6 | (b1 & 0x3F) as u32;
                if unicode < 0x80 {
                    return INVALID;
                }
                valid(2, unicode)
            }
            0xE0..0xF0 => {
                if !has_cont(3) {
                    return INVALID;
                }
                let b1 = bytes[1];
                let b2 = bytes[2];
                let unicode =
                    ((b0 & 0x0F) as u32) << 12 | ((b1 & 0x3F) as u32) << 6 | (b2 & 0x3F) as u32;
                if unicode < 0x800 || (0xD800..0xE000).contains(&unicode) {
                    return INVALID;
                }
                valid(3, unicode)
            }
            0xF0..0xF8 => {
                if !has_cont(4) {
                    return INVALID;
                }
                let b1 = bytes[1];
                let b2 = bytes[2];
                let b3 = bytes[3];
                let unicode = ((b0 & 0x07) as u32) << 18
                    | ((b1 & 0x3F) as u32) << 12
                    | ((b2 & 0x3F) as u32) << 6
                    | (b3 & 0x3F) as u32;
                if unicode < 0x10000 || unicode >= 0x110000 {
                    return INVALID;
                }
                valid(4, unicode)
            }
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

fn is_cont(b: u8) -> bool {
    b & 0xC0 == 0x80
}

const INVALID: EncNext = EncNext::Invalid { len: 1 };

fn valid(len: usize, unicode: u32) -> EncNext {
    EncNext::Valid {
        len,
        unicode: Some(char::from_u32(unicode).unwrap()),
    }
}
