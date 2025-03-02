use crate::encoding::{is_unicode_const_starter, CharPlus, EncNext, EncodingImpl, EncodingState};

const STATE_BOM_BE: EncodingState = EncodingState(1);
const STATE_BOM_LE: EncodingState = EncodingState(2);
const STATE_BOM_INVALID: EncodingState = EncodingState(3);

#[derive(Debug)]
pub(in crate::encoding) struct Utf16Impl<const LE: bool, const BOM: bool>;
#[allow(unused)]
pub(in crate::encoding) type Utf16BeImpl = Utf16Impl<false, false>;
#[allow(non_upper_case_globals)]
pub(in crate::encoding) const Utf16BeImpl: Utf16Impl<false, false> = Utf16Impl;
#[allow(unused)]
pub(in crate::encoding) type Utf16LeImpl = Utf16Impl<true, false>;
#[allow(non_upper_case_globals)]
pub(in crate::encoding) const Utf16LeImpl: Utf16Impl<true, false> = Utf16Impl;
#[allow(unused)]
pub(in crate::encoding) type Utf16BomImpl = Utf16Impl<false, true>;
#[allow(non_upper_case_globals)]
pub(in crate::encoding) const Utf16BomImpl: Utf16Impl<false, true> = Utf16Impl;

impl<const LE: bool, const BOM: bool> EncodingImpl for Utf16Impl<LE, BOM> {
    fn is_stateless(&self) -> bool {
        !BOM
    }
    fn is_ascii_compatible(&self) -> bool {
        false
    }
    fn is_ascii_substring_compatible(&self) -> bool {
        false
    }
    fn next_char(&self, bytes: &[u8], state: EncodingState) -> EncNext {
        if bytes.len() < 2 {
            return EncNext::Invalid { len: 1 };
        }
        let le = if BOM {
            match state {
                STATE_BOM_BE => false,
                STATE_BOM_LE => true,
                STATE_BOM_INVALID => return INVALID,
                _ => {
                    // Check for BOM
                    match (bytes[0], bytes[1]) {
                        (0xFE, 0xFF) => {
                            return EncNext::Shift {
                                len: 2,
                                next_state: STATE_BOM_BE,
                            }
                        }
                        (0xFF, 0xFE) => {
                            return EncNext::Shift {
                                len: 2,
                                next_state: STATE_BOM_LE,
                            }
                        }
                        _ => {
                            return EncNext::Shift {
                                len: 0,
                                next_state: STATE_BOM_INVALID,
                            }
                        }
                    }
                }
            }
        } else {
            LE
        };
        match unit(le, bytes) {
            unicode @ 0x0000..0xD800 | unicode @ 0xE000..0x10000 => valid(2, unicode),
            hi @ 0xD800..0xDC00 => {
                if bytes.len() < 4 {
                    return INVALID;
                }
                let lo = unit(le, &bytes[2..]);
                if !(0xDC00..0xE000).contains(&lo) {
                    return INVALID;
                }
                let unicode = 0x10000 + ((hi - 0xD800) << 10) + (lo - 0xDC00);
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

const INVALID: EncNext = EncNext::Invalid { len: 2 };

fn valid(len: usize, unicode: u32) -> EncNext {
    EncNext::Valid {
        len,
        unicode: Some(char::from_u32(unicode).unwrap()),
    }
}

fn unit(le: bool, bytes: &[u8]) -> u32 {
    if le {
        (u32::from(bytes[1]) << 8) | u32::from(bytes[0])
    } else {
        (u32::from(bytes[0]) << 8) | u32::from(bytes[1])
    }
}
