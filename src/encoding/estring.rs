use std::{collections::TryReserveError, fmt};

use crate::encoding::{CharPlus, EncNext, Encoding, EncodingImpl, EncodingState};

use super::charplus::SmallBytes;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct EString {
    bytes: Vec<u8>,
    encoding: Encoding,
}

impl EString {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            encoding: Encoding::UTF_8,
        }
    }

    pub fn with_encoding(encoding: Encoding) -> Self {
        Self {
            bytes: Vec::new(),
            encoding,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            bytes: Vec::with_capacity(capacity),
            encoding: Encoding::UTF_8,
        }
    }

    pub fn with_capacity_and_encoding(capacity: usize, encoding: Encoding) -> Self {
        Self {
            bytes: Vec::with_capacity(capacity),
            encoding,
        }
    }

    pub fn from_bytes(bytes: Vec<u8>, encoding: Encoding) -> Self {
        Self { bytes, encoding }
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        &mut self.bytes
    }

    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    pub fn as_estr(&self) -> EStrRef<'_> {
        EStrRef::from_bytes(&self.bytes, self.encoding)
    }

    pub fn as_estr_mut(&mut self) -> EStrMut<'_> {
        EStrMut::from_bytes(&mut self.bytes, self.encoding)
    }

    pub fn capacity(&self) -> usize {
        self.bytes.capacity()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.bytes.reserve(additional);
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.bytes.reserve_exact(additional);
    }

    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.bytes.try_reserve(additional)
    }

    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.bytes.try_reserve_exact(additional)
    }

    pub fn shrink_to_fit(&mut self) {
        self.bytes.shrink_to_fit();
    }

    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.bytes.shrink_to(min_capacity);
    }

    pub fn chars(&self) -> Chars<'_> {
        self.as_estr().chars()
    }

    pub fn is_valid(&self) -> bool {
        self.as_estr().is_valid()
    }

    pub fn starts_with_ruby_uppercase(&self) -> bool {
        self.as_estr().starts_with_ruby_uppercase()
    }
}

impl From<String> for EString {
    fn from(s: String) -> Self {
        Self {
            bytes: s.into_bytes(),
            encoding: Encoding::UTF_8,
        }
    }
}

impl From<&str> for EString {
    fn from(s: &str) -> Self {
        Self {
            bytes: s.as_bytes().to_vec(),
            encoding: Encoding::UTF_8,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct EStrRef<'a> {
    bytes: &'a [u8],
    encoding: Encoding,
    state: EncodingState,
}

impl<'a> EStrRef<'a> {
    pub fn from_bytes(bytes: &'a [u8], encoding: Encoding) -> Self {
        Self {
            bytes,
            encoding,
            state: EncodingState::default(),
        }
    }

    pub fn from_bytes_and_state(bytes: &'a [u8], encoding: Encoding, state: EncodingState) -> Self {
        Self {
            bytes,
            encoding,
            state,
        }
    }

    pub fn bytes(&self) -> &'a [u8] {
        self.bytes
    }

    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    pub fn state(&self) -> EncodingState {
        self.state
    }

    pub fn chars(&self) -> Chars<'a> {
        Chars {
            bytes: self.bytes,
            encoding: self.encoding,
            enc_impl: self.encoding.encoding_impl(),
            state: self.state,
        }
    }

    pub fn is_valid(&self) -> bool {
        self.chars().all(|ch| match ch {
            CharPlus::Unicode(_) => true,
            CharPlus::NonUnicode(_) => true,
            CharPlus::Invalid(_) => false,
            CharPlus::Shift(_) => true,
        })
    }

    pub fn starts_with_ruby_uppercase(&self) -> bool {
        self.chars().next().map_or(false, |ch| {
            self.encoding.encoding_impl().is_const_starter(ch)
        })
    }
}

impl<'a> From<&'a str> for EStrRef<'a> {
    fn from(s: &'a str) -> Self {
        Self::from_bytes(s.as_bytes(), Encoding::UTF_8)
    }
}

impl<'a> From<EStrRef<'a>> for &'a [u8] {
    fn from(s: EStrRef<'a>) -> Self {
        s.bytes
    }
}

impl AsRef<[u8]> for EStrRef<'_> {
    fn as_ref(&self) -> &[u8] {
        self.bytes
    }
}

impl<'a> fmt::Debug for EStrRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct StringBody<'a>(EStrRef<'a>);
        impl<'a> fmt::Debug for StringBody<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "\"")?;
                for ch in self.0.chars() {
                    match ch {
                        CharPlus::Unicode(ch) => write!(f, "{}", ch.escape_debug())?,
                        CharPlus::NonUnicode(bytes) => {
                            if bytes.len() == 1 {
                                write!(f, "\\x{:02x}", bytes[0])?;
                            } else {
                                write!(f, "\\x{{")?;
                                for &b in bytes.iter() {
                                    write!(f, "{:02x}", b)?;
                                }
                                write!(f, "}}")?;
                            }
                        }
                        CharPlus::Invalid(bytes) => {
                            for &b in bytes.iter() {
                                write!(f, "\\x{:02x}", b)?;
                            }
                        }
                        CharPlus::Shift(bytes) => {
                            for &b in bytes.iter() {
                                write!(f, "\\x{:02x}", b)?;
                            }
                        }
                    }
                }
                write!(f, "\"")?;
                Ok(())
            }
        }

        if self.encoding == Encoding::UTF_8 {
            write!(f, "{:?}", StringBody(*self))
        } else {
            f.debug_tuple("EString")
                .field(&StringBody(*self))
                .field(&self.encoding)
                .finish()
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct EStrMut<'a> {
    bytes: &'a mut [u8],
    encoding: Encoding,
    state: EncodingState,
}

impl<'a> EStrMut<'a> {
    pub fn from_bytes(bytes: &'a mut [u8], encoding: Encoding) -> Self {
        Self {
            bytes,
            encoding,
            state: EncodingState::default(),
        }
    }

    pub fn from_bytes_and_state(
        bytes: &'a mut [u8],
        encoding: Encoding,
        state: EncodingState,
    ) -> Self {
        Self {
            bytes,
            encoding,
            state,
        }
    }

    pub fn reborrow(&self) -> EStrRef<'_> {
        EStrRef::from_bytes_and_state(self.bytes, self.encoding, self.state)
    }

    pub fn reborrow_mut(&mut self) -> EStrMut<'_> {
        EStrMut::from_bytes_and_state(self.bytes, self.encoding, self.state)
    }

    pub fn bytes(&self) -> &[u8] {
        self.bytes
    }

    pub fn bytes_mut(&mut self) -> &mut [u8] {
        self.bytes
    }

    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    pub fn state(&self) -> EncodingState {
        self.state
    }
}

#[derive(Clone)]
pub struct Chars<'a> {
    bytes: &'a [u8],
    encoding: Encoding,
    enc_impl: &'static dyn EncodingImpl,
    state: EncodingState,
}

impl<'a> std::fmt::Debug for Chars<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Chars")
            .field("bytes", &self.bytes)
            .field("encoding", &self.encoding)
            .field("state", &self.state)
            .finish()
    }
}

impl<'a> Iterator for Chars<'a> {
    type Item = CharPlus;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bytes.is_empty() {
            return None;
        }

        let next = self.enc_impl.next_char(self.bytes, self.state);
        Some(match next {
            EncNext::Valid { len, unicode } => {
                let ch = if let Some(unicode) = unicode {
                    CharPlus::Unicode(unicode)
                } else {
                    CharPlus::NonUnicode(SmallBytes::try_from(&self.bytes[..len]).unwrap())
                };
                self.bytes = &self.bytes[len..];
                ch
            }
            EncNext::Invalid { len } => {
                let ch = CharPlus::Invalid(SmallBytes::try_from(&self.bytes[..len]).unwrap());
                self.bytes = &self.bytes[len..];
                ch
            }
            EncNext::Shift { len, next_state } => {
                let ch = CharPlus::Shift(SmallBytes::try_from(&self.bytes[..len]).unwrap());
                self.bytes = &self.bytes[len..];
                self.state = next_state;
                ch
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_utf8_debug() {
        assert_eq!(
            format!("{:?}", EStrRef::from_bytes(b"abc", Encoding::UTF_8)),
            "\"abc\""
        );
        assert_eq!(
            format!(
                "{:?}",
                EStrRef::from_bytes(b"\xe3\x81\x82", Encoding::UTF_8)
            ),
            "\"あ\""
        );
        assert_eq!(
            format!("{:?}", EStrRef::from_bytes(b"\xe3\x81", Encoding::UTF_8)),
            "\"\\xe3\\x81\""
        );
        assert_eq!(
            format!(
                "{:?}",
                EStrRef::from_bytes(b"\xef\xbf\xbe", Encoding::UTF_8)
            ),
            "\"\\u{fffe}\""
        );
    }

    #[test]
    fn test_debug_with_bom() {
        assert_eq!(
            format!(
                "{:?}",
                EStrRef::from_bytes(b"\xfe\xff\x30\x42", Encoding::UTF_16)
            ),
            "EString(\"\\xfe\\xffあ\", UTF_16)"
        );
        assert_eq!(
            format!(
                "{:?}",
                EStrRef::from_bytes(b"\xff\xfe\x42\x30", Encoding::UTF_16)
            ),
            "EString(\"\\xff\\xfeあ\", UTF_16)"
        );
        assert_eq!(
            format!("{:?}", EStrRef::from_bytes(b"\x30\x42", Encoding::UTF_16)),
            "EString(\"\\x30\\x42\", UTF_16)"
        );
    }

    #[test]
    fn test_debug_with_non_unicode_char() {
        assert_eq!(
            format!(
                "{:?}",
                EStrRef::from_bytes(b"\xa1\xa2\xa1\xa4A", Encoding::EUC_JP)
            ),
            "EString(\"\\x{a1a2}\\x{a1a4}A\", EUC_JP)"
        );
    }
}
