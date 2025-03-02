use std::{fmt, ops::Range};

use crate::encoding::{CharPlus, EString, EncNext, Encoding, EncodingImpl, EncodingState};

use super::charplus::SmallBytes;

/// Equivalent to [`&'a str`][str] but with encoding information.
/// Additionally, it can contain invalid byte sequences.
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

    pub const fn len(&self) -> usize {
        self.bytes.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    // pub fn is_char_boundary(&self, index: usize) -> bool {}

    pub const fn as_bytes(&self) -> &'a [u8] {
        self.bytes
    }

    pub const fn as_ptr(&self) -> *const u8 {
        self.bytes.as_ptr()
    }

    // pub fn get<I>(&self, index: I) -> Option<EStrRef<'a>> {}
    // pub unsafe fn get_unchecked<I>(&self, index: I) -> EStrRef<'a> {}

    // pub fn split_at(&self, mid: usize) -> (EStrRef<'a>, EStrRef<'a>) {}
    // pub fn split_at_checked(&self, mid: usize) -> Option<(EStrRef<'a>, EStrRef<'a>)> {}

    pub fn chars(&self) -> Chars<'a> {
        Chars {
            bytes: self.bytes,
            encoding: self.encoding,
            enc_impl: self.encoding.encoding_impl(),
            state: self.state,
        }
    }

    pub fn char_indices(&self) -> CharIndices<'a> {
        CharIndices {
            bytes: self.bytes,
            pos: 0,
            encoding: self.encoding,
            enc_impl: self.encoding.encoding_impl(),
            state: self.state,
        }
    }
    // pub fn bytes(&self) -> Bytes<'a> {}

    // pub fn split_whitespace(&self) -> SplitWhitespace<'_>
    // pub fn split_ascii_whitespace(&self) -> SplitAsciiWhitespace<'_>
    // pub fn lines(&self) -> Lines<'_>

    // pub fn contains<P>(&self, pat: P) -> bool
    // pub fn starts_with<P>(&self, pat: P) -> bool
    // pub fn ends_with<P>(&self, pat: P) -> bool
    // pub fn find<P>(&self, pat: P) -> Option<usize>
    // pub fn rfind<P>(&self, pat: P) -> Option<usize>
    // pub fn split<P>(&self, pat: P) -> Split<'_, P>
    // pub fn split_inclusive<P>(&self, pat: P) -> SplitInclusive<'_, P>
    // pub fn rsplit<P>(&self, pat: P) -> RSplit<'_, P>
    // pub fn split_terminator<P>(&self, pat: P) -> SplitTerminator<'_, P>
    // pub fn rsplit_terminator<P>(&self, pat: P) -> RSplitTerminator<'_, P>
    // pub fn splitn<P>(&self, n: usize, pat: P) -> SplitN<'_, P>
    // pub fn rsplitn<P>(&self, n: usize, pat: P) -> RSplitN<'_, P>
    // pub fn split_once<P>(&self, delimiter: P) -> Option<(&str, &str)>
    // pub fn rsplit_once<P>(&self, delimiter: P) -> Option<(&str, &str)>
    // pub fn matches<P>(&self, pat: P) -> Matches<'_, P>
    // pub fn rmatches<P>(&self, pat: P) -> RMatches<'_, P>
    // pub fn match_indices<P>(&self, pat: P) -> MatchIndices<'_, P>
    // pub fn rmatch_indices<P>(&self, pat: P) -> RMatchIndices<'_, P>

    // pub fn trim(&self) -> &str
    // pub fn trim_start(&self) -> &str
    // pub fn trim_end(&self) -> &str
    // pub fn trim_matches<P>(&self, pat: P) -> &str
    // pub fn trim_start_matches<P>(&self, pat: P) -> &str
    // pub fn strip_prefix<P>(&self, prefix: P) -> Option<&str>
    // pub fn strip_suffix<P>(&self, suffix: P) -> Option<&str>
    // pub fn trim_end_matches<P>(&self, pat: P) -> &str

    // pub fn parse<F>(&self) -> Result<F, <F as FromStr>::Err>

    pub fn is_ascii(&self) -> bool {
        self.encoding.is_ascii_compatible() && self.bytes.is_ascii()
    }

    // pub fn eq_ignore_ascii_case(&self, other: &str) -> bool
    // pub const fn trim_ascii_start(&self) -> &str
    // pub const fn trim_ascii_end(&self) -> &str
    // pub const fn trim_ascii(&self) -> &str
    // pub fn escape_debug(&self) -> EscapeDebug<'_>
    // pub fn escape_default(&self) -> EscapeDefault<'_>
    // pub fn escape_unicode(&self) -> EscapeUnicode<'_>

    // pub fn replace<P>(&self, from: P, to: &str) -> String
    // pub fn replacen<P>(&self, pat: P, to: &str, count: usize) -> String
    // pub fn to_lowercase(&self) -> String
    // pub fn to_uppercase(&self) -> String
    // pub fn repeat(&self, n: usize) -> String
    // pub fn to_ascii_uppercase(&self) -> String
    // pub fn to_ascii_lowercase(&self) -> String

    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    pub fn state(&self) -> EncodingState {
        self.state
    }

    pub fn to_estring(&self) -> EString {
        // TODO: check state
        EString::from_bytes(self.bytes.to_owned(), self.encoding)
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

    pub fn asciified(&self) -> EStrRef<'a> {
        let encoding = if self.is_ascii() {
            Encoding::US_ASCII
        } else {
            self.encoding
        };
        EStrRef::from_bytes_and_state(self.bytes, encoding, self.state)
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

/// Equivalent to [`&'a mut str`][str] but with encoding information.
/// Additionally, it can contain invalid byte sequences.
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

    pub const fn len(&self) -> usize {
        self.bytes.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    // pub fn is_char_boundary(&self, index: usize) -> bool {}

    pub fn as_bytes(&self) -> &[u8] {
        self.bytes
    }

    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        self.bytes
    }

    pub const fn as_ptr(&self) -> *const u8 {
        self.bytes.as_ptr()
    }

    // pub fn get<I>(&self, index: I) -> Option<EStrRef<'a>> {}
    // pub unsafe fn get_unchecked<I>(&self, index: I) -> EStrRef<'a> {}
    // pub unsafe fn get_unchecked_mut<I>(self, index: I) -> EStrMut<'a> {}

    // pub fn split_at(&self, mid: usize) -> (EStrRef<'a>, EStrRef<'a>) {}
    // pub fn split_at_mut(self, mid: usize) -> (EStrMut<'a>, EStrMut<'a>) {}
    // pub fn split_at_checked(&self, mid: usize) -> Option<(EStrRef<'a>, EStrRef<'a>)> {}
    // pub fn split_at_mut_checked(self, mid: usize) -> Option<(EStrMut<'a>, EStrMut<'a>)> {}

    pub fn chars(&self) -> Chars<'_> {
        self.reborrow().chars()
    }

    pub fn char_indices(&self) -> CharIndices<'_> {
        self.reborrow().char_indices()
    }
    // pub fn bytes(&self) -> Bytes<'a> {}

    // pub fn split_whitespace(&self) -> SplitWhitespace<'_>
    // pub fn split_ascii_whitespace(&self) -> SplitAsciiWhitespace<'_>
    // pub fn lines(&self) -> Lines<'_>

    // pub fn contains<P>(&self, pat: P) -> bool
    // pub fn starts_with<P>(&self, pat: P) -> bool
    // pub fn ends_with<P>(&self, pat: P) -> bool
    // pub fn find<P>(&self, pat: P) -> Option<usize>
    // pub fn rfind<P>(&self, pat: P) -> Option<usize>
    // pub fn split<P>(&self, pat: P) -> Split<'_, P>
    // pub fn split_inclusive<P>(&self, pat: P) -> SplitInclusive<'_, P>
    // pub fn rsplit<P>(&self, pat: P) -> RSplit<'_, P>
    // pub fn split_terminator<P>(&self, pat: P) -> SplitTerminator<'_, P>
    // pub fn rsplit_terminator<P>(&self, pat: P) -> RSplitTerminator<'_, P>
    // pub fn splitn<P>(&self, n: usize, pat: P) -> SplitN<'_, P>
    // pub fn rsplitn<P>(&self, n: usize, pat: P) -> RSplitN<'_, P>
    // pub fn split_once<P>(&self, delimiter: P) -> Option<(&str, &str)>
    // pub fn rsplit_once<P>(&self, delimiter: P) -> Option<(&str, &str)>
    // pub fn matches<P>(&self, pat: P) -> Matches<'_, P>
    // pub fn rmatches<P>(&self, pat: P) -> RMatches<'_, P>
    // pub fn match_indices<P>(&self, pat: P) -> MatchIndices<'_, P>
    // pub fn rmatch_indices<P>(&self, pat: P) -> RMatchIndices<'_, P>

    // pub fn trim(&self) -> &str
    // pub fn trim_start(&self) -> &str
    // pub fn trim_end(&self) -> &str
    // pub fn trim_matches<P>(&self, pat: P) -> &str
    // pub fn trim_start_matches<P>(&self, pat: P) -> &str
    // pub fn strip_prefix<P>(&self, prefix: P) -> Option<&str>
    // pub fn strip_suffix<P>(&self, suffix: P) -> Option<&str>
    // pub fn trim_end_matches<P>(&self, pat: P) -> &str

    // pub fn parse<F>(&self) -> Result<F, <F as FromStr>::Err>

    pub fn is_ascii(&self) -> bool {
        self.encoding.is_ascii_compatible() && self.bytes.is_ascii()
    }

    // pub fn eq_ignore_ascii_case(&self, other: &str) -> bool
    // pub const fn trim_ascii_start(&self) -> &str
    // pub const fn trim_ascii_end(&self) -> &str
    // pub const fn trim_ascii(&self) -> &str
    // pub fn escape_debug(&self) -> EscapeDebug<'_>
    // pub fn escape_default(&self) -> EscapeDefault<'_>
    // pub fn escape_unicode(&self) -> EscapeUnicode<'_>

    // pub fn replace<P>(&self, from: P, to: &str) -> String
    // pub fn replacen<P>(&self, pat: P, to: &str, count: usize) -> String
    // pub fn to_lowercase(&self) -> String
    // pub fn to_uppercase(&self) -> String
    // pub fn repeat(&self, n: usize) -> String
    // pub fn to_ascii_uppercase(&self) -> String
    // pub fn to_ascii_lowercase(&self) -> String

    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    pub fn state(&self) -> EncodingState {
        self.state
    }

    pub fn to_estring(&self) -> EString {
        self.reborrow().to_estring()
    }

    pub fn is_valid(&self) -> bool {
        self.reborrow().is_valid()
    }

    pub fn starts_with_ruby_uppercase(&self) -> bool {
        self.reborrow().starts_with_ruby_uppercase()
    }

    pub fn asciified(self) -> EStrMut<'a> {
        let encoding = if self.is_ascii() {
            Encoding::US_ASCII
        } else {
            self.encoding
        };
        EStrMut::from_bytes_and_state(self.bytes, encoding, self.state)
    }

    pub fn reborrow(&self) -> EStrRef<'_> {
        EStrRef::from_bytes_and_state(self.bytes, self.encoding, self.state)
    }

    pub fn reborrow_mut(&mut self) -> EStrMut<'_> {
        EStrMut::from_bytes_and_state(self.bytes, self.encoding, self.state)
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

#[derive(Debug, Clone)]
pub struct CharIndices<'a> {
    bytes: &'a [u8],
    pos: usize,
    // For Debug impl
    #[allow(unused)]
    encoding: Encoding,
    enc_impl: &'static dyn EncodingImpl,
    state: EncodingState,
}

impl<'a> Iterator for CharIndices<'a> {
    type Item = (Range<usize>, CharPlus);

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.bytes.len() {
            return None;
        }

        let start = self.pos;
        let next = self.enc_impl.next_char(self.bytes, self.state);
        Some(match next {
            EncNext::Valid { len, unicode } => {
                let ch = if let Some(unicode) = unicode {
                    CharPlus::Unicode(unicode)
                } else {
                    CharPlus::NonUnicode(SmallBytes::try_from(&self.bytes[..len]).unwrap())
                };
                self.pos += len;
                (start..self.pos, ch)
            }
            EncNext::Invalid { len } => {
                let ch = CharPlus::Invalid(SmallBytes::try_from(&self.bytes[..len]).unwrap());
                self.pos += len;
                (start..self.pos, ch)
            }
            EncNext::Shift { len, next_state } => {
                let ch = CharPlus::Shift(SmallBytes::try_from(&self.bytes[..len]).unwrap());
                self.pos += len;
                self.state = next_state;
                (start..self.pos, ch)
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
