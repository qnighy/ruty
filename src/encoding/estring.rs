use std::{collections::TryReserveError, fmt};

use crate::encoding::{Chars, EStrMut, EStrRef, Encoding};

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

    pub fn asciify(&mut self) {
        if self.as_estr().is_ascii() {
            self.encoding = Encoding::US_ASCII;
        }
    }

    pub fn asciified(self) -> Self {
        let mut s = self;
        s.asciify();
        s
    }
}

impl fmt::Debug for EString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <EStrRef<'_> as fmt::Debug>::fmt(&self.as_estr(), f)
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
