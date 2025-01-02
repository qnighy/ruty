use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CharPlus {
    Unicode(char),
    NonUnicode(SmallBytes),
    Invalid(SmallBytes),
    Shift(SmallBytes),
}

impl From<char> for CharPlus {
    fn from(c: char) -> Self {
        Self::Unicode(c)
    }
}

#[derive(Clone, Copy, Default)]
pub struct SmallBytes {
    len: u8,
    bytes: [u8; 4],
}

impl TryFrom<&[u8]> for SmallBytes {
    type Error = TryFromSliceError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if bytes.len() > 4 {
            return Err(TryFromSliceError(()));
        }

        let mut extended = [0; 4];
        extended[..bytes.len()].copy_from_slice(bytes);
        Ok(Self {
            len: bytes.len() as u8,
            bytes: extended,
        })
    }
}

macro_rules! impl_length {
    ($n:literal) => {
        impl From<[u8; $n]> for SmallBytes {
            fn from(bytes: [u8; $n]) -> Self {
                let mut extended = [0; 4];
                extended[..$n].copy_from_slice(&bytes);
                Self {
                    len: $n,
                    bytes: extended,
                }
            }
        }

        impl From<&[u8; $n]> for SmallBytes {
            fn from(bytes: &[u8; $n]) -> Self {
                SmallBytes::from(*bytes)
            }
        }
    };
}

impl_length!(0);
impl_length!(1);
impl_length!(2);
impl_length!(3);
impl_length!(4);

impl std::ops::Deref for SmallBytes {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &self.bytes[..self.len as usize]
    }
}

impl fmt::Debug for SmallBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <[u8] as fmt::Debug>::fmt(self, f)
    }
}

impl PartialEq for SmallBytes {
    fn eq(&self, other: &Self) -> bool {
        <[u8] as PartialEq>::eq(self, other)
    }
}

impl Eq for SmallBytes {}

impl PartialOrd for SmallBytes {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        <[u8] as PartialOrd>::partial_cmp(self, other)
    }
}

impl Ord for SmallBytes {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        <[u8] as Ord>::cmp(self, other)
    }
}

impl std::hash::Hash for SmallBytes {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        <[u8] as std::hash::Hash>::hash(self, state)
    }
}

#[derive(Debug)]
pub struct TryFromSliceError(());

impl fmt::Display for TryFromSliceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::error::Error;
        #[allow(deprecated)]
        self.description().fmt(f)
    }
}

impl std::error::Error for TryFromSliceError {
    fn description(&self) -> &str {
        "slice too long"
    }
}
