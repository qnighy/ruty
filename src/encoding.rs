mod catalog;
mod charplus;
mod estr;
mod estring;
mod iface;
mod impls;

use std::{collections::HashMap, sync::LazyLock};

pub use catalog::Encoding;
pub use charplus::CharPlus;
pub use estr::{CharIndices, Chars, EStrMut, EStrRef};
pub use estring::EString;
use iface::{EncNext, EncodingImpl};

struct AsciiCaseInsensitive<'a>(&'a str);

impl PartialEq for AsciiCaseInsensitive<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(other.0)
    }
}
impl Eq for AsciiCaseInsensitive<'_> {}
impl std::hash::Hash for AsciiCaseInsensitive<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for b in self.0.bytes() {
            state.write_u8(b.to_ascii_lowercase());
        }
    }
}

static ENCODING_MAP: LazyLock<HashMap<AsciiCaseInsensitive<'static>, Encoding>> =
    LazyLock::new(|| {
        let mut map = HashMap::<AsciiCaseInsensitive<'static>, Encoding>::new();
        for &encoding in Encoding::all() {
            for &alias in encoding.aliases() {
                map.insert(AsciiCaseInsensitive(alias), encoding);
            }
        }
        map
    });

impl Encoding {
    pub fn find(name: &str) -> Option<Self> {
        ENCODING_MAP.get(&AsciiCaseInsensitive(name)).copied()
    }

    pub fn name(&self) -> &'static str {
        self.aliases()[0]
    }

    pub fn aliases(&self) -> &'static [&'static str] {
        self.aliases_impl()
    }

    pub fn is_stateless(&self) -> bool {
        self.encoding_impl().is_stateless()
    }

    pub fn is_ascii_compatible(&self) -> bool {
        self.encoding_impl().is_ascii_compatible()
    }

    pub fn next_len(&self, bytes: &[u8], mut state: EncodingState) -> usize {
        if bytes.is_empty() {
            return 0;
        }
        loop {
            match self.encoding_impl().next_char(bytes, state) {
                EncNext::Valid { len, .. } => return len,
                EncNext::Invalid { len } => return len,
                EncNext::Shift { len, next_state } => {
                    if len > 0 {
                        return len;
                    } else {
                        state = next_state;
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct EncodingState(pub u16);

fn is_unicode_const_starter(ch: char) -> bool {
    fn is_titlecase(ch: char) -> bool {
        match ch {
            '\u{01C5}'
            | '\u{01C8}'
            | '\u{01CB}'
            | '\u{01F2}'
            | '\u{1F88}'..='\u{1F8F}'
            | '\u{1F98}'..='\u{1F9F}'
            | '\u{1FA8}'..='\u{1FAF}'
            | '\u{1FBC}'
            | '\u{1FCC}'
            | '\u{1FFC}' => true,
            _ => false,
        }
    }

    fn is_ruby_special_uppercase(ch: char) -> bool {
        match ch {
            '\u{2160}'..='\u{216F}'
            | '\u{24B6}'..='\u{24CF}'
            | '\u{1F130}'..='\u{1F149}'
            | '\u{1F150}'..='\u{1F169}'
            | '\u{1F170}'..='\u{1F189}' => true,
            _ => false,
        }
    }

    ch.is_uppercase() || is_titlecase(ch) || is_ruby_special_uppercase(ch)
}

fn is_jisx0208_const_starter(b0: u8, b1: u8) -> bool {
    match (b0, b1) {
        (0xA3, 0xC1..=0xDA) | (0xA6, 0xA1..=0xB8) | (0xA7, 0xA1..=0xC1) => true,
        _ => false,
    }
}
