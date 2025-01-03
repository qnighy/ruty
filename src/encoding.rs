mod catalog;
mod charplus;
mod estring;
mod iface;
mod impls;

pub use catalog::Encoding;
pub use charplus::CharPlus;
pub use estring::EString;
use iface::{EncNext, EncodingImpl};

impl Encoding {
    pub fn name(&self) -> &'static str {
        self.aliases()[0]
    }

    pub fn aliases(&self) -> &'static [&'static str] {
        self.aliases_impl()
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
