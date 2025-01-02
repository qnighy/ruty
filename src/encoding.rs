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
