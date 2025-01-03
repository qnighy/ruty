use magnus::encoding::EncodingCapable;
use magnus::error::Error as RbError;
use magnus::error::Result as RbResult;
use magnus::{function, RString, Ruby};
use ruty::encoding::EStrRef;

#[magnus::init(name = "ruty")]
fn init(ruby: &Ruby) -> RbResult<()> {
    let module = ruby.define_module("Ruty")?;

    module.define_module_function("erase_type", function!(rb_erase_type, 1))?;
    Ok(())
}

fn rb_erase_type(ruby: &Ruby, src: RString) -> RbResult<RString> {
    let src = RString::new_frozen(src);
    let slice = unsafe { src.as_slice() };
    let enc = find_encoding(ruby, src.enc_get())?;
    let mut diag = Vec::new();
    // TODO: handle diagnostics
    let program = ruty::parse(&mut diag, EStrRef::from_bytes(slice, enc));
    let erased = ruty::erase_type(slice, &program);
    Ok(ruby.str_from_slice(&erased))
}

fn find_encoding(ruby: &Ruby, idx: magnus::encoding::Index) -> RbResult<ruty::Encoding> {
    let enc = magnus::encoding::RbEncoding::from(idx);
    let name = enc.name();
    if let Some(enc) = ruty::Encoding::find(name) {
        Ok(enc)
    } else {
        Err(RbError::new(ruby.exception_arg_error(), "unknown encoding"))
    }
}
