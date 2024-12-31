use magnus::error::Result as RbResult;
use magnus::{function, RString, Ruby};

#[magnus::init(name = "ruty")]
fn init(ruby: &Ruby) -> RbResult<()> {
    let module = ruby.define_module("Ruty")?;

    module.define_module_function("erase_type", function!(rb_erase_type, 1))?;
    Ok(())
}

fn rb_erase_type(ruby: &Ruby, src: RString) -> RbResult<RString> {
    let src = RString::new_frozen(src);
    let slice = unsafe { src.as_slice() };
    let mut diag = Vec::new();
    // TODO: handle diagnostics
    let program = ruty::parse(&mut diag, slice);
    let erased = ruty::erase_type(slice, &program);
    Ok(ruby.str_from_slice(&erased))
}
