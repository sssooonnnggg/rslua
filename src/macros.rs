#[macro_export]
macro_rules! error {
    ($self:ident, $error_type:ident, $msg:expr) => {
        // panic! when at debug mode, otherwise return Error
        if $self.is_debug() {
            panic!("{}", &$msg);
        } else {
            println!("{}", &$msg);
            Err($error_type($msg))
        }
    };
}
