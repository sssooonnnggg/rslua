#[macro_export]
macro_rules! success {
    ($result:expr) => {
        Ok(Some($result))
    }
}

#[macro_export]
macro_rules! debuggable {
    () => {
        pub fn set_debug(&mut self, debug: bool) {
            self.debug = debug;
        }
        pub fn is_debug(&self) -> bool {
            self.debug
        }
    };
}

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
