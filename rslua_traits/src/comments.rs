pub trait Comments {
    fn get_comments(&self) -> Vec<&str>;
}