pub trait Comments {
    fn get_comments(&self) -> Vec<&str>;
    fn has_comments(&self) -> bool {
        !self.get_comments().is_empty()
    }
}
