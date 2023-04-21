pub trait Error {
    fn what(&self) -> &str;
}