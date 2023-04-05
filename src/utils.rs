pub fn success<T, E>(result: T) -> Result<Option<T>, E> {
    Ok(Some(result))
}
