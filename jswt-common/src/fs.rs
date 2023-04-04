use crate::Span;
use lazy_static::lazy_static;
use std::{collections::HashMap, sync::Mutex};

// Re-export canonicalize as part of our public API
pub use std::fs::canonicalize;

lazy_static! {
    /// Cache for file reader to avoid reopening files on disk
    /// We're making use of this as a makeshift Arena
    /// Note: watch for concurrent access during tests as those may be run in parallel
    static ref FILE_CACHE: Mutex<HashMap<String, &'static str>> =
        Mutex::new(HashMap::<String, &'static str>::new());
}

/// Replacement for fs that returns a reference to a cached
/// version of a file which has been read from disk
pub fn read_to_string<T: AsRef<str>>(path: &T) -> &'static str {
    FILE_CACHE
        .lock()
        .unwrap()
        .entry(path.as_ref().to_string())
        .or_insert_with(|| {
            Box::leak(
                std::fs::read_to_string(&path.as_ref())
                    .unwrap()
                    .into_boxed_str(),
            )
        })
}

/// Checks if the file exists in the cache
pub fn is_cached(path: &str) -> bool {
    FILE_CACHE.lock().unwrap().contains_key(path)
}

/// Inserts a value into the file cache and returns a
/// reference to the value inserted
pub fn cache(key: &str, value: String) {
    let _ = &FILE_CACHE
        .lock()
        .unwrap()
        .insert(key.to_string(), Box::leak(value.into_boxed_str()));
}

/// Returns string slice for the given span
pub fn from_span(span: &Span) -> &'static str {
    let cache = &FILE_CACHE.lock().unwrap();
    let source = cache
        .get(span.file.as_ref())
        .expect(format!("File not found in cache: {}", span.file).as_str());
    &source[span.start..span.end]
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn fs() {
        cache("key", "value".to_string());
    }
}
