pub use pretty_assertions::assert_eq;
/// Workaround for https://github.com/colin-kiegel/rust-pretty-assertions/issues/24
/// Until this lands we have to use this as our string assertions
/// 
/// Wrapper around string slice that makes debug output `{:?}` to print string same way as `{}`.
/// Used in different `assert*!` macros in combination with `pretty_assertions` crate to make
/// test failures to show nice diffs.
#[derive(PartialEq, Eq)]
#[doc(hidden)]
pub struct PrettyString<'a>(pub &'a str);

/// Make diff to display string as multi-line string
impl<'a> std::fmt::Debug for PrettyString<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

#[macro_export]
macro_rules! assert_str_eq {
    ($left:expr, $right:expr) => {
        assert_eq!(
            jswt_assert::PrettyString($left),
            jswt_assert::PrettyString($right)
        );
    };
}
