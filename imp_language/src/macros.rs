#[allow(unused_macros)]
macro_rules! d {
    ($e:expr) => {{
        let val = $e;
        ::log::debug!("{} = {:#?}", stringify!($e), val);
        val
    }};
}
