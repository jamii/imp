#[allow(unused_macros)]
macro_rules! d {
    ($e:expr) => {{
        let val = $e;
        ::log::info!("{} = {:#?}", stringify!($e), val);
        val
    }};
}
