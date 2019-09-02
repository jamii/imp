#[allow(unused_macros)]
macro_rules! d {
    ($e:expr) => {{
        let val = $e;
        info!("{} = {:#?}", stringify!($e), val);
        val
    }};
}
