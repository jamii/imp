macro_rules! time {
    ( $name:expr, $body:expr ) => {
        {
            let start = ::std::time::Instant::now();
            let result = $body;
            let elapsed = start.elapsed();
            debug!(
                "Did {} in {} ms",
                $name,
                ((elapsed.as_secs() as f64) * 1_000.0) + ((elapsed.subsec_nanos() as f64) / 1_000_000.0)
            );
            result
        }
    };
}
