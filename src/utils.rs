#[macro_export]
macro_rules! check_bit {
    ($val:expr, $i:expr) => {
        (($val & 1 << $i) != 0)
    };
}

#[macro_export]
macro_rules! get_bit {
    ($val:expr, $i:expr) => {
        if check_bit!($val, $i) {
            1
        } else {
            0
        }
    };
}
