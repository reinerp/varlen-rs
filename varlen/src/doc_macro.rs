#[cfg(feature = "doc")]
#[macro_export]
macro_rules! make_svgbobdoc {
    (
        $($t:tt)*
    ) => (
        svgbobdoc::transform!(
            $($t)*
        )
    )
}

#[cfg(not(feature = "doc"))]
#[macro_export]
macro_rules! make_svgbobdoc {
    (
        $( #![doc = $d:literal] )*
    ) => (
        std::concat!( $( $d, '\n' ),* )
    );
    (
        $( #[doc = $d:literal] )*
    ) => (
        std::concat!( $( $d, '\n' ),* )
    );
}