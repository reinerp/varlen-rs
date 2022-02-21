#[cfg(feature = "doc")]
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
macro_rules! make_svgbobdoc {
    (
        $( #![doc = $d:literal] )*
    ) => (
        core::concat!( $( $d, '\n' ),* )
    );
    (
        $( #[doc = $d:literal] )*
    ) => (
        core::concat!( $( $d, '\n' ),* )
    );
}

pub(crate) use make_svgbobdoc;
