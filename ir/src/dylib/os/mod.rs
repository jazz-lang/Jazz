macro_rules! unix {
    ($item: item) => {
        /// UNIX implementation of dynamic library loading.
        ///
        /// This module should be expanded with more UNIX-specific functionality in the future.
        $item
    }
}

macro_rules! windows {
    ($item: item) => {
        /// Windows implementation of dynamic library loading.
        ///
        /// This module should be expanded with more Windows-specific functionality in the future.
        $item
    }
}

#[cfg(unix)]
unix!(pub mod unix;);
#[cfg(unix)]
windows!(pub mod windows {});

#[cfg(windows)]
windows!(pub mod windows;);
#[cfg(windows)]
unix!(pub mod unix {});