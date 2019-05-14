#[macro_export]
macro_rules! impl_entity {
    ($name: ident) => {
        impl crate::ir::Entity for $name
        {
            fn new(idx: impl Into<usize>) -> $name { $name(idx.into() as _) }

            fn idx(self) -> usize { self.0 as usize }
        }
    };
}
