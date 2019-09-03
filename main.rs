// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

fn main() {
    use derive_debug::CustomDebug;
    use std::fmt::Debug;

    pub trait Trait {
        type Value;
    }

    #[derive(CustomDebug)]
    pub struct Field<T: Trait> {
        values: Vec<T::Value>,
    }
}
