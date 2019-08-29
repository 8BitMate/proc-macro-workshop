// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

fn main() {
    use derive_debug::CustomDebug;
    use core::marker::PhantomData;

    #[derive(Debug)]
    pub struct Field<T> {
        name: &'static str,
        bitmask: u16,
        phantom: PhantomData<T>,
    }

    println!(
        "{:#?}",
        Field {
            name: "string",
            bitmask: 0b00001101,
            phantom: Default::default()
        } as Field<()>
    )
}
