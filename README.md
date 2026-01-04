# Lazarus/Free Pascal + Rust Integration Tutorial

Tutorial and examples of the integration between Rust and Lazarus/Free Pascal.

In RustLaz there's a Rust crate that produces a library, while in the LazRust folders there are examples of the use of such library in Lazarus.

Once compiled the Rust library in release mode `cargo build --release` the obtained libray must be put in the same folder as the lazarus executable in Windows, or copied in */usr/lib/* (or wherever visible by the linker) in a Linux system.

