/*
struct Cube {
    board: [[[i8; 3]; 3]; 6],
}

fn buildCube() -> Cube {
    return Cube {
        board: <<<0, 0, 0>, <0, 0, 0>, <0, 0, 0>>,
                <<1, 1, 1>, <1, 1, 1>, <1, 1, 1>>,
                <<2, 2, 2>, <2, 2, 2>, <2, 2, 2>>,
                <<3, 3, 3>, <3, 3, 3>, <3, 3, 3>>,
                <<4, 4, 4>, <4, 4, 4>, <4, 4, 4>>,
                <<5, 5, 5>, <5, 5, 5>, <5, 5, 5>>
                >
    };
}
*/

fn main() {
    // Cube myCube = buildCube();

    use std::io::{self, Write};

    io::stdout()
        .lock()
        .write_all(b"Hello, ")
        .unwrap();

    println!("world!");
}
