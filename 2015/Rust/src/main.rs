mod days;

fn main() {
    println!("Hello, world!");

    // It's ok to copy a string literal like this, since the amount of memory required is known at
    // compile time.
    let x = "Hello";
    let y = x;

    println!("x = {}, y = {}", x, y)
}
