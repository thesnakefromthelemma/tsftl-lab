use calculus_lib::extrapolate;
use num_traits::cast::FromPrimitive;
use rand::random;

fn main () {
    let n: usize = 250000;
    let mut vec: Vec<u64> = Vec::with_capacity(n);
    for _ in 0..n {
        vec.push(random());
    }
    println!("{:?}", extrapolate(vec, FromPrimitive::from_usize(n + 1).unwrap()))
}