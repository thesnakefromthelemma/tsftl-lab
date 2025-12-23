use std::cmp::Ord;
use num_traits::Num;
use num_traits::cast::FromPrimitive;
use std::iter::zip;

pub fn diff<T: Copy + Num>(vec: &mut Vec<T>) -> Option<T> {
    let vec_diff_length = vec.len();
    if vec_diff_length >= 1 {
        let vec0 = vec[0];
        let mut cur = vec0;
        for i in 0..(vec_diff_length - 1) {
            let next = vec[i + 1];
            vec[i] = next - cur;
            cur = next;
        }
        vec.truncate(vec_diff_length - 1);
        Some(vec0)
    } else {
        None
    }
}

pub struct Taylor<'a, T> {
    pub vec: &'a mut Vec<T>
}

impl<T: Copy + Num> Iterator for Taylor<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        diff(self.vec)
    }
}

pub struct Pascal<T: Copy + Ord + Num + FromPrimitive> {
    pub row: i64,
    pub ind: i64,
    pub val: T
}

// Only works if self.row >= 0
impl<T: Copy + Ord + Num + FromPrimitive> Iterator for Pascal<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.row >= self.ind {
            let val0 = self.val;
            self.val = match (FromPrimitive::from_i64(self.row - self.ind + 1), FromPrimitive::from_i64(self.ind)) {
                (Some(num), Some(den)) => val0 * num / den,
                _ => panic!("pas-kill issue"),
            };
            self.ind = self.ind + 1;
            Some(val0)
        } else {
            None
        }
    }
}

pub fn extrapolate<T: Copy + Ord + Num + FromPrimitive>(mut vec: Vec<T>, arg: i64) -> T {
    let taylor = Taylor { vec: &mut vec };
    let pasc_val: T = FromPrimitive::from_i64(1).expect("extrapolate failed");
    let pascal = Pascal {
        row: arg,
        ind: 1,
        val: pasc_val
    };
    let zip = zip(taylor, pascal);
    let mut res: T = FromPrimitive::from_i64(0).expect("extrapolate failed");
    for (coeff, eval) in zip {
        res = res + coeff * eval;
    };
    res
}