pub fn lcm(iter: impl Iterator<Item = usize>) -> usize {
    iter.fold(1, |acc, x| acc * x / gcd(acc, x))
}

pub fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a
}
