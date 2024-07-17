use std::ops::{Deref, DerefMut};

pub struct Peek2<I: Iterator> {
    iter: I,
    peeks: Option<(I::Item, Option<I::Item>)>,
}

impl<I: Iterator> Peek2<I> {
    pub fn new(iter: I) -> Self {
        Self { iter, peeks: None }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        if let Some((ref peeked, _)) = self.peeks {
            return Some(peeked);
        }
        let el = self.iter.next()?;
        Some(&self.peeks.insert((el, None)).0)
    }

    pub fn peek2(&mut self) -> Option<&I::Item> {
        if let Some((_, ref mut peek2)) = self.peeks {
            if let Some(ref peeked) = peek2 {
                return Some(peeked);
            }
            let el = self.iter.next()?;
            Some(peek2.insert(el))
        } else {
            let el1 = self.iter.next()?;
            let el2 = self.iter.next()?;
            Some(self.peeks.insert((el1, Some(el2))).1.as_ref().unwrap())
        }
    }
}

impl<I: Iterator> Deref for Peek2<I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.iter
    }
}

impl<I: Iterator> DerefMut for Peek2<I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.iter
    }
}

impl<I: Iterator> Iterator for Peek2<I> {
    type Item = I::Item;

    #[inline]
    fn next(&mut self) -> Option<I::Item> {
        match self.peeks.take() {
            Some((peek1, Some(peek2))) => {
                self.peeks = Some((peek2, None));
                Some(peek1)
            }
            Some((peek1, None)) => Some(peek1),
            None => self.iter.next(),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let peek_len = match self.peeks {
            Some((_, Some(_))) => 2,
            Some((_, None)) => 1,
            None => 0,
        };
        let (lo, hi) = self.iter.size_hint();
        let lo = lo.saturating_add(peek_len);
        let hi = match hi {
            Some(x) => x.checked_add(peek_len),
            None => None,
        };
        (lo, hi)
    }
}

pub trait ToPeek2: Iterator {
    fn to_peek2(self) -> Peek2<Self>
    where
        Self: Sized,
    {
        Peek2::new(self)
    }
}

impl<I: Iterator<Item = T>, T> ToPeek2 for I {}

#[cfg(test)]
mod tests {
    use rand::Rng;

    use super::*;

    #[test]
    fn peek2() {
        const DATA: [u8; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let mut cur = 0;
        let mut peek2 = DATA.iter().to_peek2();
        assert_eq!(DATA[cur], *peek2.next().unwrap());
        cur += 1;
        assert_eq!(DATA[cur], **peek2.peek().unwrap());
        assert_eq!(DATA[cur], **peek2.peek().unwrap());
        assert_eq!(DATA[cur], *peek2.next().unwrap());
        cur += 1;
        assert_eq!(DATA[cur], *peek2.next().unwrap());
        cur += 1;
        assert_eq!(DATA[cur + 1], **peek2.peek2().unwrap());
        assert_eq!(DATA[cur + 1], **peek2.peek2().unwrap());
        assert_eq!(DATA[cur], **peek2.peek().unwrap());
        assert_eq!(DATA[cur], *peek2.next().unwrap());
    }

    const ITERATIONS_OR_SIZE: usize = 1000000;

    #[test]
    fn random() {
        let data: Vec<usize> = (0..ITERATIONS_OR_SIZE).collect();
        let mut ptr = 0;
        let mut peek = data.iter().to_peek2();
        for _ in 0..ITERATIONS_OR_SIZE {
            if ptr >= data.len() - 1 {
                break;
            }
            let op = rand::thread_rng().gen_range(0..3);
            match op {
                0 => {
                    assert_eq!(data[ptr], *peek.next().unwrap());
                    ptr += 1;
                }
                1 => assert_eq!(data[ptr], **peek.peek().unwrap()),
                2 => assert_eq!(data[ptr + 1], **peek.peek2().unwrap()),
                _ => unreachable!(),
            }
        }
    }
}
