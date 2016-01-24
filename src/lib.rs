//! Simple implementation of decimal floating point numbers in pure rust.
//!
//! Numbers are represented as mantissa and exponent. Mantissa is stored in
//! an integer that is specified as type parameter, so basic operations should
//! be reasonably fast.
//!
//! If a limited integer type is used for mantissa then mostly all operations
//! may cause integer overflows. While it's what should be expected for +, -,
//! *, /, in current implementation comparisons and equality checks may cause
//! overflows too.
//!
//! Conversions to f32 and f64 are fast and dirty.
#![feature(test)]
extern crate core;
extern crate num;
extern crate rustc_serialize;

use core::cmp::Ordering;
use core::ops::{Add, Sub, Mul, Div};
use core::str::FromStr;
use core::fmt;
use num::{Integer, pow};
use num::traits::{FromPrimitive, ToPrimitive, Signed};

pub type Exp = i8;

#[derive(Copy, Clone, Debug, RustcEncodable, RustcDecodable)]
pub struct Decimal<T: Copy + Integer> {
    m: T,   // mantissa
    e: Exp, // exponent
}

impl <T: Copy + Integer + FromPrimitive + ToPrimitive> Decimal<T> {
    /// Create Decimal from mantissa and exponent.
    pub fn from_parts(mantissa: T, exponent: Exp) -> Decimal<T> {
        Decimal {
            m: mantissa,
            e: exponent,
        }
    }

    fn base(self) -> T {
        T::from_u8(10).unwrap()
    }

    fn to_common_exponent(&self, other: &Decimal<T>) -> (T, T, Exp) {
        match self.e.cmp(&other.e) {
            Ordering::Equal => (self.m, other.m, self.e),
            Ordering::Less => {
                let k = pow(self.base(), (other.e - self.e) as usize);
                (self.m, other.m * k, self.e)
            },
            Ordering::Greater => {
                let k = pow(self.base(), (self.e - other.e) as usize);
                (self.m * k, other.m, other.e)
            }
        }
    }

    /// Convert to f64, possibly with precision loss and overflows. Currently
    /// conversion isn't perfect, i. e. it may be inexact even when exact
    /// conversion is possible.
    pub fn to_f64(&self) -> f64 {
        self.m.to_f64().map(|x| x * 10f64.powi(self.e as i32))
            .unwrap_or(std::f64::NAN)
    }

    /// Convert to f32, possibly with precision loss and overflows. Currently
    /// conversion isn't perfect, i. e. it may be inexact even when exact
    /// conversion is possible.
    pub fn to_f32(&self) -> f32 {
        self.m.to_f32().map(|x| x * 10f32.powi(self.e as i32))
            .unwrap_or(std::f32::NAN)
    }
}

impl <T> fmt::Display for Decimal<T>
    where T: Copy + Integer + FromPrimitive + ToPrimitive + fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.e == 0 {
            write!(f, "{}", self.m)
        } else if self.e > 0 {
            write!(f, "{}{:02$}", self.m, 0, self.e as usize)
        } else {
            let exp = pow(self.base(), (-self.e) as usize);
            let (m_abs, sign) = if self.m < T::zero() {
                (T::zero() - self.m, "-")
            } else {
                (self.m, "")
            };
            let (int_part, frac_part) = m_abs.div_mod_floor(&exp);
            write!(f, "{}{}.{:03$}", sign, int_part, frac_part,
                   (-self.e) as usize)
        }
    }
}

impl <T> PartialEq for Decimal<T>
    where T: Copy + Integer + FromPrimitive + ToPrimitive {

    fn eq(&self, other: &Self) -> bool {
        let (m1, m2, _) = self.to_common_exponent(other);
        m1 == m2
    }
}

impl <T> Eq for Decimal<T>
    where T: Copy + Integer + FromPrimitive + ToPrimitive { }


impl <T> PartialOrd for Decimal<T>
    where T: Copy + Integer + FromPrimitive + ToPrimitive {

    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl <T> Ord for Decimal<T>
    where T: Copy + Integer + FromPrimitive + ToPrimitive {

    fn cmp(&self, other: &Self) -> Ordering {
        let (m1, m2, _) = self.to_common_exponent(other);
        m1.cmp(&m2)
    }
}

#[derive(Debug)]
pub struct ParseDecimalError;

impl <T> FromStr for Decimal<T>
    where T: Copy + Integer + Signed + FromPrimitive + ToPrimitive + FromStr {

    type Err = ParseDecimalError;

    /// TODO: better validation and proper error handling.
    /// Now all valid inputs can be parsed, but some invalid also pass.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimmed = s.trim();
        if trimmed.is_empty() {
            return Err(ParseDecimalError);
        }

        let positive = trimmed.trim_left_matches('-');
        let is_negative = trimmed.len() != positive.len();

        let mut p = positive.split('.');
        let (p1, p2, p3) = (p.next(), p.next(), p.next());

        if p3.is_some() {
            return Err(ParseDecimalError);
        }

        let mut res = match p1 {
            Some(s) => if s == "" || s == "-" {
                Decimal::from_parts(T::from_u8(0).unwrap(), 0)
            } else {
                try!(T::from_str(s).map(|x| Decimal::from_parts(x, 0))
                     .map_err(|_| ParseDecimalError))
            },
            None => return Err(ParseDecimalError),
        };

        if let Some(s) = p2 {
            let y = try!(T::from_str(s).map_err(|_| ParseDecimalError));
            res = res + Decimal::from_parts(y, -(s.len() as Exp));
        }

        if is_negative {
            res.m = -res.m;
        }
        return Ok(res);
    }

}

impl <U, T> From<U> for Decimal<T>
    where U: Integer,
          T: Copy + Integer + FromPrimitive + ToPrimitive + From<U> {

    fn from(u: U) -> Self {
        Decimal::from_parts(T::from(u), 0)
    }
}


impl <T> Add for Decimal<T>
    where T: Copy + Integer + FromPrimitive + ToPrimitive {

    type Output = Decimal<T>;
    fn add(self, other: Self) -> Self {
        let (m1, m2, e) = self.to_common_exponent(&other);
        Self::from_parts(m1 + m2, e)
    }
}

impl <T> Sub for Decimal<T>
    where T: Copy + Integer + FromPrimitive + ToPrimitive {

    type Output = Decimal<T>;
    fn sub(self, other: Self) -> Self {
        let (m1, m2, e) = self.to_common_exponent(&other);
        Self::from_parts(m1 - m2, e)
    }
}

impl <T> Mul for Decimal<T>
    where T: Copy + Integer + FromPrimitive + ToPrimitive {

    type Output = Decimal<T>;
    fn mul(self, other: Self) -> Self {
        Self::from_parts(self.m * other.m, self.e + other.e)
    }
}

impl <T> Div for Decimal<T>
    where T: Copy + Integer + FromPrimitive + ToPrimitive {

    type Output = Decimal<T>;
    fn div(self, other: Self) -> Self {
        Self::from_parts(self.m / other.m, self.e - other.e)
    }
}

#[cfg(test)]
mod test {
    extern crate test;
    use self::test::Bencher;

    use std::str::FromStr;
    use super::Decimal;

    fn d(s: &str) -> Decimal<i64> {
        Decimal::from_str(s).unwrap()
    }

    fn d32(s: &str) -> Decimal<i32> {
        Decimal::from_str(s).unwrap()
    }


    #[test]
    fn test_from_str() {
        assert_eq!(d("0"), Decimal::<i64>::from_parts(0, 0));
        assert_eq!(d("547"), Decimal::<i64>::from_parts(547, 0));
        assert_eq!(d("1.14"), Decimal::<i64>::from_parts(114, -2));
        assert_eq!(d(".04"), Decimal::<i64>::from_parts(4, -2));
        assert_eq!(d("55.04"), Decimal::<i64>::from_parts(5504, -2));
        assert_eq!(d("550"), Decimal::<i64>::from_parts(55, 1));

        assert_eq!(d("-0"), Decimal::<i64>::from_parts(-0, 0));
        assert_eq!(d("-547"), Decimal::<i64>::from_parts(-547, 0));
        assert_eq!(d("-1.14"), Decimal::<i64>::from_parts(-114, -2));
        assert_eq!(d("-.04"), Decimal::<i64>::from_parts(-4, -2));
        assert_eq!(d("-55.04"), Decimal::<i64>::from_parts(-5504, -2));
        assert_eq!(d("-550"), Decimal::<i64>::from_parts(-55, 1));
    }

    #[test]
    fn test_eq_and_ord() {
        assert_eq!(d("543.54"), d("543.540"));
        assert_eq!(d("543.5400"), d("543.540"));
        assert_eq!(d("5430"), d("5430.000"));

        assert!(d("543.54") <= d("543.540"));
        assert!(d("1.5400") < d("543.540"));
        assert!(d("543") > d("1.2344"));
    }

    #[test]
    fn test_add() {
        assert_eq!(d("0.011") + d("11.32"), d("11.331"));
        assert_eq!(d("40") + d(".56"), d("40.56"));
    }

    #[test]
    fn test_sub() {
        assert_eq!(d("0.011") - d("11.32"), d("-11.309"));
        assert_eq!(d("11") - d("10.1"), d("0.9"));
    }

    #[test]
    fn test_mul() {
        assert_eq!(d("0.10") * d("11.3"), d("1.130"));
        assert_eq!(d("100") * d("11.3"), d("1130"));
        assert_eq!(format!("{}", d("11.3") * d("0.100")), "1.1300");
        assert_eq!(format!("{}", d("11.30") * d("0.1000")), "1.130000");
    }

    #[test]
    fn test_div() {
        assert_eq!(d("0.112") / d("0.001"), d("112"));
        assert_eq!(format!("{}", d("11.35") / d("0.01")), "1135");
        assert_eq!(format!("{}", d("11.36") / d("10")), "1.13");
    }

    #[test]
    fn test_to_float() {
        assert!((d("0.125").to_f64() - 0.125f64).abs() < ::std::f64::EPSILON);
        assert!((d("10.25").to_f32() - 10.25f32).abs() < 20.0 * ::std::f32::EPSILON);
    }

    #[test]
    fn test_display() {
        let t = |s| assert_eq!(format!("{}", d(s)), s);
        t("-123.45");
        t("123");
        t("120.45");
        t("120.450");
        t("0.0034");
        t("-123");
        t("-120.450");
        t("-0.0034");
        assert_eq!(format!("{}", Decimal::<i32>::from_parts(123, 3)), "123000");
        // assert_eq!(format!("{}", d("0.125")), "0.125");
        assert_eq!(format!("{}", d("-1.88")), "-1.88");
    }

    #[bench]
    fn bench_sum_1000_aligned_deci64(b: &mut Bencher) {
        let x = d("0.112");
        let y = d("34.349");
        b.iter(|| (0..1000).fold(x, |s, _| s + y));
    }

    #[bench]
    fn bench_sum_1000_aligned_deci32(b: &mut Bencher) {
        let x = d32("0.112");
        let y = d32("34.349");
        b.iter(|| (0..1000).fold(x, |s, _| s + y));
    }

    #[bench]
    fn bench_sum_1000_unaligned_deci64(b: &mut Bencher) {
        let x = d("0.112");
        let y = d("34.3");
        b.iter(|| (0..1000).fold(x, |s, _| s + y));
    }

    #[bench]
    fn bench_sum_1000_f32(b: &mut Bencher) {
        let x: f32 = 0.112;
        let y: f32 = 34.349;
        b.iter(|| (0..1000).fold(x, |s, _| s + y));
    }

    #[bench]
    fn bench_sum_1000_f64(b: &mut Bencher) {
        let x: f64 = 0.112;
        let y: f64 = 34.349;
        b.iter(|| (0..1000).fold(x, |s, _| s + y));
    }

    #[bench]
    fn bench_mul_1000_f64(b: &mut Bencher) {
        let x: f64 = 10.112;
        let y: f64 = 0.995;
        b.iter(|| (0..1000).fold(x, |s, _| s * y));
    }

    #[bench]
    fn bench_mul_1000_deci64(b: &mut Bencher) {
        let x = d("10.3");
        let y = d("2");
        // overflows
        b.iter(|| (0..1000).fold(x, |s, _| s * y));
    }

    #[bench]
    fn bench_format_deci64(b: &mut Bencher) {
        let x = d("-10.33");
        b.iter(|| format!("{}", x));
    }

    #[bench]
    fn bench_format_f64(b: &mut Bencher) {
        let x: f64 = -10.33;
        b.iter(|| format!("{}", x));
    }

    #[bench]
    fn bench_parse_deci64(b: &mut Bencher) {
        b.iter(|| Decimal::<i64>::from_str("-10.33").unwrap());
    }

    #[bench]
    fn bench_parse_f64(b: &mut Bencher) {
        b.iter(|| f64::from_str("-10.33").unwrap());
    }
}
