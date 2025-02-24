use std::{fmt, ops::Neg, str::FromStr};

use num_bigint::{BigInt, BigUint};

/// Decimal for Rational literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Decimal {
    fraction: BigInt,
    negative_exponent: u32,
}

impl Decimal {
    pub fn from_fraction_and_exponent(mut fraction: BigInt, mut exponent: i32) -> Self {
        while exponent > 0 {
            fraction *= 10;
            exponent -= 1;
        }
        while exponent < 0 && (&fraction) % 10 == BigInt::ZERO {
            fraction /= 10;
            exponent += 1;
        }
        Decimal {
            fraction,
            negative_exponent: (-exponent) as u32,
        }
    }
    pub fn fraction(&self) -> &BigInt {
        &self.fraction
    }

    pub fn negative_exponent(&self) -> u32 {
        self.negative_exponent
    }

    pub fn numerator(&self) -> &BigInt {
        &self.fraction
    }
    pub fn denominator(&self) -> BigUint {
        BigUint::from(10u32).pow(self.negative_exponent)
    }
}

impl fmt::Display for Decimal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.negative_exponent == 0 {
            return write!(f, "{}", self.fraction);
        }
        let s = self.fraction.to_string();
        let s = if s.starts_with('-') {
            write!(f, "-")?;
            &s[1..]
        } else {
            &s
        };
        if s.len() <= self.negative_exponent as usize {
            write!(f, "0.")?;
            for _ in 0..self.negative_exponent as usize - s.len() {
                write!(f, "0")?;
            }
            write!(f, "{}", s)
        } else {
            let dot_pos = s.len() - self.negative_exponent as usize;
            write!(f, "{}.{}", &s[..dot_pos], &s[dot_pos..])
        }
    }
}

impl FromStr for Decimal {
    type Err = ParseDecimalError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (s, negative) = if s.starts_with('-') {
            (&s[1..], true)
        } else if s.starts_with('+') {
            (&s[1..], false)
        } else {
            (s, false)
        };
        let (int_part, frac_part) = if let Some(dot_pos) = s.find('.') {
            (&s[..dot_pos], &s[dot_pos + 1..])
        } else {
            (s, "")
        };
        if int_part.is_empty() && frac_part.is_empty() {
            return Err(ParseDecimalError(()));
        }
        let mut fraction = if int_part.is_empty() {
            BigInt::ZERO
        } else {
            BigInt::from_str(int_part).map_err(|_| ParseDecimalError(()))?
        };
        let mut exponent = 0;
        for &b in frac_part.as_bytes() {
            if b < b'0' || b > b'9' {
                return Err(ParseDecimalError(()));
            }
            fraction *= 10;
            fraction += BigInt::from((b - b'0') as i32);
            exponent -= 1;
        }
        if negative {
            fraction = -fraction;
        }
        Ok(Decimal::from_fraction_and_exponent(fraction, exponent))
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParseDecimalError(());

impl std::error::Error for ParseDecimalError {}
impl fmt::Display for ParseDecimalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "failed to parse decimal")
    }
}

macro_rules! impl_from_integer {
    ($ty:ty) => {
        impl From<$ty> for Decimal {
            fn from(fraction: $ty) -> Self {
                Decimal {
                    fraction: BigInt::from(fraction),
                    negative_exponent: 0,
                }
            }
        }
    };
}

impl_from_integer!(u8);
impl_from_integer!(u16);
impl_from_integer!(u32);
impl_from_integer!(u64);
impl_from_integer!(u128);
impl_from_integer!(i8);
impl_from_integer!(i16);
impl_from_integer!(i32);
impl_from_integer!(i64);
impl_from_integer!(i128);
impl_from_integer!(BigInt);
impl_from_integer!(BigUint);

impl Neg for Decimal {
    type Output = Decimal;

    fn neg(self) -> Decimal {
        Decimal {
            fraction: -self.fraction,
            negative_exponent: self.negative_exponent,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_construct_reduce_positive_exponent() {
        assert_eq!(
            Decimal::from_fraction_and_exponent(BigInt::from(123), 2),
            Decimal::from_fraction_and_exponent(BigInt::from(12300), 0),
        );
    }

    #[test]
    fn test_construct_reduce_negative_exponent_small() {
        assert_eq!(
            Decimal::from_fraction_and_exponent(BigInt::from(1230000), -2),
            Decimal::from_fraction_and_exponent(BigInt::from(12300), 0),
        );
    }

    #[test]
    fn test_construct_reduce_negative_exponent_large() {
        assert_eq!(
            Decimal::from_fraction_and_exponent(BigInt::from(12300), -4),
            Decimal::from_fraction_and_exponent(BigInt::from(123), -2),
        );
    }

    #[test]
    fn test_display_zero() {
        let decimal = Decimal::from(0);
        assert_eq!(decimal.to_string(), "0");
    }

    #[test]
    fn test_display_integer_positive() {
        let decimal = Decimal::from(123);
        assert_eq!(decimal.to_string(), "123");
    }

    #[test]
    fn test_display_integer_negative() {
        let decimal = Decimal::from(-123);
        assert_eq!(decimal.to_string(), "-123");
    }

    #[test]
    fn test_display_small_positive() {
        let decimal = Decimal::from_fraction_and_exponent(BigInt::from(123), -5);
        assert_eq!(decimal.to_string(), "0.00123");
    }

    #[test]
    fn test_parse_integer_zero_unsigned() {
        let decimal: Decimal = "0".parse().unwrap();
        assert_eq!(decimal.to_string(), "0");
    }

    #[test]
    fn test_parse_integer_zero_signed() {
        let decimal: Decimal = "+0".parse().unwrap();
        assert_eq!(decimal.to_string(), "0");
    }

    #[test]
    fn test_parse_integer_positive_unsigned() {
        let decimal: Decimal = "123".parse().unwrap();
        assert_eq!(decimal.to_string(), "123");
    }

    #[test]
    fn test_parse_integer_positive_signed() {
        let decimal: Decimal = "+123".parse().unwrap();
        assert_eq!(decimal.to_string(), "123");
    }

    #[test]
    fn test_parse_integer_negative() {
        let decimal: Decimal = "-123".parse().unwrap();
        assert_eq!(decimal.to_string(), "-123");
    }

    #[test]
    fn test_parse_dot_last() {
        let decimal: Decimal = "123.".parse().unwrap();
        assert_eq!(decimal.to_string(), "123");
    }

    #[test]
    fn test_parse_dot_first() {
        let decimal: Decimal = ".123".parse().unwrap();
        assert_eq!(decimal.to_string(), "0.123");
    }

    #[test]
    fn test_parse_small() {
        let decimal: Decimal = "0.00123".parse().unwrap();
        assert_eq!(decimal.to_string(), "0.00123");
    }

    #[test]
    fn test_parse_large() {
        let decimal: Decimal = "123000000".parse().unwrap();
        assert_eq!(decimal.to_string(), "123000000");
    }
}
