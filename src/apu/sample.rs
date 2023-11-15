use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
pub struct Sample {
    value: f32,
}

impl Sample {
    pub fn as_f32(&self) -> f32 {
        self.value
    }

    pub fn from_raw_sample(sample: u8) -> Self {
        if sample == 0 {
            Default::default()
        }

        let unit_sample = f32::from(sample) / 15.0;
        let scaled_sample = unit_sample * 2.0 - 1.0;

        Self {
            value: scaled_sample,
        }
    }
}

impl From<f32> for Sample {
    fn from(value: f32) -> Self {
        Self { value }
    }
}

impl From<Sample> for f32 {
    fn from(sample: Sample) -> Self {
        sample.value
    }
}

impl From<u8> for Sample {
    fn from(value: u8) -> Self {
        let value = f32::try_from(value).unwrap();

        Sample::from(value)
    }
}

impl Add for Sample {
    type Output = Sample;

    fn add(self, rhs: Self) -> Self::Output {
        Sample {
            value: self.value + rhs.value,
        }
    }
}

impl Sub<f32> for Sample {
    type Output = Sample;

    fn sub(self, rhs: f32) -> Self::Output {
        Sample {
            value: self.value - rhs,
        }
    }
}

impl Div<f32> for Sample {
    type Output = Sample;

    fn div(self, rhs: f32) -> Self::Output {
        Sample {
            value: self.value / rhs,
        }
    }
}

impl Mul<f32> for Sample {
    type Output = Sample;

    fn mul(self, rhs: f32) -> Self::Output {
        Sample {
            value: self.value / rhs,
        }
    }
}

pub type RawSample = u8;
