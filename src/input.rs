use crate::mmu::Mmu;

const JOYP_ADDRESS: usize = 0xff00;

#[derive(Debug)]
pub enum Button {
    Up,
    Down,
    Left,
    Right,
    A,
    B,
    Select,
    Start,
}

#[derive(Default)]
pub struct Input {
    up: bool,
    down: bool,
    left: bool,
    right: bool,
    a: bool,
    b: bool,
    select: bool,
    start: bool,
}

macro_rules! check_joyp {
    ($name:ident, $bit:expr) => {
        fn $name(&self, mmu: &Mmu) -> bool {
            mmu.check_bit(JOYP_ADDRESS, $bit)
        }
    };
}

macro_rules! set_joyp_to {
    ($name:ident, $bit:expr) => {
        fn $name(&self, value: bool, mmu: &mut Mmu) {
            mmu.set_bit_to(JOYP_ADDRESS, $bit, value);
        }
    };
}

impl Input {
    pub fn button_pressed(&mut self, button: &Button) {
        match button {
            Button::Up => self.up = true,
            Button::Down => self.down = true,
            Button::Left => self.left = true,
            Button::Right => self.right = true,
            Button::A => self.a = true,
            Button::B => self.b = true,
            Button::Select => self.select = true,
            Button::Start => self.start = true,
        }
    }

    pub fn button_released(&mut self, button: &Button) {
        match button {
            Button::Up => self.up = false,
            Button::Down => self.down = false,
            Button::Left => self.left = false,
            Button::Right => self.right = false,
            Button::A => self.a = false,
            Button::B => self.b = false,
            Button::Select => self.select = false,
            Button::Start => self.start = false,
        }
    }

    check_joyp!(check_p14, 4);
    check_joyp!(check_p15, 5);

    set_joyp_to!(set_p10, 0);
    set_joyp_to!(set_p11, 1);
    set_joyp_to!(set_p12, 2);
    set_joyp_to!(set_p13, 3);
    set_joyp_to!(set_p14, 4);
    set_joyp_to!(set_p15, 5);

    pub fn tick(&self, mmu: &mut Mmu) {
        if !self.check_p14(mmu) {
            self.set_p10(!self.right, mmu);
            self.set_p11(!self.left, mmu);
            self.set_p12(!self.up, mmu);
            self.set_p13(!self.down, mmu);
            self.set_p14(true, mmu);
        }

        if !self.check_p15(mmu) {
            self.set_p10(!self.a, mmu);
            self.set_p11(!self.b, mmu);
            self.set_p12(!self.select, mmu);
            self.set_p13(!self.start, mmu);
            self.set_p15(true, mmu);
        }
    }
}
