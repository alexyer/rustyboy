use crate::ppu::Color;

pub struct FrameBuffer {
    width: usize,
    height: usize,
    buffer: Vec<Color>,
}

#[allow(unused)]
impl FrameBuffer {
    pub fn new(width: usize, height: usize) -> Self {
        Self {
            width,
            height,
            buffer: vec![Color::White; width * height],
        }
    }

    pub fn get_pixel(&self, x: usize, y: usize) -> Color {
        self.buffer[y * self.width + x]
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, color: Color) {
        self.buffer[y * self.width + x] = color;
    }

    pub fn as_slice(&self) -> &[Color] {
        &self.buffer
    }

    pub fn reset(&mut self) {
        self.buffer = vec![Color::White; self.width * self.height];
    }
}
