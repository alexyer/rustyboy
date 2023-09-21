use sdl2::{
    event::Event,
    pixels::PixelFormatEnum,
    render::{Canvas, Texture},
    video::Window,
    EventPump,
};

use crate::{
    frame_buffer::FrameBuffer,
    ppu::{GAMEBOY_HEIGHT, GAMEBOY_WIDTH},
};

const PIXEL_SIZE: usize = 2;
const WIDTH: usize = GAMEBOY_WIDTH * PIXEL_SIZE;
const HEIGHT: usize = GAMEBOY_HEIGHT * PIXEL_SIZE;

pub trait Screen {
    fn update(&mut self, frame_buffer: &FrameBuffer);
    fn poll_events(&mut self) -> bool;
}

#[derive(Default)]
pub struct Headless;

impl Screen for Headless {
    fn update(&mut self, _frame_buffer: &FrameBuffer) {
        return;
    }

    fn poll_events(&mut self) -> bool {
        return false;
    }
}

pub struct Sdl {
    canvas: Canvas<Window>,
    events: EventPump,
    texture: Texture<'static>,
}

impl Default for Sdl {
    fn default() -> Self {
        let sdl_context = sdl2::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();

        let window = video_subsystem
            .window("Rustyboy16", WIDTH as u32, HEIGHT as u32)
            .position_centered()
            .opengl()
            .build()
            .unwrap();

        let canvas = window.into_canvas().build().unwrap();
        let events = sdl_context.event_pump().unwrap();

        let creator = canvas.texture_creator();
        let texture = creator
            .create_texture(
                PixelFormatEnum::ARGB8888,
                sdl2::render::TextureAccess::Streaming,
                WIDTH as u32,
                HEIGHT as u32,
            )
            .unwrap();

        let texture = unsafe { std::mem::transmute::<_, Texture<'static>>(texture) };

        Self {
            canvas,
            events,
            texture,
        }
    }
}

impl Screen for Sdl {
    fn update(&mut self, frame_buffer: &FrameBuffer) {
        self.texture
            .with_lock(None, |buffer, pitch| {
                for y in 0..GAMEBOY_HEIGHT {
                    for x in 0..GAMEBOY_WIDTH {
                        let color: sdl2::pixels::Color = frame_buffer.get_pixel(x, y).into();

                        for w in 0..PIXEL_SIZE {
                            for h in 0..PIXEL_SIZE {
                                let offset =
                                    (y * PIXEL_SIZE + h) * pitch + (x * PIXEL_SIZE + w) * 4;

                                buffer[offset] = color.b;
                                buffer[offset + 1] = color.g;
                                buffer[offset + 2] = color.r;
                                buffer[offset + 3] = color.a;
                            }
                        }
                    }
                }
            })
            .unwrap();

        self.canvas.clear();
        self.canvas.copy(&self.texture, None, None).unwrap();
        self.canvas.present();
    }

    fn poll_events(&mut self) -> bool {
        let mut should_quit = false;

        while let Some(event) = self.events.poll_event() {
            match event {
                Event::Quit { timestamp: _ } => should_quit = true,
                _ => (),
            };
        }

        should_quit
    }
}
