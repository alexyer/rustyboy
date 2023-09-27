use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::PixelFormatEnum,
    render::{Canvas, Texture},
    video::Window,
    EventPump,
};

use crate::{
    frame_buffer::FrameBuffer,
    gb::GameBoyEvent,
    input::Button,
    ppu::{GAMEBOY_HEIGHT, GAMEBOY_WIDTH},
};

const PIXEL_SIZE: usize = 2;
const WIDTH: usize = GAMEBOY_WIDTH * PIXEL_SIZE;
const HEIGHT: usize = GAMEBOY_HEIGHT * PIXEL_SIZE;

pub trait Renderer {
    fn update(&mut self, frame_buffer: &FrameBuffer);
    fn poll_events(&mut self) -> Option<GameBoyEvent>;
}

#[derive(Default)]
pub struct Headless;

impl Renderer for Headless {
    fn update(&mut self, _frame_buffer: &FrameBuffer) {}

    fn poll_events(&mut self) -> Option<GameBoyEvent> {
        None
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

impl Renderer for Sdl {
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

    fn poll_events(&mut self) -> Option<GameBoyEvent> {
        if let Some(event) = self.events.poll_event() {
            let event = match event {
                Event::KeyDown { keycode, .. } => match keycode {
                    Some(Keycode::Up) => Some(GameBoyEvent::ButtonPressed(Button::Up)),
                    Some(Keycode::Down) => Some(GameBoyEvent::ButtonPressed(Button::Down)),
                    Some(Keycode::Left) => Some(GameBoyEvent::ButtonPressed(Button::Left)),
                    Some(Keycode::Right) => Some(GameBoyEvent::ButtonPressed(Button::Right)),
                    Some(Keycode::Z) => Some(GameBoyEvent::ButtonPressed(Button::B)),
                    Some(Keycode::X) => Some(GameBoyEvent::ButtonPressed(Button::A)),
                    Some(Keycode::Backspace) => Some(GameBoyEvent::ButtonPressed(Button::Select)),
                    Some(Keycode::Return) => Some(GameBoyEvent::ButtonPressed(Button::Start)),
                    Some(Keycode::Escape) => Some(GameBoyEvent::Quit),
                    _ => None,
                },
                Event::KeyUp { keycode, .. } => match keycode {
                    Some(Keycode::Up) => Some(GameBoyEvent::ButtonReleased(Button::Up)),
                    Some(Keycode::Down) => Some(GameBoyEvent::ButtonReleased(Button::Down)),
                    Some(Keycode::Left) => Some(GameBoyEvent::ButtonReleased(Button::Left)),
                    Some(Keycode::Right) => Some(GameBoyEvent::ButtonReleased(Button::Right)),
                    Some(Keycode::Z) => Some(GameBoyEvent::ButtonReleased(Button::B)),
                    Some(Keycode::X) => Some(GameBoyEvent::ButtonReleased(Button::A)),
                    Some(Keycode::Backspace) => Some(GameBoyEvent::ButtonReleased(Button::Select)),
                    Some(Keycode::Return) => Some(GameBoyEvent::ButtonReleased(Button::Start)),
                    _ => None,
                },
                Event::Quit { timestamp: _ } => Some(GameBoyEvent::Quit),
                _ => None,
            };

            return event;
        }

        None
    }
}
