use sdl2::pixels::Color as SdlColor;

use crate::{
    frame_buffer::FrameBuffer,
    get_bit,
    mmu::{Mmu, INT_FLAG_ADDRESS},
};

use crate::check_bit;

const TICKS_PER_SCANLINE_OAM: usize = 80;
const TICKS_PER_SCANLINE_VRAM: usize = 172;
const TICKS_PER_HBLANK: usize = 204;
const TICKS_PER_SCANLINE: usize =
    TICKS_PER_SCANLINE_OAM + TICKS_PER_SCANLINE_VRAM + TICKS_PER_HBLANK;
const _TICKS_PER_VBLANK: usize = 4560;

const TILESET_ZERO_ADDRESS: usize = 0x8000;
const TILESET_ONE_ADDRESS: usize = 0x8800;

const TILEMAP_ZERO_ADDRESS: usize = 0x9800;
const TILEMAP_ONE_ADDRESS: usize = 0x9c00;

const LCD_CONTROL_ADDRESS: usize = 0xff40;
const LCD_STATUS_ADDRESS: usize = 0xff41;

const SCY: usize = 0xff42;
const SCX: usize = 0xff43;
const WY: usize = 0xff4a;
const WX: usize = 0xff4b;

const LY_ADDRESS: usize = 0xff44;
const LYC_ADDRESS: usize = 0xff45;

const BGP_ADDRESS: usize = 0xff47;

const OBP0_ADDRESS: usize = 0xff48;
const OBP1_ADDRESS: usize = 0xff49;

pub const GAMEBOY_WIDTH: usize = 160;
pub const GAMEBOY_HEIGHT: usize = 144;
const BG_MAP_SIZE: usize = 256;

const TILES_PER_LINE: usize = 32;
const TILE_HEIGHT_PX: usize = 8;
const TILE_WIDTH_PX: usize = 8;
const TILE_BYTES: usize = 2 * 8;

const SPRITE_BYTES: usize = 4;

macro_rules! check_lcd_control {
    ($name:ident, $bit:expr) => {
        fn $name(&self, mmu: &Mmu) -> bool {
            mmu.check_bit(LCD_CONTROL_ADDRESS, $bit)
        }
    };
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Color {
    White,
    LightGray,
    DarkGray,
    Black,
}

impl From<u8> for Color {
    fn from(value: u8) -> Self {
        match value {
            0 => Color::White,
            1 => Color::LightGray,
            2 => Color::DarkGray,
            3 => Color::Black,
            _ => panic!("invalid color value"),
        }
    }
}

impl Into<SdlColor> for Color {
    fn into(self) -> SdlColor {
        match self {
            Color::White => SdlColor::RGB(224, 248, 208),
            Color::LightGray => SdlColor::RGB(136, 192, 112),
            Color::DarkGray => SdlColor::RGB(52, 104, 86),
            Color::Black => SdlColor::RGB(8, 24, 32),
        }
    }
}

#[derive(Debug)]
struct Palette(Color, Color, Color, Color);

impl Palette {
    pub fn get_color(&self, color: u8) -> Color {
        match color {
            0 => self.0,
            1 => self.1,
            2 => self.2,
            3 => self.3,
            _ => panic!("invalid color"),
        }
    }
}

impl Default for Palette {
    fn default() -> Self {
        Self(
            Color::White,
            Color::LightGray,
            Color::DarkGray,
            Color::Black,
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum VideoMode {
    HBlank,
    VBlank,
    SearchingOam,
    TransferingData,
}

struct Tile {
    buffer: [u8; TILE_HEIGHT_PX * 2 * TILE_WIDTH_PX],
}

impl Tile {
    pub fn new(mmu: &Mmu, tile_address: usize, size_multiplier: usize) -> Self {
        let mut buffer = [0; TILE_HEIGHT_PX * 2 * TILE_WIDTH_PX];

        for tile_line in 0..TILE_HEIGHT_PX * size_multiplier {
            let index_into_tile = 2 * tile_line;
            let line_start = tile_address + index_into_tile;

            let pixels_1 = mmu.read_byte(line_start);
            let pixels_2 = mmu.read_byte(line_start + 1);

            let pixel_line = Self::get_pixel_line(pixels_1, pixels_2);

            for x in 0..TILE_WIDTH_PX {
                buffer[tile_line * TILE_HEIGHT_PX + x] = pixel_line[x];
            }
        }

        Self { buffer }
    }

    pub fn get_pixel_line(byte_1: u8, byte_2: u8) -> Vec<u8> {
        let mut pixel_line = vec![];

        for i in 0..8 {
            let color = (get_bit!(byte_2, 7 - i) << 1) | (get_bit!(byte_1, 7 - i));
            pixel_line.push(color)
        }

        pixel_line
    }

    pub fn get_pixel(&self, x: usize, y: usize) -> u8 {
        self.buffer[y * TILE_HEIGHT_PX + x]
    }
}

pub struct Ppu {
    cycle_counter: usize,
    current_mode: VideoMode,
    buffer: FrameBuffer,
    pub should_draw: bool,
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            cycle_counter: 0,
            current_mode: VideoMode::SearchingOam,
            buffer: FrameBuffer::new(GAMEBOY_WIDTH, GAMEBOY_HEIGHT),
            should_draw: false,
        }
    }
}

impl Ppu {
    pub fn buffer(&self) -> &FrameBuffer {
        &self.buffer
    }

    pub fn buffer_mut(&mut self) -> &mut FrameBuffer {
        &mut self.buffer
    }

    pub fn tick(&mut self, cycles: usize, mmu: &mut Mmu, debug_disable_sprites: bool) {
        self.cycle_counter += cycles;

        match self.current_mode {
            VideoMode::HBlank => self.hblank(mmu),
            VideoMode::VBlank => self.vblank(mmu, debug_disable_sprites),
            VideoMode::SearchingOam => self.searching_oam(mmu),
            VideoMode::TransferingData => self.transfering_data(mmu),
        }
    }

    check_lcd_control!(display_enabled, 7);
    check_lcd_control!(window_enabled, 5);
    check_lcd_control!(bg_window_tile_data, 4);
    check_lcd_control!(bg_tile_map, 3);
    check_lcd_control!(sprite_size, 2);
    check_lcd_control!(sprites_enabled, 1);
    check_lcd_control!(bg_enabled, 0);

    fn searching_oam(&mut self, mmu: &mut Mmu) {
        if self.cycle_counter >= TICKS_PER_SCANLINE_OAM {
            self.cycle_counter %= TICKS_PER_SCANLINE_OAM;

            let mut lcd_status = mmu.read_byte(LCD_STATUS_ADDRESS);
            lcd_status |= 3;
            mmu.write_byte(LCD_STATUS_ADDRESS, lcd_status);

            self.current_mode = VideoMode::TransferingData;
        }
    }

    // TODO(alexyer): tests
    fn transfering_data(&mut self, mmu: &mut Mmu) {
        if self.cycle_counter >= TICKS_PER_SCANLINE_VRAM {
            self.cycle_counter %= TICKS_PER_SCANLINE_VRAM;
            self.current_mode = VideoMode::HBlank;

            let hblank_interrupt = mmu.check_bit(LCD_STATUS_ADDRESS, 3);

            if hblank_interrupt {
                mmu.set_bit_to(INT_FLAG_ADDRESS, 1, true);
            }

            let ly_coincidence_interrupt = mmu.check_bit(LCD_STATUS_ADDRESS, 6);
            let ly_coincidence = mmu.read_byte(LY_ADDRESS) == mmu.read_byte(LYC_ADDRESS);

            if ly_coincidence_interrupt && ly_coincidence {
                mmu.set_bit_to(INT_FLAG_ADDRESS, 1, true);
            }

            mmu.set_bit_to(LCD_STATUS_ADDRESS, 2, ly_coincidence);

            mmu.set_bit_to(LCD_STATUS_ADDRESS, 1, false);
            mmu.set_bit_to(LCD_STATUS_ADDRESS, 0, false);
        }
    }

    // TODO(alexyer): tests
    fn hblank(&mut self, mmu: &mut Mmu) {
        if self.cycle_counter >= TICKS_PER_HBLANK {
            self.write_scanline(mmu, mmu.read_byte(LY_ADDRESS));
            mmu.inc(LY_ADDRESS);

            self.cycle_counter %= TICKS_PER_HBLANK;

            if mmu.read_byte(LY_ADDRESS) == 144 {
                self.current_mode = VideoMode::VBlank;
                mmu.set_bit_to(LCD_STATUS_ADDRESS, 1, false);
                mmu.set_bit_to(LCD_STATUS_ADDRESS, 0, true);
                mmu.set_bit_to(INT_FLAG_ADDRESS, 0, true);
            } else {
                mmu.set_bit_to(LCD_STATUS_ADDRESS, 1, true);
                mmu.set_bit_to(LCD_STATUS_ADDRESS, 0, false);
                self.current_mode = VideoMode::SearchingOam;
            }
        }
    }

    fn vblank(&mut self, mmu: &mut Mmu, debug_disable_sprites: bool) {
        if self.cycle_counter >= TICKS_PER_SCANLINE {
            mmu.inc(LY_ADDRESS);

            self.cycle_counter %= TICKS_PER_SCANLINE;

            if mmu.read_byte(LY_ADDRESS) == 154 {
                self.write_sprites(mmu, debug_disable_sprites);

                self.should_draw = true;

                mmu.write_byte(LY_ADDRESS, 0);
                self.current_mode = VideoMode::SearchingOam;

                mmu.set_bit_to(LCD_STATUS_ADDRESS, 1, true);
                mmu.set_bit_to(LCD_STATUS_ADDRESS, 0, false);
            }
        }
    }

    fn write_sprites(&mut self, mmu: &Mmu, debug_disable_sprites: bool) {
        if !self.sprites_enabled(mmu) || debug_disable_sprites {
            return;
        }

        for sprite_n in 0..40 {
            self.draw_sprite(mmu, sprite_n);
        }
    }

    fn write_scanline(&mut self, mmu: &mut Mmu, current_line: u8) {
        if !self.display_enabled(mmu) {
            return;
        }

        if self.bg_enabled(mmu) {
            self.draw_bg_line(mmu, current_line);
        }

        if self.window_enabled(mmu) {
            self.draw_window_line(mmu, current_line);
        }
    }

    fn draw_sprite(&mut self, mmu: &Mmu, sprite_n: usize) {
        let offset_in_oam = sprite_n * SPRITE_BYTES;

        let oam_start = 0xfe00 + offset_in_oam;

        let sprite_y = mmu.read_byte(oam_start);
        let sprite_x = mmu.read_byte(oam_start + 1);

        if sprite_y == 0 || sprite_y >= 160 {
            return;
        }

        if sprite_x == 0 || sprite_x >= 168 {
            return;
        }

        let sprite_size_multiplier = if self.sprite_size(mmu) { 2 } else { 1 };

        let tile_set_location = TILESET_ZERO_ADDRESS;

        let pattern_n = mmu.read_byte(oam_start + 2);
        let sprite_attrs = mmu.read_byte(oam_start + 3);

        let use_palette_1 = check_bit!(sprite_attrs, 4);
        let flip_x = check_bit!(sprite_attrs, 5);
        let flip_y = check_bit!(sprite_attrs, 6);
        let obj_behind_bg = check_bit!(sprite_attrs, 7);

        let palette = if use_palette_1 {
            self.load_palette(mmu, OBP1_ADDRESS)
        } else {
            self.load_palette(mmu, OBP0_ADDRESS)
        };

        let tile_offset = pattern_n as usize * TILE_BYTES;

        let pattern_address = tile_set_location + tile_offset;

        let tile = Tile::new(mmu, pattern_address, sprite_size_multiplier);

        let start_y = (sprite_y as i16).wrapping_sub(16);
        let start_x = (sprite_x as i16).wrapping_sub(8);

        for y in 0..TILE_HEIGHT_PX * sprite_size_multiplier {
            for x in 0..TILE_WIDTH_PX {
                let maybe_flipped_y = if !flip_y {
                    y
                } else {
                    TILE_HEIGHT_PX
                        .wrapping_mul(sprite_size_multiplier)
                        .wrapping_sub(y)
                        .wrapping_sub(1)
                };

                let maybe_flipped_x = if !flip_x {
                    x
                } else {
                    TILE_WIDTH_PX.wrapping_sub(x).wrapping_sub(1)
                };

                let color = tile.get_pixel(maybe_flipped_x, maybe_flipped_y);

                if Color::from(color) == Color::White {
                    continue;
                }

                let screen_x = start_x.wrapping_add(x as i16) as u8;
                let screen_y = start_y.wrapping_add(y as i16) as u8;

                if !Self::is_on_screen(screen_x as u8, screen_y as u8) {
                    continue;
                }

                let existing_pixel = self.buffer.get_pixel(screen_x as usize, screen_y as usize);

                if obj_behind_bg && existing_pixel != Color::White {
                    continue;
                }

                self.buffer.set_pixel(
                    screen_x as usize,
                    screen_y as usize,
                    palette.get_color(color),
                );
            }
        }
    }

    fn is_on_screen_x(x: u8) -> bool {
        (x as usize) < GAMEBOY_WIDTH
    }

    fn is_on_screen_y(y: u8) -> bool {
        (y as usize) < GAMEBOY_HEIGHT
    }

    fn is_on_screen(x: u8, y: u8) -> bool {
        Self::is_on_screen_x(x) && Self::is_on_screen_y(y)
    }

    fn draw_bg_line(&mut self, mmu: &Mmu, current_line: u8) {
        let use_tile_set_zero = self.bg_window_tile_data(mmu);
        let use_tile_map_zero = !self.bg_tile_map(mmu);

        let palette = self.load_palette(mmu, BGP_ADDRESS);

        let tileset_address = if use_tile_set_zero {
            TILESET_ZERO_ADDRESS
        } else {
            TILESET_ONE_ADDRESS
        };

        let tilemap_address = if use_tile_map_zero {
            TILEMAP_ZERO_ADDRESS
        } else {
            TILEMAP_ONE_ADDRESS
        };

        let screen_y = current_line as usize;

        for screen_x in 0..GAMEBOY_WIDTH {
            let scrolled_x = screen_x + mmu.read_byte(SCX) as usize;
            let scrolled_y = screen_y + mmu.read_byte(SCY) as usize;

            let bg_map_x = scrolled_x % BG_MAP_SIZE;
            let bg_map_y = scrolled_y % BG_MAP_SIZE;

            let tile_x = bg_map_x / TILE_WIDTH_PX;
            let tile_y = bg_map_y / TILE_HEIGHT_PX;

            let tile_pixel_x = bg_map_x % TILE_WIDTH_PX;
            let tile_pixel_y = bg_map_y % TILE_HEIGHT_PX;

            let tile_index = tile_y * TILES_PER_LINE + tile_x;
            let tile_id_address = tilemap_address + tile_index;

            let tile_id = mmu.read_byte(tile_id_address);

            let tile_data_mem_offset = if use_tile_set_zero {
                tile_id as usize * TILE_BYTES
            } else {
                tile_id.wrapping_add(128) as usize * TILE_BYTES
            };

            let tile_data_line_offset = tile_pixel_y * 2;

            let tile_line_data_start_address = (tileset_address as u16)
                .wrapping_add(tile_data_mem_offset as u16)
                .wrapping_add(tile_data_line_offset as u16)
                as usize;

            // TODO: optimize
            let pixels_1 = mmu.read_byte(tile_line_data_start_address);
            let pixels_2 = mmu.read_byte(tile_line_data_start_address + 1);

            let pixel_color = self.get_pixel_from_line(pixels_1, pixels_2, tile_pixel_x);

            let screen_color = palette.get_color(pixel_color);

            self.buffer.set_pixel(screen_x, screen_y, screen_color);
        }
    }

    fn draw_window_line(&mut self, mmu: &Mmu, current_line: u8) {
        let use_tile_set_zero = self.bg_window_tile_data(mmu);
        let use_tile_map_zero = !self.bg_tile_map(mmu);

        let palette = self.load_palette(mmu, BGP_ADDRESS);

        let tileset_address = if use_tile_set_zero {
            TILESET_ZERO_ADDRESS
        } else {
            TILESET_ONE_ADDRESS
        };

        let tilemap_address = if use_tile_map_zero {
            TILEMAP_ZERO_ADDRESS
        } else {
            TILEMAP_ONE_ADDRESS
        };

        let screen_y = current_line as usize;
        let scrolled_y = screen_y.wrapping_sub(mmu.read_byte(WY) as usize);

        if scrolled_y >= GAMEBOY_WIDTH {
            return;
        }

        for screen_x in 0..GAMEBOY_WIDTH {
            let scrolled_x = (screen_x as u16)
                .wrapping_add(mmu.read_byte(WX) as u16)
                .wrapping_sub(7) as usize;

            let tile_x = scrolled_x / TILE_WIDTH_PX;
            let tile_y = scrolled_y / TILE_HEIGHT_PX;

            let tile_pixel_x = scrolled_x % TILE_WIDTH_PX;
            let tile_pixel_y = scrolled_y % TILE_HEIGHT_PX;

            let tile_index = tile_y * TILES_PER_LINE + tile_x;
            let tile_id_address = tilemap_address + tile_index;

            let tile_id = mmu.read_byte(tile_id_address);

            let tile_data_mem_offset = if use_tile_set_zero {
                tile_id as usize * TILE_BYTES
            } else {
                tile_id.wrapping_add(128) as usize * TILE_BYTES
            };

            let tile_data_line_offset = tile_pixel_y * 2;

            let tile_line_data_start_address = (tileset_address as u16)
                .wrapping_add(tile_data_mem_offset as u16)
                .wrapping_add(tile_data_line_offset as u16)
                as usize;

            // TODO: optimize
            let pixels_1 = mmu.read_byte(tile_line_data_start_address);
            let pixels_2 = mmu.read_byte(tile_line_data_start_address + 1);

            let pixel_color = self.get_pixel_from_line(pixels_1, pixels_2, tile_pixel_x);

            let screen_color = palette.get_color(pixel_color);

            self.buffer.set_pixel(screen_x, screen_y, screen_color);
        }
    }

    fn get_pixel_from_line(&self, byte1: u8, byte2: u8, pixel_index: usize) -> u8 {
        (get_bit!(byte2, 7 - pixel_index) << 1) | (get_bit!(byte1, 7 - pixel_index))
    }

    fn load_palette(&self, mmu: &Mmu, address: usize) -> Palette {
        let val = mmu.read_byte(address);

        let color0 = val & 3;
        let color1 = (val >> 2) & 3;
        let color2 = (val >> 4) & 3;
        let color3 = (val >> 6) & 3;

        Palette(color0.into(), color1.into(), color2.into(), color3.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_searching_oam() {
        let mut mmu = Mmu::default();
        let mut ppu = Ppu::default();

        ppu.current_mode = VideoMode::SearchingOam;
        ppu.tick(TICKS_PER_SCANLINE_OAM + 1, &mut mmu, false);

        assert_eq!(ppu.current_mode, VideoMode::TransferingData);
        assert_eq!(mmu.read_byte(LCD_STATUS_ADDRESS) & 0x3, 3);
    }
}
