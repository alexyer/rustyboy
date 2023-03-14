use crate::{
    frame_buffer::FrameBuffer,
    mmu::{Mmu, INT_FLAG_ADDRESS},
};

const TICKS_PER_SCANLINE_OAM: usize = 80;
const TICKS_PER_SCANLINE_VRAM: usize = 172;
const TICKS_PER_HBLANK: usize = 204;
const TICKS_PER_SCANLINE: usize =
    (TICKS_PER_SCANLINE_OAM + TICKS_PER_SCANLINE_VRAM + TICKS_PER_HBLANK);
const TICKS_PER_VBLANK: usize = 4560;

const TILESET_ZERO_ADDRESS: usize = 0x8000;
const TILESET_ONE_ADDRESS: usize = 0x8800;

const TILEMAP_ZERO_ADDRESS: usize = 0x9800;
const TILEMAP_ONE_ADDRESS: usize = 0x9c00;

const LCD_CONTROL_ADDRESS: usize = 0xff40;
const LCD_STATUS_ADDRESS: usize = 0xff41;

const WY: usize = 0xff4a;
const WX: usize = 0xff4b;

const LY_ADDRESS: usize = 0xff44;
const LYC_ADDRESS: usize = 0xff45;

const BGP_ADDRESS: usize = 0xff47;

const GAMEBOY_WIDTH: usize = 160;
const GAMEBOY_HEIGHT: usize = 144;
const BG_MAP_SIZE: usize = 256;

const TILES_PER_LINE: usize = 32;
const TILE_HEIGHT_PX: usize = 8;
const TILE_WIDTH_PX: usize = 8;
const TILE_BYTES: usize = 2 * 8;

macro_rules! check_lcd_control {
    ($name:ident, $bit:expr) => {
        fn $name(&self, mmu: &Mmu) -> bool {
            mmu.check_bit(LCD_CONTROL_ADDRESS, $bit)
        }
    };
}

#[derive(Copy, Clone)]
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

pub struct Ppu {
    cycle_counter: usize,
    current_mode: VideoMode,
    buffer: FrameBuffer,
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            cycle_counter: 0,
            current_mode: VideoMode::SearchingOam,
            buffer: FrameBuffer::new(GAMEBOY_WIDTH, GAMEBOY_HEIGHT),
        }
    }
}

impl Ppu {
    pub fn tick(&mut self, cycles: usize, mmu: &mut Mmu) {
        self.cycle_counter += cycles;

        match self.current_mode {
            VideoMode::HBlank => self.hblank(mmu),
            VideoMode::VBlank => self.vblank(mmu),
            VideoMode::SearchingOam => self.searching_oam(mmu),
            VideoMode::TransferingData => self.transfering_data(mmu),
        }
    }

    check_lcd_control!(display_enabled, 7);
    check_lcd_control!(window_enabled, 5);
    check_lcd_control!(bg_window_tile_data, 4);
    check_lcd_control!(bg_tile_map, 3);
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
                mmu.set_bit_to(LCD_STATUS_ADDRESS, 1, false);
                mmu.set_bit_to(LCD_STATUS_ADDRESS, 0, true);
                self.current_mode = VideoMode::SearchingOam;
            }
        }
    }

    fn vblank(&mut self, mmu: &mut Mmu) {
        if self.cycle_counter >= TICKS_PER_SCANLINE {
            mmu.inc(LY_ADDRESS);

            self.cycle_counter %= TICKS_PER_SCANLINE;

            if mmu.read_byte(LY_ADDRESS) == 154 {
                self.write_sprites(mmu);
            }
        }
    }

    fn write_sprites(&self, mmu: &Mmu) {
        if !self.sprites_enabled(mmu) {
            return;
        }

        todo!()
    }

    fn write_scanline(&mut self, mmu: &mut Mmu, current_line: u8) {
        if !self.display_enabled(mmu) {
            return;
        }

        if self.bg_enabled(mmu) {
            self.draw_bg_line(mmu, current_line);
        }

        if self.window_enabled(mmu) {
            todo!()
        }
    }

    #[allow(overflowing_literals)]
    fn draw_bg_line(&mut self, mmu: &Mmu, current_line: u8) {
        let use_tile_set_zero = self.bg_window_tile_data(mmu);
        let use_tile_map_zero = !self.bg_tile_map(mmu);

        let palette = self.load_palette(mmu);

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
            let scrolled_x = screen_x.wrapping_add((mmu.read_byte(WX) as usize).wrapping_sub(7));
            let scrolled_y = screen_y + mmu.read_byte(WY) as usize;

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
                (tile_id as i8).wrapping_add(128) as usize * TILE_BYTES
            };

            let tile_data_line_offset = tile_pixel_y * 2;

            let tile_line_data_start_address =
                tileset_address + tile_data_mem_offset + tile_data_line_offset;

            // TODO: optimize
            let pixels_1 = mmu.read_byte(tile_line_data_start_address);
            let pixels_2 = mmu.read_byte(tile_line_data_start_address + 1);

            let pixel_color = self.get_pixel_from_line(pixels_1, pixels_2, tile_pixel_x);
            let screen_color = palette.get_color(pixel_color);

            self.buffer.set_pixel(screen_x, screen_y, screen_color);
        }
    }

    fn get_pixel_from_line(&self, byte1: u8, byte2: u8, pixel_index: usize) -> u8 {
        ((byte2 << 7 - pixel_index) << 1) | byte1 << 7 - pixel_index
    }

    fn load_palette(&self, mmu: &Mmu) -> Palette {
        let bgp = mmu.read_byte(BGP_ADDRESS);

        let color0 = bgp & 3;
        let color1 = bgp >> 2 & 3;
        let color2 = bgp >> 4 & 3;
        let color3 = bgp >> 6 & 3;

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
        ppu.tick(TICKS_PER_SCANLINE_OAM + 1, &mut mmu);

        assert_eq!(ppu.current_mode, VideoMode::TransferingData);
        assert_eq!(mmu.read_byte(LCD_STATUS_ADDRESS) & 0x3, 3);
    }
}
