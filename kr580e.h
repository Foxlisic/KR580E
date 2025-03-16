#define SDL_MAIN_HANDLED

#include <SDL2/SDL.h>
#include <stdlib.h>
#include <stdio.h>

// Стандартная: 16384
#define CORESPEED 1

const char* ds_mnemonics[256] = {

    /* 00 */    "nop",  "ld",   "ld",   "inc",  "inc",  "dec",  "ld",   "rlca",
    /* 08 */    "ex",   "add",  "ld",   "dec",  "inc",  "dec",  "ld",   "rrca",
    /* 10 */    "djnz", "ld",   "ld",   "inc",  "inc",  "dec",  "ld",   "rla",
    /* 18 */    "jr",   "add",  "ld",   "dec",  "inc",  "dec",  "ld",   "rra",
    /* 20 */    "jr",   "ld",   "ld",   "inc",  "inc",  "dec",  "ld",   "daa",
    /* 28 */    "jr",   "add",  "ld",   "dec",  "inc",  "dec",  "ld",   "cpl",
    /* 30 */    "jr",   "ld",   "ld",   "inc",  "inc",  "dec",  "ld",   "scf",
    /* 38 */    "jr",   "add",  "ld",   "dec",  "inc",  "dec",  "ld",   "ccf",

    /* 40 */    "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",
    /* 48 */    "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",
    /* 50 */    "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",
    /* 58 */    "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",
    /* 60 */    "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",
    /* 68 */    "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",
    /* 70 */    "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "halt",  "ld",
    /* 78 */    "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",   "ld",

    /* 80 */    "add",  "add",  "add",  "add",  "add",  "add",  "add",  "add",
    /* 88 */    "adc",  "adc",  "adc",  "adc",  "adc",  "adc",  "adc",  "adc",
    /* 90 */    "sub",  "sub",  "sub",  "sub",  "sub",  "sub",  "sub",  "sub",
    /* 98 */    "sbc",  "sbc",  "sbc",  "sbc",  "sbc",  "sbc",  "sbc",  "sbc",
    /* A0 */    "and",  "and",  "and",  "and",  "and",  "and",  "and",  "and",
    /* A8 */    "xor",  "xor",  "xor",  "xor",  "xor",  "xor",  "xor",  "xor",
    /* B0 */    "or",   "or",   "or",   "or",   "or",   "or",   "or",   "or",
    /* B8 */    "cp",   "cp",   "cp",   "cp",   "cp",   "cp",   "cp",   "cp",

    /* C0 */    "ret",   "pop", "jp",   "jp",   "call", "push", "add",  "rst",
    /* C8 */    "ret",   "ret", "jp",   "$",    "call", "call", "adc",  "rst",
    /* D0 */    "ret",   "pop", "jp",   "out",  "call", "push", "sub",  "rst",
    /* D8 */    "ret",   "exx", "jp",   "in",   "call", "$",    "sbc",  "rst",
    /* E0 */    "ret",   "pop", "jp",   "ex",   "call", "push", "and",  "rst",
    /* E8 */    "ret",   "jp",  "jp",   "ex",   "call", "$",    "xor",  "rst",
    /* F0 */    "ret",   "pop", "jp",   "di",   "call", "push", "or",   "rst",
    /* F8 */    "ret",   "ld",  "jp",   "ei",   "call", "$",    "cp",   "rst"
};

const char* ds_reg8[3*8] = {
    "b", "c", "d", "e", "h",   "l",   "(hl)", "a",
    "b", "c", "d", "e", "ixh", "ixl", "$",    "a",
    "b", "c", "d", "e", "iyh", "iyl", "$",    "a"
};

const char* ds_reg16[3*4] = {

    "bc", "de", "hl", "sp",
    "bc", "de", "ix", "sp",
    "bc", "de", "iy", "sp"
};

const char* ds_reg16af[3*4] = {

    "bc", "de", "hl", "af",
    "bc", "de", "ix", "af",
    "bc", "de", "iy", "af"
};

const char* ds_cc[8] = {

    "nz", "z", "nc", "c",
    "po", "pe", "p", "m"
};

const char* ds_bits[8] = {

    "rlc", "rrc", "rl", "rr",
    "sla", "sra", "sll", "srl",
};

const int ds_im[8] = {0, 0, 1, 2, 0, 0, 1, 2};

// -----------------------------------------------------------------------------

class app
{
protected:

    int width, height, scale, frame_length, pticks;

    // Обработка окна
    SDL_Surface*        screen_surface;
    SDL_Window*         sdl_window;
    SDL_Renderer*       sdl_renderer;
    SDL_PixelFormat*    sdl_pixel_format;
    SDL_Texture*        sdl_screen_texture;
    SDL_Event           evt;
    Uint32*             screen_buffer;
    uint8_t             memory[65536];
    Vkr580*             kr580;

    // Disassembly:2000
    int     ds_ad;             // Текущая временная позиция разбора инструкции
    int     ds_size;           // Размер инструкции
    char    ds_opcode[16];
    char    ds_operand[32];
    char    ds_prefix[16];
    int     ds_string[64];     // Адреса в строках
    int     ds_change[16];

public:

    // Создание окна
    app(int w = 640, int h = 400, int s = 2, const char* title = "KR580 EMULATOR")
    {
        width        = w;
        height       = h;
        scale        = s;
        frame_length = 20;      // 50 кадров в секунду

        if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO)) {
            exit(1);
        }

        SDL_ClearError();
        screen_buffer       = (Uint32*) malloc(width * height * sizeof(Uint32));
        sdl_window          = SDL_CreateWindow(title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, scale*width, scale*height, SDL_WINDOW_SHOWN);
        sdl_renderer        = SDL_CreateRenderer(sdl_window, -1, SDL_RENDERER_PRESENTVSYNC);
        sdl_pixel_format    = SDL_AllocFormat(SDL_PIXELFORMAT_BGRA32);
        sdl_screen_texture  = SDL_CreateTexture(sdl_renderer, SDL_PIXELFORMAT_BGRA32, SDL_TEXTUREACCESS_STREAMING, width, height);
        SDL_SetTextureBlendMode(sdl_screen_texture, SDL_BLENDMODE_NONE);

        // Инициализация процессора
        kr580 = new Vkr580;

        // Сброс процессора
        kr580->locked  = 1;
        kr580->reset_n = 0;
        kr580->clock   = 0; kr580->eval();
        kr580->clock   = 1; kr580->eval();
        kr580->reset_n = 1;

        for (int i = 0; i < 65536; i++) memory[i] = 0x00;
    }

    // Основной цикл
    int main()
    {
        int cycles = 0;

        for (;;) {

            Uint32 ticks = SDL_GetTicks();

            while (SDL_PollEvent(& evt)) {

                // Прием событий
                switch (evt.type) {

                    case SDL_QUIT:
                        return 0;

                    case SDL_KEYDOWN:
                        // keyb = evt.key.keysym.scancode;
                        break;

                    case SDL_KEYUP:
                        // keyb = evt.key.keysym.scancode;
                        break;
                }
            }

            // Обработка определенной порции инструкции
            for (int i = 0; i < CORESPEED; i++)
            {
                // Логировать
                // printf("[%04X]\n", kr580->address);

                if (kr580->we) memory[kr580->address] = kr580->out;
                kr580->in = read(kr580->address);
                kr580->clock = 0; kr580->eval();
                kr580->clock = 1; kr580->eval();
                cycles++;
            }

            // Обновление экрана 50 fps
            if (ticks - pticks >= frame_length) {

                pticks = ticks;
                update();
                SDL_Delay(1);

                return 1;
            }
        }
    }

    // Обновить окно
    void update()
    {
        SDL_Rect dstRect;

        dstRect.x = 0;
        dstRect.y = 0;
        dstRect.w = scale * width;
        dstRect.h = scale * height;

        SDL_UpdateTexture       (sdl_screen_texture, NULL, screen_buffer, width * sizeof(Uint32));
        SDL_SetRenderDrawColor  (sdl_renderer, 0, 0, 0, 0);
        SDL_RenderClear         (sdl_renderer);
        SDL_RenderCopy          (sdl_renderer, sdl_screen_texture, NULL, &dstRect);
        SDL_RenderPresent       (sdl_renderer);
    }

    // Уничтожение окна
    int destroy()
    {
        free(screen_buffer);
        SDL_DestroyTexture(sdl_screen_texture);
        SDL_FreeFormat(sdl_pixel_format);
        SDL_DestroyRenderer(sdl_renderer);
        SDL_DestroyWindow(sdl_window);
        SDL_Quit();
        return 0;
    }

    // Чтение байта
    int read(uint16_t address)
    {
        return memory[address];
    }

    // Установка точки
    void pset(int x, int y, Uint32 cl)
    {
        if (x >= 0 && y >= 0 && x < width && y < height)
            screen_buffer[width*y + x] = cl;
    }

    // Дизассемблирование
    // -------------------------------------------------------------------------

    // Сформировать операнд (IX|IY+d)
    void ixy_disp(int prefix)
    {
        int df = ds_fetch_byte();

        if (df & 0x80) {
            sprintf(ds_prefix, "(%s-$%02x)", (prefix == 1 ? "ix" : "iy"), 1 + (df ^ 0xff));
        } else if (df) {
            sprintf(ds_prefix, "(%s+$%02x)", (prefix == 1 ? "ix" : "iy"), df);
        } else {
            sprintf(ds_prefix, "(%s)", (prefix == 1 ? "ix" : "iy"));
        }
    }

    // Прочитать байт дизассемблера
    int ds_fetch_byte()
    {
        int b = read(ds_ad);
        ds_ad = (ds_ad + 1) & 0xffff;
        ds_size++;
        return b;
    }

    // Прочитать слово дизассемблера
    int ds_fetch_word()
    {
        int l = ds_fetch_byte();
        int h = ds_fetch_byte();
        return (h << 8) | l;
    }

    // Прочитать относительный операнд
    int ds_fetch_rel()
    {
        int r8 = ds_fetch_byte();
        return ((r8 & 0x80) ? r8 - 0x100 : r8) + ds_ad;
    }

    // Дизассемблирование 1 линии
    int debug(int addr, int oppad = 0)
    {
        int op, df;
        int prefix = 0;

        ds_opcode[0]  = 0;
        ds_operand[0] = 0;
        ds_prefix[0]  = 0;
        ds_ad   = addr;
        ds_size = 0;

        // -----------------------------------------------------------------
        // Считывание опкода и префиксов
        // -----------------------------------------------------------------

        op = ds_fetch_byte();

        // -----------------------------------------------------------------
        // Разбор опкода и операндов
        // -----------------------------------------------------------------

        // Имя опкода
        sprintf(ds_opcode, "%s", ds_mnemonics[op]);

        int a = (op & 0x38) >> 3;
        int b = (op & 0x07);

        // Имя HL в зависимости от префикса
        char hlname[4];
        if (prefix == 0) sprintf(hlname, "hl");
        if (prefix == 1) sprintf(hlname, "ix");
        if (prefix == 2) sprintf(hlname, "iy");

        // Инструкции перемещения LD
        if (op >= 0x40 && op < 0x80) {

            if (a == 6 && b == 6) {
                /* halt */
            }
            // Префиксированные
            else if (prefix) {

                // Прочитать +disp8
                ixy_disp(prefix);

                // Декодирование
                if (a == 6) {
                    sprintf(ds_operand, "%s, %s", ds_prefix, ds_reg8[b]);
                } else if (b == 6) {
                    sprintf(ds_operand, "%s, %s", ds_reg8[a], ds_prefix);
                } else {
                    sprintf(ds_operand, "%s, %s", ds_reg8[8*prefix + a], ds_reg8[8*prefix + b]);
                }
            }
            else { sprintf(ds_operand, "%s, %s", ds_reg8[a], ds_reg8[b]); }
        }
        // Арифметико-логика
        else if (op >= 0x80 && op < 0xc0) {

            if (prefix) {

                if (b == 6) {

                    ixy_disp(prefix);
                    sprintf(ds_operand, "%s", ds_prefix);

                } else {
                    sprintf(ds_operand, "%s", ds_reg8[8*prefix + b]);
                }
            } else {
                sprintf(ds_operand, "%s", ds_reg8[b]);
            }
        }
        // LD r16, **
        else if (op == 0x01 || op == 0x11 || op == 0x21 || op == 0x31) {

            df = ds_fetch_word();
            sprintf(ds_operand, "%s, $%04x", ds_reg16[4*prefix + ((op & 0x30) >> 4)], df);
        }
        // 00xx x110 LD r8, i8
        else if ((op & 0xc7) == 0x06) {

            if (a == 6 && prefix) {
                ixy_disp(prefix);
                sprintf(ds_operand, "%s, $%02x", ds_prefix, ds_fetch_byte());
            } else {
                sprintf(ds_operand, "%s, $%02x", ds_reg8[8*prefix + a], ds_fetch_byte());
            }
        }
        // 00_xxx_10x
        else if ((op & 0xc7) == 0x04 || (op & 0xc7) == 0x05) {

            if (a == 6 && prefix) {
                ixy_disp(prefix);
                sprintf(ds_operand, "%s", ds_prefix);
            } else {
                sprintf(ds_operand, "%s", ds_reg8[8*prefix + a]);
            }
        }
        // 00xx x011
        else if ((op & 0xc7) == 0x03) {
            sprintf(ds_operand, "%s", ds_reg16[4*prefix + ((op & 0x30) >> 4)]);
        }
        // 00xx 1001
        else if ((op & 0xcf) == 0x09) {
            sprintf(ds_operand, "%s, %s", ds_reg16[4*prefix+2], ds_reg16[4*prefix + ((op & 0x30) >> 4)]);
        }
        else if (op == 0x02) sprintf(ds_operand, "(bc), a");
        else if (op == 0x08) sprintf(ds_operand, "af, af'");
        else if (op == 0x0A) sprintf(ds_operand, "a, (bc)");
        else if (op == 0x12) sprintf(ds_operand, "(de), a");
        else if (op == 0x1A) sprintf(ds_operand, "a, (de)");
        else if (op == 0xD3) sprintf(ds_operand, "($%02x), a", ds_fetch_byte());
        else if (op == 0xDB) sprintf(ds_operand, "a, ($%02x)", ds_fetch_byte());
        else if (op == 0xE3) sprintf(ds_operand, "(sp), %s", hlname);
        else if (op == 0xE9) sprintf(ds_operand, "(%s)", hlname);
        else if (op == 0xEB) sprintf(ds_operand, "de, %s", hlname);
        else if (op == 0xF9) sprintf(ds_operand, "sp, %s", hlname);
        else if (op == 0xC3 || op == 0xCD) sprintf(ds_operand, "$%04x", ds_fetch_word());
        else if (op == 0x22) { b = ds_fetch_word(); sprintf(ds_operand, "($%04x), %s", b, hlname); }
        else if (op == 0x2A) { b = ds_fetch_word(); sprintf(ds_operand, "%s, ($%04x)", hlname, b); }
        else if (op == 0x32) { b = ds_fetch_word(); sprintf(ds_operand, "($%04x), a", b); }
        else if (op == 0x3A) { b = ds_fetch_word(); sprintf(ds_operand, "a, ($%04x)", b); }
        else if (op == 0x10 || op == 0x18) { sprintf(ds_operand, "$%04x", ds_fetch_rel()); }
        // 001x x000 JR c, *
        else if ((op & 0xE7) == 0x20) sprintf(ds_operand, "%s, $%04x", ds_cc[(op & 0x18)>>3], ds_fetch_rel());
        // 11xx x000 RET *
        else if ((op & 0xC7) == 0xc0) sprintf(ds_operand, "%s", ds_cc[a]);
        // 11xx x010 JP c, **
        // 11xx x100 CALL c, **
        else if ((op & 0xC7) == 0xc2 || (op & 0xc7) == 0xc4) sprintf(ds_operand, "%s, $%04x", ds_cc[a], ds_fetch_word());
        // 11xx x110 ALU A, *
        else if ((op & 0xC7) == 0xc6) sprintf(ds_operand, "$%02x", ds_fetch_byte());
        // 11xx x111 RST #
        else if ((op & 0xC7) == 0xc7) sprintf(ds_operand, "$%02x", op & 0x38);
        // 11xx 0x01 PUSH/POP r16
        else if ((op & 0xCB) == 0xc1) sprintf(ds_operand, "%s", ds_reg16af[ ((op & 0x30) >> 4) + prefix*4 ] );

        int ds_opcode_len  = strlen(ds_opcode);
        int ds_operand_len = strlen(ds_operand);
        int ds_opcode_pad  = ds_opcode_len < 5 ? 5 : ds_opcode_len + 1;
        int ds_operand_pad = oppad > ds_operand_len ? oppad : ds_operand_len;

        // Привести в порядок
        for (int i = 0; i <= ds_opcode_pad; i++) {
             ds_opcode[i] = i < ds_opcode_len ? toupper(ds_opcode[i]) : (i == ds_opcode_pad-1 ? 0 : ' ');
         }

        for (int i = 0; i <= ds_operand_pad; i++) {
            ds_operand[i] = i < ds_operand_len ? toupper(ds_operand[i]) : (i == ds_operand_pad-1 ? 0 : ' ');
        }

        return ds_size;
    }
};
