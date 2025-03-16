#define SDL_MAIN_HANDLED

#include <SDL2/SDL.h>
#include <stdlib.h>
#include <stdio.h>

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
            for (int i = 0; i < 16384; i++)
            {
                // Логировать
                // printf("[%04X]\n", kr580->address);

                if (kr580->we) memory[kr580->address] = kr580->out;
                kr580->in = memory[kr580->address];
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

    // Установка точки
    void pset(int x, int y, Uint32 cl)
    {
        if (x >= 0 && y >= 0 && x < width && y < height)
            screen_buffer[width*y + x] = cl;
    }
};
