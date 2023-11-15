#include "kr580e.h"
#include "core.cc"

int main(int argc, char** argv) {

    App* core = new App(320, 200, 2);

    while (core->main()) {
        // stub
    }

    return core->destroy();
}
