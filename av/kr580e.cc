#include "kr580e.h"
#include "core.cc"

int main(int argc, char** argv) {

    App* core = new App(640, 400, 1);

    core->print("Hello World");

    while (core->main()) {
        // stub
    }

    return core->destroy();
}
