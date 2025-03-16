#include "obj_dir/Vkr580.h"
#include "kr580e.h"

int main(int argc, char** argv)
{
    app* com = new app(320, 240, 3);
    while (com->main()) { }
    return com->destroy();
}
