#include <stddef.h>
#include <stdbool.h>

// testing bool error

int value = 0;

bool gamma_accept_int(int arg0) {
    value = arg0;
    return true;
}

int gamma_get_int(void) {
    return value;
}

bool gamma_fail(void) {
    return false;
}
