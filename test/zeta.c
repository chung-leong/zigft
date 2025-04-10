#include <stddef.h>

// testing callback functions

typedef enum {
    zeta_ok,
    zeta_failure,
    zeta_invalid_arg,
    zeta_unexpected,
} zeta_status;

typedef void (*zeta_callback1)(void);
typedef zeta_status (*zeta_callback2)(int);

zeta_callback1 callback1 = NULL;
zeta_callback2 callback2 = NULL;

zeta_status zeta_set_callback1(zeta_callback1 f) {
    callback1 = f;
    return zeta_ok;
}

zeta_status zeta_call1(void) {
    if (callback1) {
        callback1();
        return zeta_ok;
    } else {
        return zeta_unexpected;
    }
}

zeta_status zeta_set_callback2(zeta_callback2 f) {
    callback2 = f;
    return zeta_ok;
}

zeta_status zeta_call2(int arg) {
    if (callback2) {
        callback2(arg);
        return zeta_ok;
    } else {
        return zeta_unexpected;
    }
}
