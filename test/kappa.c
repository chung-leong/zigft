#include <stddef.h>

// testing status retrieval

typedef enum {
    kappa_ok = 0,
    kappa_failure = -1,
    kappa_invalid_arg = -2,
    kappa_unexpected = -3,
} kappa_status;

kappa_status kappa_do_stuff(void) {
    return kappa_ok;
}

kappa_status kappa_get_last_status(void) {
    return kappa_failure;
}
