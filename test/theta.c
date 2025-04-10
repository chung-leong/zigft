#include <stddef.h>

// testing bitfields

typedef enum {
    theta_ok,
    theta_failure,
    theta_invalid_arg,
    theta_unexpected,
    theta_wrong_dwarf,
} theta_status;

typedef enum {
    theta_happy = 0x0001,
    theta_grumpy = 0x0002,
    theta_sleepy = 0x0004,
    theta_dopey = 0x0008,
    theta_bashful = 0x0010,
    theta_sneezy = 0x00020,
} theta_flags;

typedef void (*theta_callback)(theta_flags);

theta_status theta_is_sleepy(theta_flags flags) {
    if (flags & theta_sleepy) {
        return theta_ok;
    } else {
        return theta_wrong_dwarf;
    }
}

theta_flags dwarves = 0;

theta_status theta_set_dwarves(theta_flags flags) {
    dwarves = flags;
    return theta_ok;
}

void theta_invoke(theta_callback cb) {
    cb(dwarves);
}
