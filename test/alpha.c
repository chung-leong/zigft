#include <stdio.h>

// single positive status (0), positive = error

typedef enum {
    alpha_ok,
    alpha_failure,
    alpha_invalid_arg,
    alpha_unknown,
} alpha_status;

alpha_status alpha_accept_int(int arg0) {
    printf("arg0 = %d\n", arg0);
    return alpha_ok;
}

typedef struct {
    int BigNumber;
    int SmallNumber;
} alpha_struct;

alpha_status alpha_accept_struct(const alpha_struct* arg0, alpha_struct* out0) {
    out0->BigNumber = arg0->BigNumber + 1;
    out0->SmallNumber = arg0->SmallNumber + 1;
    return alpha_ok;
}

alpha_status alpha_fail(int* out0, int* out1) {
    return alpha_failure;
}

alpha_status alpha_fail_unknown(int* out0, int* out1) {
    return alpha_unknown;
}

alpha_status alpha_positive_only(int arg0) {
    if (arg0 < 0) return alpha_invalid_arg;
    return alpha_ok;
}
