#include <stddef.h>

// testing alternated error scheme
//
// error scheme: 
// positive status with 0 as default, negative numbers are errors

const char* str = NULL;

typedef enum {
    beta_excellent = 2,
    beta_good = 1,
    beta_ok = 0,
    beta_failure = -1,
    beta_invalid_arg = -2,
    beta_unexpected = -3,
} beta_status;

beta_status beta_accept_string(const char* s) {
    str = s;
    return beta_excellent;
}

beta_status beta_get_char(size_t index, char* out0) {
    if (str) {
        *out0 = str[index];
        return beta_ok;
    } else {
        return beta_unexpected;
    }
}

typedef union {
    int integer;
    double percentage;
} beta_union;

beta_status beta_accept_union(const beta_union* arg0, beta_union* out0) {
    out0->integer = arg0->integer + 1;
    return beta_ok;
}

typedef struct {
    int x;
    int y;
} beta_point;

beta_status beta_get_mood(void) {
    return beta_good;
}

beta_status beta_reject_arg(int arg0) {
    return beta_invalid_arg;
}
