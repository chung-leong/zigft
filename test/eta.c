#include <stddef.h>

// testing callbacks in struct

typedef enum {
    eta_ok,
    eta_failure,
    eta_invalid_arg,
    eta_unexpected,
} eta_status;

typedef enum {
    eta_apple,
    eta_banana,
    eta_chicken,
} eta_hello;

typedef struct {
    int number1;
    int number2;
} eta_struct;

typedef void (*eta_callback1)(void);
typedef eta_status (*eta_callback2)(const eta_struct*, eta_hello);

typedef struct {
    size_t number1;
    size_t number2;
    eta_callback1 callback1;
    eta_callback2 callback2;
} eta_call_table;

eta_call_table table = { 123, 456, NULL, NULL };

eta_status eta_set_call_table(const eta_call_table* t) {
    table = *t;
    return eta_ok;
}

eta_status eta_call1(void) {
    if (table.callback1) {
        table.callback1();
        return eta_ok;
    } else {
        return eta_unexpected;
    }
}

eta_status eta_call2(const eta_struct* arg0, eta_hello arg1) {
    if (table.callback2) {
        table.callback2(arg0, arg1);
        return eta_ok;
    } else {
        return eta_unexpected;
    }
}
