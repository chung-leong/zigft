#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

// testing void as root 

typedef void nu;

enum {
    nu_error_none = 0,
    nu_error_generic,
    nu_error_file,
    nu_error_invalid_arg,
};

nu* nu_get() {
    return (nu*) 0x1000;
}

int nu_hello(nu* handle) {
    return nu_error_none;
}

#define nu_option_one     0x0001
#define nu_option_two     0x0002
#define nu_option_three     0x0004

typedef struct { 
    int number;
    uint16_t options;
} nu_struct;

int nu_world(nu* handle, uint16_t options) {
    if (options & nu_option_one) {
        return nu_error_none;
    } else if (options & nu_option_two) {
        return nu_error_generic;
    }
}

void nu_foo(nu* handle, uint16_t options, nu_struct* ptr) {
    ptr->number = 1234;
    ptr->options = options;
}