#include <stdio.h>
#include <stdbool.h>

// testing optional pointer parameters

typedef enum {
    lambda_ok,
    lambda_failure,
    lambda_invalid_arg,
    lambda_unknown,
} lambda_status;

typedef struct {
    int number1;
    int number2;
} lambda_struct;

typedef const lambda_struct* lambda_struct_ptr;

lambda_struct_ptr struct_ptr_1 = NULL;

lambda_status lambda_accept_struct1(lambda_struct_ptr ptr) {
    struct_ptr_1 = ptr;
}

bool lambda_is_struct1_null(void) {
    return !struct_ptr_1;
}

lambda_struct_ptr struct_ptr_2 = NULL;

lambda_status lambda_accept_struct2(const lambda_struct* ptr) {
    struct_ptr_2 = ptr;
}

bool lambda_is_struct2_null(void) {
    return !struct_ptr_2;
}

typedef union {
    int number1;
    float number2;
} lambda_union;

const lambda_union* union_ptr = NULL;

lambda_status lambda_accept_union(const lambda_union* ptr) {
    union_ptr = ptr;
}

bool lambda_is_union_null(void) {
    return !union_ptr;
}
