#include <stddef.h>
#include <stdbool.h>

// testing invalid value error scheme

int number = 1234;

void* mu_get_void_ptr(bool arg) {
    return (arg) ? &number : NULL;
}

int* mu_get_int_ptr(bool arg) {
    return (arg) ? &number : NULL;
}

typedef int mu_handle;

#define INVALID_HANDLE -1

mu_handle mu_get_handle(bool arg) {
    return (arg) ? 1234 : INVALID_HANDLE;    
}
