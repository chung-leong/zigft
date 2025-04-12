#include <stddef.h>

// testing slice pointer merging

const char *string = "hello";
size_t string_len = 5;

void iota_set_string(const char *s, size_t len) {
    string = s;
    string_len = len;
}

char iota_get_char(size_t index) {
    return (index < string_len) ? string[index] : 0;
}

size_t iota_get_string_len(void) {
    return string_len;
}

const void *bytes = NULL;
size_t byte_len = 0;

void iota_set_bytes(const void *s, size_t len) {
    bytes = s;
    byte_len = len;
}

int iota_get_byte(size_t index) {
    return (index < byte_len) ? ((const unsigned char*) bytes)[index] : 0;
}

size_t iota_get_byte_len(void) {
    return byte_len;
}
