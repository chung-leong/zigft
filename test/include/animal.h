#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    int32_t number1;
    int32_t number2;
} animal_struct;

typedef enum {
    animal_ok,
    animal_sick,
    animal_dead,
    animal_crazy,
} animal_status;

typedef enum {
    animal_appearance_ugly,
    animal_appearance_pretty,
} animal_enum;

typedef struct {
    int32_t number1;
    int32_t number2;
} animal_cow;

typedef struct {
    int32_t number1;
} animal_hen;

typedef struct {
    int32_t number1;
    int32_t number2;
    int32_t number3;
} animal_pig;

animal_status animal_mate(animal_cow, animal_hen, animal_pig*);
int animal_random(int, int, int);

animal_status animal_launch(animal_pig *pig);
void animal_smell(const animal_hen *hen);

inline int animal_add(int a, int b, int c) {
    return a + b + c;
}

int __attribute__((aligned (64))) animal_pay_soccer_with(animal_pig pig);

struct evil {
    int greed;
    int lust;
    int wrath;
    int gluttony;
};

typedef struct {
    int eggs;
    int feathers;
} animal_chicken;

struct animal_kangaroo;

typedef struct animal_kangaroo animal_kangaroo; 

struct animal_kangaroo {
    __attribute__((aligned (64))) int pocket;
    int punches;
};

typedef struct animal_whale__* animal_whale_pointer;

typedef const animal_kangaroo* animal_kangaroo_pointer;

typedef union {
    int eggs;
    float weight;
} animal_duck;

typedef enum {
    apple,
    orange,
    turkey,
} animal_food;

typedef enum {
    anti_apple = -1,
    anti_orange = -2,
    anti_turkey = -0xffff,
} animal_anti_food;
