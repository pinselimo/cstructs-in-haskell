#include <stdlib.h>
#include "c_test.h"

struct StructIntDouble *sIntDouble(void) {
    struct StructIntDouble *s;
    s = malloc(sizeof(struct StructIntDouble));
    s->s2fst = 63;
    s->s2snd = 63.63;
    return s;
}

struct StructIntInt *sIntInt(void) {
    struct StructIntInt *s;
    s = malloc(sizeof(struct StructIntInt));
    s->s2fst = 63;
    s->s2snd = 63;
    return s;
}

struct StructDoubleFloat *sDoubleFloat(void) {
    struct StructDoubleFloat *s;
    s = malloc(sizeof(struct StructDoubleFloat));
    s->s2fst = 63.63;
    s->s2snd = 42.42;
    return s;
}

struct StructIntCharDouble *sIntCharDouble(void) {
    struct StructIntCharDouble *s;
    s = malloc(sizeof(struct StructIntCharDouble));
    s->s3fst = 63;
    s->s3snd = 1;
    s->s3trd = 63.63;
    return s;
}

struct StructIntDoubleChar *sIntDoubleChar(void) {
    struct StructIntDoubleChar *s;
    s = malloc(sizeof(struct StructIntDoubleChar));
    s->s3fst = 63;
    s->s3snd = 63.63;
    s->s3trd = 1;
    return s;
}

struct StructIntCharDoubleDouble *sIntCharDoubleDouble(void) {
    struct StructIntCharDoubleDouble *s;
    s = malloc(sizeof(struct StructIntCharDoubleDouble));
    s->s4fst = 63;
    s->s4snd = 1;
    s->s4trd = 63.63;
    s->s4fth = 63.63;
    return s;
}

struct StructIntDoubleDoubleInt *sIntDoubleDoubleInt(void) {
    struct StructIntDoubleDoubleInt *s;
    s = malloc(sizeof(struct StructIntDoubleDoubleInt));
    s->s4fst = 63;
    s->s4snd = 63.63;
    s->s4trd = 63.63;
    s->s4fth = 63;
    return s;
}

struct StructDoubleIntCharChar *sDoubleIntCharChar(void) {
    struct StructDoubleIntCharChar *s;
    s = malloc(sizeof(struct StructDoubleIntCharChar));
    s->s4fst = 63.63;
    s->s4snd = 63;
    s->s4trd = 1;
    s->s4fth = 1;
    return s;
}

