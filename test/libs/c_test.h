#ifndef C_TEST
#define C_TEST

struct StructIntDouble {
    int s2fst;
    double s2snd;
};

struct StructIntInt {
    int s2fst;
    int s2snd;
};

struct StructDoubleFloat {
    double s2fst;
    float  s2snd;
};

struct StructIntCharDouble {
    int s3fst;
    char s3snd;
    double s3trd;
};

struct StructIntDoubleChar {
    int s3fst;
    double s3snd;
    char s3trd;
};

struct StructIntCharDoubleDouble {
    int s4fst;
    char s4snd;
    double s4trd;
    double s4fth;
};

struct StructIntDoubleDoubleInt {
    int s4fst;
    double s4snd;
    double s4trd;
    int s4fth;
};

struct StructDoubleIntCharChar {
    double s4fst;
    int s4snd;
    char s4trd;
    char s4fth;
};

struct StructIntDouble *sIntDouble(void);
struct StructIntInt *sIntInt(void);
struct StructDoubleFloat *sDoubleFloat(void);
struct StructIntCharDouble *sIntCharDouble(void);
struct StructIntDoubleChar *sIntDoubleChar(void);
struct StructIntCharDoubleDouble *sIntCharDoubleDouble(void);
struct StructIntDoubleDoubleInt *sIntDoubleDoubleInt(void);
struct StructDoubleIntCharChar *sDoubleIntCharChar(void);

#endif // C_TEST
