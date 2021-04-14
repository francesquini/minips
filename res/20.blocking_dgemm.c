/* DP-GEMM - Double Precision GEneral Matrix Multiply - Cache Blocking Edition
 * Emilio Francesquini <e.francesquini@ufabc.edu.br> 03/2021
 */

#include "minips.h"

#pragma GCC optimize 0

#define SEED 42
#define N 64
#define BLOCK_SZ 8
#define MAX_VAL 10

double A[N][N];
double B[N][N];
double C[N][N];


// Veja arquivo 17.rng.c
int rand_z;
int rand_w;

int random_int() {
    rand_z = 36969 * (rand_z & 65535) + (rand_z >> 16);
    rand_w = 18000 * (rand_w & 65535) + (rand_w >> 16);
    return (rand_z << 16) + rand_w;
}

void init_random(int seed) {
    int n1 = seed * 48947;
    int n2 = seed * 104623;
    rand_z = (n1 != 0) ? n1 : 362436069;
    rand_w = (n2 != 0) ? n2 : 521288629;
}

//devolve um double entre 0 e 1
double random_double() {
    unsigned int rndint = random_int() + 1;
    return rndint * 2.328306435454494e-10;
}

double random_double_max(int max) {
    unsigned int rndint = random_int() + 1;
    return rndint * 2.328306435454494e-10 * max;
}


void init() {
    init_random(SEED);
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            A[i][j] = random_double_max(MAX_VAL);
            B[i][j] = random_double_max(MAX_VAL);
            C[i][j] = random_double_max(MAX_VAL);
        }
    }
}

void matrix_multiply() {
    for (int ii = 0; ii < N; ii += BLOCK_SZ) {
        for (int jj = 0; jj < N; jj += BLOCK_SZ) {
            for (int kk = 0; kk < N; kk += BLOCK_SZ) {
                for (int i = ii; i < ii + BLOCK_SZ; i++) {
                    for (int j = jj; j < jj + BLOCK_SZ; j++) {
                        for (int k = kk; k < kk + BLOCK_SZ; k++) {
                            C[i][j] += A[i][k] * B[k][j];
                        }
                    }
                }
            }
        }
    }
}

double simple_checksum () {
    double check = 0;
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            check += 2 * A[i][j] + 3 * B [i][j] + 5 * C[i][j];
        }
    }
    return check;
}

int main() {

    init();
    p_str("Checksum 0: ");
    p_double(simple_checksum());
    p_char('\n');
    matrix_multiply();
    p_str("Checksum 1: ");
    p_double(simple_checksum());
    p_char('\n');

    halt();
    return 0; //Linha inalcançável
}
