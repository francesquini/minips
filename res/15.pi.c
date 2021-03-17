#include "minips.h"

float pi_f(int n) {
   float x = 1;
   float fator = -1;
   for (int i = 1; i < n; i++) {
       float termo = 1.0 / ((2.0 * i) + 1.0);
       x += fator * termo;
       fator *= -1;
   }
   x *= 4;
   return x;
}

double pi_d(int n) {
   double x = 1;
   double fator = -1;
   for (int i = 1; i < n; i++) {
       x += fator * (1.0/((2.0 * i) + 1.0));
       fator *= -1;
   }
   x *= 4;
   return x;
}

int main() {
    p_str("Digite quantos termos:");
    int n = read_int();
    p_str("Pi Float: ");
    p_float(pi_f(n));
    p_str("\nPi Double: ");
    p_double(pi_d(n));
    p_str("\n");
    halt();
    return 0; //Linha inalcançável
}
