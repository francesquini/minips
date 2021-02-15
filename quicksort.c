#include <stdio.h>

int x[] = {2, 5, 4, 1, 3, 7, 6, 0};

int separa (int v[], int p, int r) {
   int c = v[r]; // pivô
   int t, j = p;
   for (int k = p; /*A*/ k < r; ++k)
      if (v[k] <= c) {
         t = v[j], v[j] = v[k], v[k] = t;
         ++j;
      }
   t = v[j], v[j] = v[r], v[r] = t;
   return j;
}


void quicksort (int v[], int p, int r) {
   if (p < r) {                   // 1
      int j = separa (v, p, r);   // 2
      quicksort (v, p, j-1);      // 3
      quicksort (v, j+1, r);      // 4
   }
}

void print_v () {
    for (int i = 0; i < 8; i++)
        x[i]++;
}

int main() {
    quicksort (x, 0, 7);

    return 0;
}
