/* ASCII Mandelbrot
 * Adaptado de https://people.math.sc.edu/Burkardt/c_src/mandelbrot_ascii/mandelbrot_ascii.html
 * Emilio Francesquini <e.francesquini@ufabc.edu.br> 04/2021
 */

#include "minips.h"

int main(int n, char**argc){
    float r,i,R,I,b;
    for(i=-1;i<1;i+=.06f,p_char('\n'))
        for(r=-2;I=i,(R=r)<1;r+=.03f,p_char(n+31))
            for(n=0;b=I*I,26>n++&&R*R+b<4;I=2*R*I+i,R=R*R-b+r);
    halt();
    return 0; // linha inalcançavel
}
