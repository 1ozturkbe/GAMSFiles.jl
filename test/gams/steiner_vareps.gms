* by Luis M Rios lmrios@gmail.com, May 2009
* translated from steiner_vareps.mod

* 17 variables
* m = 18
* m1 = 8
* nodes = {1 .. 18}
* arcs = {1 .. 18} x {1 .. 18}
* x = {1 .. 18, 1 .. 2}
*fix {j in m1+1..m, k in 1..2} x[j,k] := a[j,k];
*fix {j in 9..18, k in 1..2} x[j,k] := a[j,k];
* x[1,1]   x1
* x[1,2]   x2
* x[2,1]   x3
* x[2,2]   x4
* x[3,1]   x5
* x[3,2]   x6
* x[4,1]   x7
* x[4,2]   x8
* x[5,1]   x9
* x[5,2]   x10
* x[6,1]   x11
* x[6,2]   x12
* x[7,1]   x13
* x[7,2]   x14
* x[8,1]   x15
* x[8,2]   x16

Variables  x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,objvar;

Equations  e1;

e1..  x17 +
      sqrt( sqr(x17) + sqr( 2.309469 - x13 ) + sqr( 9.208211 - x14 ) ) +
      sqrt( sqr(x17) + sqr( 0.577367 - x1  ) + sqr( 6.480938 - x2  ) ) +
      sqrt( sqr(x17) + sqr( 0.808314 - x3  ) + sqr( 3.519062 - x4  ) ) +
      sqrt( sqr(x17) + sqr( 1.685912 - x5  ) + sqr( 1.231672 - x6  ) ) +
      sqrt( sqr(x17) + sqr( 4.110855 - x7  ) + sqr( 0.821114 - x8  ) ) +
      sqrt( sqr(x17) + sqr( 7.598152 - x9  ) + sqr( 0.615836 - x10 ) ) +
      sqrt( sqr(x17) + sqr( 8.568129 - x9  ) + sqr( 3.079179 - x10 ) ) +
      sqrt( sqr(x17) + sqr( 4.757506 - x11 ) + sqr( 3.753666 - x12 ) ) +
      sqrt( sqr(x17) + sqr( 3.926097 - x15 ) + sqr( 7.008798 - x16 ) ) +
      sqrt( sqr(x17) + sqr( 7.436490 - x15 ) + sqr( 7.683284 - x16 ) ) +
      sqrt( sqr(x17) + sqr( x9  - x11 ) + sqr( x10 - x12 ) ) +
      sqrt( sqr(x17) + sqr( x11 - x7  ) + sqr( x12 - x8  ) ) +
      sqrt( sqr(x17) + sqr( x7  - x5  ) + sqr( x8  - x6  ) ) +
      sqrt( sqr(x17) + sqr( x5  - x3  ) + sqr( x6  - x4  ) ) +
      sqrt( sqr(x17) + sqr( x3  - x1  ) + sqr( x4  - x2  ) ) +
      sqrt( sqr(x17) + sqr( x1  - x13 ) + sqr( x2  - x14 ) ) +
      sqrt( sqr(x17) + sqr( x13 - x15 ) + sqr( x14 - x16 ) )
      - objvar =E= 0;

* set non default bounds


* set non default levels

x17.l = 1;

* set non default marginals


Model m / all /;

m.limrow=0; m.limcol=0;

$if NOT '%gams.u1%' == '' $include '%gams.u1%'

option LP = baron;
option NLP = baron;
option MIP = baron;
option DNLP = baron;

m.optfile = 1;

Solve m using DNLP minimizing objvar;