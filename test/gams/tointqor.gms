*  NLP written by GAMS Convert at 10/06/06 11:52:29
*  
*  Equation counts
*      Total        E        G        L        N        X        C
*          1        1        0        0        0        0        0
*  
*  Variable counts
*                   x        b        i      s1s      s2s       sc       si
*      Total     cont   binary  integer     sos1     sos2    scont     sint
*         51       51        0        0        0        0        0        0
*  FX      0        0        0        0        0        0        0        0
*  
*  Nonzero counts
*      Total    const       NL      DLL
*         51        1       50        0
*
*  Solve m using NLP minimizing objvar;


Variables  x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19
          ,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35,x36
          ,x37,x38,x39,x40,x41,x42,x43,x44,x45,x46,x47,x48,x49,x50,objvar;

Equations  e1;


e1..  - (1.25*x1*x1 + 1.4*x2*x2 + 2.4*x3*x3 + 1.4*x4*x4 + 1.75*x5*x5 + 1.2*x6*
     x6 + 2.25*x7*x7 + 1.2*x8*x8 + x9*x9 + 1.1*x10*x10 + 1.5*x11*x11 + 1.6*x12*
     x12 + 1.25*x13*x13 + 1.25*x14*x14 + 1.2*x15*x15 + 1.2*x16*x16 + 1.4*x17*
     x17 + 0.5*x18*x18 + 0.5*x19*x19 + 1.25*x20*x20 + 1.8*x21*x21 + 0.75*x22*
     x22 + 1.25*x23*x23 + 1.4*x24*x24 + 1.6*x25*x25 + 2*x26*x26 + x27*x27 + 1.6
     *x28*x28 + 1.25*x29*x29 + 2.75*x30*x30 + 1.25*x31*x31 + 1.25*x32*x32 + 
     1.25*x33*x33 + 3*x34*x34 + 1.5*x35*x35 + 2*x36*x36 + 1.25*x37*x37 + 1.4*
     x38*x38 + 1.8*x39*x39 + 1.5*x40*x40 + 2.2*x41*x41 + 1.4*x42*x42 + 1.5*x43*
     x43 + 1.25*x44*x44 + 2*x45*x45 + 1.5*x46*x46 + 1.25*x47*x47 + 1.4*x48*x48
      + 0.6*x49*x49 + 1.5*x50*x50 + (5 + x1 - x31)*(5 + x1 - x31) + 1.5*(5 - x1
      + x2 + x3)*(5 - x1 + x2 + x3) + (5 - x2 + x4 + x5)*(5 - x2 + x4 + x5) + 
     0.1*(2.5 - x4 + x6 + x7)*(2.5 - x4 + x6 + x7) + 1.5*(6 - x6 + x8 + x9)*(6
      - x6 + x8 + x9) + 2*(6 - x8 + x10 + x11)*(6 - x8 + x10 + x11) + (5 - x10
      + x12 + x13)*(5 - x10 + x12 + x13) + 1.5*(6 - x12 + x14 + x15)*(6 - x12
      + x14 + x15) + 3*(10 - x11 - x13 - x14 + x16 + x17)*(10 - x11 - x13 - x14
      + x16 + x17) + 2*(6 - x16 + x18 + x19)*(6 - x16 + x18 + x19) + (5 - x9 - 
     x18 + x20)*(5 - x9 - x18 + x20) + 3*(9 - x5 - x20 - x21)*(9 - x5 - x20 - 
     x21) + 0.1*(2 - x19 + x22 + x23 + x24)*(2 - x19 + x22 + x23 + x24) + 1.5*(
     7 - x23 + x25 + x26)*(7 - x23 + x25 + x26) + 0.15*(2.5 - x7 - x25 + x27 + 
     x28)*(2.5 - x7 - x25 + x27 + x28) + 2*(6 - x28 + x29 + x30)*(6 - x28 + x29
      + x30) + (5 - x29 + x31 + x32)*(5 - x29 + x31 + x32) + 0.1*(2 - x32 + x33
      + x34)*(2 - x32 + x33 + x34) + 3*(9 - x3 - x33 + x35)*(9 - x3 - x33 + x35
     ) + 0.1*(2 + x21 - x35 + x36)*(2 + x21 - x35 + x36) + 1.2*(5 - x36 + x37
      + x38)*(5 - x36 + x37 + x38) + (5 - x30 - x37 + x39)*(5 - x30 - x37 + x39
     ) + 0.1*(2.5 - x38 - x39 + x40)*(2.5 - x38 - x39 + x40) + 2*(5 - x40 + x41
      + x42)*(5 - x40 + x41 + x42) + 1.2*(6 - x41 + x43 + x44 + x50)*(6 - x41
      + x43 + x44 + x50) + 3*(10 - x44 + x45 + x46 + x47)*(10 - x44 + x45 + x46
      + x47) + 1.5*(7 - x46 + x48)*(7 - x46 + x48) + 3*(10 - x42 - x45 - x48 + 
     x49 - x50)*(10 - x42 - x45 - x48 + x49 - x50) + 2*(6 - x26 - x34 - x43)*(6
      - x26 - x34 - x43) + (5 - x15 - x17 - x24 - x47)*(5 - x15 - x17 - x24 - 
     x47) + 1.2*(4 - x49)*(4 - x49) + 2*(4 - x22)*(4 - x22) + (4 - x27)*(4 - 
     x27)) + objvar =E= 0;

* set non default bounds


* set non default levels


* set non default marginals


Model m / all /;

m.limrow=0; m.limcol=0;

$if NOT '%gams.u1%' == '' $include '%gams.u1%'

option LP = baron;
option NLP = baron;
option MIP = baron;
option DNLP = baron;
m.reslim = 500;

Solve m using NLP minimizing objvar;

file startpoint Starting point /tointqor.sol/;
put startpoint;
put x1.l:11:4;
put x2.l:11:4;
put x3.l:11:4;
put x4.l:11:4;
put x5.l:11:4;
put x6.l:11:4;
put x7.l:11:4;
put x8.l:11:4;
put x9.l:11:4;
put x10.l:11:4;
put x11.l:11:4;
put x12.l:11:4;
put x13.l:11:4;
put x14.l:11:4;
put x15.l:11:4;
put x16.l:11:4;
put x17.l:11:4;
put x18.l:11:4;
put x19.l:11:4;
put x20.l:11:4;
put x21.l:11:4;
put x22.l:11:4;
put x23.l:11:4;
put x24.l:11:4;
put x25.l:11:4;
put x26.l:11:4;
put x27.l:11:4;
put x28.l:11:4;
put x29.l:11:4;
put x30.l:11:4;
put x31.l:11:4;
put x32.l:11:4;
put x33.l:11:4;
put x34.l:11:4;
put x35.l:11:4;
put x36.l:11:4;
put x37.l:11:4;
put x38.l:11:4;
put x39.l:11:4;
put x40.l:11:4;
put x41.l:11:4;
put x42.l:11:4;
put x43.l:11:4;
put x44.l:11:4;
put x45.l:11:4;
put x46.l:11:4;
put x47.l:11:4;
put x48.l:11:4;
put x49.l:11:4;
put x50.l:11:4;
putclose startpoint;
