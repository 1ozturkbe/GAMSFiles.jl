Sets  i    /1*4/
      j    /1*20/
;

Parameter aux(j)
         / 1   1
           2   2
           3   3
           4   4
           5   5
           6   6
           7   7
           8   8
           9   9
          10   10
          11   11
          12   12
          13   13
          14   14
          15   15
          16   16
          17   17
          18   18
          19   19
          20   20 /;

Parameter
          t(j);

t(j) = 0.2*aux(j);

Variables
           x(i)
           y(j)
           objvar;

Equations  e(j)
           e1;

e(j).. abs( power(x('1') + x('2')*t(j) - exp(t(j)),2) +
   power(x('3') + x('4')*sin(t(j)) - cos(t(j)),2)
    ) - y(j) =E= 0;
e1.. smax(j,y(j)) - objvar =E= 0;

x.l('1') = 25;
x.l('2') = 5;
x.l('3') = -5;
x.l('4') = -1;

Model m / all /;
m.limrow=0; m.limcol=0;

Solve m using DNLP minimizing objvar;