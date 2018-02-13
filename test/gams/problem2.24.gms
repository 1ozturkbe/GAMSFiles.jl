Sets  i    /1*20/
      j    /3*31/
;

Parameter auxi(i)
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

Parameter auxj(j)
         / 3   3
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
          20   20
          21   21
          22   22
          23   23
          24   24
          25   25
          26   26
          27   27
          28   28
          29   29
          30   30
          31   31 /;

Variables
           x(i)
           y(j)
           y1
           y2
           objvar;

Equations  e(j)
           e1
           e2
           e3;

e(j).. abs( sum(i, (auxi(i) - 1)*x(i)*power( (auxj(j) - 2)/29 , auxi(i) - 2 ) )
       - power( sum(i, x(i)*power( (auxj(j) - 2) / 29 , auxi(i) - 1 ) ) ,2) - 1
    ) - y(j) =E= 0;
e1.. abs( x('1') )
      - y1 =E= 0;
e2.. abs( x('2') - power(x('1'),2) - 1 )
      - y2 =E= 0;
e3.. max(y1,y2,smax(j,y(j))) - objvar =E= 0;

x.l('1') = 25;
x.l('2') = 5;
x.l('3') = -5;
x.l('4') = -1;

Model m / all /;
m.limrow=0; m.limcol=0;

Solve m using DNLP minimizing objvar;