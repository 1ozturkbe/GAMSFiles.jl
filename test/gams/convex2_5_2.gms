Sets  i    /1*5/
      j    /1*4/
; 
 
parameters   b(j)
 /1  -9.504900 
2  0.617600 
3  7.070700 
4  10.321400 
/; 
 
table   A(j,i)
            1       2       3       4       5
   1   0.1808  1.1179-34.6750 -0.7402 -0.4768
   2   1.5166 -0.5267  2.3941  0.1376  1.0633
   3  -0.7785  0.2988 21.7019 -0.0612  0.2740
   4   0.9350 -1.8687 33.7391 -0.9762  0.0000

;
Variables  
           x(i)
           objvar;
Equations  e1;
 
e1..  0.5*( sum( j, (sum( i,A(j,i)*x(i) ) - b(j))*(sum( i,A(j,i)*x(i) ) - b(j)) )) + sum(i, abs(x(i)) )
       - objvar =E= 0; 
 
Model m / all /;
m.limrow=0; m.limcol=0;
Solve m using DNLP minimizing objvar;