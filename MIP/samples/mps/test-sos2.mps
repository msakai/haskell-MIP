NAME          prob
ROWS
 N  obj
 E  c1
 E  c2
 E  c3
COLUMNS
    lam3      c1        1.0
    lam3      c2        1.0
    lam3      c3        1.0
    lam2      c1        0.5
    lam2      c2        0.125
    lam2      c3        1.0
    lam1      c3        1.0
    lam5      c1        2.5
    lam5      c2        15.625
    lam5      c3        1.0
    lam4      c1        1.5
    lam4      c2        3.375
    lam4      c3        1.0
    x         c1        -1.0
    x         obj       1.0
    y         c2        -1.0
RHS
    rhs       c3        1.0
BOUNDS
 UP bound     lam3      1.0
 UP bound     lam2      1.0
 UP bound     lam1      1.0
 UP bound     lam5      1.0
 UP bound     lam4      1.0
 LO bound     y         5.0
SOS
 S2 set1
    lam1      1.0
    lam2      2.0
    lam3      3.0
    lam4      4.0
    lam5      5.0
ENDATA
