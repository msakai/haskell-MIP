NAME          
OBJSENSE
 MAX
ROWS
 N  obj1
 E  c0
 L  c1
 L  qc0
COLUMNS
    MARK0000  'MARKER'                 'INTORG'
    x         c0        1.0
    x         c1        1.0
    x         obj1      1.0
    x         qc0       1.0
    y         c0        1.0
    y         c1        5.0
    y         obj1      1.0
    y         qc0       1.0
    z         c1        2.0
    z         obj1      1.0
    MARK0001  'MARKER'                 'INTEND'
RHS
    rhs       c0        1.0
    rhs       c1        10.0
    rhs       qc0       5.0
BOUNDS
 LI bound     x         0.0
 UI bound     x         5.0
 LI bound     y         0.0
 PL bound     y
 LI bound     z         2.0
 PL bound     z
QCMATRIX   qc0
    x         x         1.0
    x         y         -1.0
    y         x         -1.0
    y         y         3.0
ENDATA
