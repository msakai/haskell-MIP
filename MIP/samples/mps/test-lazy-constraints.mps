NAME          prob
ROWS
 N  obj
 L  c1
 L  c2
LAZYCONS
 E  c3
COLUMNS
    x3        c1        1.0
    x3        c2        1.0
    x3        obj       3.0
    x2        c1        1.0
    x2        c2        -3.0
    x2        c3        1.0
    x2        obj       2.0
    x1        c1        -1.0
    x1        c2        1.0
    x1        obj       1.0
    MARK0000  'MARKER'                 'INTORG'
    x4        c1        10.0
    x4        c3        -3.5
    x4        obj       1.0
    MARK0001  'MARKER'                 'INTEND'
RHS
    rhs       c1        20.0
    rhs       c2        30.0
BOUNDS
 LI bound     x4        2.0
 UI bound     x4        3.0
 UP bound     x1        40.0
ENDATA
