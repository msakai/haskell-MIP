MINIMIZE
obj: - x4 - 3.0 x3 - 2.0 x2 - x1
SUBJECT TO
c1: + 10.0 x4 + x3 + x2 - x1 <= 20.0
c2: + x3 - 3.0 x2 + x1 <= 30.0
c3: - 3.5 x4 + x2 = 0.0
BOUNDS
2.0 <= x4 <= 3.0
0.0 <= x3 <= +inf
0.0 <= x2 <= +inf
0.0 <= x1 <= 40.0
GENERALS
x4
SOS
set1: S1 ::  x1 : 10000.0  x2 : 20000.0  x4 : 40000.0
END
