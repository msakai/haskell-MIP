MINIMIZE
  obj: x
SUBJECT TO
  c1: - x + 0 lam1 + 0.5 lam2 + 1 lam3 + 1.5 lam4 + 2.5 lam5 = 0
  c2: - y + 0 lam1 + 0.125 lam2 + 1 lam3 + 3.375 lam4 + 15.625 lam5 = 0
  c3: lam1 + lam2 + lam3 + lam4 + lam5 = 1
BOUNDS
  0 <= lam1 <= 1
  0 <= lam2 <= 1
  0 <= lam3 <= 1
  0 <= lam4 <= 1
  0 <= lam5 <= 1
  5 <= y <= +inf
SOS
  set1: S2 :: lam1 : 1 lam2 : 2 lam3 : 3 lam4 : 4 lam5 : 5
END
