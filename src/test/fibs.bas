a = 1
b = 1
INPUT n
PRINT a
FOR i = 2 TO n
  PRINT b
  b = a + b
  a = b - a
NEXT i
