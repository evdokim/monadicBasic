DIM n AS Integer
n = 100

DIM res(n) AS Boolean

DIM i AS Integer
DIM j AS Integer

FOR i=2 TO n/2
  IF res(i) = true THEN
    FOR j = 2 TO n/i
      res(i*j) = false
    NEXT
  END IF
NEXT

PRINT res

FOR i =2 TO n
  IF res(i) THEN PRINT i
NEXT



