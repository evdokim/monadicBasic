0:
DIM n AS Integer
PRINT "input n:"
INPUT n

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

i=2
DO WHILE i<=n
  IF res(i) THEN 
    PRINT i
  ELSE
    PRINT "-"
  END IF
  i = i +1
LOOP

DIM s AS String
PRINT "try again? (yes/no)"
INPUT s
IF s="yes" THEN
  GOTO 0
END IF
