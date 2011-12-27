DIM a AS Integer
DIM i AS Integer
a = 1
0: 

IF a > 15 THEN
  GOTO 1
END IF

  FOR i = 1 TO 2
    PRINT i
    a = a + i
  NEXT
GOTO 0

1: PRINT a
