start: a = 1
IF a > 15 THEN
  GOTO end
END IF

  FOR i = 1 TO 2
    PRINT i
    a = a + i
  NEXT i
GOTO start

end: PRINT a
