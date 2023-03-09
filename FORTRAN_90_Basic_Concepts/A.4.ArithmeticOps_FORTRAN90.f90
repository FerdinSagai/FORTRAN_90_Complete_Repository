PROGRAM ARITHMETIC_OPERATIONS
	INTEGER(KIND = 4) :: A, B, C
	REAL(KIND = 4) :: X, Y, Z

	A = 10
	B = 20
	
	C = A + B
	WRITE(*,*) "Result of Addition :: ", C
	C = B - A
	WRITE(*,*) "Result of Subtraction :: ", C

	X = 1.582
	Y = 4.321
	Z = X - Y
	WRITE(*,*) "Result of Multiplication :: ", Z
	Z = Y/X
	WRITE(*,*) "Result of Division :: ", Z
	Z = MOD(Y,X)
	WRITE(*,*) "Result of Modulo :: ", Z

END PROGRAM ARITHMETIC_OPERATIONS
