MODULE VARIABLES
	INTEGER(KIND = 4) :: I, J, K
	REAL(KIND = 8) :: P, Q, R
END MODULE VARIABLES

PROGRAM DEMO_MODULES
	USE VARIABLES
	I = 10
	J = 20
	K = 30
	
	P = 1.3578
	Q = -5.217
	R = 3.1871
	
	S = 11.3578
	T = -15.217
	U = 31.1871
	
	WRITE(*,*) "MAIN"
	WRITE(*,*) "INTEGER variables " ,I, J, K
	WRITE(*,*) "REAL variables " ,P, Q, R
	
	CALL TEST
	WRITE(*,*) "Update Some Variables in the MAIN PROGRAM"
	P = P + 10
	Q = Q + 10
	R = R + 10
	WRITE(*,*) "Print Variables using the SUBROUTINE"
	CALL TEST
END PROGRAM DEMO_MODULES

SUBROUTINE TEST
	USE VARIABLES
	WRITE(*,*) "SUBROUTINE"
	WRITE(*,*) "INTEGER variables " ,I, J, K
	WRITE(*,*) "REAL variables " ,P, Q, R
END SUBROUTINE TEST