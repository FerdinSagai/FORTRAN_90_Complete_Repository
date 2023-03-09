! In Fortran, a function passes a return value, but a subroutine does not.
PROGRAM SUBROUTINES
	REAL(KIND = 4) :: A
	A = 10.00
	CALL TEST_ROUTINE(A,RESULTS)
	WRITE(*,*)RESULTS
	CALL INTERNAL_SUBROUTINE
	CALL EXTERNAL_SUBROUTINE
CONTAINS
	SUBROUTINE INTERNAL_SUBROUTINE
		WRITE(*,*)"Entering Internal Subroutine"
		WRITE(*,*)"Exiting Internal Subroutine"
	END SUBROUTINE INTERNAL_SUBROUTINE
END PROGRAM SUBROUTINES

SUBROUTINE TEST_ROUTINE(X,RESULTS)
	REAL(KIND = 4),INTENT(IN)	:: X
	REAL(KIND = 4),INTENT(OUT)	:: RESULTS

	RESULTS = X + 10
END SUBROUTINE TEST_ROUTINE
!=================================================================================================
SUBROUTINE EXTERNAL_SUBROUTINE
	USE VARIABLES
	IMPLICIT NONE
		WRITE(*,*)"Entering External Subroutine"
		WRITE(*,*)"Exiting External Subroutine"
		CALL EXTERNAL_SUBROUTINE_INTERNAL_SUBPROGRAM
CONTAINS
	SUBROUTINE EXTERNAL_SUBROUTINE_INTERNAL_SUBPROGRAM
		WRITE(*,*)"Entering External Subroutine's Internal Subprogram"
		WRITE(*,*)"Exiting External Subroutine's Internal Subprogram"
	END SUBROUTINE EXTERNAL_SUBROUTINE_INTERNAL_SUBPROGRAM
END SUBROUTINE EXTERNAL_SUBROUTINE