MODULE VARIABLES
	TYPE Particles
		REAL(8) :: X, Y
		REAL(8) :: dp
	END TYPE
CONTAINS
	SUBROUTINE Displacement (Particle, dx)
		TYPE(PARTICLES),INTENT(INOUT) :: Particle
		REAL(8),INTENT(IN) :: dx
		Particle%X = Particle%X + dx
		Particle%Y = Particle%Y + dx
	END SUBROUTINE Displacement
END MODULE VARIABLES
!=============================================================================
PROGRAM DERIVED_DATATYPES
	USE VARIABLES
	IMPLICIT NONE
	INTEGER :: I
	INTEGER :: NUM
	TYPE(Particles),DIMENSION(:),ALLOCATABLE :: P1
	
	WRITE(*,*)"ENTER NUMBER OF PARTICLES"
	READ(*,*) NUM
	
	ALLOCATE ( P1(NUM) )
	
	DO I = 1, NUM
		P1(I)%X = RAND()
		P1(I)%Y = RAND()
	END DO

	DO I = 1, NUM
		WRITE(*,*)"POSITION OF PARTICLE",I," :: ", P1(I)%X, P1(I)%Y
	END DO

	DO I = 1, NUM
		Call Displacement(P1(I),0.5d0)
	END DO		
	
	DO I = 1, NUM
		WRITE(*,*)"POSITION OF PARTICLE",I," :: ", P1(I)%X, P1(I)%Y
	END DO
END PROGRAM DERIVED_DATATYPES