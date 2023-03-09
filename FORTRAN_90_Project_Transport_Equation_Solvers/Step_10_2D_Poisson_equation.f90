PROGRAM POISSON
	INTEGER :: I
	INTEGER :: Nx, Ny
	INTEGER :: N_iter
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx, Ly
	REAL(8) :: dx, dy
	REAL(8) :: Source
	
	INTEGER :: Max_Iterations
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg, Yg
	REAL(8), DIMENSION(:,:), ALLOCATABLE :: b
	REAL(8), DIMENSION(:,:,:), ALLOCATABLE :: P
!=========================================================================================	
! Input Reading
!=========================================================================================
	Lx = 2.d0
	Ly = 2.d0
	
	Nx = 20
	Ny = 20
	Max_Iterations =100
!=========================================================================================	
! Allocation of grid
!=========================================================================================
	ALLOCATE (Xg(1:Nx), Yg(1:Ny))
	ALLOCATE (P(1:Nx,1:Ny,1:Max_Iterations))
	ALLOCATE (b(1:Nx,1:Ny))
!=========================================================================================	
! Grid generation
!=========================================================================================
	dx = Lx/Nx
	dy = Ly/Ny
	DO I = 1, Nx
		Xg(I) = (I-1)*dx
	END DO
	
	DO J = 1, Ny
		Yg(J) = (J-1)*dy
	END DO
	coeff = 0.5/(dx**2 + dy**2)
!=========================================================================================	
! Initialization
!=========================================================================================	
	! INITIAL  & BOUNDARY CONDITIONS
	DO I = 1, Nx
		DO J = 1, Ny
			P(I,J,:) = 0.d0
			b(I,J) = 0.d0
		END DO
	END DO

	b(Nx/4,Ny/4)		= 100
	b(3*Nx/4,3*Ny/4)	= -100
!=========================================================================================	
!  Scheme
!=========================================================================================	
	! COMPUTATION LOOP for CS
	DO  N_iter = 1, Max_Iterations
		DO I = 2, Nx - 1
			DO J = 2, Ny - 1
			Source = b(I,J)*dx**2*dy**2
			P(I,J,N_iter+1) = coeff * ((P(I+1,J,N_iter) + P(I-1,J,N_iter))*dy**2 + (P(I,J+1,N_iter) + P(I,J-1,N_iter))*dx**2  -  Source)
			END DO
		END DO
		DO I = 2, Nx - 1
			P(I,1,N_iter+1)	=	P(I,2,N_iter+1)
			P(I,Ny,N_iter+1)	=	P(I,Ny-1,N_iter+1)
		END DO
	END DO
	! WRITING OF RESULTS
10	OPEN(100, FILE = "10.a.Poisson_equation_CS_Explicit.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "Poisson Equation"'
		WRITE(100,*)'VARIABLES ="X","Y","P","b"'
		WRITE(100,*)'ZONE  T="CS Explicit ',1,'", I = ',Nx,' J = ',Ny,' K = ',1,' F = POINT'
		DO J = 1, Ny
			DO I = 1, Nx
				WRITE(100,*)  Xg(I), Yg(J), P(I,J,Max_Iterations), b(I,J)
			END DO	
		END DO
	CLOSE (100)	
END PROGRAM POISSON