PROGRAM LAPLACE
	INTEGER :: I
	INTEGER :: Nx, Ny
	INTEGER :: N_iter
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx, Ly
	REAL(8) :: dx, dy
	
	INTEGER :: Max_Iterations
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg, Yg
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
		END DO
	END DO
	DO J = 1, Ny
		P(Nx,J,:) = J*dy
	END DO	
!=========================================================================================	
!  Scheme
!=========================================================================================	
	! COMPUTATION LOOP for CS
	DO  N_iter = 1, Max_Iterations
		DO I = 2, Nx - 1
			DO J = 2, Ny - 1
				P(I,J,N_iter+1) = coeff * ((P(I+1,J,N_iter) + P(I-1,J,N_iter))*dy**2 + (P(I,J+1,N_iter) + P(I,J-1,N_iter))*dx**2)
			END DO
		END DO
	END DO
	! WRITING OF RESULTS
10	OPEN(100, FILE = "9.a.Laplace_equation_CS_Explicit.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "LAPLACE Equation"'
		WRITE(100,*)'VARIABLES ="X","Y","P"'
		WRITE(100,*)'ZONE  T="CS Explicit ',1,'", I = ',Nx,' J = ',Ny,' K = ',1,' F = POINT'
		DO J = 1, Ny
			DO I = 1, Nx
				WRITE(100,*)  Xg(I), Yg(J), P(I,J,Max_Iterations)
			END DO	
		END DO
	CLOSE (100)	
END PROGRAM LAPLACE