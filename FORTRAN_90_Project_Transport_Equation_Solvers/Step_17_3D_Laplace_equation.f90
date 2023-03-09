PROGRAM LAPLACE
	INTEGER :: I, J, K
	INTEGER :: Nx, Ny, Nz
	INTEGER :: N_iter
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx, Ly, Lz
	REAL(8) :: dx, dy, dz
	
	INTEGER :: Max_Iterations
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg, Yg, Zg
	REAL(8), DIMENSION(:,:,:,:), ALLOCATABLE :: P
	REAL(8) :: X_Derivative, Y_Derivative, Z_Derivative
!=========================================================================================	
! Input Reading
!=========================================================================================
	Lx = 2.d0
	Ly = 2.d0
	Lz = 2.d0	
	Nx = 20
	Ny = 20
	Nz = 20
	Max_Iterations =100
!=========================================================================================	
! Allocation of grid
!=========================================================================================
	ALLOCATE (Xg(1:Nx), Yg(1:Ny), Zg(1:Nz))
	ALLOCATE (P(1:Nx,1:Ny,1:Nz,1:Max_Iterations))
!=========================================================================================	
! Grid generation
!=========================================================================================
	dx = Lx/Nx
	DO I = 1, Nx
		Xg(I) = (I-1)*dx
	END DO
	
	dy = Ly/Ny
	DO J = 1, Ny
		Yg(J) = (J-1)*dy
	END DO

	dz = Lz/Nz
	DO K = 1, Nz
		Zg(K) = (K-1)*dz
	END DO	
	coeff = 0.5/(dx**2 + dy**2+ dz**2)
!=========================================================================================	
! Initialization
!=========================================================================================	
	! INITIAL  & BOUNDARY CONDITIONS
	DO I = 1, Nx
		DO J = 1, Ny
			DO K = 1, Nz
				P(I,J,K,:) = 0.d0
			END DO
		END DO
	END DO
	DO K = 1, Nz
		DO J = 1, Ny
			P(Nx,J,K,:) = J*dy + K*dz
		END DO	
	END DO	
!=========================================================================================	
!  Scheme
!=========================================================================================	
	! COMPUTATION LOOP for CS
	DO  N_iter = 1, Max_Iterations-1
		DO I = 2, Nx - 1
			DO J = 2, Ny - 1
				DO K = 2, Nz - 1
					X_Derivative = (P(I+1,J,K,N_iter) + P(I-1,J,K,N_iter))*dy**2*dz**2 
					Y_Derivative = (P(I,J+1,K,N_iter) + P(I,J-1,K,N_iter))*dx**2*dz**2
					Z_Derivative = (P(I,J,K+1,N_iter) + P(I,J,K-1,N_iter))*dx**2*dy**2
					P(I,J,K,N_iter+1) = coeff * (X_Derivative + Y_Derivative + Z_Derivative)
				END DO
			END DO
		END DO
	END DO

	! WRITING OF RESULTS
	OPEN(100, FILE = "17.a.Laplace_equation_CS_Explicit.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "LAPLACE Equation"'
		WRITE(100,*)'VARIABLES ="X","Y","Z","P"'
		WRITE(100,*)'ZONE  T="CS Explicit ',1,'", I = ',Nx,' J = ',Ny,' K = ',Nz,' F = POINT'
		DO K = 1, Nz
			DO J = 1, Ny
				DO I = 1, Nx
					WRITE(100,*)  Xg(I), Yg(J), Zg(K), P(I,J,K,Max_Iterations)
				END DO	
			END DO
		END DO
	CLOSE (100)	
END PROGRAM LAPLACE