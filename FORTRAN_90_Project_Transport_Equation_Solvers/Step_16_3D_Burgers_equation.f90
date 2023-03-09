PROGRAM BURGERS3D
	INTEGER :: I, J, K
	INTEGER :: Nx, Ny, Nz, Nt
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx, Ly, Lz
	REAL(8) :: dx, dy, dz
	
	INTEGER :: time_loop, end_time
	REAL(8) :: time_step
	REAL(8)	:: coefficient_convection, coefficient_diffusion
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg, Yg, Zg
	REAL(8) :: Convection_Term, Diffusion_Term
	REAL(8) :: d, CFL
	REAL(8), DIMENSION(:), ALLOCATABLE :: Phi
	REAL(8), DIMENSION(:,:), ALLOCATABLE :: U, V, W
	REAL(8) :: Speed
!=========================================================================================	
! Input Reading
!=========================================================================================
	Nx =41
	Ny =41
	Nz =41
	Lx = 2
	Ly = 2
	Lz = 2
	Nt =10
	sigma = 0.50d0
	coefficient_convection = 1.0d0
	coefficient_diffusion	= 0.07
!=========================================================================================	
! Allocation of grid
!=========================================================================================
	ALLOCATE (Phi(1:Nx))
	ALLOCATE (Xg(1:Nx), Yg(1:Ny), Zg(1:Nz))
	ALLOCATE (U(1:Nx,1:Nt), V(1:Ny,1:Nt), W(1:Nz,1:Nt))
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
	time_step = MIN(dx,dy,dz)*coefficient_diffusion
!=========================================================================================	
! Initialization
!=========================================================================================	
	! INITIAL  & BOUNDARY CONDITIONS
	DO I = 1, Nx
		IF(I.LE.10) THEN
			U(I,1) = 0.10
		ELSE IF(I.LE.21)  THEN
			U(I,1) = 2.d0
		ELSE
			U(I,1) = 0.10
		END IF
	END DO
	
	DO J = 1, Ny
		IF(J.LE.10) THEN
			V(J,1) = 0.10
		ELSE IF(J.LE.21)  THEN
			V(J,1) = 2.d0
		ELSE
			V(J,1) = 0.10
		END IF
	END DO

	DO K = 1, Nz
		IF(K.LE.10) THEN
			W(K,1) = 0.10
		ELSE IF(K.LE.21)  THEN
			W(K,1) = 2.d0
		ELSE
			W(K,1) = 0.10
		END IF
	END DO
!=========================================================================================	
! FTCS Scheme
!=========================================================================================	
	! COMPUTATION LOOP for FTCS
	DO  time_loop = 1, Nt
		DO I = 2, Nx - 1
			Convection_Term = (time_step/dx) * U(I,time_loop)*(U(I,time_loop)-U(I-1,time_loop))
			Diffusion_Term = (coefficient_diffusion*time_step/dx**2) *(U(I+1,time_loop) -2*U(I,time_loop) + U(I-1,time_loop))
			U(I,time_loop+1) = U(I,time_loop) - Convection_Term + Diffusion_Term
		END DO
		DO J = 2, Ny - 1
			Convection_Term = (time_step/dy) * V(J,time_loop)*(V(J,time_loop) - V(J-1,time_loop))
			Diffusion_Term = (coefficient_diffusion*time_step/dy**2) *(V(J+1,time_loop) -2*V(J,time_loop) + V(J-1,time_loop))
			V(J,time_loop+1) = V(J,time_loop) - Convection_Term + Diffusion_Term
		END DO
		DO K = 2, Nz - 1
			Convection_Term = (time_step/dz) * W(K,time_loop)*(W(K,time_loop) - W(K-1,time_loop))
			Diffusion_Term = (coefficient_diffusion*time_step/dz**2) *(W(K+1,time_loop) -2*W(K,time_loop) + W(K-1,time_loop))
			W(K,time_loop+1) = W(K,time_loop) - Convection_Term + Diffusion_Term
		END DO
	END DO
	! WRITING OF RESULTS
	OPEN(100, FILE = "16.a.Burgers_Equation_3D.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "Burgers 3D Equation"'
		WRITE(100,*)'VARIABLES ="X","Y","Z","U","V","W","R"'
		DO time_loop = 1, Nt
			WRITE(100,*)'ZONE  T="FTBS Explicit ',time_loop,'", I = ',Nx,' J = ',Ny,' K = ',Nz,' F = POINT'
			DO K = 1, Nz
				DO J = 1, Ny
					DO I = 1, Nx
						Speed = SQRT(U(I,time_loop)**2 + V(J,time_loop)**2 + W(K,time_loop)**2)
						WRITE(100,*) Xg(I), Yg(J), Zg(K), U(I,time_loop), V(J,time_loop), W(K,time_loop), Speed
					END DO
				END DO
			END DO
		END DO
	CLOSE (100)	

END PROGRAM BURGERS3D