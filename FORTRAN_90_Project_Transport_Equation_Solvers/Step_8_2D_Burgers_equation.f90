PROGRAM BURGERS1D
	INTEGER :: I
	INTEGER :: Nx, Ny,Nt
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx, Ly
	REAL(8) :: dx, dy
	
	INTEGER :: time_loop, end_time
	REAL(8) :: time_step
	REAL(8)	:: coefficient_convection, coefficient_diffusion
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg, Yg
	REAL(8) :: Convection_Term, Diffusion_Term
	REAL(8) :: d, CFL
	REAL(8), DIMENSION(:), ALLOCATABLE :: Phi
	REAL(8), DIMENSION(:,:), ALLOCATABLE :: U, V
	REAL(8) :: Speed
!=========================================================================================	
! Input Reading
!=========================================================================================
	Nx =101
	Ny =101
	Lx = 2
	Ly = 2
	Nt =100
	sigma = 0.50d0
	coefficient_convection = 1.0d0
	coefficient_diffusion	= 0.07
!=========================================================================================	
! Allocation of grid
!=========================================================================================
	ALLOCATE (Xg(1:Nx), Yg(1:Nx))
	ALLOCATE (Phi(1:Nx))
	ALLOCATE (U(1:Nx,1:Nt), V(1:Ny,1:Nt))
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
	
	time_step = MIN(dx,dy)*coefficient_diffusion
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
			Diffusion_Term = (coefficient_diffusion*time_step/dx**2) *(V(J+1,time_loop) -2*V(J,time_loop) + V(J-1,time_loop))
			V(J,time_loop+1) = V(J,time_loop) - Convection_Term + Diffusion_Term
		END DO
	END DO
	! WRITING OF RESULTS
10	OPEN(100, FILE = "8.a.Burgers_Equation_2D.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "Burgers 1D Equation"'
		WRITE(100,*)'VARIABLES ="X","Y","U","V","R"'
		DO  time_loop = 1, Nt	
			WRITE(100,*)'ZONE  T="FTCS Explicit ',time_loop,'", I = ',Nx,' J = ',Ny,' K = ',1,' F = POINT'
			DO J = 1, Ny
				DO I = 1, Nx
					Speed = SQRT(U(I,time_loop)**2 + V(J,time_loop)**2)
					WRITE(100,*)  Xg(I), Yg(J),U(I,time_loop), V(J,time_loop), Speed
				END DO
			END DO
		END DO
	CLOSE (100)	

END PROGRAM BURGERS1D