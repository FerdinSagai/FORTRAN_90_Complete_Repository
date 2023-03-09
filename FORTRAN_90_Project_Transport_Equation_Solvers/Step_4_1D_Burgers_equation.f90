PROGRAM BURGERS1D
	INTEGER :: I
	INTEGER :: Nx, Nt
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx
	REAL(8) :: dx
	
	INTEGER :: time_loop, end_time
	REAL(8) :: time_step
	REAL(8)	:: coefficient_convection, coefficient_diffusion
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg
	REAL(8) :: Convection_Term, Diffusion_Term
	REAL(8) :: d, CFL
	REAL(8), DIMENSION(:), ALLOCATABLE :: Phi
	REAL(8), DIMENSION(:,:), ALLOCATABLE :: U
!=========================================================================================	
! Input Reading
!=========================================================================================
	Nx =101
	Lx = 2
	Nt =5000
	sigma = 0.50d0
	coefficient_convection = 1.0d0
	coefficient_diffusion	= 0.07
!=========================================================================================	
! Allocation of grid
!=========================================================================================
	ALLOCATE (Xg(1:Nx))
	ALLOCATE (Phi(1:Nx))
	ALLOCATE (U(1:Nx,1:Nt))
!=========================================================================================	
! Grid generation
!=========================================================================================
	dx = 2*PI/(Nx-1)
	time_step = dx*coefficient_diffusion
	DO I = 1, Nx
		Xg(I) = (I-1)*dx
	END DO
!=========================================================================================	
! Initialization
!=========================================================================================	
	! INITIAL  & BOUNDARY CONDITIONS
	DO I = 1, Nx
		phi(I) = exp(-Xg(I)**2/(4*coefficient_diffusion))  + exp(-(Xg(I)-2*PI)**2/(4*coefficient_diffusion))
	END DO
	
	DO I = 2, Nx
		U(I,1) =-((2*coefficient_diffusion)/phi(I)*(phi(I)-phi(I-1))/dx )+ 4 
	END DO
	U(1,1) = U(Nx,1) 
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
		Convection_Term = (time_step/dx) * U(1,time_loop)*(U(1,time_loop)-U(Nx-1,time_loop))
		Diffusion_Term = (coefficient_diffusion*time_step/dx**2) *(U(2,time_loop) -2*U(1,time_loop) + U(Nx-1,time_loop))
		U(1,time_loop+1) 	= U(1,time_loop) - Convection_Term + Diffusion_Term
		U(Nx,time_loop+1)=	U(1,time_loop+1)
	END DO
	! WRITING OF RESULTS
	OPEN(100, FILE = "4.a.Burgers_Equation_.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "Burgers 1D Equation"'
		WRITE(100,*)'VARIABLES ="X","U"'
		DO  time_loop = 1, Nt	
			WRITE(100,*)'ZONE  T="FTCS Explicit ',time_loop,'", I = ',Nx,' J = ',1,' K = ',1,' F = POINT'
			DO I = 1, Nx
				WRITE(100,*)  Xg(I), U(I,time_loop)
			END DO
		END DO
	CLOSE (100)	

END PROGRAM BURGERS1D