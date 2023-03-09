PROGRAM DIFFUSION
	INTEGER :: I
	INTEGER :: Nx, Nt
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx
	REAL(8) :: dx
	
	INTEGER :: time_loop, end_time
	REAL(8) :: time_step
	REAL(8)	:: coefficient_diffusion
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg
	
	REAL(8) :: d, Residue
	REAL(8), DIMENSION(:,:), ALLOCATABLE :: U
!=========================================================================================	
! Input Reading
!=========================================================================================
	Nx =41
	Lx = 2
	Nt =30
	sigma = 0.50d0
	coefficient_diffusion = 0.3d0
!=========================================================================================	
! Allocation of grid
!=========================================================================================
	ALLOCATE (Xg(1:Nx))
	ALLOCATE (U(1:Nx,1:Nt))
!=========================================================================================	
! Grid generation
!=========================================================================================
	dx = Lx/Nx
	time_step = (sigma/coefficient_diffusion) * dx**2
	DO I = 1, Nx
		Xg(I) = (I-1)*dx
	END DO
!=========================================================================================	
! Initialization
!=========================================================================================	
	! INITIAL  & BOUNDARY CONDITIONS
	DO I = 1, Nx
		IF(I.LE.10) THEN
			U(I,1) = 1.00
		ELSE IF(I.LE.21)  THEN
			U(I,1) = 2.d0
		ELSE
			U(I,1) = 1.00
		END IF
	END DO
	d = coefficient_diffusion*time_step/dx**2
	WRITE(*,*)"Diffusion Number :: ", d
!=========================================================================================	
! FTCS Scheme
!=========================================================================================	
	! COMPUTATION LOOP for FTCS
	DO  time_loop = 1, Nt
		DO I = 2, Nx - 1
			U(I,time_loop+1) = U(I,time_loop) + d*(U(I+1,time_loop) -2*U(I,time_loop)+ U(I-1,time_loop))
		END DO
	END DO
	! WRITING OF RESULTS
10	OPEN(100, FILE = "3.a.Diffusion_Equation_FTCS_Explicit.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "Diffusion Equation"'
		WRITE(100,*)'VARIABLES ="X","U"'
		DO  time_loop = 1, Nt	
			WRITE(100,*)'ZONE  T="FTCS Explicit ',time_loop,'", I = ',Nx,' J = ',1,' K = ',1,' F = POINT'
			DO I = 1, Nx
				WRITE(100,*)  Xg(I), U(I,time_loop)
			END DO
		END DO
	CLOSE (100)	
!=========================================================================================	
! Reinitialization for Crank-Nicholson
!=========================================================================================
	! INITIAL  & BOUNDARY CONDITIONS
!	DO J = 1, Ny
!		U(1,J) = 0.d0
!		U_new(1,J) = 0.d0
!	END DO
	! COMPUTATION LOOP for CRANK NICHOLSON FTCS implicit average	
!	DO  time_loop = 1, end_time
!		Residue = 0.d0
!		U(1,1) = -40
!		U(1,Ny) = 40
!		U_new(1,1) = U(1,1)
!		U_new(1,Ny) = U(1,Ny)
!		DO J = 2, Ny - 1
!			U_new(1,J) = ((1 - d) * U(1,J) + 0.50*d*(U_new(1,J+1)+U(1,J+1) + U_new(1,J-1)+U(1,J-1)))/(1 + d)
!		END DO
		!Pause
!		DO J = 1, Ny
!			Residue = MAX(Residue, ABS(U_new(1,J)-U(1,J)))	
!			U(1,J) = U_new(1,J)
!		END DO
!		WRITE(*,*)"Residual CN:: ",time_loop*time_step,Residue
!		IF (Residue.LT.1E-05) GO TO 20
!	END DO
	! WRITING OF RESULTS
!20	OPEN(100, FILE = "Results_Couette_Flow.dat", FORM = "FORMATTED", ACCESS="APPEND")
!		WRITE(100,*)'ZONE  T="Crank-Nicholson", I = ',1,' J = ',Ny,' F = POINT'
!		DO J = 1, Ny
!			WRITE(100,*) Yg(J), U(1,J)
!		END DO
!	CLOSE (100)	
END PROGRAM DIFFUSION