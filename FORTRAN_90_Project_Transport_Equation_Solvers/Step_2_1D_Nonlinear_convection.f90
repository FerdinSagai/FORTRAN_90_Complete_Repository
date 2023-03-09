! 1D WAVE EQUATION :: FIRST ORDER
PROGRAM WAVE_EQUATION
	INTEGER :: I
	INTEGER :: Nx, Nt
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx
	REAL(8) :: dx
	
	INTEGER :: time_loop, end_time
	REAL(8) :: time_step
	REAL(8)	:: coefficient_convection
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg
	
	REAL(8) :: CFL, Residue
	REAL(8), DIMENSION(:,:), ALLOCATABLE :: U
!=========================================================================================	
! Input Reading
!=========================================================================================
	Nx =41
	Lx = 2
	Nt =20
	sigma = 0.50d0
	coefficient_convection = 1
!=========================================================================================	
! Allocation of grid
!=========================================================================================
	ALLOCATE (Xg(1:Nx))
	ALLOCATE (U(1:Nx,1:Nt))
!=========================================================================================	
! Grid generation
!=========================================================================================
	dx = Lx/Nx
	time_step = sigma * dx
	DO I = 1, Nx
		Xg(I) = (I-1)*dx
	END DO
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
	CFL = coefficient_convection*time_step/dx
	WRITE(*,*)"Courant Freidrich Lewy Number :: ", CFL
!=========================================================================================	
! FTBS Scheme
!=========================================================================================	
	! COMPUTATION LOOP for FTBS
	DO  time_loop = 1, Nt
		DO I = 2, Nx - 1
			U(I,time_loop+1) = U(I,time_loop) - U(I,time_loop)*time_step/dx * (U(I,time_loop) - U(I-1,time_loop))
		END DO
	END DO
	! OUTPUT WRITING	
	OPEN(100, FILE = "2.a.Nonlinear_Wave_1D_FTBS.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "Wave Motion"'
		WRITE(100,*)'VARIABLES ="X","U"'
		DO time_loop = 1, Nt
			WRITE(100,*)'ZONE  T="FTBS Explicit ',time_loop,'", I = ',Nx,' J = ',1,' K = ',1,' F = POINT'
			DO I = 1, Nx
				WRITE(100,*) Xg(I), U(I,time_loop)
			END DO
		END DO
	CLOSE (100)		
END PROGRAM WAVE_EQUATION