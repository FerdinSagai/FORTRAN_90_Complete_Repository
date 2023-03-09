! 1D WAVE EQUATION :: FIRST ORDER
PROGRAM WAVE_EQUATION
	INTEGER :: I, J, K
	INTEGER :: Nx, Ny, Nz, Nt
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx, Ly, Lz
	REAL(8) :: dx, dy, dz
	
	INTEGER :: time_loop, end_time
	REAL(8) :: time_step
	REAL(8)	:: coefficient_convection
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg, Yg, Zg
	
	REAL(8) :: CFL, Residue
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
	Nt =20
	sigma = 0.50d0
	coefficient_convection = 1
!=========================================================================================	
! Allocation of grid
!=========================================================================================
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
	time_step = sigma * MAX(dx,dy,dz)
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
	
	CFL = coefficient_convection*time_step/MAX(dx,dy,dz)
	WRITE(*,*)"Courant Freidrich Lewy Number :: ", CFL
!=========================================================================================	
! FTBS Scheme
!=========================================================================================	
	! COMPUTATION LOOP for FTBS
	DO  time_loop = 1, Nt
		DO I = 2, Nx - 1
			U(I,time_loop+1) = U(I,time_loop) - U(I,time_loop)*time_step/dx * (U(I,time_loop) - U(I-1,time_loop))
		END DO
		DO J = 2, Ny - 1
			V(J,time_loop+1) = V(J,time_loop) - V(J,time_loop)*time_step/dy * (V(J,time_loop) - V(J-1,time_loop))
		END DO	
		DO K = 2, Nz - 1
			W(K,time_loop+1) = W(K,time_loop) - W(K,time_loop)*time_step/dz * (W(K,time_loop) - W(K-1,time_loop))
		END DO	
	END DO
	! OUTPUT WRITING	
	OPEN(100, FILE = "14.a.NonLinear_Wave_3D_FTBS.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "Wave Motion"'
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
END PROGRAM WAVE_EQUATION