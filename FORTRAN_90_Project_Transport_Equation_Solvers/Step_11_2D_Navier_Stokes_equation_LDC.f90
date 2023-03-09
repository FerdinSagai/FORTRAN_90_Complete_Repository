PROGRAM NSEQ_LDC

	INTEGER :: I
	INTEGER :: Nx, Ny
	INTEGER :: time_loop, N_iter
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx, Ly
	REAL(8) :: dx, dy, dt
	REAL(8) :: Source,nu
	REAL(8)	:: coefficient_convection, coefficient_diffusion
	
	REAL(8) :: Term1, Term2, Term2a, Term2b, Term2c
	
	INTEGER :: Max_Iterations, Nt
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg, Yg
	!REAL(8), DIMENSION(:,:,:), ALLOCATABLE :: U, V, P
	REAL(8), DIMENSION(:,:,:), ALLOCATABLE :: U, V, P, b
!=========================================================================================	
! Input Reading
!=========================================================================================
	Lx = 2.d0
	Ly = 2.d0
	
	Nx = 41
	Ny = 41
	
	Nt = 100
	Max_Iterations =50
	
	coefficient_convection	= 1.00
	coefficient_diffusion 	= 0.10
	
	rho 					= 1.00
	nu 					= 0.1
	dt					= 0.001
!=========================================================================================	
! Allocation of grid
!=========================================================================================
	ALLOCATE (Xg(1:Nx), Yg(1:Ny))
	ALLOCATE (U(1:Nx,1:Ny,1:Nt), V(1:Ny,1:Ny,1:Nt))
	ALLOCATE (P(1:Nx,1:Ny,1:Max_Iterations))
	ALLOCATE (b(1:Nx,1:Ny,1:Nt))
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
			U(I,J,:) = 0.d0
			V(I,J,:) = 0.d0
			P(I,J,:) = 0.d0
			b(I,J,:) = 0.d0
		END DO
	END DO

	U(:,Ny,:) = 1.00
!=========================================================================================	
!  Scheme
!=========================================================================================	
	! COMPUTATION LOOP for CS
	DO  time_loop = 1, Nt-1
		DO I = 2, Nx - 1
			DO J = 2, Ny - 1
				Term1 = (1/dt)*((u(i+1,j,time_loop)-u(i-1,j,time_loop))/(2*dx) + (v(i,j+1,time_loop)-v(i,j-1,time_loop))/(2*dy))
				Term2a = ( (u(i+1,j,time_loop) - u(i-1,j,time_loop))/(2*dx) )**2
				Term2b = 2*((u(i,j+1,time_loop)-u(i,j-1,time_loop))/(2*dy)*(v(i+1,j,time_loop)-v(i-1,j,time_loop))/(2*dx))
				Term2c = ( (v(i,j+1,time_loop) - v(i,j-1,time_loop))/(2*dy) )**2
				Term2 = Term2a +Term2b +Term2c 
				b(i,j,time_loop) = rho*(Term1 - Term2)
			END DO
		END DO
		
		DO  N_iter = 1, Max_Iterations-1
			DO I = 2, Nx - 1
				DO J = 2, Ny - 1
					Source = b(i,j,time_loop)*(dx**2)*(dy**2)
					P(i,j,N_iter+1) = coeff* ( (P(i+1,j,N_iter)+P(i-1,j,N_iter))*dy**2 + (P(i,j+1,N_iter)+P(i,j-1,N_iter))*dx**2 - Source)
				END DO
			END DO
			P(:,Ny,N_iter+1)	= P(:,Ny-1,N_iter+1)
			P(1,:,N_iter+1)	= P(2,:,N_iter+1)
			P(:,1,N_iter+1)	= P(:,2,N_iter+1)
			P(:,Ny,N_iter+1)	= 0
		END DO
		
		DO I = 2, Nx - 1
			DO J = 2, Ny - 1    
				UDUDX = u(i,j,time_loop)*dt/dx*(u(i,j,time_loop)-u(i-1,j,time_loop))
				VDUDY = v(i,j,time_loop)*dt/dy*(u(i,j,time_loop)-u(i,j-1,time_loop))
				D2UDX2 = dt/dx**2*(u(i+1,j,time_loop) - 2*u(i,j,time_loop) + u(i-1,j,time_loop))
				D2UDY2 = dt/dy**2*(u(i,j+1,time_loop) - 2*u(i,j,time_loop) + u(i,j-1,time_loop))
				Convection_term = UDUDX + VDUDY
				Diffusion_term = nu*(D2UDX2 + D2UDY2)
				Pressure_term = dt/(2*rho*dx)*(p(i+1,j,Max_Iterations)-p(i-1,j,Max_Iterations))
				U(i,j,time_loop+1) = U(i,j,time_loop) - Convection_term - Pressure_term + Diffusion_term
				
				UDVDX = u(i,j,time_loop)*dt/dx*(v(i,j,time_loop)-v(i-1,j,time_loop))
				VDVDY = v(i,j,time_loop)*dt/dy*(v(i,j,time_loop)-v(i,j-1,time_loop))
				D2VDX2 = dt/dx** 2*(v(i+1,j,time_loop) - 2*v(i,j,time_loop) + v(i-1,j,time_loop))
				D2VDY2 = dt/dy**2*(v(i,j+1,time_loop) - 2*v(i,j,time_loop) + v(i,j-1,time_loop))
				Convection_term = UDVDX + VDVDY
				Diffusion_term = nu*(D2VDX2 + D2VDY2)
				Pressure_term = dt/(2*rho*dy)*(p(i,j+1,Max_Iterations)-p(i,j-1,Max_Iterations))
				V(i,j,time_loop+1) = V(i,j,time_loop) - Convection_term - Pressure_term + Diffusion_term
			END DO
		END DO
		U(1,:,time_loop+1)  = 0
		U(:,1,time_loop+1)  = 0
		U(Nx,:,time_loop+1) = 0
		U(:,Ny,time_loop+1) = 1
		
		V(1,:,time_loop+1)  = 0
		V(:,1,time_loop+1)  = 0
		V(Nx,:,time_loop+1) = 0
		V(:,Ny,time_loop+1) = 0
	END DO
	! WRITING OF RESULTS
	OPEN(100, FILE = "11.a.Navier_Stokes_LDC.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "LDC NS Equation"'
		WRITE(100,*)'VARIABLES ="X","Y","U","V","P","b"'
		DO time_loop = 1, Nt
			WRITE(100,*)'ZONE  T="CS Explicit ',time_loop,'" , I = ',Nx,' J = ',Ny,' K = ',1,' F = POINT'
			DO J = 1, Ny
				DO I = 1, Nx
					WRITE(100,*)  Xg(I), Yg(J), U(I,J,time_loop), V(I,J,time_loop),P(I,J,Max_Iterations) ,b(I,J,time_loop) 
				END DO	
			END DO
		END DO
	CLOSE (100)	
END PROGRAM NSEQ_LDC