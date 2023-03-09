PROGRAM NSEQ_LDC

	INTEGER :: I,J,K,N
	INTEGER :: Nx, Ny, Nz
	INTEGER :: time_loop, N_iter
	REAL(8) :: PI = 3.1416
	REAL(8) :: Lx, Ly, Lz
	REAL(8) :: dx, dy, dz
	REAL(8) :: dt
	REAL(8) :: DUDX, DVDY, DWDZ
	REAL(8) :: DUDX2, DVDY2, DWDZ2
	REAL(8) :: Source,nu
	REAL(8)	:: coefficient_convection, coefficient_diffusion
	
	REAL(8) :: Term1, Term2, Term2a, Term2b, Term2c
	
	INTEGER :: Max_Iterations, Nt
	REAL(8), DIMENSION(:), ALLOCATABLE :: Xg, Yg, Zg
	REAL(8), DIMENSION(:,:,:,:), ALLOCATABLE :: U, V,W, P, b
!=========================================================================================	
! Input Reading
!=========================================================================================
	Lx = 2.d0
	Ly = 2.d0
	Lz = 2.d0
	
	Nx = 41
	Ny = 41
	Nz = 41
	
	Nt = 1000
	Max_Iterations =50
	
	coefficient_convection	= 1.00
	coefficient_diffusion 	= 0.10
	
	rho 					= 1.00
	nu 					= 0.1
	dt					= 0.001
!=========================================================================================	
! Allocation of grid
!=========================================================================================
	ALLOCATE (Xg(1:Nx), Yg(1:Ny), Zg(1:Nz))
	ALLOCATE (U(1:Nx,1:Ny,1:Nz,1:Nt), V(1:Ny,1:Ny,1:Nz,1:Nt), W(1:Ny,1:Ny,1:Nz,1:Nt))
	ALLOCATE (P(1:Nx,1:Ny,1:Nz,1:Max_Iterations))
	ALLOCATE (b(1:Nx,1:Ny,1:Nz,1:Nt))
!=========================================================================================	
! Grid generation
!=========================================================================================
	dx = Lx/Nx
	dy = Ly/Ny
	dz = Lz/Nz
	DO I = 1, Nx
		Xg(I) = (I-1)*dx
	END DO
	
	DO J = 1, Ny
		Yg(J) = (J-1)*dy
	END DO
	
	DO K = 1, Nz
		Zg(K) = (K-1)*dz
	END DO
	coeff = 0.5/(dx**2 + dy**2 + dz**2)
!=========================================================================================	
! Initialization
!=========================================================================================	
	! INITIAL  & BOUNDARY CONDITIONS
	DO I = 1, Nx
		DO J = 1, Ny
			DO K = 1, Nz
				U(I,J,K,:) = 0.d0
				V(I,J,K,:) = 0.d0
				W(I,J,K,:) = 0.d0
				P(I,J,K,:) = 0.d0
				b(I,J,K,:) = 0.d0
			END DO
		END DO
	END DO
	U(:,Ny,:,:) = 1.00
!=========================================================================================	
!  Scheme
!=========================================================================================
	! COMPUTATION LOOP for CS
	DO  time_loop = 1, Nt-1
		DO I = 2, Nx - 1
			DO J = 2, Ny - 1
				DO K = 2, Nz - 1
					DUDX	= (u(i+1,j,k,time_loop) - u(i-1,j,k,time_loop))/(2*dx)
					DVDY	= (v(i,j+1,k,time_loop) - v(i,j-1,k,time_loop))/(2*dy)
					DWDZ	= (w(i,j,k+1,time_loop) - w(i,j,k-1,time_loop))/(2*dz)
					Term1	= (1/dt)*(DUDX + DVDY + DWDZ)	
					
					DUDX2	= DUDX**2
					DVDY2	= DVDY**2
					DWDZ2	= DWDZ**2
					
					Term2a = 2*((u(i,j+1,k,time_loop)-u(i,j-1,k,time_loop))/(2*dy)*(v(i+1,j,k,time_loop)-v(i-1,j,k,time_loop))/(2*dx))
					Term2b = 2*((u(i,j,k+1,time_loop)-u(i,j,k-1,time_loop))/(2*dz)*(w(i+1,j,k,time_loop)-w(i-1,j,k,time_loop))/(2*dx))
					Term2c = 2*((w(i,j+1,k,time_loop)-w(i,j-1,k,time_loop))/(2*dy)*(v(i,j,k+1,time_loop)-v(i,j,k-1,time_loop))/(2*dz))
					
					Term2 = DUDX2 +  DVDY2 + DWDZ2 + Term2a +Term2b +Term2c 
					b(i,j,k,time_loop) = rho*(Term1 - Term2)
				END DO
			END DO
		END DO
		
		DO  N_iter = 1, Max_Iterations-1
			DO I = 2, Nx - 1
				DO J = 2, Ny - 1
					DO K = 2, Nz - 1
						Source = b(i,j,k,time_loop)*(dx**2)*(dy**2)*(dz**2)
						TermPx = (P(i+1,j,k,N_iter)+P(i-1,j,k,N_iter))*dy**2*dz**2
						TermPy = (P(i,j+1,k,N_iter)+P(i,j-1,k,N_iter))*dx**2*dz**2
						TermPz = (P(i,j,k+1,N_iter)+P(i,j,k-1,N_iter))*dx**2*dy**2
						P(i,j,k,N_iter+1) = coeff* (TermPx+TermPy+TermPz  - Source)
					END DO
				END DO
			END DO
			P(1,:,:,N_iter+1)	= P(2,:,:,N_iter+1)
			P(Nx,:,:,N_iter+1)	= P(Nx-1,:,:,N_iter+1)
			
			P(:,1,:,N_iter+1)	= P(:,2,:,N_iter+1)
			P(:,Ny,:,N_iter+1)	= P(:,Ny-1,:,N_iter+1)
			
			P(:,:,1,N_iter+1)	= P(:,:,2,N_iter+1)
			P(:,:,Nz,N_iter+1)	= P(:,:,Nz-1,N_iter+1)
			
			P(:,Ny,:,N_iter+1)	= 0
		END DO
		
		DO I = 2, Nx - 1
			DO J = 2, Ny - 1    
				DO K = 2, Nz - 1    
					UDUDX = u(i,j,k,time_loop)*dt/dx*(u(i,j,k,time_loop)-u(i-1,j,k,time_loop))
					VDUDY = v(i,j,k,time_loop)*dt/dy*(u(i,j,k,time_loop)-u(i,j-1,k,time_loop))
					WDUDZ = w(i,j,k,time_loop)*dt/dz*(u(i,j,k,time_loop)-u(i,j,k-1,time_loop))
					
					D2UDX2 = dt/dx**2*(u(i+1,j,k,time_loop) - 2*u(i,j,k,time_loop) + u(i-1,j,k,time_loop))
					D2UDY2 = dt/dy**2*(u(i,j+1,k,time_loop) - 2*u(i,j,k,time_loop) + u(i,j-1,k,time_loop))
					D2UDZ2 = dt/dz**2*(u(i,j,k+1,time_loop) - 2*u(i,j,k,time_loop) + u(i,j,k-1,time_loop))
					
					Convection_term = UDUDX + VDUDY + WDUDZ
					Diffusion_term = nu*(D2UDX2 + D2UDY2 + D2UDZ2)
					Pressure_term = dt/(2*rho*dx)*(p(i+1,j,k,Max_Iterations)-p(i-1,j,k,Max_Iterations))
					U(i,j,k,time_loop+1) = U(i,j,k,time_loop) - Convection_term - Pressure_term + Diffusion_term
					! ********************************************************************
					UDVDX = u(i,j,k,time_loop)*dt/dx*(v(i,j,k,time_loop)-v(i-1,j,k,time_loop))
					VDVDY = v(i,j,k,time_loop)*dt/dy*(v(i,j,k,time_loop)-v(i,j-1,k,time_loop))
					WDVDZ = w(i,j,k,time_loop)*dt/dz*(v(i,j,k,time_loop)-v(i,j,k-1,time_loop))
					
					D2VDX2 = dt/dx** 2*(v(i+1,j,k,time_loop) - 2*v(i,j,k,time_loop) + v(i-1,j,k,time_loop))
					D2VDY2 = dt/dy**2*(v(i,j+1,k,time_loop) - 2*v(i,j,k,time_loop) + v(i,j-1,k,time_loop))
					D2VDZ2 = dt/dz**2*(v(i,j,k+1,time_loop) - 2*v(i,j,k,time_loop) + v(i,j,k-1,time_loop))
					
					Convection_term = UDVDX + VDVDY + WDVDZ
					Diffusion_term = nu*(D2VDX2 + D2VDY2 + D2VDZ2)
					Pressure_term = dt/(2*rho*dy)*(p(i,j+1,k,Max_Iterations)-p(i,j-1,k,Max_Iterations))
					V(i,j,k,time_loop+1) = V(i,j,k,time_loop) - Convection_term - Pressure_term + Diffusion_term
					! ********************************************************************
					UDWDX = u(i,j,k,time_loop)*dt/dx*(w(i,j,k,time_loop)-w(i-1,j,k,time_loop))
					VDWDY = v(i,j,k,time_loop)*dt/dy*(w(i,j,k,time_loop)-w(i,j-1,k,time_loop))
					WDWDZ = w(i,j,k,time_loop)*dt/dz*(w(i,j,k,time_loop)-w(i,j,k-1,time_loop))
					
					D2WDX2 = dt/dx** 2*(w(i+1,j,k,time_loop) - 2*w(i,j,k,time_loop) + w(i-1,j,k,time_loop))
					D2WDY2 = dt/dy**2*(w(i,j+1,k,time_loop) - 2*w(i,j,k,time_loop) + w(i,j-1,k,time_loop))
					D2WDZ2 = dt/dz**2*(w(i,j,k+1,time_loop) - 2*w(i,j,k,time_loop) + w(i,j,k-1,time_loop))
					
					Convection_term = UDWDX + VDWDY + WDWDZ
					Diffusion_term = nu*(D2WDX2 + D2WDY2 + D2WDZ2)
					Pressure_term = dt/(2*rho*dz)*(p(i,j,k+1,Max_Iterations)-p(i,j,k-1,Max_Iterations))
					W(i,j,k,time_loop+1) = W(i,j,k,time_loop) - Convection_term - Pressure_term + Diffusion_term
				END DO
			END DO
		END DO
		
		U(1,:,:,time_loop+1)  = 0
		U(Nx,:,:,time_loop+1) = 0
		U(:,1,:,time_loop+1)  = 0
		U(:,Ny,:,time_loop+1) = 1
		U(:,:,1,time_loop+1)  = 0
		U(:,:,Nz,time_loop+1) = 0

		V(1,:,:,time_loop+1)  = 0
		V(Nx,:,:,time_loop+1) = 0
		V(:,1,:,time_loop+1)  = 0
		V(:,Ny,:,time_loop+1) = 0
		V(:,:,1,time_loop+1)  = 0
		V(:,:,Nz,time_loop+1) = 0
		
		W(1,:,:,time_loop+1)  = 0
		W(Nx,:,:,time_loop+1) = 0
		W(:,1,:,time_loop+1)  = 0
		W(:,Ny,:,time_loop+1) = 0
		W(:,:,1,time_loop+1)  = 0
		W(:,:,Nz,time_loop+1) = 0
	END DO
!=========================================================================================	
!  Output
!=========================================================================================	
	! WRITING OF RESULTS
	OPEN(100, FILE = "11.a.Navier_Stokes_LDC.dat", FORM = "FORMATTED")
		WRITE(100,*)'TITLE = "LDC NS Equation"'
		WRITE(100,*)'VARIABLES ="X","Y","Z","U","V","W","P","b"'
		DO N = 1, Nt
			WRITE(100,*)'ZONE  T="CS Explicit ',N,'" , I = ',Nx,' J = ',Ny,' K = ',Nz,' F = POINT'
			DO K = 1, Nz
				DO J = 1, Ny
					DO I = 1, Nx
						WRITE(100,*)  Xg(I), Yg(J), Zg(K), U(I,J,K,N), V(I,J,K,N), W(I,J,K,N),P(I,J,K,Max_Iterations) ,b(I,J,K,N) 
					END DO	
				END DO
			END DO
		END DO
	CLOSE (100)	
END PROGRAM NSEQ_LDC