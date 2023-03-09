PROGRAM BASICIO
! This is how a comment is written in FORTRAN 90
	CHARACTER(80) :: YOUR_NAME
	WRITE(*,*) "HELLO, WORLD!"					! Entereing text that will be displayed on the terminal
	WRITE(*,*)									! Entereing an empty line in the terminal
	
	
	WRITE(*,*) "Enter your name"
	READ(*,*) YOUR_NAME
	WRITE(*,*) "HELLO, ", YOUR_NAME
END PROGRAM BASICIO
