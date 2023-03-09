PROGRAM FILES_IO

	CHARACTER(LEN = 80)	:: USER_NAME
	CHARACTER(LEN =  4)	:: USER_GRADE
	INTEGER(KIND = 4)	:: USER_ID
	REAL(KIND = 8)		:: SALARY


	WRITE(*,*) "Enter the user name ::" 
	READ(*,*) USER_NAME
	WRITE(*,*) "Enter the user grade ::" 
	READ(*,*) USER_GRADE
	WRITE(*,*) "Enter the user id ::" 
	READ(*,*) USER_ID
	WRITE(*,*) "Enter the user Salary ::" 
	READ(*,*) SALARY

	OPEN (10,FILE='Information.txt',FORM='FORMATTED')
		WRITE (10,*)
		WRITE (10,*) USER_NAME
		WRITE (10,*) USER_GRADE
		WRITE (10,*) USER_ID
		WRITE (10,*) SALARY
	CLOSE(10) 

	OPEN(3,FILE='Information.txt',FORM='FORMATTED')
		READ(3,*)
		READ(3,*) USER_NAME
		READ(3,*) USER_GRADE
		READ(3,*) USER_ID
		READ(3,*) SALARY
	CLOSE(3)
	
	
		WRITE(*,*) "The user name is ::", USER_NAME
		WRITE(*,*) "The user grade is ::",USER_GRADE
		WRITE(*,*) "The user id is ::",USER_ID
		WRITE(*,*) "The user salary is ::",SALARY
END PROGRAM FILES_IO