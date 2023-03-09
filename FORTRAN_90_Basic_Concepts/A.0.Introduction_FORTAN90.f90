PROGRAM BASIC_INFO
	WRITE(*,*) "Welcome to my documentation of FORTRAN 90."
	WRITE(*,*) "It is easily the first programming language that I achieved mastery in."
	WRITE(*,*) "Compared to other languages it is a strange prog. language with some peculiarities."
	WRITE(*,*) "It is weaker than most of the other modern languages, but it makes up for it through simpliicity."
	WRITE(*,*) "Even to this day, FORTRAN 90 is so engraved in my mind, that Pseudocodes I generate for complex solvers automatically in FORTRAN syntax"
	WRITE(*,*) "Lets check out some of the appealling and annoying aspects of FORTRAN 90 !"
	WRITE(*,*) 
	WRITE(*,*) 
	WRITE(*,*) "						APPEALS  "
	WRITE(*,*) "1. SYNTAX"
	WRITE(*,*) "The syntax of FORTRAN 90 is so simple, that is is very easy to code ideas and first drafts!"
	WRITE(*,*) "True, python is much better for this but FORTRAN codes are vanilla without any libraries and can be directly translated to other languages.!"
	WRITE(*,*) "						ANNOYANCES "
	WRITE(*,*) "1. DECLARATION OF VARIABLES"
	WRITE(*,*) "All variables in FORTRAN must be declared at the beginning of the program."
	WRITE(*,*) "This is unlike modern languages where variables with limited scope can be declared anywhere."
	WRITE(*,*) "This makes using temporary variables difficult!"

	WRITE(*,*) "						UNDETERMINABLE "
	WRITE(*,*) "1. CASE SENSITIVITY"
	WRITE(*,*) "FORTRAN is case insensitive. So essentially, variables X and x are the same. This is a mixed bag."
	WRITE(*,*) "On one hand you can just code without concerning yourself with maintaining the same case, however, this also limits the number of variables you can deploy."
	WRITE(*,*) "There can arise instances in coding solvers were you want to use the same variable name, differing in case alone"
	WRITE(*,*) "For instance, maybe U and u represent X velocity before and after an update."
END PROGRAM BASIC_INFO