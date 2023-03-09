PROGRAM DATATYPES

CHARACTER  			:: Letter 
CHARACTER(LEN = 90) :: String 
COMPLEX x

REAL(KIND = 4)		:: float_real
REAL(KIND = 8)		:: double_real 
REAL(KIND = 16)		:: long_double_real
DOUBLE PRECISION	:: alternate_double_real

INTEGER(KIND = 1)	:: Num
INTEGER(KIND = 2)	:: short_Num
INTEGER(KIND = 4)	:: long_Num
INTEGER(KIND = 8)	:: long_long_Num

LOGICAL(KIND = 1)	::	L1
LOGICAL(KIND = 2)	::	L2
LOGICAL(KIND = 4)	::	L3
LOGICAL(KIND = 8)	::	L4


Letter = "A"
WRITE(*,*) "Print out the character variable		::", Letter

String = "FORTRAN90"
WRITE(*,*) "Print out the character string variable	::", String

x = (1,-4)
WRITE(*,*) "Print out the complex variable			::", REALPART(x), " + ", IMAGPART(x),"i"

Num				= 21
WRITE(*,*) "Print out the integer(1) variable			::", Num
short_Num		= 21477
WRITE(*,*) "Print out the integer(2) variable			::", short_Num
long_Num		= 214778943
WRITE(*,*) "Print out the integer(4) variable			::", long_Num
long_long_Num	= 214778943
WRITE(*,*) "Print out the integer(8) variable			::", long_long_Num


float_real				= 0.2147789436873768736574565656465468
WRITE(*,*) "Print out the real(4) variable			::", float_real
double_real 			= 0.2147789436873768736574565656465468
WRITE(*,*) "Print out the real(8) variable			::", double_real
long_double_real		= 0.2147789436873768736574565656465468
WRITE(*,*) "Print out the real(16)variable			::", long_double_real
alternate_double_real	= 0.2147789436873768736574565656465468
WRITE(*,*) "Print out the double precision variable	::", alternate_double_real

L1	= .TRUE.
WRITE(*,*) "Print out the logical(1) variable			::", L1
L2	= .TRUE.
WRITE(*,*) "Print out the logical(2) variable			::", L2
L3	= .FALSE.
WRITE(*,*) "Print out the logical(4) variable			::", L3
L4	= .FALSE.
WRITE(*,*) "Print out the logical(8) variable			::", L4
END PROGRAM DATATYPES