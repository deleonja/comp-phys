! Números de Fibonnacci: fórmula recursiva

! 0, 1, 0+1=1, 1+1=2, 2+1=3, 3+2=5, 5+3=8, 8+5=13...

PROGRAM fibonacci
IMPLICIT none

! 1. Definir un arreglo f donde se almacenarán los números de Fibonacci y guaradar en f0=0 y f1=1, los primeros dos números de la serie de Fibonacci
INTEGER(4) :: n, i
INTEGER(16), ALLOCATABLE ,DIMENSION(:) :: fibonacci_numbers


! 2. Definir el número n de la serie de fibonacci que se desea
 OPEN(UNIT=12,FILE="param.conf",STATUS="OLD")
   READ(12,*) n
 CLOSE(12)
 
ALLOCATE(fibonacci_numbers(n+1)) ! los arreglos de FORTRAN95 inician en 1 

fibonacci_numbers(1) = 0
fibonacci_numbers(2) = 1

! 3. Desde i=3 hasta i=n hacer:
!   3.1 f[i]=f_[i-1]+f_[i-2]
DO i = 3, n+1
  fibonacci_numbers(i) = fibonacci_numbers(i-1)+fibonacci_numbers(i-2) 
END DO

WRITE(*,*) fibonacci_numbers(n+1)

END PROGRAM fibonacci

