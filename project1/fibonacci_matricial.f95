! Números de fibonacci: forma matricial 

PROGRAM fibonacci_mat
IMPLICIT none

! 1. Definir la matriz que se elevará a la n-ésima potencia
INTEGER(4) :: n, i, j, k, l
INTEGER(4), DIMENSION(2,2) :: fibonacci_matrix
INTEGER(4), DIMENSION(2,2) :: f_result, dummy

fibonacci_matrix(1,1) = 1
fibonacci_matrix(1,2) = 1
fibonacci_matrix(2,1) = 1
fibonacci_matrix(2,2) = 0

dummy = fibonacci_matrix

! 2. Definir el número n de la serie de fibonacci que se desea 
n = 10

! 3. Hacer la multiplicación de matrices 
DO l = 1, n - 2
  f_result = 0
  DO i = 1, 2
    DO j = 1, 2
      DO k = 1, 2
        f_result(i,j) = f_result(i,j) + fibonacci_matrix(i,k)*dummy(k,j)
      END DO
    END DO
  END DO
  dummy = f_result
END DO  

WRITE(*,*) f_result(1,1)

END PROGRAM
