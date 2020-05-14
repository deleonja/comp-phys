! 1. Para 1<i<m hacer:
!    Hacer el producto de la entrada ij de A con la entrada 

PROGRAM kronecker
IMPLICIT none

INTEGER(4) :: i, j, k, l

INTEGER, DIMENSION(2,2) :: A
INTEGER, DIMENSION(3,3) :: B
INTEGER, DIMENSION(6,6) :: C


FORALL(j = 1:2) A(j,j) = 1
FORALL(j = 1:3) B(j,j) = 1


DO i = 1, 2
  DO j = 1, 2
    DO k = 1, 3
      DO l = 1, 3
        C(3*(i-1)+k, 3*(j-1)+l) = A(i,j)*B(k,l)  
      END DO 
    END DO
  END DO
END DO

DO i = 1, 6
  WRITE(*,*) C(i,i)
END DO

END PROGRAM kronecker
