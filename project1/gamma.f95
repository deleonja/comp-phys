PROGRAM use_of_gamma_function
implicit none
    complex(8) :: z, a
    integer :: n
    
    z = cmplx(1,2)
    n = 100

    a = gamma_function(z, n)

END PROGRAM use_of_gamma_function


FUNCTION gamma_function( z, n )
IMPLICIT none 
    COMPLEX(8) :: denominator, z, gamma_function
    INTEGER(8) :: i, n, factorial

    denominator = 1
    DO i = 1, (n + 1)  
        denominator = denominator*(z + i - 1)
    END DO 

    gamma_function = (factorial(n) / denominator) * n**z
END FUNCTION gamma_function

FUNCTION factorial(num)
IMPLICIT none
    INTEGER(8) :: i, num, result, factorial
    DO i = 1, num
        result = result*i 
    END DO
    factorial = result 
RETURN 
END FUNCTION factorial 


