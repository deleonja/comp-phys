!    26.02.2020
!    goldbachs_conjecture.f95
!    José Alfredo de León (deleongarrido.jose@gmail.com)

!    Programa Goldbach's conjecture

!    Codificación del texto: UTF-8
!    Compiladores probados: GNU Fortran (Ubuntu 18.04 Linux) 7.5.0
!    Instrucciones de compilación: no requiere nada más
!    gfortran -Wall -pedantic -std=f95 -c -o gammaR.o gammaR.f95
!    gfortran -o gammaR.x gammaR.o 

!    Copyright (C) 2020
!    J. A. de León Garrido
!    deleongarrido.jose@gmail.com
!
!    This program is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.
!  
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    General Public License for more details.
!  
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see
!    <http://www.gnu.org/licenses/>.
!  
!    
!    This program implements the Euler's definition of the Gamma
!    function. Because of the restriction dictated by the variable's 
!    precision this implementation uses a value of n up to 20. Therefore,
!    the range of use of this implementation is extremely limited. 

PROGRAM gamma_recurisve
IMPLICIT none
! 1. Implement the gamma function. Go to the bottom of the program. 

! 2. Define the problem's variables
    COMPLEX(16) :: z, a, gamma_function
    REAL(16) :: real_z, imaginary_z
    INTEGER(16) :: n

! 3. Ask the user for the number to which it'll be calculated its Gamma function
    !WRITE(*,*) 'A continuación ingrese el número del que quiera calcular la función Gamma, utilizando la definición de Euler.'
    !WRITE(*,*) 'Ingrese la parte real del número:'
    !READ(*,*) real_z


    !WRITE(*,*) 'Ingrese la parte imaginaria del número:'
    !READ(*,*) imaginary_z

! 4. Deffensive programming: stop the program if the user enters a number that is 
! out of the domain of the implemented definition of the Gamma function
    !IF (real_z .LE. 0) THEN 
    !    IF ( real_z .EQ. real((int(real_z)))) THEN
    !        WRITE(*,*) 'Ha ingresado un número cuya parte real es entero y menor o igual a cero. El programa se ha detenido.'
    !        STOP
    !    END IF
    !END IF 

    OPEN(UNIT=12, FILE='gamma_param.txt',STATUS='OLD')
        READ(12,*) real_z
        READ(12,*) imaginary_z
    CLOSE(12)
    
! 5. Execute the gamma function of the number entered by the user
    z = cmplx(real_z,imaginary_z)
    n = 33

! 6. Print the result
    a = gammaR(z, n)

    write(*,*) a
END PROGRAM gamma_recurisve


FUNCTION gamma_function( z, n )
IMPLICIT none 
! 1. Define the variables of the gamma function
    COMPLEX(16) :: denominator, z, gamma_function, gammaR
    INTEGER(16) :: n, factorial

! 2. Define the control variables
    INTEGER(16) :: i

! 3. Implement the factorial function. (Go to the next function)

! 4. Calculate the denominator of the Euler's definition
    denominator = 1
    DO i = 1, (n + 1)  
        denominator = denominator*(z + i - 1)
    END DO 

! 5. Calculate the value of the gamma function
    gamma_function = (factorial(n) / denominator) * n**z
END FUNCTION gamma_function

FUNCTION factorial(num)
IMPLICIT none
! 1. Define the function's variables
    INTEGER(16) :: num, factorial

! 2. Define the control variables
    INTEGER(16) :: i

! 3. Compute de factorial's procedure
    factorial = 1 !debes inicializar tu funcio
    DO i = 1, num
        factorial = factorial*i 
    END DO
RETURN 
END FUNCTION factorial 

RECURSIVE FUNCTION gammaR(z, n) RESULT(gamma)
IMPLICIT none
! 1. Define the subroutine's variables

    ! Define the 
    COMPLEX(16) :: z, gamma_function, gamma
    REAL(16) :: real_z, imaginary_z
    
    ! Define the variables for the peg's labels
    INTEGER(16) :: n

! 2. 
    IF (REAL(z) .LE. 1) THEN 
        gamma = gamma_function(z, n)
    ELSE 
        real_z = REAL(z) - 1
        imaginary_z = AIMAG(z)
        z = cmplx(real_z, imaginary_z)
        gamma = z * gammaR(z, n)
    END IF 

END FUNCTION gammaR


! for i in `seq 0 0.05 0.95`; do echo $i $i > param.conf ; /usr/bin/time -f "%e %M"  -a -o gamma2_resources.txt ./gamma2.x ; done
