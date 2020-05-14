!    26.02.2020
!    goldbachs_conjecture.f95
!    José Alfredo de León (deleongarrido.jose@gmail.com)

!    Programa Goldbach's conjecture

!    Codificación del texto: UTF-8
!    Compiladores probados: GNU Fortran (Ubuntu 18.04 Linux) 4.8.5
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o holaMundo.o holaMundo.f90
!    gfortran -o holaMundo.x holaMundo.o 

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
!    This program does the following:
!    1. Creates a list of the prime numbers in [2,n]
!    2. Finds the representation of n as sum of two primes
!    3. Finds the representation of n as sum of three primes
!  
!    To verify the Goldbach's conjecture a for loop, for i=2,4,6,...,n; must be
!    implemented in the bash to create a file 'param.conf' with the number 'i' in
!    it, and run parts 1 and 2 of this program in each iteration.

PROGRAM goldbachs_conjecture
IMPLICIT none

!	Problem's integer variables:
INTEGER(4) :: i, j, k, l 
INTEGER(4) :: n, num_of_primes, greatest_multiple, factor
INTEGER(4) :: counter2, counter3

! Problem's logical variable:
LOGICAL(4) :: switch

! Control variable
INTEGER(4) :: err

! Problem's data arrays
INTEGER, ALLOCATABLE, DIMENSION(:) :: numbers 
INTEGER, ALLOCATABLE, DIMENSION(:) :: primes 
INTEGER, ALLOCATABLE, DIMENSION(:) :: prime1
INTEGER, ALLOCATABLE, DIMENSION(:) :: prime2
INTEGER, ALLOCATABLE, DIMENSION(:) :: prime3

! Read the number n from param.conf up to which wants to verify de 
! Goldbach's conjecture and which wants to find the representation
! as sum of 2 and 3 primes
 OPEN(UNIT=12,FILE="param.conf",STATUS="OLD",IOSTAT=err)
   READ(12,*) n
 CLOSE(12)

! Assign the dimension of 'numbers' and 'primes' 
ALLOCATE(numbers(n),STAT=err)
ALLOCATE(primes(n), STAT=err) ! Apriori, we don't know the number of primes
!                               in [2,n], but it's certain it won't exceed n

primes = 0

!	Storage the numbers [2,n] in 'numbers'
DO i = 1, n-1 
  numbers(i) = i + 1
END DO

! Search for the last number that will be used in the Eratosthenes' algorithm
switch = .false. 
j = 0
DO WHILE (switch .eqv. .false.)
  j = j + 1
  IF (numbers(j)**2 .gt. n) THEN
    greatest_multiple = numbers(j)
    switch = .true.
  END IF
END DO

!j = 0
!DO WHILE (NOT numbers(j)**2 .gt. n)
!  j = j + 1
!END DO
!greatest_multiple = numbers(j)

! The following do loop reproduces the Eratosthenes' algorithm to 
!	find the prime numbers in 'numbers'.
DO i = 1, greatest_multiple 
  factor = 2
  IF (numbers(i) .eq. 0) THEN ! If i+1 has been crossed out go to 
                              ! i+2.
    CYCLE
  END IF
  j = numbers(i)
  DO WHILE (j*factor .le. n)
    numbers(j*factor - 1) = 0 ! Cross out the number j*factor 
                              !	(i.e. all the multiples of j)
    factor = factor + 1
  END DO
END DO

! The following do loop storages the prime numbers in 'primes'.
j = 1
DO i = 1, n-1
  IF (numbers(i) .ne. 0) THEN
    primes(j) = numbers(i)
    j = j + 1
  END IF
END DO
num_of_primes = j - 1 ! number of primes in [2,n]

! Allocate the dimension of the arrays that will storage the primes that
! sum n. 
! 
! prime1 + prime2 = n
ALLOCATE(prime1(num_of_primes**2), STAT=err)
ALLOCATE(prime2(num_of_primes**2), STAT=err) 

prime1 = 0
prime2 = 0

!	The following nested do while loops search for two prime numbers
!	in 'primes' that sum 'n'.
k = 1
j = 1
DO WHILE (primes(j) .ne. 0) !	Keep going while the j-th number in 'primes' is actually a prime
  i = 1
  DO WHILE (primes(i) .ne. 0) !	Keep going while the i-th number in 'primes' is actually a prime
    IF (primes(j) + primes(i) .eq. n) THEN !	If two primes sum 'n' enter the if
      prime1(k) = primes(j)
      prime2(k) = primes(i) 
      WRITE(*,*) prime1(k) + prime2(k), ' = ', prime1(k), ' + ', prime2(k)
      k = k + 1
    END IF
    i = i + 1
  END DO
  j = j + 1
END DO

! Look for only one of the possible representations as a sum of two primes of n 
counter2 = 0
DO j = 1, k/2
    WRITE(*,*) prime1(j) + prime2(j), ' = ', prime1(j), ' + ', prime2(j)
    counter2 = counter2 + 1
END DO

DEALLOCATE(prime1)
DEALLOCATE(prime2)

! Allocate the dimension of the arrays that will storage the primes that
! sum n. 
! 
! prime1 + prime2 + prime3 = n
ALLOCATE(prime1(num_of_primes**3), STAT=err)
ALLOCATE(prime2(num_of_primes**3), STAT=err) 
ALLOCATE(prime3(num_of_primes**3), STAT=err) 

prime1 = 0
prime2 = 0
prime3 = 0

!	The following nested do while loops search for three prime numbers
!	in 'primes' that sum 'n'.
j = 1
l = 1
DO WHILE (primes(j) .ne. 0) !	Keep going while the j-th number in 'primes' is actually a prime
  i = 1
  DO WHILE (primes(i) .ne. 0) !	Keep going while the i-th number in 'primes' is actually a prime
    k = 1
    DO WHILE (primes(k) .ne. 0)
      IF (primes(j) + primes(i) + primes(k) .eq. n) THEN !	If two primes sum 'n' enter the if
        prime1(l) = primes(j)
        prime2(l) = primes(i)
        prime3(l) = primes(k)
        l = l + 1
      END IF
      k = k + 1
    END DO
    i = i + 1
  END DO
  j = j + 1
END DO

! Look for only one of the possible representations as a sum of three primes of n 
DO j = 1, (l-1)
    DO i = 1, (l-1)
      IF ((prime1(j)*prime2(j)*prime3(j) .eq. prime1(i)*prime2(i)*prime3(i)) .and. (i .ne. j)) THEN
        prime1(i) = 0
        prime2(i) = 0
        prime3(i) = 0
      END IF
    END DO
END DO

! Rearrange the entries in the arrays to avoid having zeros between non-zero entries
counter3 = 0
j = 1
DO i = 1, l-1
  IF (prime1(i) .NE. 0) THEN
    prime1(j) = prime1(i)
    prime2(j) = prime2(i)
    prime3(j) = prime3(i)
    j = j + 1
    counter3 = counter3 + 1
  END IF
END DO

END PROGRAM goldbachs_conjecture
