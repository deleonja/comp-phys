PROGRAM hanoi_towers
IMPLICIT none 
    INTEGER(4) :: counter, temp, IOstatus, n

    OPEN(UNIT=12, FILE="param.conf", STATUS="OLD")
        READ(12,*) n
    CLOSE(12)
    
    OPEN(UNIT=12, FILE='temp.dat', STATUS="NEW")

! 1. Call the recursive subroutine that implements the algorithm
        CALL move(n, 'a', 'b', 'c')
    CLOSE(12)

    OPEN(UNIT=12, FILE='temp.dat', STATUS="OLD")
        counter = -1
        IOstatus = 0
            DO WHILE (IOstatus .ge. 0)
                READ(12,*, IOSTAT=IOstatus) temp
                counter = counter + temp
                !WRITE(*,*) 'counter = ', counter
                !WRITE(*,*) 'IOstatus= ', IOstatus
            END DO
    CLOSE(12, STATUS="DELETE")

    WRITE(*,*) 'Cantidad de pasos = ', counter
END PROGRAM hanoi_towers

RECURSIVE SUBROUTINE move(n, a, b, c)
IMPLICIT none
! 1. Define the subroutine's variables

! Define the variable for the number of disks
    INTEGER(4) :: n, counter
! Define the variables for the peg's labels
    CHARACTER :: a, b, c
! 2. Implement a decision making structure to specify the disk movements. 

    counter = 0
! If there's only one disk left to move then move it to peg 'c'
    IF (n .EQ. 1) THEN 
        !WRITE(*,*) a, '->', c
        WRITE(12,*) 1
        !WRITE(*,*) 1
! If there's n > 1 disks left to move then:
! - move n-1 disks from peg 'a' to peg 'b'
! - move 1 disk from peg 'a' to peg 'c'
! - move n-1 disks from peg 'b' to peg 'c'
    ELSE 
        CALL move(n-1, a, c, b)
        CALL move(1, a, b, c)
        CALL move(n-1, b, a, c)
    END IF 
    
    
    !WRITE(12,*) counter
    
    
    !WRITE(*,*) counter

END SUBROUTINE move
! for i in `seq 0 20`; do echo $i > param.conf; /usr/bin/time -f "%e %M %P"  -a -o tower_of_hanoi.rend ./tower_of_hanoi.x ; done
