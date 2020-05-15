PROGRAM buffons_needle
IMPLICIT none
    REAL(16) :: random, length, x, theta, distance, d, calculated_pi
    INTEGER :: n, needles, i 
    REAL, PARAMETER :: Pi = 3.1415927

    n = 0

    OPEN(UNIT=12, FILE="param.conf", STATUS="OLD")
        READ(12,*) needles
    CLOSE(12)

    d = 1.0
    length = 1.0 

    DO i = 1, needles
        random = 0
        CALL random_number(random)
        x = random * ( d / 2.0 )

        random = 0 
        CALL random_number(random)
        theta = random * (Pi/2)

        distance = (length / 2.0) * SIN( theta )

        IF (x .le. distance) THEN
            n = n + 1
        END IF
    END DO

    calculated_pi = (2.0 * needles) / n

    WRITE(*,*) 'Pi = ', calculated_pi
    
    OPEN(UNIT=12,FILE="calculated_pi_16.bn",STATUS="OLD",POSITION="APPEND")
        WRITE(12,*) needles, calculated_pi
    CLOSE(12)

END PROGRAM buffons_needle