PROGRAM monty_hall
IMPLICIT none
! 1. Variables of the problem
INTEGER(4) :: jackpot, final_door, i
REAL(4) :: random, wins, losses, wins_without_changing, losses_without_changing
INTEGER(2), DIMENSION(3) :: doors
INTEGER(2), DIMENSION(2) :: doors_to_open
INTEGER(2), DIMENSION(2) :: participants_door
LOGICAL :: change_of_door

! Initialize the variables that need to initilialized
wins = 0 
losses = 0
wins_without_changing = 0
losses_without_changing = 0
DO i=1, 1000000
    ! 2. Generate a random number to assign the door behind which the jackpot will be
    change_of_door = .false.
    doors = 0
    random = 0
    CALL RANDOM_NUMBER(random)

    IF (random .LT. (1.0/3.0)) THEN
        doors(1) = 1
        jackpot = 1
    ELSE IF (random .LT. (2.0/3.0)) THEN
        doors(2) = 1
        jackpot = 2
    ELSE 
        doors(3) = 1
        jackpot = 3
    END IF

    ! 3. Generate a random number to simulate the door's decision of the participant
    random = 0
    CALL RANDOM_NUMBER(random)

    IF (random .LT. (1.0/3.0)) THEN
        doors_to_open(1) = 1
        doors_to_open(2) = 2
        participants_door(1) = 3
    ELSE IF (random .LT. (2.0/3.0)) THEN
        participants_door(1) = 1
        doors_to_open(1) = 2
        doors_to_open(2) = 3
    ELSE 
        doors_to_open(1) = 1
        participants_door(1) = 2
        doors_to_open(2) = 3
    END IF

    ! 4. Implement Monty's decision of opening one of the doors

    IF (participants_door(1) .EQ. jackpot) THEN
        random = 0
        CALL RANDOM_NUMBER(random)
        IF (random .LT. (1.0/2.0)) THEN 
            participants_door(2) = doors_to_open(1)
        ELSE
            participants_door(2) = doors_to_open(2)
        END IF 
    ELSE 
        IF (doors_to_open(1) .EQ. jackpot) THEN
            participants_door(2) = doors_to_open(1)
        ELSE IF (doors_to_open(2) .EQ. jackpot) THEN
            participants_door(2) = doors_to_open(2)
        END IF
    END IF 

    !WRITE(*,*) participants_door, jackpot

    ! 5. Participant decides randomly whether to change or not his door
    random = 0
    CALL RANDOM_NUMBER(random)
    IF (random .LT. 0.5) THEN
        final_door = participants_door(2)
        change_of_door = .true.
    ELSE 
        final_door = participants_door(1)
    END IF


    ! 6. Monty opens the participant's door
    IF (change_of_door) THEN
        IF (jackpot .EQ. final_door) THEN
            wins = wins + 1
        ELSE 
            losses = losses + 1
        END IF
    ELSE 
        IF (jackpot .EQ. final_door) THEN
            wins_without_changing = wins_without_changing + 1
        ELSE 
            losses_without_changing = losses_without_changing + 1
        END IF
    END IF
END DO

WRITE(*,*) 'Changing the door:'
!WRITE(*,*) 'Total = ', wins+losses
WRITE(*,*) 'Percentage of wins = ', wins/(wins+losses)*100.0, '%'
WRITE(*,*) 'Percentage of losses = ', losses/(wins+losses)*100.0, '%'
WRITE(*,*) ''
WRITE(*,*) 'Not changing the door:'
!WRITE(*,*) 'Total = ', wins_without_changing+losses_without_changing
WRITE(*,*) 'Percentage of wins = ', wins_without_changing/(wins_without_changing+losses_without_changing)*100.0, '%'
WRITE(*,*) 'Percentage of losses = ', losses_without_changing/(wins_without_changing+losses_without_changing)*100.0, '%'

END PROGRAM monty_hall