PROGRAM monty_hall
IMPLICIT none
! 1. Variables of the problem
INTEGER(4) :: wins, losses
REAL(4) :: door_assignment 
REAL(4), DIMENSION(3) :: doors
INTEGER(4), PARAMETER :: seed = 760013

! Initialize the variables that need to initilialized
wins = 0 
losses = 0
doors = 0

! 2. Generate a random number to assign the door behind which the jackpot will be
CALL RANDOM_NUMBER(doors)
WRITE(*,*) 'Random number = ', doors
WRITE(*,*) doors + 1
WRITE(*,*) (doors .le. 1.0/3.0)

! 3. Generate a random number to simulate the door's decision of the participant

! 4. Implement Monty's decision of opening one of the doors

! 5. Participant decides randomly to change or not his door

! 6. Monty opens the participant's door
END PROGRAM monty_hall