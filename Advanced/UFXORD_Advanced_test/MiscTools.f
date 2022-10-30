! Personalized Miscellaneous Tools 
!
! v2.02 03/03/16
! Number of routines 2
! Revision #02 
!
! v3.01 29/01/16
! - LocateItem FUNCTION added
! v3.02 
! - RandomNumber is completely changed; now more efficient
      MODULE MiscTools
      
        IMPLICIT NONE
      
      CONTAINS
  
        CHARACTER(LEN = 12) FUNCTION TimeStamp ()
          REAL, SAVE :: time1
          REAL :: time2, dT
          LOGICAL, SAVE :: firstRun = .TRUE.
          CHARACTER (LEN = 12) :: tempText
          
          tempText = '                   '
         IF (firstRun .EQV. .TRUE.) THEN
           firstRun = .FALSE.
           CALL cpu_time (time1)
           TimeStamp = '0.000'
!           WRITE (6,*) 't1',Time1
         ELSE
           CALL cpu_time (time2)
           dT = time2 - time1
           WRITE (tempText,'(F8.3)') dT
           TimeStamp = tempText
!           WRITE (6,*) 't1',Time1
!           WRITE (6,*) 't2',Time2
         END IF
          
        END FUNCTION TimeStamp

        REAL*8 FUNCTION RandomNumber ()
          LOGICAL :: firstRun = .TRUE.
          REAL*8 :: tempNum1, tempNum2
          
          IF (firstRun .EQV. .TRUE.) THEN
            firstRun = .FALSE.
            CALL random_seed ()
          END IF
          
          CALL random_number (tempNum1)
          CALL random_number (tempNum2)
          
          RandomNumber = tempNum1 - tempNum2
        END FUNCTION RandomNumber


        
!       REAL*8 FUNCTION RandomNumber ()
!         REAL*8 :: tempNum
!         REAL*8, SAVE :: lastNum
!         REAL :: time
!         INTEGER :: i, signSelect = -1
!         
!         CALL random_seed ()
!         CALL random_number (tempNum)
!         CALL cpu_time (time)
!         tempNum = tempNum * time
!         IF (tempNum .GT. lastNum) signSelect = +1
!         
!         
!         DO WHILE (tempNum .LT. 1.0D0)
!           tempNum = tempNum * 10.0D0 
!           signSelect = -1*signSelect
!         END DO
!         lastNum = tempNum
!         RandomNumber = signSelect*tempNum / 10.0D0
!       END FUNCTION RandomNumber
c       
c it    item
c ls    list
c lss   list size
c
c The function searches ls of size lss for the it
c
      INTEGER FUNCTION LocateItem (it, ls, lss)
        REAL*8, INTENT(IN)   :: it
        REAL*8, DIMENSION(*), INTENT(IN) :: ls
        INTEGER, INTENT(IN) :: lss

        INTEGER :: i

        LocateItem = 0
        DO i = 1, lss
          IF (ls(i) .EQ. it) LocateItem = i
        END DO

      END FUNCTION LocateItem
!
! Quick sorts in ascending order the (ls) list of size (lss)
!             
      SUBROUTINE QuickSort (ls, lss)
        REAL*8, DIMENSION(*), INTENT(INOUT) :: ls
        INTEGER, INTENT(IN)   :: lss
        
         
      END SUBROUTINE QuickSort

      
      END MODULE MiscTools
        
     