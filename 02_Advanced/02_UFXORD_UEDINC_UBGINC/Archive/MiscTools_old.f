!
! Personalized Tools 
! Last Update: 08/12/2015
! Version 1.10
! Revision #2
!
      MODULE MyTools
      
        IMPLICIT NONE
      
      CONTAINS
  
        CHARACTER(LEN = 12) FUNCTION TimeStamp()
          REAL, SAVE :: time1
          REAL :: time2, dT
          LOGICAL, SAVE :: firstRun = .TRUE.
          CHARACTER (LEN = 12) :: tempText
          
          tempText = '                   '
         IF (firstRun .EQV. .TRUE.) THEN
           firstRun = .FALSE.
           CALL cpu_time (time1)
           TimeStamp = '0.000'
           WRITE (6,*) 't1',Time1
         ELSE
           CALL cpu_time (time2)
           dT = time2 - time1
           WRITE (tempText,'(F8.3)') dT
           TimeStamp = tempText
           WRITE (6,*) 't1',Time1
           WRITE (6,*) 't2',Time2
         END IF
          
        END FUNCTION TimeStamp
        
        REAL*8 FUNCTION RandomNumber()
          REAL*8 :: tempNum
          REAL*8, SAVE :: lastNum
          REAL :: time
          INTEGER :: i, signSelect = -1
          
          CALL random_number (tempNum)
          CALL cpu_time (time)
          tempNum = tempNum * time
          IF (tempNum .GT. lastNum) signSelect = +1
          
          
          DO WHILE (tempNum .LT. 1.0D0)
            tempNum = tempNum * 10.0D0 
            signSelect = -1*signSelect
          END DO
          lastNum = tempNum
          RandomNumber = signSelect*tempNum / 10.0D0
        END FUNCTION RandomNumber
        
        
      END MODULE MyTools
        
     