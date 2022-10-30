#INCLUDE 'MarcTools.f'

      SUBROUTINE UEDINC(uInc,uIncsub)
        USE MarcTools
        USE FileTools
        
        IMPLICIT NONE

!     ** Start of generated type statements **
        INTEGER uInc, uIncsub
!     ** END of generated type statements **

        CHARACTER(LEN=*), PARAMETER :: fileName = 'result.txt'
        INTEGER :: i
        INTEGER, SAVE :: fileUnit
100     FORMAT (A20,I2)        
        
      
        IF (uInc .EQ. 1) THEN 
          CALL FindFreeUnit (fileUnit)
          OPEN (UNIT = fileUnit, File = fileName, ACCESS = 'SEQUENTIAL',
     &          STATUS = 'REPLACE', ACTION = 'WRITE')
        
          CALL PrintIPCoordLst (fileUnit)
          CALL PrintNodCoordLst (1, fileUnit)
          CALL PrintElapsedTime (fileUnit)
        END IF
      
        IF (uInc .GE. 1) THEN
          WRITE (fileUnit,100) 'Increment No. ', uInc
          
          CALL PrintNodCoordLst (2, fileUnit)
          
          DO i = 1, 3
            WRITE (fileUnit,100) 'Stress ', i
            CALL PrintNodValIPLst (10+i,fileUnit)
          END DO

          DO i = 1, 3
            WRITE (fileUnit,100) 'Stress ', i
            CALL PrintNodValIPLst (10+i, fileUnit)
          END DO
        END IF
        WRITE(fileUnit,*) 'timinc=', timinc
        IF (uInc .EQ. 10) THEN
          CALL PrintElapsedTime(fileUnit)
          CLOSE(fileUnit)
        END IF
      RETURN
      END
