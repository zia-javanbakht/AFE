!
! Personal FileTools Module
!
! Ver 1.00 13/12/15
! Ver 2.00 03/03/16
! - DeleteFile subroutine is added
! Ver 3.00 03/03/16
! - AutoRenameFile is added
! Ver 3.01 03/03/16
! - FindFreeUnit is changed from a fUnitnction to a subroutine

      MODULE FileTools
!
        IMPLICIT NONE
!
      CONTAINS
!
      SUBROUTINE FindFreeUnit (fName)
        INTEGER, INTENT(OUT) :: fName
        INTEGER, PARAMETER :: MINUNIT = 10, MAXUNIT = 200
        INTEGER :: i
        LOGICAL :: uCon, found
      
        found = .FALSE.
        i = MINUNIT
        DO WHILE ((i .LT. MAXUNIT) .AND. (found .EQV. .FALSE.))
          INQUIRE (UNIT = i, OPENED = uCon)
          IF (uCon .EQV. .FALSE.) THEN 
            found = .TRUE.
          ELSE
            i = i + 1
          END IF
        END DO
        IF ((i .EQ. MAXUNIT) .AND. (found .EQV. .FALSE.)) THEN
          fName = -1
        ELSE
          fName = i
        END IF
      END SUBROUTINE FindFreeUnit
! fName = filename
! fUnit = fileunit
!
       SUBROUTINE DeleteFile (fName)
         CHARACTER(LEN=*), INTENT (IN) :: fName

         INTEGER :: fUnit
         LOGICAL :: fe

         CALL FindFreeUnit (fUnit)
         INQUIRE (FILE = fName, EXIST=fe)
         IF (fe .EQV. .TRUE.) THEN
           OPEN  (UNIT = fUnit, FILE = fName, STATUS = 'OLD')
           CLOSE (UNIT = fUnit, STATUS = 'DELETE')
         END IF
       END SUBROUTINE DeleteFile       
!
! fName: filename
!
      SUBROUTINE AutoFilename (fName)
        CHARACTER (LEN=*), INTENT(INOUT) :: fName
        
        CHARACTER (LEN = LEN(fName)+8) :: tempfName
        CHARACTER (LEN = 3) :: digit
        INTEGER :: i, j
        LOGICAL :: fileExist
90        FORMAT (i3.3)          
      
        fileExist = .TRUE.
        i = 0
        DO WHILE (fileExist .EQV. .TRUE.)
          i = i + 1
          WRITE (digit, 90) i
          tempfName = trim(fName)//'_'//digit//'.txt'

          INQUIRE (FILE = tempfName, EXIST=fileExist)
        END DO
        fName = tempfName
        RETURN
       
      END SUBROUTINE AutoFilename

        
      END MODULE FileTools
