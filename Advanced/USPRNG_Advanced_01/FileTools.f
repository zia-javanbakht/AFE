!
! Personal FileTools Module
!
! Ver 1.00 13/12/15
! Ver 2.00 03/03/16
! - DeleteFile subroutine is added
!
      MODULE FileTools
!
        IMPLICIT NONE
!
      CONTAINS
!
        SUBROUTINE FindFreeUnit (fn)
          INTEGER, INTENT(OUT) :: fn
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
            fn = -1
          ELSE
            fn = i
          END IF
        END SUBROUTINE FindFreeUnit
!
! fn = filename
! fu = fileunit
! fe = file exists
        SUBROUTINE DeleteFile (fn)
          CHARACTER(LEN=*), INTENT (IN) :: fn
          
          INTEGER :: fu
          LOGICAL :: fe
          
          CALL FindFreeUnit (fu)
          INQUIRE (FILE = fn, EXIST=fe)
          IF (fe .EQV. .TRUE.) THEN
            OPEN  (UNIT = fu, FILE = fn, STATUS = 'OLD')
            CLOSE (UNIT = fu, STATUS = 'DELETE')
          END IF
        
        END SUBROUTINE DeleteFile
      END MODULE FileTools
