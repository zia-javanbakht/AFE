#include 'MiscTools.f'
#include 'FileTools.f'
      SUBROUTINE ufxord(xord,ncrd,n)
        USE MiscTools
        USE FileTools
        IMPLICIT NONE

!     ** Start of generated type statements **
        INTEGER n, ncrd
        REAL*8 xord
!     ** End of generated type statements **

        DIMENSION xord(ncrd)

        INTEGER :: fileUnit
        COMMON /CommonData/ fileUnit
        SAVE /CommonData/

        INTEGER :: i
        REAL*8, PARAMETER :: MAXTOLERANCE = 1.D0
        REAL*8 :: randNum
        LOGICAL :: firstRun = .TRUE.
        CHARACTER (LEN=20) :: fileName = 'MyResults'
        
        IF (firstRun .EQV. .TRUE.) THEN
          firstRun = .FALSE.
          CALL FindFreeUnit (fileUnit)
          CALL AutoFilename (fileName)

          OPEN (UNIT = fileUnit, File = fileName, ACCESS = 'SEQUENTIAL',
     &         STATUS = 'NEW', ACTION = 'READWRITE', FORM = 'FORMATTED')
          WRITE (fileUnit, 102)
          WRITE (fileUnit, 100)
          WRITE (fileUnit, 102)
        END IF
          WRITE (fileUnit,101) n, (xord(i), i = 1, 2)
          DO i = 1, 2
            randNum = RandomNumber()
            xord(i) = xord(i) + (MAXTOLERANCE * randNum)
          END DO
          WRITE (fileUnit,101) n, (xord(i), i = 1, 2)          
        RETURN
100   FORMAT ('NODE X-COORD Y-COORD')
101   FORMAT (I4,1X,2(F7.4,1X))
102   FORMAT (21('-'))

      END

      SUBROUTINE ubginc(inc,incsub)
        USE MiscTools

        IMPLICIT NONE
!     ** Start of generated type statements **
        INTEGER inc, incsub
!     ** End of generated type statements **

        IF (inc .EQ. 0) WRITE(6,*) 'First timestamp...', TimeStamp()
        RETURN
      END

      SUBROUTINE uedinc(inc,incsub)
        USE MiscTools
        
        IMPLICIT NONE
!     ** Start of generated type statements **
        INTEGER inc, incsub, i
!     ** End of generated type statements **

        INTEGER :: fileUnit
        COMMON /CommonData/ fileUnit
        SAVE /CommonData/

        IF (inc .EQ. 10) THEN
          WRITE (fileUnit, "(21('-'))")
          IF (inc .EQ. 10) WRITE(fileUnit,'(A)') 
     &    'Elapsed time is '//TimeStamp()//' second(s)'
          CLOSE (UNIT = fileUnit)
        END IF
        RETURN
      END
