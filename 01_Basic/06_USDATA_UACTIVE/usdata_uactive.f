      subroutine usdata(kin,kou,ic)
        IMPLICIT NONE
        INCLUDE 'dimen'
c     ** Start of generated type statements **
        INTEGER ic, kin, kou
c     ** End of generated type statements **
c
c user subroutine for usdata option
c
c kin  = input unit
c kou  = output unit
c ic   = 1 : pre-reader
c      = 2 : real reader
c
        INTEGER, DIMENSION(20) :: elementList
        INTEGER :: i
        COMMON /marc_usdacm/ elementList
        
c
        IF (ic .EQ. 1) WRITE (6,*) 'Pre-reader'
        IF (ic .EQ. 2) THEN
          WRITE (6,*) 'Reader'
          READ  (kin,*) (elementList(i), i = 1, nusdat)
          WRITE (kou,*) (elementList(i), i = 1, nusdat)
        END IF
      
        return
      end

      subroutine uactive(m,n,mode,irststr,irststn,inc,time,timinc)
        IMPLICIT NONE
        INCLUDE 'dimen'
        
c     ** Start of generated type statements **
      integer inc, irststn, irststr, m, mode, n
      real*8 time, timinc
c     ** End of generated type statements **
      dimension m(3),mode(3)
c
c     user routine to activate or deactivate an element
c
c     m(1)        - user element id 
c     m(2)        - master element number for local adaptivity
c     m(3)        - internal element number
c     n           - internal elsto number
c
c     mode(1)=-1  - deactivate element and remove element from post file
c     mode(1)=-11 - deactivate element and keep element on post file
c     mode(1)=2   - leave in current status
c     mode(1)=1   - activate element and add element to post file
c     mode(1)=11  - activate element and keep status on post file
c
c     mode(2)=0   - activate/deactivate element in all physics passes
c     mode(2)=1   - only activate/deactivate mechanical part in coupled
c     mode(2)=2   - only activate/deactivate thermal part in coupled
c
c     mode(3)=0   - activation/deactivation at the end of increment
c     mode(3)=1   - activation/deactivation at the beginning of increment
c
c     irststr     - reset stresses to zero
c     irststn     - reset strains to zero
c     inc         - increment number
c     time        - time at beginning of increment
c     timinc      - incremental time
c
        INTEGER, DIMENSION(20) :: elementList
        INTEGER :: i
        COMMON /marc_usdacm/ elementList
        
        WRITE (6,*) '** UACTIVE'
!       WRITE (6,*) 'nusdat=', nusdat
!       WRITE (6,*) (elementList(i), i = 1, nusdat)
!        mode(2) = 2
!        IF (inc .GT. 1) THEN  
!          IF (m(1) .EQ. elementList(inc-1)) THEN
!            mode(1) = -1
!            mode(2) = 0
!            mode(3) = 1
!            irststr = 1
!            irststn = 1
!          END IF
!        END IF
        RETURN
      end
