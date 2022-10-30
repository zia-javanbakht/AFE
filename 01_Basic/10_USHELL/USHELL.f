      subroutine ushell(thick,xintp,ncrd,m,nn)
        implicit none

c     ** Start of generated type statements **
        integer m, ncrd, nn
        real*8 thick, xintp
c     ** End of generated type statements **
c
c     user routine to update thickness of shell - 
c     not recommended if large strain simulation because 
c       it will interfere with updating of shell thickness
c
c     input
c     xintp    - integration point coordinates in global system
c     ncrd     - number of integration point coordinates
c     m(1)     - element number
c     m(2)     - internal number
c     nn       - integration point number
c
c     output
c     thick    - thickness
c
        dimension xintp(*),m(2)
        
        REAL*8, PARAMETER :: TLEN = 5.D0, t0 = 0.1D0
        
        WRITE (6,*) '*** USHELL'
        WRITE (6,*) 'xintp               :', xintp(1:3)
        WRITE (6,*) 'ncrd                :', ncrd
        WRITE (6,*) 'Element ID #        :', m(1)
        WRITE (6,*) 'Element internal #  :', m(2)
        WRITE (6,*) 'Integration point # :', nn
        
        WRITE (6,*) 'Thick               :', thick        
        
        !thick = ((xintp(1)/TLEN)+1)*t0
        thick = t0

        WRITE (6,*) 'New thickness       :', thick        

        RETURN
      END
