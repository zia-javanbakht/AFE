      subroutine uinstr(s,ndi,nshear,n,nn,kcus,xintp,ncrd,inc,time,
     1                  timeinc)

        IMPLICIT NONE

c     ** Start of generated type statements **
      integer inc, kcus, n, ncrd, ndi, nn, nshear
      real*8 s, time, timeinc, xintp
c     ** End of generated type statements **
      dimension s(*),xintp(ncrd),n(2),kcus(2)
c    *******************************
c    s      stress vector
c    ndi    number of stress components
c    nshear number of shear stress components
c    n(1)   user element number
c    n(2)   internal element number
c    nn     interation point number
c    kcus(1) layer number
c    kcus(2) internal layer number
c    xintp  array of int point coordinates
c    ncrd   number of coordinates
c    inc    increment number
c    time   total time at begin of increment
c    timeinc incremental time
c    *******************************
c
        REAL*8, PARAMETER :: TLEN = 100.D0, TSTRESS = 50.D0
      
        s(1) = xintp(1)*TSTRESS/TLEN
        s(2) = -s(1)
        s(3) = 0.D0
      
      RETURN
      END
