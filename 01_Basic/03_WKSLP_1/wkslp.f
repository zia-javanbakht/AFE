      subroutine wkslp(m,nn,kcus,matus,slope,ebarp,eqrate,stryt,dt,
     *  ifirst)
#ifdef _IMPLICITNONE
      implicit none
#else
      implicit logical (a-z)
#endif
c     ** Start of generated type statements **
      real*8 dt, ebarp, eqrate
      integer ifirst, kcus, m, matus, nn
      real*8 slope, stryt
c     ** End of generated type statements **
      dimension matus(2),kcus(2)
c* * * * * *
c
c     user subroutine to define work hardening slope.
c
c     m            is the current user element number
c     nn           is the integration point number
c     kcus(1)      layer number
c     kcus(2)      internal layer number
c     matus(1)     is the current user material id
c     matus(2)     is the current internal material id
c     slope        work hardening slope to be defined in this routine
c     ebarp        is the total equivalent plastic strain
c     eqrate       is the equivalent plastic strain rate
c     stryt        current yield stress that optionally can be defined in this routine
c     dt           is the current total temperature
c     ifirst       flag distinguishing tenth cycle properties for
c                  ornl option
c
c    the internal element number mint can be obtained with
c      mint=ielint(m)
c
c* * * * * *

      write (6,*) '**WKSLP'
      WRITE (6,*) 'Plastic Strain ', ebarp
c     IF (ebarp .EQ. 0.0) THEN
c       slope = 0.0D0
c       stryt = 190.0D0
c     ELSE  -2933.33*\value x*\value x+ 880*\value x + 190;
        stryt = (-2933.33D0)*ebarp**2 + (880.D0)*ebarp + 190.D0
        slope = (-2933.33D0*2.D0)*ebarp + (880.D0)
c       IF (stryt .LT. 190.0) stryt = 190.0D0
c     END IF
      WRITE (6,*) 'Yield stress ', stryt
      return
      end
