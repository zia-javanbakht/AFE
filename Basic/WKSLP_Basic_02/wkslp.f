      subroutine wkslp(m,nn,kcus,matus,slope,ebarp,eqrate,stryt,dt,
     *  ifirst)

      implicit none
      INCLUDE "ctable"
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
      REAL*8 :: stressFun, slopeFun, plasticStrain, yieldStress
      INTEGER :: i, idTable
      write (6,*) '**WKSLP'
      WRITE (6,*) 'Plastic Strain ', ebarp
!        stressFun = (-2933.33D0)*ebarp**2 + (880.D0)*ebarp + 190.D0
!        slopeFun  = (-2933.33D0*2.D0)*ebarp + (880.D0)


        CALL tabva2 (1.D0, yieldStress, 2, 0, 1)
        WRITE(6,*) 'stress tabl =', yieldStress

!        WRITE(6,*) 'stress func =', stressFun
!        WRITE(6,*) 'slope func  =', slopeFun

        stryt = yieldStress
      return
      end
