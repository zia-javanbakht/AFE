      subroutine plotv(v,s,sp,etot,eplas,ecreep,t,m,nn,kcus,ndi,
     *                 nshear,jpltcd)
c* * * * * *
c
c     define a variable for contour plotting (user subroutine).
c
c     v            variable to be put onto the post file
c     s (idss)     stress array
c     sp           stresses in preferred direction
c     etot         total strain (generalized)
c     eplas        total plastic strain
c     ecreep       total creep strain
c     t            array of state variable (temperature first)
c     m(1)         user element number
c     m(2)         internal element number
c     m(3)         material id
c     m(4)         internal material id
c     nn           integration point number
c     kcus(1)      layer number
c     kcus(2)      internal layer number
c     ndi          number of direct stress components
c     nshear       number of shear stress components
c     jpltcd       the absolute value of the user's entered post code
c
c
c        the components of s, sp, etot, eplas and ecreep are given in the order
c        as defined in volume B for the current element.
c
c* * * * * *

      IMPLICIT NONE
      
      INCLUDE '../common/matdat'

c     ** Start of generated type statements **
      real*8 ecreep, eplas, etot
      integer jpltcd, kcus, m, ndi, nn, nshear
      real*8 s, sp, t, v
c     ** End of generated type statements **
      dimension s(*),etot(*),eplas(*),ecreep(*),sp(*),m(4),kcus(2),
     *          t(*)
c
!      et(3)=E
!      xu(3)=poisson
      REAL*8 :: inv1, inv2

      
      IF (jpltcd .EQ. 1) THEN
        inv1 = s(1) + s(2)
        v =  inv1 / 3.D0
      ELSE IF (jpltcd .EQ. 2) THEN
        inv1 = s(1) + s(2)
        inv2 = s(1)*s(2)-s(3)**2
        v =  SQRT(2.D0*(inv1**2)-6.D0*inv2) / 3.D0
      ELSE IF (jpltcd .EQ. 3) THEN
        v =  -xu(3)*(s(1) + s(2))/et(3)
      END IF
      
      
      WRITE (6,*) 'm(1)', m(1)      
      WRITE (6,*) 'm(2)', m(2)      
      WRITE (6,*) 'm(3)', m(3)      
      WRITE (6,*) 'm(4)', m(4)      
      WRITE (6,*) 'nn', nn

      WRITE (6,*) 's(1)', s(1)
      WRITE (6,*) 's(2)', s(2)
      WRITE (6,*) 's(3)', s(3)
      WRITE (6,*) 'xu(3)', xu(3)
      WRITE (6,*) 'et(3)', et(3)
      WRITE (6,*) 'jpltcd ', jpltcd

      
      WRITE (6,*) 'v', v

      RETURN
      END

