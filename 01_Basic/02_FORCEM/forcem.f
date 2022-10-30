      subroutine forcem(press,th1,th2,nn,n)

      IMPLICIT NONE
      INCLUDE 'creeps'

c     ** Start of generated type statements **
      REAL*8 :: prnorm(3)
      character*32 :: cdum, bcname
      COMMON /marc_lpres3/prnorm
      COMMON /marc_bclabel/cdum, bcname
      integer n, nn
      real*8 press, th1(*), th2(*)
      
      
c     ** End of generated type statements **
      dimension n(10)
c* * * * * *
c
c     defined non-uniformed distributed force on an element.
c
c   If NOT table driven input style:
c     press        distributed load increment magnitude
c                  if follower force then give total magnitude at the end of increment
c   If table driven input style:
c     press        total distributed load magnitude at the end of increment
c---------------------
c   2D analyis
c
c     th1          1st component of the integration point coordinate
c     th2          2nd component of the integration point coordinate
c---------------------
c   3D analyis
c
c     th1          x,y,z coordinte of the integration point
c     th2          direction vector of the pressure
c---------------------
c   2D and 3D analysis
c     nn           integration point number (for distributed load analysis)
c     n(1)         user element number
c     n(2)         parameter identifying the type of load
c     n(3)         not used
c     n(4)         not used
c     n(5)         distributed load index for non-table driven input style
c                  it can be used to indentify which loads is being process 
c                  in case of multiple DIST LOADS definition in the model
c     n(6)         not used
c     n(7)         internal element number
c     n(8)         not used
c     n(9)         0= not general CID load flag
c                  1= general CID load flag
c     n(10)        not used
c     cdum         not used
c     bcname       boundary condition name for table driven input style
c                  it can be used to indentify which loads is being process
c                  in case of multiple DIST LOADS definition in the model
c     prnorm       direction cosine of the direction of the load 
c                  with respect to the global system. 
c                  It has to be set for general CID traction load, 
c                  nonuniform volumetric load (IBODY=107), 
c                  nonuniform force per unit length (IBODY=111) or
c                  nonuniform for per unit area (IBODY=113
c* * * * * *
      REAL*8, PARAMETER :: TFORCE = -1000.0, TLEN = 100.0
      REAL*8 :: maxForcePerLen, curTime, totTime  
! Linear distribution
      curTime = timinc + cptim
      totTime = 1.0D0
      
      maxForcePerLen = (2.0*TFORCE/TLEN)
      press = (th1(1)/TLEN)*maxForcePerLen*curTime/totTime
      
      write(6,*) 'press = ', press
!      prnorm = [1.7*0.5D0, 1.7*0.5D0, 0.0D0]
      
      
      WRITE (6,*) 'Load index       ', n(5)
      WRITE (6,*) 'User Element No. ', n(1)
      WRITE (6,*) 'Int. Element No. ', n(7)
      WRITE (6,*) 'Int. point No.   ', nn
      WRITE (6,*) 'coord x          ', th1(1), th1(2), th1(3)
      WRITE (6,*) 'coord y          ', th2(1), th2(2), th2(3)
      WRITE (6,*) 'timinc ', timinc
      WRITE (6,*) 'cptime ', cptim
      
           
      return
      end
