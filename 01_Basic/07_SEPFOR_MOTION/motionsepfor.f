      subroutine motion(x,f,v,time,dtime,nsurf,inc)
c
c
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c     user routine to provide surface motion data
c
c------------------------------------------------------------------------
c     Caution: The increment number (inc), which is passed from the
c              calling routine, is useful in making sure the data given
c              is appropriately attributed to the relevant increments.
c              For example, a center of rotation given for increment 0
c                       if(inc.eq.0) then
c                          x(1)=...
c                          etc.
c                       endif
c              will be updated internally as motion and deformation take
c              place.  However, if the increment is not specified, then
c              this center of rotation will be reset each time this routine
c              is read, i.e. every increment. Obviously the results will
c              be different for the two cases.
c
c              The same caution is also applicable to the surface number
c              (nsurf).  Thus the data should be coupled only with the
c              surface number(s) for which it is meant. For example:
c                       if(nsurf.eq.2 .or. nsurf.eq.4) then
c                          x(1)=...
c                          ...
c                          v(1)=...
c                          etc.
c                       endif
c
c   Important: The correct use of increment numbers and surface numbers
c              may give rise to more complex (or nested) if statements.
c              For example:
c                       if(inc.eq.2 .and. nsurf.eq.3) then
c                          ...
c                       elseif(....) then
c                          if(....) then
c                             ...
c                          endif
c                          ...
c                       endif
c------------------------------------------------------------------------
c
c 2-d:
c     input  :   nsurf      - number of the surface for which data is
c                             requested
c                time       - the time at which data is requested
c                dtime      - the current time increment
c                x(3)       - current die defining coordinates:
c                             x(1) = 1st coordinate of center of
c                                        rotation
c                             x(2) = 2nd coordinate of center of
c                                        rotation
c                             x(3) = angle rotated around z-axis
c
c                f(3)       - the current surface load:
c                             f(1) = 1st component of load
c                             f(2) = 2nd component of load
c                             f(3) = moment
c                inc        - the increment number
c
c     output :   v(3)       - current surface velocities
c                             v(1) = 1st component of the velocity at
c                                        the center of rotation.
c                             v(2) = 2nd component of the velocity at
c                                        the center of rotation.
c                             v(3) = angular velocity
c
c 3-d:
c     input  :   nsurf      - the number of the surface for which data
c                             is requested
c                time       - the time at which data is requested
c                dtime      - the current time increment
c                x(6)       - current die defining coordinates:
c                             x(1) = 1st coordinate of center of
c                                        rotation
c                             x(2) = 2nd coordinate of center of
c                                        rotation
c                             x(3) = 3rd coordinate of center of
c                                        rotation
c                             Axis for specifying angular velocity:
c                             x(4) = 1st component of direction cosine
c                             x(5) = 2nd component of direction cosine
c                             x(6) = 3rd component of direction cosine
c
c                f(6)       - the current surface load:
c                             f(1) = 1st component of load
c                             f(2) = 2nd component of load
c                             f(3) = 3nd component of load
c                             f(4) = 1st component of moment
c                             f(5) = 2nd component of moment
c                             f(6) = 3rd component of moment
c                inc        - the increment number
c
c     output :   v(4)       - current surface velocities
c                             v(1) = 1st component of the velocity at
c                                        the center of rotation
c                             v(2) = 2nd component of the velocity at
c                                        the center of rotation
c                             v(3) = 3nd component of the velocity at
c                                        the center of rotation
c                             v(4) = angular velocity around axis defined
c                                    above with x(4), x(5), and x(6).
c
c
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c
#ifdef _IMPLICITNONE
      implicit none
#else
      implicit logical (a-z)
#endif
c     ** Start of generated type statements **
      real*8 dtime, f
      integer inc, nsurf
      real*8 time, v, x
c     ** End of generated type statements **
      dimension x(*),v(*),f(*)
c
c
      REAL*8, PARAMETER :: PI = 3.1415927D0

      IF (nsurf .EQ. 2) THEN
!       v(1) = 50.D0*SIN(2.D0*PI*time)
      write(6,*) '** motion 1'
      write(6,*) 'nsurf=', nsurf
      write(6,*) 'time=', time
      write(6,*) 'dtime=', dtime
      write(6,*) 'inc=', inc
      write(6,*) 'x=', x(1:3)
      write(6,*) 'v=', v(1:3)
      write(6,*) 'f=', f(1:3)
          
       v(1) = 0.D0
       v(2) = 2.D0*SIN(2.0D0*PI*time)
!       v(1) = 0.D0
      END IF
      write(6,*) 'x=', x(1:3)
      return
      end
      
      
      subroutine sepfor(fnorm,ftang,ibody,nnode,inc)
#ifdef _IMPLICITNONE
      implicit none
#else
      implicit logical (a-z)
#endif
c     ** Start of generated type statements **
      real*8 fnorm, ftang
      integer ibody, inc, nnode
c     ** End of generated type statements **
c
c     fnorm: normal separation force (output)
c     ftang: tangential separation force (output, not necessary for most cases)
c     ibody: current body or die number the node touched (input)
c     nnode: current touched external node number (input)
c     inc  : current increment number
c
      INTEGER, PARAMETER :: NCOUNT = 6
      INTEGER, DIMENSION(NCOUNT) :: nodeList
      REAL*8,  DIMENSION(NCOUNT) :: forceList
      INTEGER :: i
      
      nodeList = [13, 15, 17, 19, 21, 2]
      forceList= [0.D0, 1.D0, 2.D0, 3.D0, 4.D0, 5.D0]
      
      WRITE (6,*) '** SEPFOR'
      WRITE (6,*) 'inc= ', inc
      WRITE (6,*) 'nnode= ', nnode
      WRITE (6,*) 'ibody=', ibody
      WRITE (6,*) 'fnorm=', fnorm
      WRITE (6,*) 'ftang=', ftang
!     IF (ibody .EQ. 3) THEN
!       IF (inc .GT. 1) THEN
!         WRITE (6,*) 'touching'
!         DO i = 1, NCOUNT
!           write(6,*) 'node(i)= ', nodeList(i)
!           IF (nodeList(i) .EQ. nnode) THEN
 !                     write(6,*) 'inside'
!             ftang = forceList(i)*10*0.5
!             fnorm = forceList(i)*10
!           END IF
!         END DO
!       END IF
!     END IF
!
!     Note that even for the glued curve this will work and must be careful about it.      
      IF (ibody .EQ. 3 ) THEN
        SELECT CASE (nnode)
        CASE (6)
          fnorm = 300.D0
          ftang = 0.D0
        CASE (8)
          fnorm = 450.D0
          ftang = 0.D0
        CASE(10)
          fnorm = 550.D0
          ftang = 0.D0
        CASE(12)
          fnorm = 240.D0
          ftang = 0.D0
        CASE DEFAULT
          fnorm = 500.D0
          ftang = 0.D0
        END SELECT
      END IF 
      

      return
      end
