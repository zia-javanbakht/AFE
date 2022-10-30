#include 'MarcTools.f'
      SUBROUTINE UFCONN(j,itype,lm,nnodmx)
        IMPLICIT NONE
!     ** Start of generated type statements **
        INTEGER itype, j, lm, nnodmx
!     ** End of generated type statements **
        DIMENSION lm(*)
      
        INTEGER :: i

        IF (j .EQ. 1) itype = -9
            
        RETURN
      END

      SUBROUTINE USELEM (m,xk,xm,nnode,ndeg,f,r,
     * jtype,dispt,disp,ndi,nshear,jpass,nstats,ngenel,
     * intel,coord,ncrd,iflag,idss,t,dt,etota,gsigs,de,
     * geom,jgeom,sigxx,nstrmu)

        USE MarcTools, ONLY : GetDistance
        IMPLICIT NONE

        INCLUDE 'concom'
        INCLUDE 'matdat'
!     ** Start of generated type statements **
        REAL*8 coord, de, disp, dispt, dt, etota, f, geom, gsigs
        INTEGER idss, iflag, intel, jpass, jgeom, jtype, m, ncrd, ndeg
        INTEGER ndi, ngenel, nnode, nshear, nstats, nstrmu
        REAL*8 r, sigxx, t, xk, xm
!     ** End of generated type statements **
        DIMENSION xk(idss,idss),xm(idss,idss),dispt(ndeg,*),disp(ndeg,*)
        DIMENSION t(nstats,*),dt(nstats,*),coord(ncrd,*)
        DIMENSION etota(ngenel,*),gsigs(ngenel,*),de(ngenel,*)
        DIMENSION f(ndeg,*),r(ndeg,*),sigxx(nstrmu,*),geom(*),jgeom(*)

        INTEGER :: i
        REAL*8 ::  elMod, length, ksi, strain, stress, internalForce,
     &             k11, k12, k22, b1, b2
     
        REAL*8, DIMENSION(3)    :: point1, point2
        REAL*8, DIMENSION(ncrd) :: cij
        REAL*8, DIMENSION(2,2)  :: kLocal
        REAL*8, DIMENSION(4,4)  :: kGlobal
        REAL*8, DIMENSION(2,4)  :: transMat, tempMat
        REAL*8, DIMENSION(4,2)  :: transMatT
        REAL*8, DIMENSION(4)    :: dispGlobal, intForceGLobal
        REAL*8, DIMENSION(2)    :: dispLocal, intForceLocal

        elMod  = et(3)

        CALL CalcTransMatrix()
  
        IF ((iflag .EQ. 2) .OR. (iflag .EQ. 4)) THEN
          IF (ncycle .EQ. 0) THEN
            dispGlobal(1) = disp(1,1)
            dispGlobal(2) = disp(2,1)
            dispGlobal(3) = disp(1,2)
            dispGlobal(4) = disp(2,2)
          ELSE
            dispGlobal(1) = disp(1,1) + dispt(1,1)
            dispGlobal(2) = disp(2,1) + dispt(2,1)
            dispGlobal(3) = disp(1,2) + dispt(1,2)
            dispGlobal(4) = disp(2,2) + dispt(2,2)
          END IF
        END IF
        
        ksi = 0.D0
        
        SELECT CASE (iflag)
        CASE (1)
          jtype = -jtype
        CASE (2)
          CALL CalcStiffness()
          CALL CalcStressStrain()
          CALL CalcInternalForce(ksi)
        CASE (3)
          jtype = -jtype
        CASE (4)
          CALL CalcStiffness()
          CALL CalcStressStrain()
          CALL CalcInternalForce(ksi)
        CASE (5)
          jtype = -jtype
        END SELECT
        
        RETURN
      CONTAINS
        SUBROUTINE CalcTransMatrix()
          point1 = 0.D0
          point2 = 0.D0
          DO i = 1, ncrd
            point1(i) = coord(i,1)
            point2(i) = coord(i,2)
          END DO
          length = GetDistance (point1, point2)
          
          DO i = 1, ncrd
            cij(i) = (coord(i,2) - coord(i,1)) / length
          END DO
            transMat = 0.D0
            transMat(1,1) = cij(1)
            transMat(1,2) = cij(2)
            transMat(2,3) = cij(1)
            transMat(2,4) = cij(2)
 
            CALL GMTRA(transMat, transMatT, 2, 4)
            RETURN
        END SUBROUTINE CalcTransMatrix
	  
      	FUNCTION GetArea(ksi)
      	  REAL*8, INTENT(IN) :: ksi
      	  REAL*8 :: GetArea
      	  
      	  GetArea = geom(1)
      	  RETURN
      	END FUNCTION GetArea
		
        SUBROUTINE CalcStiffness()
          REAL*8 :: ksi, jacobian, weight
          
          jacobian = length / 2.D0
          b1 = -0.5D0
          b2 = +0.5D0
          
          ksi    = 0.D0
          weight = 2.D0
          
          k11 = ((b1 * b1) * elMod * GetArea(ksi) / jacobian) * weight  
          k12 = ((b1 * b2) * elMod * GetArea(ksi) / jacobian) * weight
          k22 = ((b2 * b2) * elMod * GetArea(ksi) / jacobian) * weight

          kLocal(1,1) = k11
          kLocal(1,2) = k12
          kLocal(2,1) = k12
          kLocal(2,2) = k22
          
          kLocal = kLocal * (1.D0 - inc * 0.05D0)
          CALL GMPRD (kLocal, transMat, tempMat, 2, 2, 4)
          CALL GMPRD (transMatT, tempMat, kGlobal, 4, 2, 4)
          xk = kGlobal
          RETURN
        END SUBROUTINE CalcStiffness
        
        SUBROUTINE CalcInternalForce(ksi)
          REAL*8, INTENT(IN) :: ksi
 
          internalForce    = stress * GetArea(ksi)
          intForceLocal(1) = -internalForce
          intForceLocal(2) = +internalForce
          
          CALL GMPRD (transMatT, intForceLocal, intForceGLobal,4,2,1)
 
          r(1,1) = intForceGLobal(1)
          r(2,1) = intForceGLobal(2)
          r(1,2) = intForceGLobal(3)
          r(2,2) = intForceGLobal(4)
          RETURN
        END SUBROUTINE CalcInternalForce
        
        SUBROUTINE CalcStressStrain()
          CALL GMPRD (transMat, dispGlobal, dispLocal, 2, 4, 1)
          strain = (dispLocal(2) - dispLocal(1)) / length
          stress = elMod * strain
          
          de(1,1)    = strain
          etota(1,1) = strain
          gsigs(1,1) = stress
          sigxx(1,1) = stress

          RETURN
        END SUBROUTINE CalcStressStrain
        
      END
