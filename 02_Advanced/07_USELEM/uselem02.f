#include 'MarcTools.f'
      SUBROUTINE UFCONN(j,itype,lm,nnodmx)
        IMPLICIT NONE

!     ** Start of generated type statements **
        INTEGER itype, j, lm, nnodmx
!     ** End of generated type statements **
        DIMENSION lm(*)
      
        INTEGER :: i

        itype = -9
            
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
! If there is an Inc suffix, the variable is an incremental one, 
! otherwise it is a total value.
        INTEGER :: i
        REAL*8 ::  elMod, length, ksi,
     &             dispTot1, dispTot2, strain, stress, 
     &             dispInc1, dispInc2, strainInc, stressInc,
     &             internalForceInc, internalForce, k11, k12, k22, b1,b2
     
        REAL*8, PARAMETER :: A1 = 40.D0, A2 = 80.D0
      
        REAL*8, DIMENSION(3) :: point1, point2
 
        elMod  = et(3)
        
        point1 = 0.D0
        point2 = 0.D0
        DO i = 1, ncrd
          point1(i) = coord(i,1)
          point2(i) = coord(i,2)
        END DO
        length = GetDistance (point1, point2)
        
        IF ((iflag .EQ. 2) .OR. (iflag .EQ. 4)) THEN
          IF (ncycle .EQ. 0) THEN
            dispInc1 = disp(1,1)
            dispInc2 = disp(1,2)
          ELSE
            dispTot1 = disp(1,1) + dispt(1,1)
            dispTot2 = disp(1,2) + dispt(1,2)
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
	  
      	FUNCTION GetArea(ksi)
      	  REAL*8, INTENT(IN) :: ksi
      	  REAL*8 :: GetArea
      	  
      	  GetArea = A1 + (A2 - A1) * GetX(ksi) / LENGTH

      	  RETURN
      	END FUNCTION GetArea
      	
      	FUNCTION GetX (ksi)
              REAL*8, INTENT(IN) :: ksi
      	  REAL*8 :: GetX
      	  
      	  GetX = 0.5D0 * (ksi + 1.D0) * Length
      	  RETURN
      	END FUNCTION GetX
		
        SUBROUTINE CalcStiffness()
          REAL*8 :: x, ksi, jacobian, weight
          
          jacobian = length / 2.D0
          b1       = -0.5D0
          b2       = +0.5D0
          
          ksi      = 0.D0
          weight   = 2.D0
          
          k11 = ((b1 * b1) * elMod * GetArea(ksi) / jacobian) * weight
          k12 = ((b1 * b2) * elMod * GetArea(ksi) / jacobian) * weight
          k22 = ((b2 * b2) * elMod * GetArea(ksi) / jacobian) * weight
          
          xk(1,1) =  k11
          xk(1,2) =  0.D0
          xk(1,3) =  k12
          xk(1,4) =  0.D0

          xk(2,:) =  0.D0

          xk(3,1) =  k12
          xk(3,2) =  0.D0
          xk(3,3) =  k22
          xk(3,4) =  0.D0

          xk(4,:) =  0.D0

          RETURN
        END SUBROUTINE CalcStiffness
        
        SUBROUTINE CalcInternalForce(ksi)
          REAL*8, INTENT(IN) :: ksi
          
          internalForceInc = stressInc * GetArea(ksi)
          internalForce    = stress * GetArea(ksi)
          
          IF (ncycle .GT. 0) THEN
            r(1,1) = +internalForce
            r(2,1) = 0.D0
            r(1,2) = -internalForce
            r(2,2) = 0.D0
          ELSE
            r(1,1) = +internalForceInc
            r(2,1) = 0.D0
            r(1,2) = -internalForceInc
            r(2,2) = 0.D0
          END IF
          RETURN
        END SUBROUTINE CalcInternalForce
        
        SUBROUTINE CalcStressStrain()      
          strainInc = (dispInc2 - dispInc1) / length
          strain    = (dispTot2 - dispTot1) / length
          stress    = elMod * strain
          stressInc = elMod * strainInc
          
          IF (ncycle .GT. 0) THEN
            de(1,1)    = strain
            etota(1,1) = strain
            gsigs(1,1) = stress
            sigxx(1,1) = stress
          ELSE
            de(1,1)    = strainInc
            etota(1,1) = strainInc
            gsigs(1,1) = stressInc
            sigxx(1,1) = stressInc
          END IF
          RETURN
        END SUBROUTINE CalcStressStrain
        
      END
