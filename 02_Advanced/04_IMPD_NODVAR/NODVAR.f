#include 'MarcTools.f'
      SUBROUTINE UEDINC (inc, incsub)
        USE MarcTools, ONLY : MakeNodValLst, GetDistance, GetNodCoord, 
     &                        ExtractSetItemLst
        USE FileTools
        
        IMPLICIT NONE
        
        INCLUDE 'matdat'

!     ** Start of generated type statements **
        INTEGER inc, incsub
!     ** END of generated type statements **

        CHARACTER (*), PARAMETER :: FILENAME         = 'result.txt',
     &                              FORCE_NOD_SET    = 'ForceNodes',
     &                              NECK_NOD_SET     = 'NeckNodes',
     &                              DISTANCE_NOD_SET = 'DistanceNodes'
        REAL*8, PARAMETER :: PI = 4.D0 * ATAN (1.D0)

        REAL*8, ALLOCATABLE, DIMENSION(:,:) :: reactionForceLst
        REAL*8, ALLOCATABLE, DIMENSION(:)   :: reactionForceSumLst
        
        REAL*8, SAVE :: initialArea, initialRadius, initialLength,  
     &                  elasticModulus
        
        REAL*8 :: engStress, trueStress, engStrain, trueStrain, 
     &            truePlasticStrain, trueElasticStrain, 
     &            currentRadius, currentArea, currentLength

        INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) :: distanceNodLst,
     &                                              forceNodLst,
     &                                              neckNodLst

        INTEGER, SAVE :: nDistanceNodLst, nForceNodLst, nNeckNodLst,
     &                   fileUnit
        
        INTEGER :: i, nComp, nReactionForceLst

100     FORMAT (109('-'))
200     FORMAT (A3, X, 8(A14, X))
300     FORMAT (I3, X, 8(F14.4, X))      

        IF (inc .EQ. 0) THEN 
          CALL FindFreeUnit (fileUnit)
          OPEN (UNIT = fileUnit, File = FILENAME, ACCESS = 'SEQUENTIAL',
     &          STATUS = 'REPLACE', ACTION = 'WRITE')
         

          CALL ExtractSetItemLst (DISTANCE_NOD_SET, distanceNodLst,
     &                            ndistanceNodLst)

          CALL ExtractSetItemLst (FORCE_NOD_SET, forceNodLst,
     &                            nForceNodlst)
      
          CALL ExtractSetItemLst (NECK_NOD_SET, neckNodLst, nNeckNodLst)
         
          initialRadius = GetDistance (GetNodCoord (neckNodLst(1)), 
     &                                 GetNodCoord (neckNodLst(2)))
        
          initialArea   = PI * (initialRadius ** 2.D0)
         
          initialLength = GetDistance (GetNodCoord (distanceNodLst(1)),
     &                                 GetNodCoord (distanceNodLst(2)))
          elasticModulus = et(1)
          
          WRITE (fileUnit, 100)  
          WRITE (fileUnit, 200) 'Inc', 'React. Force X', 'Eng. Stress', 
     & 'True Stress', 'Eng. Strain', 'True Strain', 'True Pl.Strain', 
     & 'True El.Strain'
          WRITE (fileUnit, 100)
        ELSE
          CALL MakeNodValLst (5, reactionForceLst, nReactionForceLst
     &                             , nComp, forceNodLst, nForceNodLst)
          
          IF (.NOT. ALLOCATED (reactionForceSumLst)) 
     &               ALLOCATE (reactionForceSumLst(nComp))
          
          reactionForceSumLst = SUM (reactionForceLst, 1)
          
          currentRadius = GetDistance (GetNodCoord (neckNodLst(1),2), 
     &                                 GetNodCoord (neckNodLst(2),2))
     
          currentArea   = PI * (currentRadius ** 2.D0)

          engStress  = reactionForceSumLst(1) / initialArea
          trueStress = reactionForceSumLst(1) / currentArea
          
          currentLength= GetDistance(GetNodCoord (distanceNodLst(1), 2),
     &                               GetNodCoord (distanceNodLst(2), 2))
          
          engStrain = (currentLength - initialLength) / initialLength

          trueStrain = Log (currentLength / initialLength)
          trueElasticStrain = trueStress / elasticModulus
          truePlasticStrain = trueStrain - trueElasticStrain
          
          WRITE (fileUnit, 300) inc, reactionForceSumLst(1), 
     & engStress, trueStress, engStrain, trueStrain,
     & truePlasticStrain, trueElasticStrain
        END IF
        
        IF (inc .EQ. 10) THEN
          WRITE (fileUnit, 100)
          CLOSE (fileUnit)
        END IF
        
        RETURN
      END SUBROUTINE UEDINC