#INCLUDE 'MarcTools.f'

      MODULE CommonData
        IMPLICIT NONE
     
        CHARACTER*32, PARAMETER :: SETNAME = 'ElementList'
        REAL*8, PARAMETER :: yStress = 210.D0
        
        INTEGER, ALLOCATABLE :: edgeList(:,:) 
        INTEGER :: edgeCount
        
        REAL*8, ALLOCATABLE :: edgeStress(:)
        LOGICAL, ALLOCATABLE :: yMask(:)
      END MODULE CommonData

      SUBROUTINE ubginc(ubInc,ubIncsub)

        USE CommonData
        USE MarcTools   
        
        IMPLICIT NONE

!     ** Start of generated type statements **
        INTEGER ubInc, ubIncsub
!     ** End of generated type statements **

        INTEGER, PARAMETER :: MAXEDGE = 8

        INTEGER, ALLOCATABLE, DIMENSION(:)   :: elLst
        INTEGER :: elNum

        INTEGER, ALLOCATABLE, DIMENSION(:,:) :: curEdLst
        INTEGER :: curEl, curEdNum

        INTEGER, ALLOCATABLE, DIMENSION(:,:) :: orgEdLst
        INTEGER :: orgEdNum

        INTEGER, ALLOCATABLE, DIMENSION(:,:) :: refEdLst
        INTEGER :: nRefEdLst
        
        INTEGER :: i, j, k

        IF (ubInc .EQ. 0) THEN

          CALL ExtractSetItemLst (SETNAME, elLst, elNum)

          IF (elNum .GT. 0) THEN
            ALLOCATE (orgEdlst(2, elNum*MAXEDGE))
            orgEdNum = 0

            DO i = 1, elNum
              curEl = elLst(i)
              CALL ExtractElmEdgeLst (curEl, curEdLst, curEdNum)
              DO j = 1, curEdNum
                DO k = 1, 2
                  orgEdLst(k, orgEdNum + j) = curEdLst(k, j)
                END DO
              END DO
              orgEdNum = orgEdNum + curEdNum
            END DO

            CALL PutSmallFirst (orgEdLst, orgEdNum)
            CALL DelRepeated2D (orgEdlst, orgEdNum, refEdlst, nRefEdLst)

            CALL DelElmFreeEdge (refEdlst, nRefEdLst, edgeList)
            edgeCount = size(edgeList,2)

            Allocate (edgeStress(edgeCount))
            Allocate (yMask(edgeCount))
            yMask  = .FALSE.
            edgeStress = 0.D0
          ELSE
            CALL QUIT(1234)
          END IF
        END IF
        RETURN
      END
      
      SUBROUTINE usplit_mesh (icall,nodelist,nlist,iedgelist,nedgelist,
     $     ifacelist,nfacelist,inc,time,timeinc)

        USE CommonData
        IMPLICIT NONE
        
! ** Start of generated type statements **
        INTEGER nodelist,nlist,iedgelist,nedgelist,ifacelist,nfacelist
        INTEGER icall,inc
        REAL*8  time,timeinc
        DIMENSION nodelist(*),iedgelist(2,*),ifacelist(4,*)
!  ** End of generated type statements **

        INTEGER :: i, yEdNum
        INTEGER, ALLOCATABLE, DIMENSION(:) :: yIndex

        IF (icall .EQ. 3) THEN
          yMask = [(edgeStress(i) .GT. yStress,i=1,edgeCount)]
          yEdNum = COUNT(yMask)
          ALLOCATE (yIndex,SOURCE=PACK([(i,i=1,edgeCount)],yMask))
          
          nEdgeList = yEdNum
          iEdgeList(:,1:yEdNum) = edgeList(:,yIndex)
        END IF 
        RETURN
      END
      
      SUBROUTINE uedinc(inc,incsub)

        USE CommonData
        USE MarcTools, ONLY: GetElmEdgeVal
        IMPLICIT NONE
!     ** Start of generated type statements **
      INTEGER inc, incsub
!     ** End of generated type statements **

        INTEGER :: i
        
        IF (inc .GT. 0) THEN
          DO i = 1, edgeCount
            edgeStress(i) = 
     &                 GetElmEdgeVal (edgeList(1,i),edgeList(2,i),17,2)
            WRITE(6,*) edgeList(1,i),'-',edgeList(2,i)
            WRITE(6,*) 'stress ', edgeStress(i)
          END DO
        END IF
        RETURN
      END