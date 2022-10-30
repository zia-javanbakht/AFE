#include 'MiscTools.f'
#include 'FileTools.f'
!***************************************************************************************************
! 
! v17.001 29/01/16
! - Module name changed to MarcTools
!
! v 20.002 07/03/16
! - An edge list is changed to edgeList(2,*) array
! - DelRepeated subroutine added
! - PutSmallFirst subroutin added
! - ExtractElmEdgeLst updated
! - GetIndex funcion added
!
! v 21.001 10/03/16
! - DelRepeated subroutine added
!
! v 23.001 12/03/16
! - CalcExStressNode & SingleNodeExStress subroutines are added
! - Constant parameter MAXNPERE added
! - More descriptive approach is acquired regarding adding comments
!   It is also advised to acquire a consistent approach in naming the arguments
! of the subprogram in the following versions.
!
! v 23.002 14/03/16
! - some comments added 
!
! v 25.001 15/3/2016
! - PrintSetLst   subroutine added
! - PrintSetItemLstID   subroutine added
! - PrintSetItemLstName subroutine added
! v 25.003 17/3/2016
! - various stuff
! v 25.010 21/3/2016
! - final version of GetIPCoord
! v 26.001 22/3/2016
! - GetElmAveVal function added
! v 27.001 23/3/2016
! - GetNodCoord SUBROUTINE is rewritten
! v 29.001 24/3/2016
! - PrintNodCoordLst & PrintIPCoordLst added
! v 31.002 27/3/2016
! - GetElmIPCount FUNCTION
! - GetIPVal FUNCTION
! - GetNodExtraVal is updated
! v 30.01 
!***************************************************************************************************
      MODULE MarcTools
        USE MiscTools

        IMPLICIT NONE
        
        INCLUDE 'spaceivec'
        INCLUDE 'spaceset'
        INCLUDE 'array2'
        INCLUDE 'cdominfo'
        INCLUDE 'dimen'
        INCLUDE 'elemdata'
        INCLUDE 'elmcom'
        INCLUDE 'heat'
        INCLUDE 'space'
        INCLUDE 'blnk'
        INCLUDE 'prepro'
        INCLUDE 'nzro1'
        INCLUDE 'iautcr' 
        INCLUDE 'creeps' 
        INCLUDE 'concom' 
        
!***************************************************************************************************
!
!    Some constant parameters are considered in this module which
! are used to ALLOCATE some arrays with unknown number of elements.
! Alternative to this direct approach, it is possible to extract 
! the relating information from the common-blocks of MARC. To do 
! this a constructor subroutine will be required to initialize
! such values. This subroutine must be executed prior to calling 
! any other subroutines.
!
!    Note that the subroutines in which the Elmvar utility-subroutine
! is used must be used only in element loops. These subroutines are
! marked with an asterisk.
!    In contrast, the subroutines with the Nodvar utility-subroutine can
! be used everywhere. However, the user must understand that the nodal 
! values may not be the final values of that increment. This depends
! in which stage of the analysis the Nodvar utility is executed. These
! subroutines are marked with two asterisks.
!
! List of subroutines:
!
! 1**   CalcNodVal                
! 2     DelElmFreeEdge            
! 3     ExtractElmEdgeLst         
! 13    ExtractElmNodLst              
! 14    ExtractElmNodLst2             
! 4**   ExtractNodCloseIPLst      
! 5     ExtractSetItemLst         
! 6**   GetElmArea                
! 7*    GetElmAveVal              
! 8**   GetElmCenCoord            
! 9     GetElmEdgeVal             
! 10    GetElmExtID               
! 11    GetElmIPCount             
! 12    GetElmIntID               
! 15    GetIPCoord                
! 16*   GetIPVal                  
! 17**  GetNodCoord               
! 18    GetNodExtID               
! 19    GetNodExtraVal            
! 20    GetNodIPVal               
! 21    GetNodIntID               
! 22    IsElmIDValid              
! 23    IsItemInSet               
! 24    IsNodIDValid              
! 25    MakeElmIDLst              
! 26    MakeIPCoordLst            
! 27    MakeIPValLst              
! 28    MakeNodCoordLst           
! 29    MakeNodIDLst              
! 30    MakeNodValIPLst           
! 31**  MakeNodValLst             
! 32    PrintElmIDGroupedLst      
! 33    PrintElmIDLst             
! 34    PrintIPCoordLst           
! 35    PrintIPValLst             
! 36**  PrintNodCoordLst          
! 37    PrintNodIDLst             
! 38    PrintNodValIPLst          
! 39**  PrintNodValLst            
! 40    PrintSetItemLstID         
! 41    PrintSetItemLstName       
! 42    PrintSetLst               

! Rules of naming the subroutines:
! 
!  1. If the return value of the subroutine is a 1D array, the lst suffix is used.
!  2. No plural names is used.
!  3. All start with a verb.
!  4. `Get' is used for functions since they return something.
!  5. `Is' is used for functions returning a logical value since they ask a question.
!  6. `Calc' is used for subroutines since they calculate something more complex than functions.
!  7. `Print' is obviously used for subroutines since they do not return a value and just print.
!  8. `make' is used for subroutines which collect the data for a range of nodes/items i.e. result is a 2D array
!  9. Checking the validity of the nodID or elmId is done in the Get Functions which are responsible 
!     of the single node/ element handling
! 10. Three levels of subprogram:
!           Get     works on a single node or element
!           Make    uses Get one to create a list
!           Print   uses Make ones to get a list
! 
! List of abbreviations used in subroutines:
!     
!      nod         : node/nodal
!      elm         : element/elemental
!      IP          : integration point
!      lst         : list (1D array)
!      ave         : average/averaged
!      cen         : center/central
!      val         : value
!      coord       : coordinates
!      extra       : extrapolated
!      num         : number
!      rand        : random
!      rep         : repetition
!  
!   For SUBROUTINES:
!      Del         : delete/remove
!      Calc        : calculate
!      Print       : print
!      Put         : put/move
!      Extract     : extract
!      make        : make/collect/gather
!
!   For FUNCTIONS:
!      Get
!      Is
!
! Using Consistent Variable Names:
!
!     Some abbreviations are used to avoid long variable names. Usually if a name has four letters
!   or less, all the letters would be kept. But for names more than four letters, an abbreviation
!   will be used which usually consists of 3 - 4 letters.
!   The following names are consistently used as the name of the local variables or parameters:
!
!     nodID       : user ID of the node
!     nodIDLst    : a list of node IDs
!     intNodID    : internal ID of the node
!     elmID       : user ID of the element
!     elmIDLst    : a list of the element IDs
!     intElmID    : internal ID of the element
!     nodLst      : array of nodes
!     elmLst      : array of elements
!     nElm        : number of elements
!     nNod        : number of nodes
!     IP          : IP number
!     nIP         : number of IPs
!     IPLst       : array of IPs
!     setName     : set name
!     itemID      : ID of an item in a set
!     lst         : a 1D array (items are general)
!     nlst        : number of elements in the list
!     itemLst     : list of items (items are of type integer)
!     nItemLst    : number of items in the list
!     tItemLst    : temp list of items
!     tCoordLst   : temporary list of coordinates
!     curElm      : current element
!     curNod      : current node
!     curIP       : curret integration point
!     distLst     : list of distances
!     curElmVal   : current elemental value
!     curNodVal   : current nodal value
!     curIPVal    : current integration point value
!     totIPVal    : total integration point value
!     curVal      : current value
!     totVal      : total value
!     meanVal     : mean value
!     IPCoord     : coordinates of the integraion point
!     curIPCoord  : current coordinates of the integration point
!     totIPCoord  : total coordinates of the integration point
!     nodCoord    : coordinates of the node
!     transVal    : translated value
!     extraVal    : extrapolated value
!     averaVal    : average value
!
!
!  Data Dictionary Template:
!
!***************************************************************************************************
! FUNCTION 
!
! Objective:
!   Returns the undeformed/deformed coordinates of a node ID.
!   Returns three zero coordinates in the case of an error.
!
! Input(s):
!   nodID        INTEGER             external node ID
!   nodState     INTEGER(OPTIONAL)   the state of the node
!                                    1: undeformed state
!                                    2: deformed state
! Output(s):
!             REAL*8(3)          coordinates of the nodID
!
! Auxiliary variable(s):
!   nComp        INTEGER             number of returned components
!   dataType     INTEGER             the returned data type
!   nodCoord     REAL*8(3)           coordinates of the nodID
!   nodDisp      REAL*8(3)           displacement of the nodID
!   i            INTEGER             loop counter
!   coordState   INTEGER             holds the same state as nodState 
!
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   none
!
! Restrictions:
!   none
!
! Future Updates:
!   none
!
! Last update: 1/4/2016
!***************************************************************************************************
! Variables/macros used for debugging:
!   
!   _DEBUG_MODE_ON : if defined turns the debug mode on. Descriptions will be printed. 
!   _W             : WRITE (-FU,*)  
!   _FU             : file unit (default is 6)
!***************************************************************************************************
#define _FU 20
#define _DEBUG_MODE_ON
#define _W WRITE(_FU,*)
!***************************************************************************************************
! PARAMETER Description:
!   MAXNPERE = Maximum number of Nodes per Element
!              This is not required because there is a similar parameter available in
!              the common-block lass: nnode (number of nodes in element). Therefore,
!              currently the following line will not be used:
!    
!     MAXN       INTEGER         maximum number of element neighbors
!     MAX_ITEM   INTEGER         maximum number of items in a set
!
!***************************************************************************************************
!        INTEGER, PARAMETER :: MAXN = 27
        INTEGER, PARAMETER :: MAX_SET_ITEM = 1000
       CONTAINS
!***************************************************************************************************
! SUBROUTIN
!
! Objective:
!   This subroutine extracts the nodal values for the specified nodal post code.
!
! Input(s):
!   nodID          INTEGER       node user ID
!   nodCode        INTEGER       nodal post code
!                                Example node post codes:
!                                   0 Coordinates
!                                   1 Displacement
!                                   2 Rotation
!                                   3 External Force
!                                   4 External Moment
!                                   5 Reaction Force
!                                   6 Reaction Moment
!                                  79 Total displacement
!
! Output(s):
!   vallst         REAL*8(:)     list of values
!   nValLst        INTEGER       number of values in the list
!
! Auxiliary variable(s):
!   nComp          INTEGER       number of components
!   tValLst        REAL*8(10)    temporary holder of the values
!
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   none
!
! Required module variable(s):
!   none
!
! Last update: 29/08/2016
!*************************************************************************************************** 
      SUBROUTINE CalcNodVal (nodID, nodCode, valLst, nValLst)
        INTEGER, INTENT(IN) :: nodId, nodCode
        REAL*8, ALLOCATABLE, INTENT(OUT) :: valLst(:)
        INTEGER, INTENT(OUT) :: nValLst
        
        INTEGER :: dataType
        REAL*8, DIMENSION(10) :: tValLst
        
        CALL NodVar (nodCode, nodID, tValLst, nValLst, dataType)
        
        IF (nValLst .NE. 0) THEN
          ALLOCATE(valLst, SOURCE = tValLst(1:nValLst))
        END IF 
        
      END SUBROUTINE CalcNodVal
!***************************************************************************************************
! SUBROUTINE MakeNodValLst 
!
! Objective:
!   This subroutine makes a list of values for the listed nodes. If no nodes are specified, this is
!   done for all of the nodes in the model.
!   The dimensions of the valLst are nValLst and nComp.
!
! Input(s):
!   nodCode      INTEGER             nodal post code
!                                    Example node post codes:
!                                       0 Coordinates
!                                       1 Displacement
!                                       2 Rotation
!                                       3 External Force
!                                       4 External Moment
!                                       5 Reaction Force
!                                       6 Reaction Moment
!                                      79 Total displacement
!   nodLst       INTEGER(*)          a list of node IDs (optional)
!   nNodLst      INTEGER             number of nodes in the nodLst (optional)
!
! Output(s):
!   valLst       REAL*8(nValLst,nComp)          coordinates of the nodID
!   nValLst      INTEGER                        number of nodes returned (=nNodLst or nnumnp)
!   nComp        INTEGER                        number of returned components per node
!
! Auxiliary variable(s):
!   ou           INTEGER             used output for printing
!   nodID        INTEGER             node user ID
!   intNodID     INTEGER             internal element ID
!   numnp        INTEGER             number of total nodes in the model (dimen.cmn)
!   tempVallst   REAL*8(:)           temporary value list for each node
!   nTempValLst  INTEGER             number of values in the temp list
!   i            INTEGER             loop counter


!   nComp        INTEGER             number of returned components
!   dataType     INTEGER             the returned data type
!   nodCoord     REAL*8(3)           coordinates of the nodID
!   nodDisp      REAL*8(3)           displacement of the nodID
!   i            INTEGER             loop counter
!   coordState   INTEGER             holds the same state as nodState 
!   inc          INTEGER             increment number (concom.cmn)
!
! Required common-blocks:
!   dimen.cmn
!
! Internal subprogram(s):
!   PrintTableHeader   
!   PrintFormattedValueList
!   PrintLine ()
!
! Required subprogram(s):
!   none
!
! Restrictions:
!   none
!
! Future Updates:
!   - check if node exist in the model.
!   - Algorithm for the shortest path to use for a node-path instead of a range of nodes e.g. in CollectNodValLst
!
! Last update: 2/4/2016
!***************************************************************************************************      
      SUBROUTINE MakeNodValLst (nodCode, valLst, nValLst, nComp, 
     &                           nodLst, nNodLst)
     
        INTEGER, INTENT(IN) :: nodCode
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: nodLst
        INTEGER, INTENT(IN), OPTIONAL :: nNodLst
        REAL*8, ALLOCATABLE, DIMENSION(:,:), INTENT(OUT) :: valLst
        INTEGER, INTENT(OUT) :: nValLst, nComp
        
        INTEGER ::  nodID, intNodID, nTempValLst, i
        REAL*8, ALLOCATABLE, DIMENSION(:) :: tempValLst
        
        IF (Present(nodLst) .AND. Present(nNodLst)) THEN
          DO i = 1, nNodLst
            nodID = nodLst(i)
            CALL CalcNodVal (nodID, nodCode, tempValLst, nTempValLst)
            
            IF (nTempValLst .EQ. 0) THEN
              CALL QUIT(1234)
            ELSE
              IF (Allocated(valLst) .EQV. .FALSE.) 
     &            ALLOCATE (valLst(nNodLst, nTempValLst))
              valLst(i,:) = tempValLst(:)
            END IF
          END DO
          nComp   = nTempValLst
          nValLst = nNodLst
        ELSE
          DO intNodID = 1, numnp
            nodID = GetNodExtID(intNodID)
            
            CALL CalcNodVal (nodID, nodCode, tempValLst, nTempValLst)

            IF (nTempValLst .EQ. 0) THEN
              CALL QUIT(1234)
            ELSE
              IF (Allocated(valLst) .EQV. .FALSE.) 
     &            ALLOCATE(valLst(numnp, nTempValLst))
              valLst(intNodID,:) = tempValLst(:)
            END IF
          END DO
          nComp   = nTempValLst
          nValLst = numnp
        END IF
        RETURN
        
      END SUBROUTINE MakeNodValLst
!***************************************************************************************************
! SUBROUTINE DelElmFreeEdge
!
! Objective:
!   Finds the free edges of an element (exterior edges) in a list and deletes them.
!
! Input(s):
!   edgeLst      INTEGER(2,*)        list of edges
!   nEdge        INTEGER             number of edges in edgeLst
!
! Output(s):
!                INTEGER             the list of edges with the exterior ones removed
!
! Auxiliary variable(s):
!   i            INTEGER             loop counter
!   edgeNod1     INTEGER             first node of the current edge
!   secondNod    INTEGER             second node of the current edge

!   nodCoord     REAL*8(3)           coordinates of the nodID
!   nodDisp      REAL*8(3)           displacement of the nodID

!   coordState   INTEGER             holds the same state as nodState 
!                                    if the latter is present
!
!   maxnp        INTEGER             max. no. of connections to a node
!
! Required common-blocks:
!   dimen.cmn
!
! Required subprogram(s):
!   none
!
! Restrictions:
!   none
!
! Future Updates:
!   Make the nodState an optional argument.
!
! Last update: 31/3/2016
!*************************************************************************************************** 
      SUBROUTINE DelElmFreeEdge (edgeLst, nEdge, refinedEdgeLst)
        INTEGER, DIMENSION(2,*), INTENT(INOUT) :: edgeLst
        INTEGER, INTENT(IN) :: nEdge
        INTEGER, ALLOCATABLE, INTENT(OUT) :: refinedEdgeLst(:,:)

        INTEGER :: edgeNod1, nElmLstNod1, edgeNod2, nElmLstNod2
        INTEGER :: i, nInterLst

        LOGICAL, ALLOCATABLE, DIMENSION(:) :: aMask
        INTEGER, ALLOCATABLE, DIMENSION(:) :: anIndex, interLst,
     &                                        elmLstNod1, elmLstNod2

        ALLOCATE (aMask(nEdge))
        ALLOCATE (elmLstNod1(maxnp))
        ALLOCATE (elmLstNod2(maxnp))

        elmLstNod1  = 0
        elmLstNod2  = 0
        nElmLstNod1 = 0
        nElmLstNod2 = 0
          
        DO i = 1, nEdge
          edgeNod1 = edgeLst(1, i)
          edgeNod2 = edgeLst(2, i)

          CALL UT_ELEMENTS_AT_NODE (edgeNod1, elmLstNod1, nElmLstNod1)
          CALL UT_ELEMENTS_AT_NODE (edgeNod2, elmLstNod2, nElmLstNod2)
         
          CALL ExtractIntersectLst (elmLstNod1, nElmLstNod1,
     &         elmLstNod2, nElmLstNod2, interLst, nInterLst )
     
          aMask(i) = nInterLst .GT. 1
        END DO

        ALLOCATE(anIndex, source = PACK([(i, i = 1, nEdge)], aMask))
        ALLOCATE(refinedEdgeLst, source = edgeLst(:, anIndex))
        RETURN
      END SUBROUTINE DelElmFreeEdge
!***************************************************************************************************
! SUBROUTINE ExtractElmEdgeLst
!
! Objective:
!   Extracts the list of edges for an element.
!
! Input(s):
!   elmID         INTEGER           user element ID
!
! Output(s):
!   edgeLst       INTEGER(:,:)      a list of node pairs i.e. edges
!   nEdgeLst      INTEGER           number of edges in edgeLst
!
! Auxiliary variable(s):
!   nodLst        INTEGER(:)        external node ID list
!   nNodLst       INTEGER           number of nodes in nodLst
!   i             INTEGER           loop counter
!
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   none
!
! Restrictions:
!   none
!
! Future Updates:
!   none
!
! Last update: 1/4/2016
!***************************************************************************************************
      SUBROUTINE ExtractElmEdgeLst (elmID, edgeLst, nEdgeLst)
        INTEGER, INTENT(IN) :: elmID
        INTEGER, ALLOCATABLE, DIMENSION(:,:), INTENT(OUT) :: edgeLst
        INTEGER, INTENT(OUT):: nEdgeLst
        
        INTEGER, ALLOCATABLE, DIMENSION(:) :: nodLst
        INTEGER :: nNodLst, i 
        
        CALL ExtractElmNodLst (elmID, nodLst, nNodLst)
        nEdgeLst = nNodLst
        ALLOCATE (edgeLst(2, nNodLst))
        DO i = 1, (nNodLst - 1)
          edgeLst(1, i) = nodLst(i)
          edgeLst(2, i) = nodLst(i+1)
        END DO
          edgeLst(1, nNodLst) = nodLst(nNodLst)
          edgeLst(2, nNodLst) = nodLst(1)
        RETURN
      END SUBROUTINE ExtractElmEdgeLst
!***************************************************************************************************
! SUBROUTINE ExtractNodCloseIPLst
!
! Objective:
!   This subroutine extracts the closest integration points of the adjacent element with respect to
! a node
!
! Input(s):
!   nodID          INTEGER       nodeNumber
!
! Output(s):
!   nLst           INTEGER       number of items in each of the following lists (0 for error)
!   elmLst         INTEGER(:)    elementList
!   IPLst          INTEGER(:)    GaussList
!
! Auxiliary variable(s):
!   tElmLst        INTEGER(:)   temporary list of elements
!   nTElmLst       INTEGER      number of items in the temporary Lists of elements
!   i              INTEGER      loop counter
!   curElm         INTEGER      current element
!   curIP          INTEGER      curret integration point
!   nodCoord       INTEGER(3)   coordinates of the nodID
!   IPCoord        INTEGER(3)   coordinates of the IP
!   maxNP          INTEGER      maximum number of connection to a node (dimen)
!                               which is used to allocate the tElmLst
!   nIntBmx        INTEGER      max. no. of IPs (dimen)
!
! Required common-blocks:
!   dimen.cmn
!
! Required subprogram(s):
!   none
!
! Required module variable(s):
!   none
!
! Last update: 21/3/2016
!*************************************************************************************************** 
      SUBROUTINE ExtractNodCloseIPLst (nodID, nItemLst, elmLst, IPLst)
        INTEGER, INTENT(IN) :: nodID
        INTEGER, INTENT(OUT):: nItemLst
        INTEGER, ALLOCATABLE, INTENT(OUT):: elmLst(:), IPLst(:)

        INTEGER, ALLOCATABLE :: tElmLst(:)
        INTEGER :: nTElmLst, i, curIP, curElm
        
        REAL*8, DIMENSION(3) :: nodCoord, IPCoord
        REAL*8, ALLOCATABLE :: distanceLst(:)
      
        ALLOCATE (tElmLst(maxNP))
        CALL ut_elements_at_node (nodID, tElmLst ,nTElmLst)
        IF (nTElmLst .EQ. 0) THEN
          nItemLst = 0
          CALL QUIT(1234)
          RETURN
        ELSE 
          nItemLst = nTElmLst
          ALLOCATE (elmLst(nItemLst))
          ALLOCATE (IPLst(nItemLst))
          
          nodCoord = GetNodCoord (nodID, 1)
          
          ALLOCATE (distanceLst(nintbmx))
          
          DO i = 1, nItemLst
            curElm = tElmLst(i)
            elmLst(i) = curElm
              DO curIP = 1, nintbmx
                IPCoord = GetIPCoord(curElm, curIP)
                distanceLst(curIP) = GetDistance(nodCoord, IPCoord)
              END DO
              IPLst(i) = Minloc(distanceLst, 1)
          END DO
        END IF
      END SUBROUTINE ExtractNodCloseIPLst
!***************************************************************************************************
! SUBROUTINE ExtractSetItemLst
!
! Objective:
!     This subroutine extarcts the items in the set named (setName).                               
!
! Input(s):
!   setName   CHARACTER(*)               the name of the set
!
! Output(s):
!   itemLst   INTEGER(:)                 external node ID list
!   nItemLst  INTEGER                    number of items in the itemLst
!
! Auxiliary variable(s):
!   i            INTEGER                 loop counter
!   tItemList    INTEGER(MAX_SET_ITEM)   temporary list of items of a set
!   isFound      INTEGER                 flag indicating if the set exists
!   nItem        INTEGER                 number of items in the list
!   setType      INTEGER                 indicates the set type

!
! Required common-blocks:
!   elemdata.cmn 
!   elmcom.cmn
!
! Required subprogram(s):
!   GetElmIntID
!
! Required module variable(s):
!   MAX_ITEM
!
! Last update: 14/3/2016
!***************************************************************************************************
        SUBROUTINE ExtractSetItemLst (setName, itemLst, nItemLst)
           
          CHARACTER(LEN=*), INTENT(IN)                    :: setName
          INTEGER, INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: itemLst
          INTEGER, INTENT(OUT)                            :: nItemLst
           
          INTEGER :: tItemLst(MAX_SET_ITEM), isFound, setType, i
         
          CALL Marc_SetInf(setName, isFound, tItemLst, setType, nItemLst)
          
          IF (isFound .EQ. 1) THEN
            ALLOCATE (itemLst(nItemLst))
            itemLst = [(tItemLst(i), i = 1, nItemLst)]
          ELSE 
            nItemLst = 0
          END IF
          
          RETURN
        END SUBROUTINE ExtractSetItemLst       
        

        
        
        
!***************************************************************************************************
! FUNCTION GetElmArea
!
! Objective:
!   This function returns the surface area of a 4-node quadrilateral element based on its original shape.
!
! Input(s):
!   elmID          INTEGER       element user ID
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   nodLst      INTEGER(:)      external node ID list
!   nNodLst     INTEGER         number of nodes in nodLst
!   nodCoord    REAL*8(:)       coordinates of the nodes
!   iNod        INTEGER         loop counter for nodLst
!   elmArea     REAL*8          element area
!   x1,y1       REAL*8          coordinates of the first point in the array
!   x2,y2       REAL*8          coordinates of the second point in the array
!   
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   ExtractElmNodLst SUBROUTINE
!   GetNodCoord FUNCTION
!
! Required module variable(s):
!   none
!
! Last update: 26/3/2016
!***************************************************************************************************      
      FUNCTION GetElmArea (elmID)
        INTEGER, INTENT(IN) :: elmID
        REAL*8 :: GetElmArea
        
        INTEGER, ALLOCATABLE :: nodLst(:)
        REAL*8, ALLOCATABLE :: nodCoord(:,:)
        REAL*8 :: elmArea, x1, x2, y1, y2
        INTEGER :: nNodLst, iNod
        
        CALL ExtractElmNodLst (elmID, nodLst, nNodLst)
        
        IF (nNodLst .EQ. 0) THEN
          CALL QUIT(1234)
        ELSE
          ALLOCATE (nodCoord(nNodLst,3))
          DO iNod = 1, nNodLst
            nodCoord(iNod,:) = GetNodCoord (nodLst(iNod), 1)
          END DO
          elmArea = 0.D0
          DO iNod = 1, nNodLst
            x1 = nodCoord(iNod,1)
            y1 = nodCoord(iNod,2)
            IF (iNod .EQ. nNodLst) THEN
              x2 = nodCoord(1,1)
              y2 = nodCoord(1,2)
            ELSE
              x2 = nodCoord(iNod+1,1)
              y2 = nodCoord(iNod+1,2)
            END IF 
            elmArea = (x1*y2) - (y1*x2) + elmArea
          END DO          
          GetElmArea = elmArea * 0.5D0
        END IF
      END FUNCTION GetElmArea
!***************************************************************************************************
! FUNCTION GetElmCenCoord
!
! Objective:
!   This function returns the coordinates of the element center which is calculated as the average
! of the integration point coordinates.
!
! Input(s):
!   elmID          INTEGER       element user ID
!   calcMeth       INTEGER       calculation of the center based on
!                                1: average of the original nodal points coordinates
!                                2: average of the deformed nodal points coordinates
!                                3: average of the IP points coordinates
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   IP             INTEGER      integration point
!   nIP            INTEGER      number of integeration points
!   intElmID       INTEGER      internal element ID
!   curCoord       REAL*8(3)    current coordinates 
!   totCoord       REAL*8(3)    total coordinates
!
! Required common-blocks:
!   elmcom.cmn
!
! Required subprogram(s):
!   GetElmIPCount
!
! Required module variable(s):
!   none
!
! Last update: 22/3/2016
!***************************************************************************************************
      FUNCTION GetElmCenCoord (elmID, calcMeth)
        INTEGER, INTENT(IN) :: elmID, calcMeth
        REAL*8, DIMENSION(3) :: GetElmCenCoord
        
        INTEGER :: nIP, IP, IntElmID, nNod, iNod
        REAL*8, DIMENSION(3) :: curCoord, totCoord
        INTEGER, ALLOCATABLE :: nodLst(:)
        
        curCoord = 0.D0
        totCoord = 0.D0
        
        intElmID = GetElmIntID (elmID)
        
        IF (intElmID .EQ. 0) THEN
          CALL QUIT(1234)
        ELSE
          SELECT CASE (calcMeth)
            CASE (1:2)
              CALL ExtractElmNodLst (elmID, nodLst, nNod)
              DO iNod = 1, nNod
                IF (calcMeth .EQ. 1) THEN 
                  curCoord = GetNodCoord(nodLst(iNod), 1)
                ELSE 
                  curCoord = GetNodCoord(nodLst(iNod), 2)
                END IF 
                totCoord = totCoord + curCoord
              END DO
              GetElmCenCoord = totCoord / nNod
          CASE(3)
            nIP = GetElmIPCount(elmID)
            
            DO IP = 1, nIP
              curCoord = GetIPCoord (elmID, IP)
              totCoord = totCoord + curCoord
            END DO
            GetElmCenCoord = totCoord / nIP
          END SELECT 
        END IF
        RETURN
      END FUNCTION GetElmCenCoord
!***************************************************************************************************
! FUNCTION GetElmEdgeVal
!
! Objective:
!   This function returns the calculated edge value for an elemental quantity e.g. stress.
!
! Input(s):
!   nodID1         INTEGER       first use node ID of the edge
!   nodID2         INTEGER       second user node ID of the edge
!   elmCode        INTEGER       element post code
!                                Example codes:
!                                  1-6     componenets of strain
!                                  11-16   componenets of stress
!                                  17      von Mises Stress
!                                  18      mean normal Stress
!   calcMeth       INTEGER      specifies the calculation method
!                                 1: Translation    (unweighted averaging among elements)
!                                 2: Extrapolation  (unweighted averaging among elements)
!                                 3: Average        (unweighted averaging among elements)
!                                 5: Translation    (weighted averaging with the area of each element as weight)
!                                 6: Extrapolation  (weighted averaging with the area of each element as weight)
!                                 7: Average        (weighted averaging with the area of each element as weight)
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   ExtractElmNodLst SUBROUTINE
!   
!
! Required module variable(s):
!   none
!
! Restrictions:
!   Only for quadrilateral elements with 4 nodes
!
! Future Updates:
!   none
!
! Last update: 1/4/2016
!*************************************************************************************************** 
      FUNCTION GetElmEdgeVal (nodID1, nodID2, elmCode, calcMeth)
        INTEGER, INTENT(IN) :: nodID1, nodID2, elmCode, calcMeth
        REAL*8 :: GetElmEdgeVal
   
        GetElmEdgeVal = 
     &            (GetNodIPVal(nodID1, elmCode, calcMeth) +
     &             GetNodIPVal(nodID2, elmCode, calcMeth)) / 2.D0

       END FUNCTION GetElmEdgeVal
!***************************************************************************************************
! FUNCTION GetElmExtID
!
! Objective:
!   Converts the internal ID of an element to its external/user ID.
!   Returns zero in the case of an error.
!
! Input(s):
!   intElmID  INTEGER        internal element ID
!
! Output(s):
!             INTEGER        external element ID
!
! Auxiliary variable(s):
!   nElIDs    INTEGER        flag if non-zero indicates non-consecutive node numbering (prepro.cmn)
!   iElIDs_d  INTEGER(*)     contains the external ID of the nodes if nElIDs is flagged (prepro.cmn)
!   numEL     INTEGER        total number of elements in the mesh (dimen.cmn)
!
! Required common-blocks:
!   prepro.cmn
!   dimen.cmn
!
! Required subprogram(s):
!   none
!
! Old listing:
!
!        INTEGER FUNCTION GetElmExtID (intElmID)
!          INTEGER, INTENT (IN) :: intElmID
!
!          GetElmExtID = 0
!          IF (nelids .EQ. 0) THEN
!            IF (intElmID .LE. numEl) GetElmExtID = intElmID
!          ELSE
!            GetElmExtID = iElIDs_d(intElmID)
!          END IF
!
!        END FUNCTION GetElmExtID
!
! Last update: 29/08/2016
!***************************************************************************************************
        INTEGER FUNCTION GetElmExtID (intElmID)
          INTEGER, INTENT (IN) :: intElmID

          INTEGER :: ielext, tIntElmID
          
          tIntElmID = ielext (intElmID)
          
          IF (tIntElmID .LE. 0) THEN
            CALL QUIT(1234)
          ELSE
            GetElmExtID = tIntElmID
          END IF

        END FUNCTION GetElmExtID
!***************************************************************************************************
! FUNCTION GetElmIPCount
!
! Objective:
!   This function returns the number of element integration points. Default return value is zero.
!
! Input(s):
!   elmID          INTEGER      element user ID
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   intElmID       INTEGER      internal element ID
!   jintel         INTEGER      number of integration points for the current element (elmcom.cmn))
!   
! Required common-blocks:
!   elmcom.cmn
!
! Required subprogram(s):
!   GetElmExtID
!
! Required module variable(s):
!   none
!
! Last update: 27/3/2016
!*************************************************************************************************** 
      FUNCTION GetElmIPCount (elmID)
        INTEGER, INTENT(IN) :: elmID
        INTEGER :: GetElmIPCount
        
        INTEGER :: intElmID
        
        GetElmIPCount = 0
        
        IF (IsElmIDValid(elmID) .EQV. .TRUE.) THEN
          intElmID = GetElmIntID(ElmID)
          IF (intElmID .EQ. 0) THEN
            CALL QUIT(1234)
          ELSE
            CALL SetEl (IntElmID)
            GetElmIPCount = jintel
          END IF
        ELSE
          CALL QUIT(1234)
        END IF
        RETURN
      END FUNCTION GetElmIPCount
!***************************************************************************************************
! FUNCTION GetElmIntID
!
! Objective:
!   Converts the external ID of an element to its internal ID.
!   Returns zero in the case of error.
!
! Input(s):
!   elmID     INTEGER       external element ID
!
! Output(s):
!             INTEGER       internal element ID
!
! Auxiliary variable(s):
!   nElIDs    INTEGER       flag if non-zero indicates non-consecutive element numbering (prepro.cmn)
!   iElIDs_d  INTEGER(*)    contains the external ID of the nodes if nElIDs is flagged (prepro.cmn)
!   i         INTEGER       loop counter
!   numEL     INTEGER        total number of elements in the mesh (dimen.cmn)
!
! Required common-block(s):
!   prepro.cmn
!   dimen.cmn
!
! Required subprogram(s):
!   GetElmExtID
!
! Old listing:
!
!        INTEGER FUNCTION GetElmIntID (elmID)
!          INTEGER, INTENT (IN) :: elmID
!
!          IF (nElIDs .EQ. 0) THEN
!            GetElmIntID = elmID
!          ELSE
!            i = 1 
!            found = .FALSE.
!            DO WHILE ((i .LE. NumEL) .AND. (.NOT. found))
!              IF (elmID .EQ. GetElmExtID(i)) THEN
!                found = .TRUE.
!              ELSE
!                i = i + 1
!              END IF
!            END DO
!            
!            IF (found .EQV. .TRUE.) THEN
!              GetElmIntID = i
!            ELSE
!              GetElmIntID = 0
!            END IF
!
!          END IF 
!        END FUNCTION GetElmIntID
!
! Last update: 29/8/2016
!***************************************************************************************************
        INTEGER FUNCTION GetElmIntID (elmID)
          INTEGER, INTENT (IN) :: elmID

          INTEGER :: ielint, tElmIntID
          
          tElmIntID = ielint (elmID)

          IF (tElmIntID .LE. 0) THEN
            CALL QUIT (1234)
          ELSE
            GetElmIntID = tElmIntID
          END IF
         
        END FUNCTION GetElmIntID
!***************************************************************************************************
! SUBROUTINE ExtractElmNodLst
! 
! Objective:
!   This subroutine extarcts the attached nodes to an element using the ELNODES utility subroutine.
!   Returns zero as the number of nodes (nNod).
!
! Input(s):
!   elmID     INTEGER         external element ID
!
! Output(s):
!   nodLst    INTEGER(:)      external node ID list
!   nNod      INTEGER         number of nodes in nodLst
!
! Auxiliary variable(s):
!   i         INTEGER         loop counter
!   intElID   INTEGER         internal element number
!   tNodLst   INTEGER(:)      temporary list of nodes
!   nNodmx    INTEGER         maximum number of nodes per element in the model (dimen.cmn) 
!
! Required common-blocks:
!   dimen.cmn
!
! Required subprogram(s):
!   GetElmExtID 
!   Elnodes
!
! Required module variable(s):
!   none
!
! Last update: 17/3/2016
!***************************************************************************************************  
        SUBROUTINE ExtractElmNodLst (elmID, nodLst, nNod)
          INTEGER, INTENT(IN) :: elmID
          INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: nodLst
          INTEGER, INTENT(OUT) :: nNod
          
          INTEGER :: i, intElID
          INTEGER, ALLOCATABLE ::tNodLst(:)
          
          ALLOCATE(tNodLst(nnodmx))

          intElID = GetElmIntID (elmID)
          IF (intElID .NE. 0) THEN
            CALL ElNodes (intElID, nNod, tNodLst)
            ALLOCATE (nodLst(nNod))
            DO i = 1, nNod
              nodLst(i) = GetNodExtID(tNodLst(i))
            END DO
          ELSE
            nNod = 0
          END IF
        END SUBROUTINE ExtractElmNodLst
!***************************************************************************************************
!***************************************************************************************************
! SUBROUTINE ExtractElmNodLst2
!
! Objective:
!     This subroutine extarcts the attached nodes to an element using the IELCON array provided in 
!   the ELCOM common-block. This subroutine can be updated for 
!
! Input(s):
!   elmID     INTEGER         external element ID
!
! Output(s):
!   nodLst    INTEGER(:)      external node ID list
!   nNod      INTEGER         number of nodes in nodLst
!
! Auxiliary variable(s):
!   i            INTEGER         loop counter
!   intElNum     INTEGER         equivalent internal element ID of elmID
!   iElCon       INTEGER(*)      element connection array (elmdata.cmn)
!   nNode        INTEGER         number of nodes in an element (elmcom.cmn)
!   dataIndex    INTEGER         pointer to the location of element connection
!   nElTyp       INTEGER         number element types in the model
!
! Required common-blocks:
!   elemdata.cmn 
!   elmcom.cmn
!   dimen.cmn
!
! Required subprogram(s):
!   GetElmIntID
!   GetNodExtID
!
! Required module variable(s):
!   none
!
! Last update: 17/3/2016
!***************************************************************************************************   
        SUBROUTINE ExtractElmNodLst2 (elmID, nodLst, nNod)
          INTEGER, INTENT(IN)  :: elmID
          INTEGER, ALLOCATABLE, INTENT(OUT) :: nodLst(:)
          INTEGER, INTENT(OUT) :: nNod
  
          INTEGER :: i, intElNum, dataIndex
          
          nNod = 0
          IF (nEltyp .EQ. 1) THEN
            intElNum = GetElmIntID (elmID)
            IF (intElNum .NE. 0) THEN
              ALLOCATE (nodLst(nNode))
              dataIndex = nNode * (intElNum - 1)
              DO i = 1, nNode
                nodLst(i) = GetNodExtID (iElCon(dataIndex + i))
              END DO
              nNod = nNode
            END IF
          END IF
        END SUBROUTINE ExtractElmNodLst2
!***************************************************************************************************
! FUNCTION GetElmAveVal
!
! Objective:
!   This function returns the average of an elemental value
!
! Input(s):
!   elmID          INTEGER       element user ID
!   elmCode        INTEGER       element post code
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   IP             INTEGER      integration point
!   nIP            INTEGER      number of integeration points
!   intElmID       INTEGER      internal element ID
!   curElmVal      REAL*8       current elemental value
!   totElmVal      REAL*8       current elemental value
!
! Required common-blocks:
!   elmcom.cmn
!
! Required subprogram(s):
!   none
!
! Required module variable(s):
!   none
!
! Last update: 22/3/2016
!*************************************************************************************************** 
!***************************************************************************************************
      FUNCTION GetElmAveVal (elmID, elmCode)
        INTEGER, INTENT(IN) :: elmID, elmCode
        REAL*8 :: GetElmAveVal
        
        INTEGER :: nIP, IP, IntElmID
        REAL*8 :: curElmVal, totElmVal
        
        curElmVal = 0.D0
        totElmVal = 0.D0
        
        intElmID = GetElmIntID(elmID)

        IF (intElmID .EQ. 0) THEN
          CALL QUIT(1234)
        ELSE
          nIP = GetElmIPCount(elmID)
          
          DO IP = 1, nIP
            CALL elmvar (elmCode, elmID, IP, 0, curElmVal)        
            totElmVal = totElmVal + curElmVal
          END DO
          GetElmAveVal = totElmVal / nIP
        END IF 

        RETURN
      END FUNCTION GetElmAveVal
!***************************************************************************************************
! FUNCTION GetIPCoord
!
! Objective:
!   This function returns the corrdinates of the integration points of an element. Apparently, the
! IP coordinates are stored in the VarsElem(*) array while the maximum number of coordinates (ncrd)
! is used to specify the coordinates.
!
! Input(s):
!   elmID          INTEGER      user ID of the element
!   IP             INTEGER      integration point number
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   iGroup         INTEGER      loop counter for groups (elmcom.cmn)
!   
!   intElID        INTEGER      internal element ID
!   intElType      INTEGER      internal element type
!   nElInGroup     INTEGER      number of elements in each group
!   nELGroups      INTEGER      number of total element groups (elemdata.cmn)
!   iElGroup_ElNum INTEGER(*)   element internal IDs of the current group (elemdata.cmn)       
!   iElType        INTEGER(*)   type of element for the element of the current group (elemdata.cmn) 
!   n              INTEGER      ??? 
!                               the wrat3n utility-subroutine uses to calculate an initial offset 
!                               to handle out-of-core element storage
!   ityp           INTEGER      element type (elmcom.cmn)
!   ielsbn         INTEGER      Pointer to beginning of element storage in varselem (array2.cmn)
!   nelstr         INTEGER      the amount of memory per element in terms of integer words (nzro1.cmn)
!   varselem       REAL*8(*)    double precision versions of ints* sharing the same storage to hold
!                               Marc's (space.cmn)
!   dataIndex      INTEGER      pointer to the location of element connection
!   elmIndex       INTEGER      is the index of the internal element ID in the iElGroup_Elnum array
!                               which is used in the wrat3n subroutine
!   lofr           INTEGER      ???   (heat.cmn)
!   ncrdmx         INTEGER      max. no. of coordinates per node (dimen.cmn)
!   ncrd           INTEGER      no. of coordinates per node for the current element (dimen.cmn)
!   icrxpt         INTEGER      Pointer to integration point coordinates (array2.cmn)
!   ityp           INTEGER      ??? internal element type (elmcom.cmn)
!
! Required common-blocks:
!   elmcom.cmn
!   dimen.cmn
!   elemdata.cmn
!   array2.cmn
!   nzro1.cmn
!   heat.cmn
!   space.cmn
!
! Required subprogram(s):
!   none
!
! Required module variable(s):
!   none
!
! Last update: 21/3/2016
!***************************************************************************************************         
      FUNCTION GetIPCoord (elmID, IP)
        INTEGER, INTENT(IN)  :: elmID, IP
        REAL*8, DIMENSION(3) :: GetIPCoord

        REAL*8 ::  tCoord
        INTEGER :: i, nElInGroup, intElmID, dataIndex, n, elmIndex
        
        GetIPCoord = [0.D0, 0.D0, 0.D0]

          intElmID = GetElmIntID(elmID)
          iGroup = 0
          elmIndex = 0
          DO WHILE ((iGroup .LT. nELGroups) .AND. (elmIndex .EQ. 0))
            iGroup = iGroup + 1
            CALL Setup_ElGroups (iGroup, nElInGroup, 0, 0, 0)
            elmIndex = GetIndex (iElGroup_ElNum,nElInGroup,intElmID)
          END DO
          
          IF (elmIndex .EQ. 0) THEN 
            CALL QUIT(1234)
          ELSE
            ityp = ieltype(intElmID)
            CALL SetEl (intElmID)
            CALL wrat3n (VarsElem(ielsbn), n, elmIndex, iGroup, 0)
            lofr = (n - 1) * nelstr
            dataIndex = icrxpt + (IP - 1) * ncrdmx + lofr
            DO i = 1, ncrd
              tCoord = varselem(dataIndex)
              GetIPCoord(i) = tCoord
              dataIndex = dataIndex + 1
            END DO
           END IF

      END FUNCTION GetIPCoord
!***************************************************************************************************
! FUNCTION GetIPVal
!
! Objective:
!   This function returns the value of elmCode in integration point IP of element elmID. (Scalar values)
!
! Input(s):
!   elmID          INTEGER       element user ID
!   IP             INTEGER       integration point
!   elmCode        INTEGER       element post code
!                                  Sample codes:
!                                  1-6     componenets of strain
!                                  11-16   componenets of stress
!                                  17      von Mises Stress
!                                  18      mean normal Stress
! Output(s):
!   none
!
! Auxiliary variable(s):
!   elmIPVal       REAL*8       value of the elmCode quantity in the IP
!   elmAve         REAL*8       average value of the quantity, defined by elmCode, in the element
!   elmCen         REAL*8(3)    coordinates of the element center
!   nodCoord       REAL*8(3)    coordinates of the nodID
!   IPCoord        REAL*8(3)    coordinates of the IP
!   cenNodDis      REAL*8       distance between the node and the center of element
!   IPNodDis       REAL*8       distance between the IP and the center of element
!   
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   none
!
! Required module variable(s):
!   none
!
! Restrictions:
!   it only returns the scalar elemental values.
!
! Last update: 27/3/2016
!*************************************************************************************************** 
      FUNCTION GetIPVal (elmID, IP, elmCode)
        INTEGER, INTENT(IN) :: elmID, IP, elmCode
        REAL*8 :: GetIPVal
        
        REAL*8 :: elmIPVal
        
        IF ((IP .EQ. 0) .OR. (elmID .EQ. 0))THEN
          CALL QUIT(1234)
        ELSE
          CALL ElmVar (elmCode, elmID, IP, 0, elmIPVal)
          GetIPVal = elmIPVAl
        END IF
        RETURN
      END FUNCTION GetIPVal
!***************************************************************************************************
! FUNCTION GetNodCoord
!
! Objective:
!   Returns the undeformed/deformed coordinates of a node ID.
!   Returns three zero coordinates in the case of an error.
!
! Input(s):
!   nodID        INTEGER             external node ID
!   nodState     INTEGER(OPTIONAL)   the state of the node
!                                    1: undeformed state
!                                    2: deformed state
! Output(s):
!             REAL*8(3)          coordinates of the nodID
!
! Auxiliary variable(s):
!   nComp        INTEGER             number of returned components
!   dataType     INTEGER             the returned data type
!   nodCoord     REAL*8(3)           coordinates of the nodID
!   nodDisp      REAL*8(3)           displacement of the nodID
!   i            INTEGER             loop counter
!   coordState   INTEGER             holds the same state as nodState 
!                                    if the latter is present
!
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   none
!
! Restrictions:
!   none
!
! Future Updates:
!   Make the nodState an optional argument.
!
! Last update: 23/3/2016
!***************************************************************************************************  
      FUNCTION GetNodCoord (nodID, nodState)
        INTEGER, INTENT(IN)  :: nodID 
        INTEGER, OPTIONAL, INTENT(IN)  :: nodState
        REAL*8, DIMENSION(3) :: GetNodCoord
        
        INTEGER :: nComp, dataType, i
        REAL*8, DIMENSION(3) :: nodCoord, nodDisp
        INTEGER :: coordState

        nodCoord = [0.0D0, 0.0D0, 0.0D0]
        
        IF (PRESENT(nodState)) THEN
         coordState = nodState
        ELSE
         coordState = 1
        END IF

        CALL NodVar (0, nodID, nodCoord, nComp, dataType)
        
        IF (coordState .EQ. 1) THEN 
          GetNodCoord = nodCoord
        ELSE
          CALL NodVar (1, nodID, nodDisp, nComp, dataType)
          GetNodCoord = nodCoord + nodDisp  
        END IF
        IF (nComp .EQ. 2) GetNodCoord(3) = 0.D0
      END FUNCTION GetNodCoord
!***************************************************************************************************
! FUNCTION GetNodExtID
!
! Objective:
!   Converts the internal ID of a node to its external/user ID.
!   Returns zero on error.
!
! Input(s):
!   intNodID  INTEGER        internal node ID
!
! Output(s):
!             INTEGER        external node ID
!
! Auxiliary variable(s):
!   nNoIDs    INTEGER        flag if non-zero indicates non-consecutive node numbering (prepro.cmn)
!   iNoIDs_d  INTEGER(*)     contains the external ID of the nodes if nNoIDs is flagged (prepro.cmn)
!   numnp     INTEGER        number of total nodes (dimen.cmn)
!
! Required common-blocks:
!   prepro.cmn
!   dimen.cmn
!
! Required subprogram(s):
!   none
!
! Old listing:
!
!      INTEGER FUNCTION GetNodExtID (intNodID)
!        INTEGER, INTENT (IN) :: intNodID
!        
!        GetNodExtID = 0
!        IF (nNoIDs .EQ. 0) THEN
!          IF (nNoIDs .LE. numnp) GetNodExtID = intNodID
!        ELSE
!          GetNodExtID = iNoIDs_d(intNodID)
!        END IF
!      END FUNCTION GetNodExtID
!
! Last update: 29/8/2016
!***************************************************************************************************
      INTEGER FUNCTION GetNodExtID (intNodID)
        INTEGER, INTENT (IN) :: intNodID
        
        INTEGER :: tIntNodID, nodext
        
        tIntNodID = nodext (intNodID)
        
          IF (tIntNodID .LE. 0) THEN
            CALL QUIT(1234)
          ELSE
            GetNodExtID = tIntNodID
          END IF
          
      END FUNCTION GetNodExtID
!***************************************************************************************************
! FUNCTION GetNodIntID
!
! Objective:
!   Converts the external ID of a node to its internal ID.
!   Returns zero in the case of error.
!
! Input(s):
!   NodID     INTEGER       external node ID
!
! Output(s):
!             INTEGER       internal node ID
!
! Auxiliary variable(s):
!   nNoIDs    INTEGER        flag if non-zero indicates non-consecutive node numbering (prepro.cmn)
!   iNoIDs_d  INTEGER(*)     contains the external ID of the nodes if nNoIDs is flagged (prepro.cmn)
!   i         INTEGER        loop counter
!   numnp     INTEGER        number of total nodes (dimen.cmn)
!
! Required common-blocks:
!   prepro.cmn
!   dimen.cmn
!
! Required subprogram(s):
!   GetNodExtID
!
! Old listing:
!
!        INTEGER FUNCTION GetNodIntID (nodID)
!          INTEGER, INTENT (IN) :: nodID
!          
!          INTEGER :: i
!          LOGICAL :: found
!
!          IF (nNoIDs .EQ. 0) THEN
!            GetNodIntID = nodID
!          ELSE
!            i = 1
!            found = .FALSE.
!
!            DO WHILE ((i .LE. NumNP) .AND. (.NOT. found))
!              IF (nodID .EQ. GetNodExtID(i)) THEN
!                found = .TRUE.
!              ELSE
!                i = i + 1
!              END IF
!            END DO
!            
!            IF (found .EQV. .TRUE.) THEN
!              GetNodIntID = i
!            ELSE
!              GetNodIntID = 0
!            END IF
!            
!          END IF
!        END FUNCTION GetNodIntID
!
!
! Last update: 29/8/2016
!***************************************************************************************************
        INTEGER FUNCTION GetNodIntID (nodIntID)
          INTEGER, INTENT (IN) :: nodIntID
          
          INTEGER :: nodint, tNodIntID
          
          tNodIntID = nodint (nodIntID)
          
          IF (tNodIntID .LE. 0) THEN
            CALL QUIT (1234)
          ELSE
            GetNodIntID = tNodIntID
          END IF

        END FUNCTION GetNodIntID
!***************************************************************************************************
! FUNCTION GetNodExtraVal
!
! Objective:
!   This function returns the extrapolated value of the IPs to nodes. 
!
! Input(s):
!   elmID          INTEGER       element user ID
!   nodID          INTEGER       nodeNumber
!   IP             INTEGER       integration point
!   elmCode        INTEGER       element post code
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   IPValLst       REAL*8(:)    list of the value of the elmCode in each integration point
!   nIP            INTEGER      number of integration points for elmID
!   iIP            INTEGER      loop counter for the integration point number
!
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   GetIPVal FUNCTION
!   GetElmIPCount FUNCTION
!
! Required module variable(s):
!   none
!
! Last update: 27/3/2016
!
! Restrictions:
!   only for 4-node quadrilateral elements.
!
! Pending Updates:
!   - Covering several types of elements based on their element type number
!   - Covering several other types of extrapolation
!
! old version of the function:
!
!      FUNCTION GetNodExtraVal (elmID, nodID, IP, elmCode)
!        INTEGER, INTENT(IN) :: elmID, nodID, IP, elmCode
!        REAL*8 :: GetNodExtraVal
!        
!        REAL*8 :: cenCoord(3), nodCoord(3), IPCoord(3)
!        REAL*8 :: elmAve, elmIPVal ,cenNodDis,cenIPDis 
!        CALL ElmVar (elmCode, elmID, IP, 0, elmIPVal)
!        
!        elmAve    = GetElmAveVal (elmID, elmCode)
!        cenCoord  = GetElmCenCoord (elmID, 1)
!        nodCoord  = GetNodCoord (nodID, 1)
!        IPCoord   = GetIPCoord (elmID, IP)
!
!!        nodDis = GetDistance ([0.D0,0.D0,0.D0], nodCoord)
!!        IPDis  = GetDistance ([0.D0,0.D0,0.D0], IPCoord)
!!        cenDis = GetDistance ([0.D0,0.D0,0.D0], cenCoord)
!        
!        cenNodDis = GetDistance (cenCoord, nodCoord)
!        cenIPDis  = GetDistance (cenCoord, IPCoord)
!        
!        GetNodExtraVal = elmAve + 
!     &         (elmIPVal - elmAve)*(cenNodDis)/(cenIPDis)
!
!      END FUNCTION GetNodExtraVal
!
!
!***************************************************************************************************      
      FUNCTION GetNodExtraVal (elmID, nodID, IP, elmCode)
        INTEGER, INTENT(IN) :: elmID, nodID, IP, elmCode
        REAL*8 :: GetNodExtraVal
        
        REAL*8, ALLOCATABLE :: IPValLst(:)
        INTEGER :: nIP, iIP
        
        nIP = GetElmIPCount (elmID)
        
        IF (nIP .NE. 4) THEN 
          CALL QUIT(1234)
        ELSE
          ALLOCATE (IPValLst(nIP))
          DO iIP = 1, nIP
            IPValLst(iIP)  = GetIPVal (elmId, iIP, elmCode)
          END DO

          SELECT CASE (IP)
            CASE(1)
              GetNodExtraVal = 
     &        0.25D0 * ((1.D0 + 3.D0 * SQRT(3.D0)) * IPValLst(1) +
     &                  (1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(2) +
     &                  (1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(3) +
     &                  (1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(4))
            CASE(2)
              GetNodExtraVal = 
     &        0.25D0 * ((1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(1) +
     &                  (1.D0 + 3.D0 * SQRT(3.D0)) * IPValLst(2) +
     &                  (1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(3) +
     &                  (1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(4)) 
            CASE(4)
              GetNodExtraVal = 
     &        0.25D0 * ((1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(1) +
     &                  (1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(2) +
     &                  (1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(3) +
     &                  (1.D0 + 3.D0 * SQRT(3.D0)) * IPValLst(4)) 
            CASE(3)
              GetNodExtraVal = 
     &        0.25D0 * ((1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(1) +
     &                  (1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(2) +
     &                  (1.D0 + 3.D0 * SQRT(3.D0)) * IPValLst(3) +
     &                  (1.D0 - 1.D0 * SQRT(3.D0)) * IPValLst(4)) 
          END SELECT
        END IF
      END FUNCTION GetNodExtraVal
    

      
!***************************************************************************************************
! Function GetNodIPVal
!
! Objective:
!   This function returns the equivalent value of an elemental value on the node
!
! Input(s):
!   nodID          INTEGER       nodeNumber
!   elmCode        INTEGER       element post code
!                                Example codes:
!                                  1-6     componenets of strain
!                                  11-16   componenets of stress
!                                  17      von Mises Stress
!                                  18      mean normal Stress
!   calcMeth       INTEGER      specifies the calculation method
!                                 1: Translation    (unweighted averaging among elements)
!                                 2: Extrapolation  (unweighted averaging among elements)
!                                 3: Average        (unweighted averaging among elements)
!                                 5: Translation    (weighted averaging with the area of each element as weight)
!                                 6: Extrapolation  (weighted averaging with the area of each element as weight)
!                                 7: Average        (weighted averaging with the area of each element as weight)
!
! Output(s):
!
! Auxiliary variable(s):
!   elmLst
!   elmLst         INTEGER(:)   list of elements
!   IPLst          INTEGER(:)   list of IPs
!   nItemLst       INTEGER      number of items in the previous two lists          
!   i              INTEGER      loop counter
!   curVal         REAL*8       current elemental value

!   curElm         INTEGER      current element
!   curIP          INTEGER      current integration point


!   curIPVal       INTEGER      curret integration point
!   nodCoord       INTEGER(3)   coordinates of the nodID
!   IPCoord        INTEGER(3)   coordinates of the IP
!   maxNP          INTEGER      maximum number of connection to a node (dimen)
!                               which is used to allocate the tElmLst
!   nIntB          INTEGER      max. no. of IPs (dimen)
!
! Required common-blocks:
!   dimen.cmn
!
! Required subprogram(s):
!   ExtractNodCloseIPLst SUBROUTINE
!   GetNodExtraVal FUNCTION
!   GetElmAveVal   FUNCTION
!
! Required module variable(s):
!   none
!
! Last update: 21/3/2016
!*************************************************************************************************** 
      REAL*8 FUNCTION GetNodIPVal (nodID, elmCode, calcMeth)
        INTEGER, INTENT(IN) :: nodID, elmCode, calcMeth
        
        INTEGER :: curElm, curIP, nItemLst, i
        INTEGER, ALLOCATABLE ::  elmLst(:), IPLst(:)
        REAL*8, ALLOCATABLE ::  areaLst(:)
        REAL*8 :: curVal, totVal, elmAve, elmCen(3), curArea, totArea
        
        totVal = 0.D0
        CALL ExtractNodCloseIPLst (nodID, nItemLst, elmLst, IPLst)

        IF (nItemLst .EQ. 0) THEN
          CALL QUIT(1234)
        ELSE
          ALLOCATE (areaLst(nItemLst))
          totArea = 0.D0
          
          DO i = 1, nItemLst
            curElm = elmLst(i)
            curIP  = IPLst(i)
            SELECT CASE (calcMeth)
              CASE (1)
                curVal = GetIPVal (curElm,curIp,elmCode)
                arealst(i) = 1.D0
              CASE (2)
                curVal = GetNodExtraVal(curElm, nodID, curIP, elmCode)
                arealst(i) = 1.D0
              CASE (3)
                curVal = GetElmAveVal (curElm, elmCode)
                arealst(i) = 1.D0
              CASE (4)
                curVal = GetIPVal (curElm,curIp,elmCode)
                arealst(i) = GetElmArea(curElm)
              CASE (5)
                curVal = GetNodExtraVal(curElm, nodID, curIP, elmCode)
                arealst(i) = GetElmArea(curElm)                
              CASE (6)
                curVal = GetElmAveVal (curElm, elmCode)
                arealst(i) = GetElmArea(curElm)
            END SELECT
            curArea = areaLst(i)
            totArea = totArea + curArea
            totVal  = totVal  + (curVal*curArea)
          END DO
          GetNodIPVal = totVal / totArea            
        END IF
      END FUNCTION GetNodIPVal
!***************************************************************************************************
! LOGICAL FUNCTION IsItemInSet
!
! Objective:
!     This functions returns a .TRUE. value if an item exists in a set; otherwise a .FALSE.
!
! Input(s):
!   setName      CHARACTER(*)            name of the set
!   itemID       INTEGER                 ID of the set item    
!   outUnit      INTEGER                 indicates the output unit (optional)
!                                        default = 6
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   i            INTEGER                 loop counter
!   itemList     INTEGER(MAX_SET_ITEM)   list of items of a set
!   isFound      INTEGER                 flag indicating if the set exists
!   nItem        INTEGER                 number of items in the list
!   setType      INTEGER                 indicates the set type
!   ou           INTEGER                 currently used output unit
!
! Required common-blocks:
!   none         
!
! Required subprogram(s):
!   none        
!
! Required module variable(s):
!   MAX_SET_ITEM
!
! Last update: 15/3/2016
!***************************************************************************************************        
      LOGICAL FUNCTION IsItemInSet (setName, itemID, outUnit)
        CHARACTER(LEN=*), INTENT(IN)  :: setName
        INTEGER, INTENT(IN)           :: itemID
        INTEGER, INTENT(IN), OPTIONAL :: outUnit
        
        INTEGER, DIMENSION(MAX_SET_ITEM) :: itemLst
        INTEGER :: isFound, setType, nItemLst, i, ou

        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF        
        
        WRITE (ou,*) '** IsItemInSet SUBROUTINE'
        IsItemInSet = .FALSE.
        CALL Marc_SetInf(setName, isFound, itemLst, setType, nItemLst)
        IF (isFound .EQ. 1) THEN
          WRITE(ou,*) '* Set ', setName, ' was found.'
          DO i = 1, nItemLst
            IF (itemID .EQ. itemLst(i)) THEN
              WRITE(ou,*) '* Item ', itemID, 'was found.'
              IsItemInSet = .TRUE.
            END IF
          END DO
        ELSE IF (isFound .EQ. 0) THEN
          WRITE(ou,*) '* setName = ', setName, ' was not found.'
        END IF
        RETURN
      END FUNCTION IsItemInSet
      
!***************************************************************************************************
! SUBROUTINE PrintElmIDLst
!
! Objective:
!   This subroutine lists all of the elements of the model.
!
! Input(s):
!   none
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   
!
! Required common-blocks:
!   elemdata.cmn 
!   elmcom.cmn
!   dimen.cmn
!
! Required subprogram(s):
!   GetElmExtID
!
! Required module variable(s):
!   none
!
! Last update: 29/3/2016
!*************************************************************************************************** 
      SUBROUTINE PrintElmIDLst (outUnit)
        INTEGER, OPTIONAL, INTENT(IN) :: outUnit
 
        INTEGER, ALLOCATABLE, DIMENSION(:) :: elmIDLst
        INTEGER :: ou, i 

100     FORMAT (A19,1X,A19)
200     FORMAT (I19,1X,I19)
        
       
        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF 
        
        CALL MakeElmIDLst (elmIDLst)
        
        DO i = 1, numel
          WRITE (ou,100) 'Element Internal ID', 'Element External ID'
          WRITE (ou,200) i, elmIDLst(i)
        END DO
        RETURN
      END SUBROUTINE PrintElmIDLst
        
      
!***************************************************************************************************
! SUBROUTINE PrintElmIDGroupedLst
!
! Objective:
!   This subroutine lists all of the elements of the model with the corresponding element groups.
!
! Input(s):
!   none
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   i              INTEGER      loop counter for groups
!   j              INTEGER      loop counter for elements
!   intElID        INTEGER      internal element ID
!   intElType      INTEGER      internal element type
!   nElInGroup     INTEGER      number of elements in each group
!   nELGroups      INTEGER      number of total element groups (elemdata.cmn)
!   iElGroup_ElNum INTEGER(*)   element internal IDs of the current group (elemdata.cmn)       
!   iElType        INTEGER(*)   type of element for the element of the current group (elemdata.cmn)       
!   nnode          INTEGER      no. of nodes per element (elmcom.cmn)
!   intel          INTEGER      No. of IPs (elmcom.cmn)
!   lclass         INTEGER      Element class (elmcom.cmn)
!   ityp           INTEGER      Internal element type (elmcom.cmn)
!   jtype          INTEGER      user element type (elmcom.cmn)
!   ncrdel         INTEGER      No. of coordinates (elmcom.cmn)
!   ndegel         INTEGER      No. of DOFs (elmcom.cmn)

! Required common-blocks:
!   elemdata.cmn 
!   elmcom.cmn
!   dimen.cmn
!
! Required subprogram(s):
!   GetElmExtID
!
! Required module variable(s):
!   none
!
! Last update: 29/3/2016
!*************************************************************************************************** 
      SUBROUTINE PrintElmIDGroupedLst (outUnit)
        INTEGER, OPTIONAL, INTENT(IN) :: outUnit
 
        INTEGER :: i, j, intElID, intElType, nElInGroup, ou

100     FORMAT (A30,1X,I4)
        
        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF        

        WRITE (ou,*) '** PrintElmIDGroupedLst SUBROUTINE'
        WRITE (ou,100) 'No. of Element Group(s): ', nElGroups
        WRITE (ou,*)
        DO i = 1, nElGroups
          CALL Setup_ElGroups(i, nElInGroup, 0, 0, 0)
          WRITE (ou,100) 'Element Group No.:', i
          WRITE (ou,100) 'No. of nodes per element:', nnode
          WRITE (ou,100) 'No. of IPs per element:', intel
          WRITE (ou,100) 'Element class:', lclass
          WRITE (ou,100) 'Internal element type:', ityp
          WRITE (ou,100) 'Element type:', jtype
          WRITE (ou,100) 'No. of coordinates:', ncrdel
          WRITE (ou,100) 'No. of DOFs per node:', ndeg
          WRITE (ou,100) 'No. of DOFs:', ndegel
          WRITE (ou,100) 'No. of elements in the group:', nElInGroup
          WRITE (ou,*) 'List of elements in the group: '
          DO j = 1, nElInGroup
            intElID   = iElGroup_ElNum(j)
            intElType = iElType(intElID)
            WRITE (ou,100) 'Internal ID:', intElID
            WRITE (ou,100) 'User ID:', GetElmExtID(intElID)
            WRITE (ou,100) 'Internal Type:', intElType
          END DO
          WRITE (ou,*)
        END DO
      END SUBROUTINE PrintElmIDGroupedLst
!***************************************************************************************************
! SUBROUTINE PrintIPValLst
!
! Objective:
!   This subroutine prints the values in the integration points of all elements in the model
!   The value is specified by elmCode and the output by outUnit.
!
! Input(s):
!
!   elmCode        INTEGER      element post code
!                                  1-6     componenets of strain
!                                  11-16   componenets of stress
!                                  17      von Mises Stress
!                                  18      mean normal Stress
!
!   outUnit        INTEGER      indicates the output unit (optional)
!                               default = 6
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   ou             INTEGER      used output for printing
!   elmID          INTEGER      element user ID
!   intElmID       INTEGER      internal element ID
!   IP             INTEGER      integration point
!   nIP            INTEGER      number of integeration points
!   IPValLst       REAL*8(:,:)  value of the elmCode quantity in the IP
!   nIPValLst      REAL*8(:,:)  list of values for the requested elmCode
!   
! Required common-blocks:
!   elmcom.cmn
!
! Required subprogram(s):
!   GetElmExtID
!   GetElmIPCount
!
! Required module variable(s):
!   none
!
! Future Updates:
!   none
!
! Last update: 3/4/2016
!*************************************************************************************************** 
      SUBROUTINE PrintIPValLst (elmCode, outUnit, elmIDLst, nElmIDLst)
        INTEGER, INTENT(IN) :: elmCode
        INTEGER, INTENT(IN), OPTIONAL :: outUnit
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: elmIDLst
        INTEGER, INTENT(IN), OPTIONAL :: nElmIDLst
        
        INTEGER :: ou,i , elmID, intElmID, IP, nIP, nIPValLst
        REAL*8, ALLOCATABLE, DIMENSION(:,:) :: IPValLst
        
100     FORMAT (A13,X,I4)
200     FORMAT (A7,X,A2,X,A16,X)
300     FORMAT (I7,X,I2,X,F16.4,X)
400     FORMAT (28('-'))
        
        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF
        
        WRITE (ou, 100) 'Increment No.', inc
        WRITE (ou, 100) 'Element Post Code', elmCode
        WRITE (ou, 400)
        WRITE (ou, 200) 'Element', 'IP', 'Value'
        WRITE (ou, 400)
        
        IF (Present(elmIDLst) .AND. Present(nElmIDLst)) THEN
          CALL MakeIPValLst (elmCode, IPValLst, nIPValLst,
     &                          elmIDLst, nElmIDLst)
          
          DO i = 1, nIPValLst
            elmID = elmIDLst(i)
            nIP   = GetElmIPCount (elmID)
            DO IP = 1, nIp
              WRITE (ou, 300)  elmID,IP, IPValLst(i,IP)
            END DO
            WRITE (ou,400)
          END DO
        ELSE
          CALL MakeIPValLst (elmCode, IPValLst, nIPValLst)
          DO intElmID = 1, nIPValLst
            elmID = GetElmExtID (intELmID)
            nIP   = GetElmIPCount (elmID)
            DO IP = 1, nIp
              WRITE (ou, 300)  elmID,IP, IPValLst(intElmID,IP)
            END DO
            WRITE (ou,400)
          END DO
        END IF
        
      END SUBROUTINE PrintIPValLst
      
      
      
      
      
      
      
      
      
!***************************************************************************************************
! SUBROUTINE PrintIPCoordLst
!
! Objective:
!   This subroutine lists all nodes of the model along with their coordinates. 
!   The coordinates can be both deformed and undeformed ones.
!   There is an optional parameter to direct the output results.
!
! Input(s):
!   nodState       INTEGER      the state of the node
!                               1: undeformed state
!                               2: deformed state
!   outUnit        INTEGER      indicates the output unit (optional)
!                               default = 6
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   ou             INTEGER      currently used output unit
!   IP             INTEGER      integration point
!   nIP            INTEGER      number of integeration points
!   elmID          INTEGER      external element ID
!   intElmID       INTEGER      internal element ID
!   jintel         INTEGER      number of integration points for the current element (elmcom.cmn))
!
! Required common-blocks:
!   elmcom.cmn
!
! Required subprogram(s):
!   GetElmExtID
!   GetIPCoord
!
! Required module variable(s):
!   none
!
! Last update: 27/3/2016
!***************************************************************************************************     
      SUBROUTINE PrintIPCoordLst (outUnit, elmIDLst, nElmIDLst)
        INTEGER, INTENT(IN), OPTIONAL :: outUnit
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: elmIDLst
        INTEGER, INTENT(IN), OPTIONAL :: nElmIDLst

        INTEGER :: ou, elmID, intElmID, IP, nIP, nIPCoordLst, i
        REAL*8, ALLOCATABLE, DIMENSION(:,:,:) :: IPCoordLst
        
100     FORMAT (A7,X,A2,X,3(A15,X))
200     FORMAT (I7,X,I2,X,3(F15.4,X))
300     FORMAT (59('-'))
        
        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF
        
        WRITE (ou,*) 'Integration Point Coordinates'
        WRITE (ou,300)
        WRITE (ou,100) 'Element', 'IP', 'X', 'Y', 'Z'
        WRITE (ou,300)
        
        IF (Present (elmIDLst) .AND. Present (nElmIDLst)) THEN
          CALL MakeIPCoordLst (IPCoordLst, nIPCoordLst,
     &                          elmIDLst, nElmIDLst)
          
          DO i = 1, nIPCoordLst
            elmID = elmIDLst(i)
            nIP   = GetElmIPCount (elmID)
            DO IP = 1, nIp
              WRITE (ou, 200)  elmID,IP, IPCoordLst(i,IP,:)
            END DO
            WRITE (ou,300)
          END DO
        ELSE

          CALL MakeIPCoordLst (IPCoordLst, nIPCoordLst)

          DO intElmID = 1, nIPCoordLst
            elmID = GetElmExtID (intELmID)
            nIP   = GetElmIPCount (elmID)
            DO IP = 1, nIp
              WRITE (ou, 200)  elmID,IP, IPCoordLst(intELmID,IP,:)
            END DO
            WRITE (ou,300)
          END DO
        END IF
      END SUBROUTINE PrintIPCoordLst   
!***************************************************************************************************
! SUBROUTINE PrintNodCoordLst
!
! Objective:
!   This subroutine lists all nodes of the model along with their coordinates. 
!   The coordinates can be both deformed and undeformed ones.
!   There is an optional parameter to direct the output results.
!
! Input(s):
!   nodState       INTEGER      the state of the node
!                               1: undeformed state
!                               2: deformed state
!   outUnit        INTEGER      indicates the output unit (optional)
!                               default = 6
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   ou             INTEGER      currently used output unit
!   numnp          INTEGER      number of nodes in mesh (dimen.cmn)
!
! Required common-blocks:
!   dimen.cmn
!
! Required subprogram(s):
!   GetElmExtID
!   MakeNodCoordLst
!
! Required module variable(s):
!   none
!
! Last update: 2/4/2016
!***************************************************************************************************     
      SUBROUTINE PrintNodCoordLst (nodState, outUnit, nodLst, nNodLst)
        INTEGER, INTENT(IN), OPTIONAL :: nodState
        INTEGER, INTENT(IN), OPTIONAL :: outUnit
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: nodLst
        INTEGER, INTENT(IN), OPTIONAL :: nNodLst
        
        INTEGER :: i, ou, ns, nodID, nNodCoordLst
        REAL*8, ALLOCATABLE, DIMENSION(:,:) :: nodCoordLst
        
100     FORMAT (A10,X,3(A15,X))
200     FORMAT (I10,X,3(F15.4,X))
300     FORMAT (59('-'))

        
        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF
        
        IF (Present (nodState)) THEN
          ns = nodState
        ELSE
          ns = 1
        END IF
        
        IF (ns .EQ. 1) THEN
          WRITE(ou,*) 'Original Nodal Coordinates'
        ELSE
          WRITE(ou,*) 'Deformed Nodal Coordinates'
        END IF
        
        WRITE (ou,300)
        WRITE (ou,100) 'Node ID', 'X', 'Y', 'Z'
        WRITE (ou,300)
        
        IF (Present(nodLst) .AND. Present(nNodLst)) THEN
          CALL MakeNodCoordLst(nodCoordLst, nNodCoordLst, ns,
     &                         nodLst, nNodlst)
     
          DO i = 1, nNodCoordLst
            nodID = nodLst(i)
            WRITE (ou, 200) nodID, nodCoordLst(i,:)
          END DO
        ELSE
          
          CALL MakeNodCoordLst(nodCoordLst, nNodCoordLst, ns)
     
          DO i = 1, nNodCoordLst
            nodID = GetNodExtID(i)
            WRITE (ou, 200) nodID, nodCoordLst(i,:)
          END DO        
        
        END IF
        WRITE (ou,300)        
      END SUBROUTINE PrintNodCoordLst
!***************************************************************************************************
! SUBROUTINE PrintNodIDLst
!
! Objective:
!   This subroutine lists all of the nodes of the model along with their essential properties.
!
! Input(s):
!   none
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   i              INTEGER      loop counter for groups
!   numnp          INTEGER      number of nodes in mesh (dimen.cmn)
!   ndegmx         INTEGER      max. no. of DOFs per node (dimen.cmn)
!   nnodmx         INTEGER      max. no. of nodes per element (dimen.cmn)
!   ncrdmx         INTEGER      number of total element groups (dimen.cmn)
!   maxnp          INTEGER      element internal IDs of the current group (dimen.cmn)
!
! Required common-blocks:
!   dimen.cmn
!
! Required subprogram(s):
!   GetElmExtID
!
! Required module variable(s):
!   none
!
! Last update: 18/3/2016
!*************************************************************************************************** 
      SUBROUTINE PrintNodIDLst ()
        INTEGER :: i
100     FORMAT (A35,I4)

        WRITE (6,*) '** PrintNodIDLst SUBROUTINE'
        WRITE (6,100) 'Total no. of nodes:', numnp
        WRITE (6,100) 'Max. No. of DoF per node:', ndegmx
        WRITE (6,100) 'Max. No. of nodes per element:', nnodmx
        WRITE (6,100) 'Max. No. of coordinates per node:', ncrdmx
        WRITE (6,100) 'Max. No. of connections to a node:', maxnp
        DO i = 1, numnp
          WRITE (6,100) 'Internal node ID:', i
          WRITE (6,100) 'User node ID:', GetNodExtID(i)
        END DO
      END SUBROUTINE PrintNodIDLst
!***************************************************************************************************
! SUBROUTINE PrintNodValLst 
!
! Objective:
!   Returns the undeformed/deformed coordinates of a node ID.
!   Returns three zero coordinates in the case of an error.
!
! Input(s):
!   nodCode      INTEGER             nodal post code
!                                    Example node post codes:
!                                       0 Coordinates
!                                       1 Displacement
!                                       2 Rotation
!                                       3 External Force
!                                       4 External Moment
!                                       5 Reaction Force
!                                       6 Reaction Moment
!                                      79 Total displacement
!   nodLst       INTEGER(*)          a list of node IDs (optional)
!   nNodLst      INTEGER             number of nodes in the nodLst (optional)
!   outUnit      INTEGER             indicates the output unit (optional)
!                                    default = 6
!
! Output(s):
!             REAL*8(3)          coordinates of the nodID
!
! Auxiliary variable(s):
!   ou           INTEGER             used output for printing
!   nodID        INTEGER             node user ID
!   intNodID     INTEGER             internal element ID
!   numnp        INTEGER             number of total nodes in the model (dimen.cmn)
!   vallst       REAL*8(:)           list of values
!   nValLst      INTEGER             number of values in the list
!   i            INTEGER             loop counter


!   nComp        INTEGER             number of returned components
!   dataType     INTEGER             the returned data type
!   nodCoord     REAL*8(3)           coordinates of the nodID
!   nodDisp      REAL*8(3)           displacement of the nodID
!   i            INTEGER             loop counter
!   coordState   INTEGER             holds the same state as nodState 
!   inc          INTEGER             increment number (concom.cmn)
!
! Required common-blocks:
!   dimen.cmn
!
! Internal subprogram(s):
!   PrintTableHeader   
!   PrintFormattedValueList
!   PrintLine ()
!
! Required subprogram(s):
!   GetNodExtID
!   MakeNodValLst
!
! Restrictions:
!   none
!
! Future Updates:
!   - check if node exist in the model.
!   - Algorithm for the shortest path to use for a node-path instead of a range of nodes e.g. in CollectNodValLst
!
! Last update: 1/4/2016
!
!
! Obsolecent version:
!
!      SUBROUTINE PrintNodValLst (nodCode, nodLst, nNodLst, outUnit)
!        INTEGER, INTENT(IN) :: nodCode
!        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: nodLst
!        INTEGER, INTENT(IN), OPTIONAL :: nNodLst, outUnit
!        
!        INTEGER :: ou, nodID, intNodID, nValLst, i
!        REAL*8, ALLOCATABLE, DIMENSION(:) :: valLst
!        LOGICAL :: tableHeaderPrinted
!
!100     FORMAT (A15,X,I2)        
!
!        IF (Present (outUnit)) THEN
!          ou = outUnit
!        ELSE
!          ou = 6
!        END IF
!
!        WRITE (ou, 100) 'Increment No.', inc
!        WRITE (ou, 100) 'Node Post Code', NodCode
!        tableHeaderPrinted = .FALSE.
!        
!        IF (Present(nodLst) .AND. Present(nNodLst)) THEN
!          DO i = 1, nNodLst
!            nodID = nodLst(i)
!            CALL CalcNodVal (nodID, nodCode, valLst, nValLst)
!            
!            IF (nValLst .EQ. 0) THEN
!              CALL QUIT(1234)
!            ELSE
!              IF (tableHeaderPrinted .EQV. .FALSE.)CALL PrintTableHeader
!              CALL PrintFormattedValueList
!            END IF
!          END DO
!        CALL PrintLine
!        ELSE
!          DO intNodID = 1, numnp
!            nodID = GetNodExtID(intNodID)
!            
!            CALL CalcNodVal (nodID, nodCode, valLst, nValLst)
!
!            IF (nValLst .EQ. 0) THEN
!              CALL QUIT(1234)
!            ELSE
!              IF (tableHeaderPrinted .EQV. .FALSE.)CALL PrintTableHeader
!              CALL PrintFormattedValueList
!            END IF
!          END DO
!        CALL PrintLine
!        END IF
!        RETURN
!        
!      CONTAINS
!!***************************************************************************************************
!        SUBROUTINE PrintTableHeader ()
!          INTEGER :: j
!            
!200       FORMAT ('Node ID',10(5X,'Component',I3,:))
!          
!          CALL PrintLine
!          WRITE (ou, 200) [(j, j = 1, nValLst)]
!          CALL PrintLine
!          
!          tableHeaderPrinted = .TRUE.
!        END SUBROUTINE PrintTableHeader
!!***************************************************************************************************        
!        SUBROUTINE PrintFormattedValueList ()
!          INTEGER :: j
!          
!500       FORMAT (I7,X,10(F16.4,X,:))
!
!          WRITE (ou, 500) nodID, [(valLst(j), j = 1, nValLst)] 
!        END SUBROUTINE PrintFormattedValueList
!!***************************************************************************************************          
!        SUBROUTINE PrintLine ()
!          INTEGER :: j
!          
!400       FORMAT (8('-'),10(17A1,:))
!          
!          WRITE (ou, 400) [('-', j = 1, nValLst*17)]
!        END SUBROUTINE PrintLine
!
!      END SUBROUTINE PrintNodValLst
!***************************************************************************************************            
      SUBROUTINE PrintNodValLst (nodCode, nodLst, nNodLst, outUnit)
        INTEGER, INTENT(IN) :: nodCode
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: nodLst
        INTEGER, INTENT(IN), OPTIONAL :: nNodLst, outUnit
        
        INTEGER :: nodID, nValLst, i, j, nComp, ou
        REAL*8, ALLOCATABLE, DIMENSION(:,:) :: valLst

100     FORMAT (A15,X,I2)        
500     FORMAT (I7,X,10(F16.4,X,:))

        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF

        WRITE (ou, 100) 'Increment No.', inc
        WRITE (ou, 100) 'Node Post Code', NodCode
        
        IF (Present(nodLst) .AND. Present(nNodLst)) THEN
          CALL MakeNodValLst (nodCode, valLst, nValLst, nComp,
     &                        nodLst, nNodLst)
          CALL PrintTableHeader
          DO i = 1, nValLst
            WRITE (ou, 500) nodLst(i), [(valLst(i,j), j = 1, nComp)] 
          END DO 
        ELSE
          CALL MakeNodValLst (nodCode, valLst, nValLst, nComp)
          CALL PrintTableHeader
          DO i = 1, nValLst
            WRITE (ou, 500) GetNodExtID(i),[(valLst(i,j), j = 1, nComp)] 
          END DO 
        END IF
        CALL PrintLine          
        RETURN
        
      CONTAINS

      SUBROUTINE PrintTableHeader ()
        INTEGER :: j
          
200     FORMAT ('Node ID',10(5X,'Component',I3,:))
        
        CALL PrintLine
        WRITE (ou, 200) [(j, j = 1, nComp)]
        CALL PrintLine
        
      END SUBROUTINE PrintTableHeader

      SUBROUTINE PrintLine ()
        INTEGER :: j
        
400     FORMAT (8('-'),10(17A1,:))
        
        WRITE (ou, 400) [('-', j = 1, nComp*17)]
      END SUBROUTINE PrintLine

      END SUBROUTINE PrintNodValLst      
      
      
      
      
      
      
      
      
      
      
      
!***************************************************************************************************
! SUBROUTINE PrintNodValIPLst
!
! Objective:
!   This subroutine prints all the nodal values for all nodes of the element. All the extrapolated,
! translate, and the average values are printed for the elmCode to the outUnit.
!
! Input(s):
!
!   elmCode        INTEGER      element post code
!   outUnit        INTEGER      indicates the output unit (optional)
!                               default = 6
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   ou             INTEGER      used output for printing
!   elmID          INTEGER      element user ID
!   intElmID       INTEGER      internal element ID
!   uTransVal      REAL*8       transpolated elmCode value (unweighted)
!   uExtraVal      REAL*8       extrapolated elmCode value (unweighted)
!   uAveraVal      REAL*8       averaged elmCode value     (unweighted)
!   wTransVal      REAL*8       transpolated elmCode value (weighted by the area of element)
!   wExtraVal      REAL*8       extrapolated elmCode value (weighted by the area of element)
!   wAveraVal      REAL*8       averaged elmCode value     (weighted by the area of element)
!   numnp          INTEGER      number of nodes in mesh (dimen.cmn)
!
! Required common-blocks:
!   dimen.cmn
!
! Required subprogram(s):
!   GetElmExtID
!
! Required module variable(s):
!   none
!
! Last update: 26/3/2016
!*************************************************************************************************** 
      SUBROUTINE PrintNodValIPLst (elmCode, outUnit, nodLst, nNodLst)
        INTEGER, INTENT(IN) :: elmCode
        INTEGER, INTENT(IN), OPTIONAL :: outUnit
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: nodLst
        INTEGER, INTENT(IN), OPTIONAL :: nNodLst
        
        INTEGER :: ou, nodID, i, nVal
        REAL*8, ALLOCATABLE, DIMENSION(:) :: uTransVal, uExtraVal, 
     &   uAveraVal, wTransVal, wExtraVal, wAveraVal
        
100     FORMAT (A13,X,I4)
200     FORMAT (A7,X,6(A16,X))
400     FORMAT (110('-'))
500     FORMAT (8X,A50,1X,A50)

        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF
        
        WRITE (ou, 100) 'Increment No.', inc
        WRITE (ou, 100) 'Element Post Code', elmCode
        WRITE (ou, 400)
        WRITE (ou, 500) 'Unweighted', 'Weighted'
        WRITE (ou, 200) 'Node', 'Translated', 'Extrapolated', 'Average', 
     &                          'Translated', 'Extrapolated', 'Average' 
        WRITE (ou, 400)
         
        IF (Present(nodLst) .AND. Present(nNodLst)) THEN
          CALL MakeNodValIPLst (elmCode, 1, 
     &                         uTransVal, nVal, nodLst, nNodlst)
          CALL MakeNodValIPLst (elmCode, 2,
     &                         uExtraVal, nVal, nodLst, nNodlst)
          CALL MakeNodValIPLst (elmCode, 3,
     &                         uAveraVal, nVal, nodLst, nNodlst)
          CALL MakeNodValIPLst (elmCode, 4,
     &                         wTransVal, nVal, nodLst, nNodlst)
          CALL MakeNodValIPLst (elmCode, 5,
     &                         wExtraVal, nVal, nodLst, nNodlst)
          CALL MakeNodValIPLst (elmCode, 6,
     &                         wAveraVal, nVal, nodLst, nNodlst)
     
          DO i = 1, nVal
            nodID = nodLst(i)
            CALL PrintValue
          END DO
          
        ELSE
        
          CALL MakeNodValIPLst (elmCode, 1, uTransVal, nVal)
          CALL MakeNodValIPLst (elmCode, 2, uExtraVal, nVal)
          CALL MakeNodValIPLst (elmCode, 3, uAveraVal, nVal)
          CALL MakeNodValIPLst (elmCode, 4, wTransVal, nVal)
          CALL MakeNodValIPLst (elmCode, 5, wExtraVal, nVal)
          CALL MakeNodValIPLst (elmCode, 6, wAveraVal, nVal)

          DO i = 1, nVal
            nodID = GetNodExtID(i)
            CALL PrintValue
          END DO
        END IF
      
        WRITE (ou, 400)
        RETURN
        
      CONTAINS
      
        SUBROUTINE PrintValue ()
300       FORMAT (I7,X,6(F16.4,X))        

          WRITE (ou, 300) nodID, uTransVal(i), uExtraVal(i), 
     &        uAveraVal(i), wTransVal(i), wExtraVal(i), wAveraVal(i)
        END SUBROUTINE PrintValue
        
      END SUBROUTINE PrintNodValIPLst
!***************************************************************************************************
! SUBROUTINE PrintSetItemLstID
!
! Objective:
!   Prints the information regarding a specific set ID to the output-file.
!
! Input(s):
!   setID          INTEGER      ID of the set
!   outUnit        INTEGER      indicates the output unit (optional)
!                               default = 6
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   isetnum        INTEGER      set ID                       (spaceset.cmn)
!   isetdat        INTEGER(7,*) array of set information     (spaceset.cmn)
!   ou             INTEGER      currently used output unit
!
! Required common-blocks:
!   spaceset.cmn
!
! Required subprogram(s):
!   none
!
! Last update: 15/3/2016
!***************************************************************************************************
      SUBROUTINE PrintSetItemLstID (setID, outUnit)
        INTEGER, INTENT(IN) :: setID
        INTEGER, INTENT(IN), OPTIONAL :: outUnit
        
        INTEGER :: ou
        
100     FORMAT (A28, 1X, I32)
101     FORMAT (A28, 1X, L32)
102     FORMAT (A28, 1x, A32)
        
        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF

        WRITE (ou,*) '** PrintSetItemLstID SUBROUTINE'
        WRITE (ou,100) 'Set no.:', setID
        WRITE (ou,102) 'Set name:', setnam(setid)
        WRITE (ou,100) 'Set type:', isetdat(1,setid)
        WRITE (ou,100) 'No. of item(s):', isetdat(2,setid)
        WRITE (ou,101) 'Sorted list:', isetdat(3,setid) .EQ. 1
        WRITE (ou,101) 'B.C. flag:', isetdat(4,setid) .EQ. 1
        WRITE (ou,100) 'No. of item(s) (full mode):', isetdat(5,setid)
        WRITE (ou,101) 'Updates in remeshing:', isetdat(6,setid) .EQ. 1
        WRITE (ou,*)
        RETURN
      END SUBROUTINE PrintSetItemLstID
!***************************************************************************************************
! SUBROUTINE PrintSetItemLstName
!
! Objective:
!   Prints the information regarding a specific set name to the output-file.
!
! Input(s):
!   setName        CHARACTER(*) name of the set
!                               Note that originally a set is 32 characters and thus
!                               CHARACTER*32 could have been used.
!   outUnit        INTEGER      indicates the output unit (optional)
!                               default = 6
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   isetnum        INTEGER      set ID                       (spaceset.cmn)
!   isetdat        INTEGER(7,*) array of set information     (spaceset.cmn)
!   notFound       LOGICAL      status of search for the sName
!   ou             INTEGER      currently used output unit
!
! Required common-blocks:
!   spaceset.cmn
!
! Required subprogram(s):
!   PrintSetItemLstID
!
! Last update: 15/3/2016
!***************************************************************************************************     
      SUBROUTINE PrintSetItemLstName (setName, outUnit)
        CHARACTER(LEN=*), INTENT(IN) :: setName
        INTEGER, INTENT(IN), OPTIONAL :: outUnit

        INTEGER :: i, ou
        LOGICAL :: notFound

        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF
        
        notFound = .TRUE.
        i = 1
        DO WHILE ((i .LE. ndSet) .AND. (notFound))
          IF (setName .EQ. setNam(i)) then
            CALL PrintSetItemLstID(i, ou)
            notFound = .FALSE.
          ELSE
            i = i + 1
          END IF            
        END DO
        IF (notFound .EQV. .TRUE.) WRITE(ou,*) setName,' is not found.'
        RETURN             
      END SUBROUTINE PrintSetItemLstName
!***************************************************************************************************
! SUBROUTINE PrintSetLst
!
! Objective:
!   Prints the information regarding sets to the output-file.
!
! Input(s):
!   outUnit        INTEGER      indicates the output unit (optional)
!                               default = 6
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   ndset          INTEGER      No. of total set(s)           (spaceset.cmn)
!   nsetmx         INTEGER      Max. no. of set(s)            (spaceset.cmn)
!   nchnam         INTEGER      Max. set name characters      (spaceset.cmn)
!   mxitmset       INTEGER      Max. no. of set item(s)       (spaceset.cmn)
!   i              INTEGER      loop counter
!   ou             INTEGER      currently used output unit
!
! Required common-blocks:
!   spaceset.cmn
!
! Required subprogram(s):
!   PrintSetItemLstID
!
! Last update: 27/3/2016
!***************************************************************************************************
      SUBROUTINE PrintSetLst (outUnit)
        INTEGER, INTENT(IN), OPTIONAL :: outUnit
        
        INTEGER :: i, ou
100     FORMAT (A27,1X,I4)
        
        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF

        WRITE (ou,100) 'Total sets:', ndset
        WRITE (ou,100) 'Max. no. of sets:', nsetmx
        WRITE (ou,100) 'Max. set name characters:', nchnam
        WRITE (ou,100) 'Max. no. of set items:', mxitmset 
        WRITE (ou,*)
        DO i = 1, ndset
          CALL PrintSetItemLstID (i, ou)
        END DO
        RETURN
      END SUBROUTINE PrintSetLst
!***************************************************************************************************
! SUBROUTINE MakeNodIDLst
!
! Objective:
!   Making a list of all node IDs in a model.
!
! Input(s):
!   none
!
! Output(s):
!   nodLst       INTEGER(:)          list of the nodID
!
! Auxiliary variable(s):
!   nComp        INTEGER             number of returned components
!   dataType     INTEGER             the returned data type
!   nodCoord     REAL*8(3)           coordinates of the nodID
!   nodDisp      REAL*8(3)           displacement of the nodID
!   i            INTEGER             loop counter
!   coordState   INTEGER             holds the same state as nodState 
!
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   none
!
! Restrictions:
!   none
!
! Future Updates:
!   none
!
! Last update: 2/4/2016
!***************************************************************************************************
      SUBROUTINE MakeNodIDLst (nodIDLst)
        INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: nodIDLst
        
        INTEGER :: i
        
        ALLOCATE (nodIDLst(numnp))
        nodIDLst = [(GetNodExtID(i), i = 1, numnp)]
        RETURN
      END SUBROUTINE MakeNodIDLst
!***************************************************************************************************
! SUBROUTINE MakeElmIDLst
!
! Objective:
!   Making a list of all element IDs in a model.
!
! Input(s):
!   none
!
! Output(s):
!   elmIDLst     INTEGER(:)          list of the element IDs
!
! Auxiliary variable(s):
!   i            INTEGER             loop counter
!
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   none
!
! Restrictions:
!   none
!
! Future Updates:
!   none
!
! Last update: 2/4/2016
!***************************************************************************************************
      SUBROUTINE MakeElmIDLst (elmIDLst)
        INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: elmIDLst
        
        INTEGER :: i
        
        ALLOCATE (elmIDLst(numel))
        elmIDLst = [(GetElmExtID(i), i = 1, numel)]
        RETURN
      END SUBROUTINE MakeElmIDLst
!***************************************************************************************************
! FUNCTION IsNodIDValid
!
! Objective:
!   Searches if a nodID is valid (exists).
!
! Input(s):
!   none
!
! Output(s):
!                       LOGICAL             .TRUE. if the nodID exists
!   
! Auxiliary variable(s):
!   nodIDLst            INTEGER(:)          list of the nodID
!
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   GetIndex
!
! Restrictions:
!   none
!
! Future Updates:
!   none
!
! Last update: 2/4/2016
!***************************************************************************************************
      FUNCTION IsNodIDValid (nodID)
        INTEGER, INTENT(IN) :: nodID
        LOGICAL :: IsNodIDValid
        
        INTEGER, ALLOCATABLE, DIMENSION(:) :: nodIDLst
        
        CALL MakeNodIDLst (nodIDLst)
        IF (GetIndex (nodIDLst, numnp, nodID) .EQ. 0) THEN
          IsNodIDValid = .FALSE.
        ELSE
          IsNodIDValid = .TRUE.
        END IF
        RETURN
      END FUNCTION IsNodIDValid
!***************************************************************************************************
! FUNCTION IsElmIDValid
!
! Objective:
!   Searches if a elmID is valid (exists).
!
! Input(s):
!   none
!
! Output(s):
!                       LOGICAL             .TRUE. if the nodID exists
!   
! Auxiliary variable(s):
!   nodIDLst            INTEGER(:)          list of the nodID
!
! Required common-blocks:
!   none
!
! Required subprogram(s):
!   GetIndex
!
! Restrictions:
!   none
!
! Future Updates:
!   none
!
! Last update: 2/4/2016
!***************************************************************************************************
      FUNCTION IsElmIDValid (elmID)
        INTEGER, INTENT(IN) :: elmID
        LOGICAL :: IsElmIDValid
        
        INTEGER, ALLOCATABLE, DIMENSION(:) :: elmIDLst
        
        CALL MakeElmIDLst (elmIDLst)
        IF (GetIndex (elmIDLst, numel, elmID) .EQ. 0) THEN
          IsElmIDValid = .FALSE.
        ELSE
          IsElmIDValid = .TRUE.
        END IF
        RETURN
      END FUNCTION IsElmIDValid
!***************************************************************************************************
! SUBROUTINE MakeNodCoordLst 
!
! Objective:
!   This subroutine makes a list of values for the listed nodes. If no nodes are specified, this is
!   done for all of the nodes in the model.
!   The dimensions of the valLst are nValLst and nComp.
!
! Input(s):
!   nodCode      INTEGER             nodal post code
!                                    Example node post codes:
!                                       0 Coordinates
!                                       1 Displacement
!                                       2 Rotation
!                                       3 External Force
!                                       4 External Moment
!                                       5 Reaction Force
!                                       6 Reaction Moment
!                                      79 Total displacement
!   nodLst       INTEGER(*)          a list of node IDs (optional)
!   nNodLst      INTEGER             number of nodes in the nodLst (optional)
!
! Output(s):
!   valLst       REAL*8(nValLst,nComp)          coordinates of the nodID
!   nValLst      INTEGER                        number of nodes returned (=nNodLst or nnumnp)
!   nComp        INTEGER                        number of returned components per node
!
! Auxiliary variable(s):
!   nodID        INTEGER             node user ID
!   intNodID     INTEGER             internal element ID
!   i            INTEGER             loop counter
!   numnp        INTEGER             number of nodes in mesh (dimen.cmn)  
!
! Required common-blocks:
!   dimen.cmn
!
! Internal subprogram(s):
!   GetNodCoord
!   GetNodExtID
!
! Required subprogram(s):
!   none
!
! Restrictions:
!   none
!
! Future Updates:
!   none
!
! Last update: 2/4/2016
!***************************************************************************************************      
      SUBROUTINE MakeNodCoordLst (nodCoordLst, nNodCoordLst, nodState,
     &                            nodLst, nNodLst)
     
        REAL*8, ALLOCATABLE, DIMENSION(:,:), INTENT(OUT) :: nodCoordLst
        INTEGER, INTENT(OUT) :: nNodCoordLst
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: nodLst
        INTEGER, INTENT(IN), OPTIONAL :: nNodLst
        INTEGER, INTENT(IN), OPTIONAL :: nodState
        
        INTEGER ::  nodID, intNodID, i, ns
        
        IF (Present(nodState)) THEN
          ns = nodState
        ELSE 
          ns = 1
        END IF
        
        IF (Present(nodLst) .AND. Present(nNodLst)) THEN
          ALLOCATE (nodCoordLst(nNodLst,3))
          DO i = 1, nNodLst
            nodID = nodLst(i)
            nodCoordLst(i,:) = GetNodCoord (nodID, ns)
          END DO
          nNodCoordLst = nNodLst
        ELSE
          ALLOCATE (nodCoordLst(numnp,3))
          DO intNodID = 1, numnp
            nodID = GetNodExtID(intNodID)
            nodCoordLst(intNodID,:) = GetNodCoord (nodID, ns)
          END DO
          nNodCoordLst = numnp
        END IF
        RETURN
      END SUBROUTINE MakeNodCoordLst
!***************************************************************************************************
! SUBROUTINE MakeIPCoordLst 
!
! Objective:
!   This creates a list of coordinates for the integration points of all the elements in a model or 
!   a list of specified elements.
!
!
! Input(s):
!   nodCode      INTEGER             nodal post code
!                                    Example node post codes:
!                                       0 Coordinates
!                                       1 Displacement
!                                       2 Rotation
!                                       3 External Force
!                                       4 External Moment
!                                       5 Reaction Force
!                                       6 Reaction Moment
!                                      79 Total displacement
!   nodLst       INTEGER(*)          a list of node IDs (optional)
!   nNodLst      INTEGER             number of nodes in the nodLst (optional)
!
! Output(s):
!   valLst       REAL*8(nValLst,nComp)          coordinates of the nodID
!   nValLst      INTEGER                        number of nodes returned (=nNodLst or nnumnp)
!   nComp        INTEGER                        number of returned components per node
!
! Auxiliary variable(s):
!   nodID        INTEGER             node user ID
!   intNodID     INTEGER             internal element ID
!   i            INTEGER             loop counter
!   numnp        INTEGER             number of nodes in mesh (dimen.cmn)  
!   nintbmx      INTEGER             Max. no. of IPs
!
! Required common-blocks:
!   dimen.cmn
!
! Internal subprogram(s):
!   GetNodCoord
!   GetNodExtID
!
! Required subprogram(s):
!   none
!
! Restrictions:
!   All the elements in the elmIDLst must be of the same type. Currently the maximum number of 
!   the IPs in a model is used.
!   Also because of the different number of IPs for elements, one may use the GetElmIPCount subroutine
!   when necessary to obtain this number.
!
! Future Updates:
!   none
!
! Last update: 2/4/2016
!***************************************************************************************************   3082   
      SUBROUTINE MakeIPCoordLst (IPCoordLst, nIPCoordLst, 
     &                             elmIDLst, nElmIDLst)
     
        REAL*8, ALLOCATABLE, DIMENSION(:,:,:), INTENT(OUT) :: IPCoordLst
        INTEGER, INTENT(OUT) :: nIPCoordLst
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: elmIDLst
        INTEGER, INTENT(IN), OPTIONAL :: nElmIDLst

        INTEGER ::  elmID, intElmID, i, j, nIP
        
        IF (Present(elmIDLst) .AND. Present(nElmIDLst)) THEN

          ALLOCATE (IPCoordLst(nElmIDLst,nintbmx,3))
          IPCoordLst = 0.D0
          
          DO i = 1, nElmIDLst
            elmID = elmIDLst (i)
            nIP   = GetElmIPCount (elmID)
            DO j = 1, nIP
              IPCoordLst(i,j,:) = GetIPCoord (elmID, j)
            END DO
          END DO
          
          nIPCoordLst = nElmIDLst
        ELSE

          ALLOCATE (IPCoordLst(numel,nintbmx,3))
          IPCoordLst = 0.D0

          DO intElmID = 1, numel
            elmID = GetElmExtID (intElmID)
            nIP   = GetElmIPCount (elmID)
            DO j = 1, nIP
              IPCoordLst(intElmID,j,:) = GetIPCoord (elmID, j)
            END DO
          END DO
          
          nIPCoordLst = numel
        END IF
        RETURN
      END SUBROUTINE MakeIPCoordLst      
!***************************************************************************************************
! SUBROUTINE MakeIPValLst
!
! Objective:
!   This subroutine makes the list of IP values for a selected list of element/ all elements of the
!   model.
!
! Input(s):
!   elmID          INTEGER       element user ID
!   IP             INTEGER       integration point
!   elmCode        INTEGER       element post code
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   nIntBmx        INTEGER      max. no. of IPs (dimen)
!   numel          INTEGER      number of elements in model (dimen)
!   intElmID       INTEGER      internal element ID
!   elmID          INTEGER      element user ID
!   IP             INTEGER      integration point
!   i              INTEGER      loop counter
!   nIP            INTEGER      number of integeration points
!
! Required common-blocks:
!   dimen.cmn
!
! Required subprogram(s):
!   none
!
! Required module variable(s):
!   none
!
! Restrictions:
!   none
!
! Last update: 3/4/2016
!*************************************************************************************************** 
      SUBROUTINE MakeIPValLst (elmCode, IPValLst, nIPValLst, 
     &                             elmIDLst, nElmIDLst)
     
        INTEGER, INTENT(IN) :: elmCode
        REAL*8, ALLOCATABLE, DIMENSION(:,:), INTENT(OUT) :: IPValLst
        INTEGER, INTENT(OUT) :: nIPValLst
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: elmIDLst
        INTEGER, INTENT(IN), OPTIONAL :: nElmIDLst

        INTEGER ::  elmID, intElmID, i, nIP, IP
        
        IF (Present(elmIDLst) .AND. Present(nElmIDLst)) THEN
          ALLOCATE (IPValLst(nElmIDLst,nintbmx))
          IPValLst = 0.D0
          
          DO i = 1, nElmIDLst
            elmID = elmIDLst (i)
            nIP   = GetElmIPCount (elmID)
            DO IP = 1, nIP
              IPValLst(i, IP) = GetIPVal (elmID, IP, elmCode)
            END DO
          END DO
          
          nIPValLst = nElmIDLst
        ELSE
          ALLOCATE (IPValLst(numel,nintbmx))
          IPValLst = 0.D0

          DO intElmID = 1, numel
            elmID = GetElmExtID (intElmID)
            nIP   = GetElmIPCount (elmID)
            DO IP = 1, nIP
              IPValLst(intElmID, IP) = GetIPVal (elmID, IP, elmCode)
            END DO
          END DO
          
          nIPValLst = numel
        END IF
        RETURN
      END SUBROUTINE MakeIPValLst      
!***************************************************************************************************
! SUBROUTINE MakeNodValIPLst 
!
! Objective:
!   This subroutine makes a list of nodal values transferred to the nodes. If no nodes are 
!   specified, this is done for all of the nodes in the model.
!
! Input(s):
!   nodCode      INTEGER             nodal post code
!                                    Example node post codes:
!                                       0 Coordinates
!                                       1 Displacement
!                                       2 Rotation
!                                       3 External Force
!                                       4 External Moment
!                                       5 Reaction Force
!                                       6 Reaction Moment
!                                      79 Total displacement
!   nodLst       INTEGER(*)          a list of node IDs (optional)
!   nNodLst      INTEGER             number of nodes in the nodLst (optional)
!
! Output(s):
!   valLst       REAL*8(nValLst,nComp)          coordinates of the nodID
!   nValLst      INTEGER                        number of nodes returned (=nNodLst or nnumnp)
!   nComp        INTEGER                        number of returned components per node
!
! Auxiliary variable(s):
!   ou           INTEGER             used output for printing
!   nodID        INTEGER             node user ID
!   intNodID     INTEGER             internal element ID
!   numnp        INTEGER             number of total nodes in the model (dimen.cmn)
!   tempVallst   REAL*8(:)           temporary value list for each node
!   nTempValLst  INTEGER             number of values in the temp list
!   i            INTEGER             loop counter


!   nComp        INTEGER             number of returned components
!   dataType     INTEGER             the returned data type
!   nodCoord     REAL*8(3)           coordinates of the nodID
!   nodDisp      REAL*8(3)           displacement of the nodID
!   i            INTEGER             loop counter
!   coordState   INTEGER             holds the same state as nodState 
!   inc          INTEGER             increment number (concom.cmn)
!
! Required common-blocks:
!   dimen.cmn
!
! Internal subprogram(s):
!   none
!
! Required subprogram(s):
!   ExtractNodCloseIPLst
!
! Restrictions:
!   none
!
! Future Updates:
!   - check if node exist in the model.
!   - Algorithm for the shortest path to use for a node-path instead of a range of nodes e.g. in CollectNodValLst
!
! Last update: 2/4/2016
!***************************************************************************************************      
      SUBROUTINE MakeNodValIPLst (elmCode, calcMeth, 
     &   nodValIPLst, nNodValIPLst, nodLst, nNodLst)
     
        INTEGER, INTENT(IN) :: calcMeth, elmCode
        INTEGER, DIMENSION(*), INTENT(IN), OPTIONAL :: nodLst
        INTEGER, INTENT(IN), OPTIONAL :: nNodLst
        REAL*8, ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: nodValIPLst
        INTEGER, INTENT(OUT) :: nNodValIPLst
        
        INTEGER ::  nodID, intNodID, nTempValLst, i
        REAL*8, ALLOCATABLE, DIMENSION(:) :: tempValLst
        
        IF (Present(nodLst) .AND. Present(nNodLst)) THEN
          ALLOCATE (nodValIPLst(nNodLst))
          DO i = 1, nNodLst
            nodID = nodLst(i)
            nodValIPLst(i) = GetNodIPVal (nodID, elmCode, calcMeth)
          END DO
          nNodValIPLst = nNodLst
        ELSE
          ALLOCATE (nodValIPLst(numnp))
          DO intNodID = 1, numnp
            nodID = GetNodExtID (intNodID)
            nodValIPLst(intNodID)=GetNodIPVal (nodID, elmCode, calcMeth)
          END DO
          nNodValIPLst = numnp
        END IF
        RETURN
        
      END SUBROUTINE MakeNodValIPLst
      
      END MODULE MarcTools  
      
      
      
      
      
      
      
      
      
!***************************************************************************************************      
! 28/03/16   The following lines are the old subprograms used to calculate nodal stress. They are 
! not used anymore and instead updated to more general ones.            
!***************************************************************************************************      
!      
!      
!
!
!       REAL*8 FUNCTION CalcStressNode (nodeNum)
!         INTEGER, INTENT(IN) :: nodeNum
!         
!         INTEGER, PARAMETER :: MAXNEI = 10
!         INTEGER :: nCount, i, GPStress(MAXNEI)
!         INTEGER, ALLOCATABLE ::  elList(:), GPList(:)
!         REAL*8 :: curStress, totStress
!         
!         totStress = 0.D0
!         CALL ExtractNodCloseIPLst (nodeNum, nCount, elList, GPList)
!! DEBUGGING
!!        WRITE (6,*) 'nodeNum :', nodeNum
!!        WRITE (6,*) 'nCount  :', nCount
!!        WRITE (6,*) 'elList  :', elList
!!        WRITE (6,*) 'GPList  :'
!!        WRITE (6,*) ''
!         
!         DO i = 1, nCount
!           CALL elmvar (17, elList(i), GPList(i), 0, curStress)
!           totStress = totStress + curStress
!         END DO
!         CalcStressNode = totStress / nCount
!       END FUNCTION CalcStressNode
!***************************************************************************************************
! CalcExStressNode:    Calculate the extrapolated von Mises stress of a node 
!***************************************************************************************************
!***************************************************************************************************
!       REAL*8 FUNCTION CalcExStressNode (nodeNum)
!         INTEGER, INTENT(IN) :: nodeNum
!!***************************************************************************************************       
!! curEl: current element
!! curNo: current node
!! curClGP: current Closest GP
!! curAve: 
!         INTEGER, PARAMETER :: MAXNEI = 10
!         INTEGER :: nCount, i, j, GPStress(MAXNEI)
!         INTEGER, ALLOCATABLE ::  elList(:), GPList(:)
!         REAL*8 :: curStress, totStress
!         
!         INTEGER :: curEl, curNo, curClGP 
!!***************************************************************************************************
!         totStress = 0.D0
!
!         CALL ExtractNodCloseIPLst (nodeNum, nCount, elList, GPList)
!         
!! DEBUGGING
!!        WRITE (6,*) 'nodeNum :', nodeNum
!!        WRITE (6,*) 'nCount  :', nCount
!!        WRITE (6,*) 'elList  :', elList
!!        WRITE (6,*) 'GPList  :'
!!        WRITE (6,*) ''
!! nCount: neighbor count
!!
!         DO i = 1, nCount
!           curEl = elList(i)
!           curClGP = GPList(i)
!!
!! It is required to calculate the extrapolated stress from 
!! each one of the neighbors (total neighbor number is nCount)
!! and add them toghether. 
!!
!           CALL SingleNodeExStress (nodeNum, curEl, curClGP, curStress)
!           
!           totStress = totStress + curStress
!         END DO
!         CalcExStressNode = totStress / nCount
!       END FUNCTION CalcExStressNode
!***************************************************************************************************
!***************************************************************************************************
! SUBROUTINE SingleNodeExStress
! 
! Objective:
!   This subroutine extracts the extrapolated stress of the IP point (IP) to the node (nodID) but 
! does not accounts for the stress from the neighboring elements. Note that this subroutine does not
! cover the cases for reduced integration points or if the number of nodes are less than that of the
! IPs.
!
! Input(s):
!   nodID     INTEGER         external node ID
!   elmID     INTEGER         external element ID
!   IP        INTEGER         IP number
!
! Output(s):
!   exStress  REAL*8          extrapolated von Mises stress
!
! Auxiliary variable(s):
!
! aNodeList: is the nodes close to the integration point 
!            e.g. aNodeList(3) is the node closest to the
!            integration point number 3
!
! aIPStress: is the corresponding stress of each integration point
!
!   nNode     INTEGER         number of nodes per element (lass common-block)
!   intEl     INTEGER         number of integration points per element (lass common-block)
!   i         INTEGER         loop counter
!   j         INTEGER         loop counter
!   nodLst    INTEGER(:)      list of nodes attached to the element(elmID)
!   nLst      INTEGER         number of nodes in nodLst
!   intElNum  INTEGER         internal element number
!   num       INTEGER         number of nodes
!   tempList  INTEGER(MAXN)   temporary list of 
!
! Required common-blocks:
!   lass
!
! Required subprogram(s):
!   GetElmExtID 
!   Elnodes
!
! Required module variable(s):
!   MAXN
!
! Last update: 14/3/2016
!*************************************************************************************************** 
!     SUBROUTINE SingleNodeExStress (nodID, elmID, IP, exStress)
!       INTEGER, INTENT(IN) :: nodID, elmID, IP
!       REAL*8 , INTENT(OUT):: exStress
!
!       INTEGER :: i, j, nLst
!       INTEGER, DIMENSION(:), ALLOCATABLE  :: nodLst
!       REAL*8 , DIMENSION(:), ALLOCATABLE  :: IPStressLst
!       
!       REAL*8, DIMENSION(10) :: test
!
!       WRITE (6,*) 'nnode=', nnode
!       WRITE (6,*) 'intel=', intel
!       
!       ALLOCATE (IPStressLst(nnode))
!       IPStressLst = 0.D0
!
!       DO i = 1, intel
!         CALL elmvar (17, elmID, i, 0, IPStressLst(i))
!         CALL elmvar (17, elmID, i, 0, test(1))
!         WRITE (6,*) 'test ', test
!       END DO
!       WRITE (6,*) 'here'
! Extract all the nodes of the element
!       CALL ExtractElmNodLst (elmID, nodLst, nLst)
!       IF (nLst .GT. 0) THEN
!         WRITE (6,*) 'nodlst:', nodLst
!         WRITE (6,*) 'nlst:  ', nLst
!       ELSE
!         CALL QUIT(1234)
!       END IF
!     END SUBROUTINE SingleNodeExStress
!
!     
!     
!
!
!   Old structure of the function      
!
!          LOGICAL FUNCTION CheckInSet (setName, memberNumber)
!            CHARACTER*32, INTENT(IN) :: setName
!            INTEGER, INTENT(IN)            :: memberNumber
!
!            INTEGER, PARAMETER :: maxMemberCount = 1000
!            INTEGER :: memberList(maxMemberCount), 
!     &                        isFound, setType, memberCount, i
!            
!            CALL marc_setinf (setName, isFound, memberList, setType, 
!     &     memberCount)
!            IF (isFound .EQ. 1) THEN
!              WRITE(6,*) '** setName = ', setName, ' is found.'
!              WRITE(6,*) '** List = ', (memberList(i), i=1, memberCount)
!              DO i = 1 ,memberCount
!                IF (memberNumber .EQ. memberList(i)) THEN
!                  WRITE(6,*) '** Member = ', memberNumber, 'is found.'
!                  CheckInSet = .TRUE.
!                  RETURN
!                END IF
!              END DO         
!            ELSE
!              WRITE(6,*) '** setName = ', setName, ' was not found.'
!              CheckInSet = .FALSE.
!              RETURN
!            END IF
!            CheckInSet = .FALSE.
!            WRITE(6,*) '** Member = ', memberNumber, ' not found.'
!            RETURN
!          END FUNCTION CheckInSet
!      END MODULE ElementTools
        
      