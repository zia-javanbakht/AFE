!***************************************************************************************************
! Personalized Miscellaneous Tools 
!
! v2.02 03/03/16
! Number of routines 2
! Revision #02 
!
! v3.01 29/01/16
! - GetIndex FUNCTION added
! v3.02 
! - GetRandNum is completely changed; now more efficient
!
! v10.01
! - Comments added
! v10.01
! - ExtractIntersectLst added
!***************************************************************************************************
      MODULE MiscTools
 
        IMPLICIT NONE
       
      CONTAINS
!***************************************************************************************************
! List of subroutines:
!   
!   1 DelRepeated
!   2 DelRepeated2D
!   3 ExtractIntersectLst 
!   4 GetDistance
!   5 GetIndex 
!   6 GetRandNum
!   7 GetRepCount
!   8 PrintElapsedTime
!   9 PutSmallFirst
!  10 SwapInt
!  11 SwapReal
!
!***************************************************************************************************
!***************************************************************************************************
! SUBROUTINE ExtractIntersectLst
!
! Objective:
!   Returns the list of intersecting items for two arrays.
!
! Input(s):
!   itemLst1        INTEGER(*)       first list of items
!   nItemLst1       INTEGER          number of the items in the first list
!   itemLst2        INTEGER(*)       second list of items
!   nItemLst2       INTEGER          number of the items in the second list

!
! Output(s):
!   interLst        INTEGER(:)       the list of intesecting items 
!   nInterLst       INTEGER(:)       number of intersecting items
!
! Auxiliary variable(s):
!   i            INTEGER             loop counter
!   tLst         INTEGER(:)          temporary list 
!   nLst         INTEGER             number of items in tLst
!   lstMask      INTEGER(:)          a list containg the mask values
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
! Last update: 31/3/2016
!***************************************************************************************************
      SUBROUTINE ExtractIntersectLst (itemLst1, nItemLst1, 
     & itemLst2, nItemLst2, interlst, nInterLst)
     
        INTEGER, DIMENSION(*), INTENT(IN) :: itemLst1, itemLst2
        INTEGER, INTENT(IN) :: nItemLst1, nItemLst2
        INTEGER, ALLOCATABLE, INTENT(OUT) :: interLst(:)
        INTEGER, INTENT(OUT) :: nInterLst

        INTEGER :: i, nLst
        LOGICAL, ALLOCATABLE, DIMENSION(:) :: lstMask
        INTEGER, ALLOCATABLE, DIMENSION(:) :: tLst
        
        ALLOCATE (lstMask(nItemLst1))
        
        DO i = 1, nItemLst1
          lstMask(i) = ANY (itemLst1(i) .EQ. itemLst2(:nItemLst2))
        END DO
        
        nLst = Count(lstMask)
        
        IF (nLst .NE. 0) THEN
          ALLOCATE (tLst, SOURCE = Pack (itemLst1(1:nItemLst1),
     &              MASK=lstMask))
          CALL DelRepeated (tLst, nLst, interLst, nInterLst)
        ELSE
          nInterLst = nLst
        END IF  
      
      END SUBROUTINE ExtractIntersectLst
!***************************************************************************************************
! SUBROUTINE DelRepeated
!
! Objective:
!   This subroutine removes all the recurring items of a list but preserves one instance.
!
! Input(s):
!   itemLst        INTEGER(*)   list of items to be searched
!   nItemLst       INTEGER      number of items in the list
!
! Output(s):
!   outItemLst     INTEGER(:)   the output item list 
!   nItemLst       INTEGER      number of items in the output list 
!
! Auxiliary variable(s):
!   lstMask        INTEGER(:)   a list containg the mask values
!   lstIndex       INTEGER(:)   a list containg the indices of another list
!   i              INTEGER      loop counter
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
! Last update: 31/3/2016
!*************************************************************************************************** 
      SUBROUTINE DelRepeated(itemLst, nItemLst, outItemLst, nOutItemLst)
        INTEGER, DIMENSION(*), INTENT(IN) :: itemLst
        INTEGER, INTENT(IN)               :: nItemLst
        INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: outItemLst
        INTEGER, INTENT(OUT) :: nOutItemLst

        LOGICAL, ALLOCATABLE, DIMENSION(:) :: lstMask
        INTEGER, ALLOCATABLE, DIMENSION(:) :: lstIndex
        INTEGER :: i

        ALLOCATE (lstMask(nItemLst))
        lstMask = .TRUE.

        DO i = nItemLst, 2, -1
          lstMask(i) = .NOT. (ANY (itemLst(:i-1) .EQ. itemLst(i)))
        END DO

        ALLOCATE(lstIndex, source = PACK([(i, i=1, nItemLst)], lstMask))
        ALLOCATE(outItemLst, source = itemLst(lstIndex))
        nOutItemLst = Size (outItemLst)
      END SUBROUTINE DelRepeated
!***************************************************************************************************
! SUBROUTINE DelRepeated2D
!
! Objective:
!   This subroutine removes all the recurring pairs in a list but preserves one pair.
!
! Input(s):
!   itemLst        INTEGER(2,*) list of items to be searched
!   nItemLst       INTEGER      number of items in the list
!
! Output(s):
!   outItemLst     INTEGER(:,:)   the output item list 
!   nItemLst       INTEGER      number of items in the output list 

!
! Auxiliary variable(s):
!   lstMask        INTEGER(:)   a list containg the mask values
!   lstIndex       INTEGER(:)   a list containg the indices of another list
!   i              INTEGER      loop counter
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
! Pending Updates:
!   Covering every array type and not only those with the (2,*) dimension.
!
! Last update: 31/3/2016
!*************************************************************************************************** 
      SUBROUTINE DelRepeated2D (itemLst, nItemLst, 
     &                          outItemLst, nOutItemLst)

        INTEGER, DIMENSION(2,*), INTENT(IN) :: itemLst
        INTEGER, INTENT(IN) :: nItemLst
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: outItemLst
        INTEGER, INTENT(OUT) :: nOutItemLst

        LOGICAL, ALLOCATABLE, DIMENSION(:) :: lstMask
        INTEGER, ALLOCATABLE, DIMENSION(:) :: lstIndex
        INTEGER :: i

        ALLOCATE (lstMask(nItemLst))
        lstMask = .TRUE.

        DO i = nItemLst, 2, -1
          lstMask(i) = .NOT.(ANY (itemLst(1,:i-1) .EQ. itemLst(1,i) 
     &                 .AND. itemLst(2,:i-1) .EQ. itemLst(2,i)))
        END DO

        ALLOCATE(lstIndex, source=PACK([(i, i=1, nItemLst)], lstMask))

        ALLOCATE(outItemLst, source=itemLst(:,lstIndex))
        nOutItemLst = Size (outItemLst, 2)
        
      END SUBROUTINE DelRepeated2D
!***************************************************************************************************
! FUNCTION GetDistance
!
! Objective:
!   This function returns the distance between two coordinates.
!
! Input(s):
!   pointA         INTEGER      coordinates of point A
!   pointB         INTEGER      coordinates of point B
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   none
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
! Last update: 28/3/2016
!*************************************************************************************************** 
      FUNCTION GetDistance (pointA, pointB)
        REAL*8, DIMENSION(3), INTENT(IN) :: pointA, pointB
        REAL*8 :: GetDistance
           
        GetDistance = SQRT ((pointA(1)-pointB(1))**2.D0 + 
     &                      (pointA(2)-pointB(2))**2.D0 +
     &                      (pointA(3)-pointB(3))**2.D0)
      END FUNCTION GetDistance 
!***************************************************************************************************
! FUNCTION GetIndex
!
! Objective:
!   This function returns the index of the searchItem in the itemLst. If the search was unsuccessful
! a zero will be returned.
!
! Input(s):
!   itemLst        INTEGER      list of items to be searched
!   nItemLst       INTEGER      number of items in the list
!   searchItem     INTEGER      the item to be searched in the itemLst
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   itemLst        INTEGER      list of items to be searched
!   nItemLst       INTEGER      number of items in the list
!   searchItem     INTEGER      the item to be searched in the itemLst
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
! Last update: 28/3/2016
!***************************************************************************************************
      FUNCTION GetIndex (itemLst, nItemLst, searchItem)
        INTEGER, INTENT(IN), DIMENSION(*) :: itemLst
        INTEGER, INTENT(IN) :: nItemLst, searchItem
        INTEGER :: GetIndex
       
        INTEGER :: i
       
        GetIndex = 0

        DO i = 1, nItemLst
          IF (itemLst(i) .EQ. searchItem) THEN
            GetIndex = i
            EXIT
          END IF
        END DO
      END FUNCTION GetIndex
!***************************************************************************************************
! FUNCTION GetRandNum
!
! Objective:
!   Returns a random number in the [-1,+1] range.
!
! Input(s):
!   none
!
! Output(s):
!                REAL*8(3)           a random number in the [-1,+1] range
!
! Auxiliary variable(s):
!   randNum1     REAL*8(3)           temporary number 1
!   randNum2     REAL*8(3)           temporary number 2
!   firstRun     LOGICAL             indicates the first run
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
! Last update: 23/3/2016
!
! Old listings:       
!
!       REAL*8 FUNCTION GetRandNum ()
!         REAL*8 :: tempNum
!         REAL*8, SAVE :: lastNum
!         REAL :: time
!         INTEGER :: i, signSelect = -1
!         
!         CALL random_seed ()
!         CALL random_number (tempNum)
!         CALL cpu_time (time)
!         tempNum = tempNum * time
!         IF (tempNum .GT. lastNum) signSelect = +1
!         
!         
!         DO WHILE (tempNum .LT. 1.0D0)
!           tempNum = tempNum * 10.0D0 
!           signSelect = -1*signSelect
!         END DO
!         lastNum = tempNum
!         GetRandNum = signSelect*tempNum / 10.0D0
!       END FUNCTION GetRandNum
!***************************************************************************************************
      FUNCTION GetRandNum ()
        REAL*8 :: GetRandNum
        
        REAL*8 :: randNum1, randNum2
        LOGICAL :: firstRun = .TRUE.
        
        IF (firstRun .EQV. .TRUE.) THEN
          firstRun = .FALSE.
          CALL Random_Seed ()
        END IF
        
        CALL Random_Number (randNum1)
        CALL Random_Number (randNum2)
        
        GetRandNum = randNum1 - randNum2
      END FUNCTION GetRandNum
!***************************************************************************************************
! FUNCTION GetRepCount
!
! Objective:
!   This function returns the number of repetitions of an item in an array.
!
! Input(s):
!   itemLst        INTEGER(*)   list of items to be searched
!   nItemLst       INTEGER      number of items in the list
!   item           INTEGER      the item to be searched in itemLst
!
! Output(s):
!                  INTEGER      number of repetitions of item in itemLst
!
! Auxiliary variable(s):
!   lstMask        INTEGER(:)   a list containg the mask values
!   i              INTEGER      loop counter
!   searchItem     INTEGER      the item to be searched in the itemLst
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
! Last update: 31/3/2016
!*************************************************************************************************** 
      FUNCTION GetRepCount (itemLst, nItemLst, searchItem)
        INTEGER, DIMENSION (*), INTENT(IN) :: itemLst
        INTEGER, INTENT(IN) :: nItemLst
        INTEGER, INTENT(IN) :: searchItem
        INTEGER :: GetRepCount
        
        INTEGER :: i
        LOGICAL, ALLOCATABLE :: lstMask(:)
        
        ALLOCATE (lstMask(nItemLst))
        
        DO i = 1, nItemLst
          lstMask(i) =  searchItem .EQ. itemLst(i)
        END DO
        
        GetRepCount = COUNT (lstMask)
      END FUNCTION GetRepCount      
!***************************************************************************************************
! SUBROUTINE PrintElapsedTime
!
! Objective:
!   This subroutine prints the elapsed time between its two executions.
!
! Input(s):
!   outUnit        INTEGER(OPTIONAL)   indicates the output unit (optional)
!                                      default = 6
!
! Output(s):
!   none
!
! Auxiliary variable(s):
!   ou             INTEGER             currently used output unit
!   firstRun       LOGICAL             its value is .TRUE. in the first run
!   startTime      REAL                start time for measuring
!   stopTime       REAL                stop time of the measuring
!   elapsedTime    REAL                elapsed time between measurements
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
! Pending Updates:
!   none
!
! Last update: 28/3/2016
!***************************************************************************************************
      SUBROUTINE PrintElapsedTime (outUnit)
        INTEGER, INTENT(IN), OPTIONAL :: outUnit

        INTEGER :: ou        
        REAL :: startTime, stopTime, elapsedTime
        SAVE :: startTime
        LOGICAL, SAVE :: firstRun = .TRUE.

100     FORMAT (A22, X, F8.3, X,'seconds')
        
        IF (Present (outUnit)) THEN
          ou = outUnit
        ELSE
          ou = 6
        END IF
        
        IF (firstRun .EQV. .TRUE.) THEN
          firstRun = .FALSE.
          CALL cpu_time (startTime)
          WRITE (ou, '(A22)') 'Stopwatch started...'
        ELSE
          CALL cpu_time (stopTime)
          elapsedTime = stopTime - startTime
          WRITE (ou, 100) 'Elapsed time:', elapsedTime
          firstRun = .TRUE.
        END IF
      END SUBROUTINE PrintElapsedTime
!***************************************************************************************************
! SUBROUTINE PutSmallFirst
!
! Objective:
!   This subroutine compares the pairs of array, and moves the smaller component to the first
! position of the array.
!
! Input(s):
!   itemLst        INTEGER(2,*) list of items to be searched
!   nItemLst       INTEGER      number of items in the list
!
! Output(s):
!   itemLst        INTEGER(2,*) list of items to be searched
!
! Auxiliary variable(s):
!   i              INTEGER      loop counter
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
! Pending Updates:
!   none
!
! Last update: 28/3/2016
!***************************************************************************************************       
      SUBROUTINE PutSmallFirst (itemLst, nItemLst)
        INTEGER, DIMENSION(2,*), INTENT(INOUT) :: itemLst
        INTEGER, INTENT(IN) :: nItemLst
        
        INTEGER :: i
        
        DO i = 1, nItemLst
          IF (itemLst(1,i) .GT. itemLst(2,i)) THEN
           CALL SwapInt (itemLst(1,i), itemLst(2,i))
          END IF
        END DO
      END SUBROUTINE PutSmallFirst
!***************************************************************************************************
! SUBROUTINE SwapInt
!
! Objective:
!   This subroutine swaps the value of two integer numbers.
!
! Input(s):
!   num1           INTEGER      first number
!   num2           INTEGER      second number
!
! Output(s):
!   num1           INTEGER      second number
!   num2           INTEGER      first number
!
! Auxiliary variable(s):
!   tNum           INTEGER      temporary number
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
! Pending Updates:
!   none
!
! Last update: 28/3/2016
!***************************************************************************************************  
      SUBROUTINE SwapInt (num1, num2)
        INTEGER, INTENT(INOUT) :: num1, num2 
        
        INTEGER :: tNum
        
        tNum = num1
        num1 = num2
        num2 = tNum
      END SUBROUTINE SwapInt
!***************************************************************************************************
! SUBROUTINE SwapReal
!
! Objective:
!   This subroutine swaps two double precision real numbers.
!
! Input(s):
!   num1           INTEGER      first number
!   num2           INTEGER      second number
!
! Output(s):
!   num1           INTEGER      second number
!   num2           INTEGER      first number
!
! Auxiliary variable(s):
!   tNum           INTEGER      temporary number
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
! Pending Updates:
!   none
!
! Last update: 28/3/2016
!***************************************************************************************************  
      SUBROUTINE SwapReal (num1, num2)
        REAL*8, INTENT(INOUT) :: num1, num2 
        
        REAL*8 :: tNum
        
        tNum = num1
        num1 = num2
        num2 = tNum
      END SUBROUTINE SwapReal


















      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      END MODULE MiscTools
!***************************************************************************************************
! The old codes from previous versions:
!
!       
! it    item
! ls    list
! lss   list size
!
! The function searches ls of size lss for the it
!
!     INTEGER FUNCTION GetIndex (it, ls, lss)
!       REAL*8, INTENT(IN)   :: it
!       REAL*8, DIMENSION(*), INTENT(IN) :: ls
!       INTEGER, INTENT(IN) :: lss
!
!       INTEGER :: i
!
!       GetIndex = 0
!       DO i = 1, lss
!         IF (ls(i) .EQ. it) GetIndex = i
!       END DO
!
!     END FUNCTION GetIndex
!
! Quick sorts in ascending order the (ls) list of size (lss)
!             
!     SUBROUTINE QuickSort (ls, lss)
!       REAL*8, DIMENSION(*), INTENT(INOUT) :: ls
!       INTEGER, INTENT(IN)   :: lss
!       
!        
!     END SUBROUTINE QuickSort
!        
!***************************************************************************************************