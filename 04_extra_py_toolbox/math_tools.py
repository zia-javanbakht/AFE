# MathTools MODULE v19.07.11

# NOTES
# The functions are written assuming 3D coordinates. However, when possible a 2D version is added for more flexibility.
# Nevertheless, some functions are inherently 2D such as those which deal with planar boxes (rectangles).
#
# The order of naming the 2D functions are point<seg<box and it is preserved in argument ordering, e.g., FindPointInBox (aPoint, aBox)
#
# UPDATE LOG
# 08/07/2019		test against 0.0 was changed to ZERO in IsPointInBetween() but remaind in IsPointOnLine()
# 11/07/2019		GetSegBoxState is completely rewritten to consider the exceptional cases (through vertex random points). State IV in which both of the points are out of the box is not written; it is not required yet; see NOTES_Logic structuring.pdf for details.
# 16/07/2019
#

# FUTURE UPDATES
# > Move to numpy and remove my own functions

# USED MODULES

import sys
import math
import numpy as np
import random
from scipy.linalg import eigh

# GLOBAL VARIABLES

PI = math.atan(1.) * 4.
ZERO = 1.e-10

# In order to consider the points close to the boundary as points on the
# boundary
SEG_TOL = 0.01


# FUNCTIONS

def weib2(x, a, b):
    """
            Two parameter Weibull PDF (probability distribution function):
                    a	scaling parameter
                    b	shape parameter
    """
    temp = (b / a) * ((x / a)**(b - 1)) * np.exp(-(x / a)**b)
    return temp


def lognormal(x, mu, sigma):
    '''
x         ia a numpy array or a float
mu        mean
sigma     std

Note the beauty of using numpy array is that you can write methods for ndarrays exactly as if it is a real object whereas for a list you cannot.
    '''
    pdf = 1 / (x * np.sqrt(2 * np.pi) * sigma) * \
        np.exp(-0.5 * ((np.log(x) - mu) / sigma)**2.)
    return pdf

# *************************************************************************************************
# MakePoint3D ()
#   Checks if a point has three dimensions; if not, a third dimension with a zero value is added
# *************************************************************************************************


def MakePoint3D(aPoint):
    """Checks if a point has three dimensions; if not, a third dimension with a zero value is added."""
    if (len(aPoint) == 2):
        newPoint = [aPoint[0], aPoint[1], 0.]
    else:
        newPoint = aPoint
    return newPoint


def InterLinePlane(dirVec, arbPoint, attNum, interPoint):
    """Calculates intersection of line with plane using direction vector and attitude numbers."""
    temp = -1.0 * (attNum[1] * arbPoint[1] + attNum[2] * arbPoint[2] + attNum[3] * arbPoint[3] +
                   attNum[4]) / (attNum[1] * dirVec[1] + attNum[2] * dirVec[2] + attNum[3] * dirVec[3])
    interPoint = [
        arbPoint(1) +
        temp *
        dirVec(1),
        arbPoint(2) +
        temp *
        dirVec(2),
        arbPoint(3) +
        temp *
        dirVec(3)]
    return


def InterSegPlane(pointA, pointB, attNum, interPoint):
    """Calculates intersection of a line segment with a plane.
    
    Args:
        pointA: First point of the segment
        pointB: Second point of the segment
        attNum: Attitude numbers of the plane [A, B, C, D] for Ax+By+Cz+D=0
        interPoint: Intersection point (output parameter)
    
    Returns:
        bool: True if intersection point is between pointA and pointB
    """
    tDirVec = pointB - pointA
    InterLinePlane(tDirVec, pointA, attNum, interPoint)
    return IsPointInBetween(pointA, interPoint, pointB)

# Check if pointB is in between A and C


def IsPointInBetween(pointA, pointB, pointC):
    """Checks if pointB is between pointA and pointC on a line.
    
    Args:
        pointA: First point
        pointB: Point to test
        pointC: Third point
    
    Returns:
        bool: True if pointB lies between pointA and pointC
    """
    test = abs(
        GetDistance(
            pointA,
            pointB) +
        GetDistance(
            pointB,
            pointC) -
        GetDistance(
            pointA,
            pointC))
    # testing against zero is always troublesome so
    #    return (test == 0.)
    # is replaced by
    return abs(test) <= ZERO


def GetDistance(pointA, pointB):
    """Calculates the Euclidean distance between two points.
    
    Args:
        pointA: First point (list or tuple)
        pointB: Second point (list or tuple)
    
    Returns:
        float: Euclidean distance between the points
    """
    temp = 0.
    for i in range(len(pointA)):
        temp = temp + (pointA[i] - pointB[i])**2.
    return temp**0.5


def GetVecLen(aVec):
    """Calculates the length (magnitude) of a vector.
    
    Args:
        aVec: Vector as list or array
    
    Returns:
        float: Length of the vector
    """
    return np.linalg.norm(np.array(aVec))
#
#  xSum = 0.E0
#  for x in aVec:
#    xSum += x**2.
#  return (xSum)**0.5


def GetCross(vec1, vec2):
    """Calculates the cross product of two vectors.
    
    Args:
        vec1: First vector
        vec2: Second vector
    
    Returns:
        list: Cross product as a list
    """
    return np.cross(vec1, vec2).tolist()
# OBSOLETE
#  if len(vec1) == 2:
#    tVec1 = vec1 + [0]
#  else:
#    tVec1 = vec1
#
#  if len(vec2) == 2:
#    tVec2 = vec2 + [0]
#  else:
#    tVec2 = vec2
#
# return [tVec1[1]*tVec2[2] - tVec1[2]*tVec2[1], tVec1[2]*tVec2[0] -
# tVec1[0]*tVec2[2], tVec1[0]*tVec2[1] - tVec1[1]*tVec2[0]]

# Returns the angle between two vectors in degrees.


def GetVecAng(vec1, vec2):
    """Returns the angle between two vectors in degrees.
    
    Args:
        vec1: First vector
        vec2: Second vector
    
    Returns:
        float: Angle between vectors in degrees
    """
    return math.degrees(math.acos(GetDot(vec1, vec2) /
                        (GetVecLen(vec1) * GetVecLen(vec2))))

# Checks if two vectors are collinear


def AreVecCollinear(vec1, vec2):
    """Checks if two vectors are collinear.
    
    Args:
        vec1: First vector
        vec2: Second vector
    
    Returns:
        bool: True if vectors are collinear
    """
    return abs(math.radians(GetVecAng(vec1, vec2))) <= ZERO


def GetDet(mat):
    """Calculates the determinant of a 3x3 matrix.
    
    Args:
        mat: 3x3 matrix as list of lists
    
    Returns:
        float: Determinant of the matrix
    """
    return mat[0][0] * (mat[1][1] * mat[2][2] - mat[1][2] * mat[2][1]) - mat[0][1] * (mat[1][0] * \
                        mat[2][2] - mat[1][2] * mat[2][0]) + mat[0][2] * (mat[1][0] * mat[2][1] - mat[1][1] * mat[2][0])


def GetDot(vec1, vec2):
    """Calculates the dot product of two vectors.
    
    Args:
        vec1: First vector
        vec2: Second vector
    
    Returns:
        float: Dot product of the vectors
    """
    tSum = 0.
    for i in range(len(vec1)):
        tSum += vec1[i] * vec2[i]
    return tSum


def GetVecDir(point1, point2):
    """Calculates the unit direction vector from point1 to point2.
    
    Args:
        point1: Starting point
        point2: Ending point
    
    Returns:
        list: Unit direction vector
    """
    tLen = GetDistance(point1, point2)
    return [(y - x) / tLen for x, y in zip(point1, point2)]


def GetTriple(vec1, vec2, vec3):
    """Calculates the scalar triple product of three vectors.
    
    Args:
        vec1: First vector
        vec2: Second vector
        vec3: Third vector
    
    Returns:
        float: Scalar triple product vec1 · (vec2 × vec3)
    """
    return GetDot(vec1, GetCross(vec2, vec3))


def GetPolyArea(pointLst):
    """Calculates the area of a polygon defined by a list of 3D points.
    
    Args:
        pointLst: List of 3D points defining the polygon vertices
    
    Returns:
        float: Area of the polygon
    
    Raises:
        SystemExit: If less than 3 points are provided
    """
    nPointLst = len(pointLst)
    if nPointLst < 3:
        print("Error in GetPolyArea")
        sys.exit()

    total = [0., 0., 0.]

    for i in range(nPointLst):
        j = int((i + 1) % nPointLst)
        total = [
            x + y for x,
            y in zip(
                total,
                GetCross(
                    pointLst[i],
                    pointLst[j]))]

    normVec = GetPlaneNorm(pointLst[0:3])

    return abs(GetDot(total, normVec)) * 0.5

# Returns the attitude number for 3 non-coplanar points


def GetPlane(pointLst):
    """Returns the attitude numbers for 3 non-coplanar points defining a plane.
    
    Args:
        pointLst: List of at least 3 3D points
    
    Returns:
        list: Attitude numbers [a, b, c, d] for plane equation ax+by+cz+d=0
    
    Raises:
        SystemExit: If less than 3 points are provided
    """
    nPointLst = len(pointLst)
    if nPointLst < 3:
        print("Error in GetPolyArea")
        sys.exit()

    a = GetDet([[1., pointLst[0][1], pointLst[0][2]], [
               1., pointLst[1][1], pointLst[1][2]], [1., pointLst[2][1], pointLst[2][2]]])
    b = GetDet([[pointLst[0][0], 1., pointLst[0][2]], [
               pointLst[1][0], 1.0, pointLst[1][2]], [pointLst[2][0], 1., pointLst[2][2]]])
    c = GetDet([[pointLst[0][0], pointLst[0][1], 1.], [
               pointLst[1][0], pointLst[1][1], 1.], [pointLst[2][0], pointLst[2][1], 1.]])
    d = GetDet([[pointLst[0][0],
                 pointLst[0][1],
                 pointLst[0][2]],
                [pointLst[1][0],
                 pointLst[1][1],
                 pointLst[1][2]],
                [pointLst[2][0],
                 pointLst[2][1],
                 pointLst[2][2]]])

    return [a, b, c, d]


def GetPlaneNorm(pointLst):
    """Calculates the unit normal vector of a plane defined by points.
    
    Args:
        pointLst: List of at least 3 3D points defining the plane
    
    Returns:
        list: Unit normal vector of the plane
    
    Raises:
        SystemExit: If less than 3 points are provided
    """
    nPointLst = len(pointLst)
    if nPointLst < 3:
        print("Error in GetPolyArea")
        sys.exit()

    aPlane = GetPlane(pointLst)
    vecLen = GetVecLen(aPlane[0:4])
    tNorm = [x / vecLen for x in aPlane[0:4]]

    return tNorm


# ------------------------------
# 2D point/segment/box functions
# ------------------------------

def IsPointInBox(aPoint, aBox):
    """Returns True if the point is inside (not on the border) of the box.
    
    Args:
        aPoint: 2D point coordinates
        aBox: 2D bounding box as list of corner points
    
    Returns:
        bool: True if point is strictly inside the box
    """
# just to make sure the order of coordinates is correct
    x1 = min([item[0] for item in aBox])
    x2 = max([item[0] for item in aBox])
    y1 = min([item[1] for item in aBox])
    y2 = max([item[1] for item in aBox])

    return ((aPoint[0] < x2) and (aPoint[0] > x1)
            and (aPoint[1] < y2) and (aPoint[1] > y1))


def IsSegInBox(aLine, aBox):
    """Checks if both endpoints of a line segment are inside a 2D box.
    
    Args:
        aLine: Line segment defined by two endpoints
        aBox: 2D bounding box
    
    Returns:
        bool: True if both endpoints are inside the box
    """
    return ((IsPointInBox(aLine[0][0:2], aBox))
            and (IsPointInBox(aLine[1][0:2], aBox)))


def GetPointBoxState(pCoord, aBox):
    """Determines the state of a point relative to a 2D box.
    
    Possible states occurring between a box and a point in a 2D space:
        - error: 0
        - in box: 1 (excludes the points on the edge)
        - out of box: 2
        - on the border: 3
        - on a vertex: 4
    
    Args:
        pCoord: Point coordinates
        aBox: 2D bounding box
    
    Returns:
        int: State code (0-4) indicating point-box relationship
    """
    allSeg = GetBoxSeg(aBox)

    state = 0

    if IsPointInBox(pCoord[0:2], aBox):
        state = 1
    else:
        point_seg_state = [GetPointSegState(
            pCoord[0:2], aSeg) for aSeg in allSeg]
        if any(stat == 1 for stat in point_seg_state):
            state = 3
        elif any(stat == 4 for stat in point_seg_state):
            state = 4
        else:
            state = 2
    return state

# *******************************************************************
# This gets the minCorner and maxCorner of a box and
# return the segments making up its edges in the order
# of south, east, north, west edge.
#
# NOTE
#   A box is a 2D entity, and thus its segments are 2D.
# *******************************************************************


def GetBoxSeg(aBox):
    """Gets the segments making up the edges of a 2D box.
    
    Returns the segments in the order: south, east, north, west edge.
    The box is a 2D entity, and thus its segments are 2D.
    
    Args:
        aBox: 2D box defined by min and max corners
    
    Returns:
        list: List of four segments [southSeg, eastSeg, northSeg, westSeg]
    
    Raises:
        ValueError: If box corners have incompatible dimensions
    """
    minB, maxB = aBox
# check for incompatible dimensions
    if (len(minB) != len(maxB)):
        try:
            raise ValueError('Incompatible dimensions')
        except Exception as error:
            print('Error: ', repr(error))

# The work is done only in the XY-plane
#   Extract the segments of the box
    southSeg = [[minB[0], minB[1]], [maxB[0], minB[1]]]
    eastSeg = [[maxB[0], minB[1]], [maxB[0], maxB[1]]]
    northSeg = [[maxB[0], maxB[1]], [minB[0], maxB[1]]]
    westSeg = [[minB[0], maxB[1]], [minB[0], minB[1]]]

    return [southSeg, eastSeg, northSeg, westSeg]


def GetPointSegState(aPoint, aSeg):
    """Determines the state of a point relative to a line segment.
    
    Possible states between a line segment and a point:
        - error: 0
        - on the segment: 1
        - out of the segment and not along the line: 2
        - out of the segment but along the line: 3
        - on one of the end points of the segment: 4
    
    Args:
        aPoint: Point coordinates
        aSeg: Line segment defined by two points
    
    Returns:
        int: State code (0-4) indicating point-segment relationship
    """
    state = 0
    if IsPointOnLine(aPoint, aSeg):
        if IsPointInBetween(aSeg[0], aPoint, aSeg[1]):
            # this state includes the case in which points are on either ends
            if (aPoint == aSeg[0]) or (aPoint == aSeg[1]):
                state = 4
            else:
                state = 1
        else:
            state = 3
    else:
        state = 2

    return state

# ---------------------------------------------------
# Checks if a point is on a line.
# The line is defined by two of its points; a segment of it.
# http://www.lucidarme.me/?p=1952


def IsPointOnLine(aPoint, aSeg):
    """Checks if a point is on a line defined by two of its points (a segment).
    
    The line is defined by two of its points; a segment of it.
    Reference: http://www.lucidarme.me/?p=1952
    
    Args:
        aPoint: Point to test (2D or 3D)
        aSeg: Line segment defined by two points
    
    Returns:
        bool: True if the point lies on the line
    """
    # IsPointInBetween (aSeg[0], aPoint)
    # deal with 2D points
    aPnt = MakePoint3D(aPoint)
# There is a possibility of having a 2D seg from a box:
    if len(aSeg[0]) == 2:
        newSeg = [MakePoint3D(aSeg[0]), MakePoint3D(aSeg[1])]
    else:
        newSeg = aSeg

    v1 = [y - x for x, y in zip(newSeg[0], aPnt)]
    v2 = [y - x for x, y in zip(newSeg[0], newSeg[1])]

    # numpy procedure is replaced due to a bug
    # v1Crossv2 = GetCross(v1,v2)
    # replaced by numpy algorithm
    # return GetVecLen(v1Crossv2) == 0.
    return np.linalg.norm(np.cross(v1, v2)) == 0.0


def GetSegSegState(seg1, seg2):
    """Determines the geometric relationship between two 2D line segments.
    
    Currently designed for 2D space.
    
    Possible states for two segments in 2D:
        + Collinear:
            - overlapping: 1
            - one common point: 2  
            - disjoint: 3
        - Parallel: 4
        - One intersection point: 5
        - No intersection: 6
    
    Args:
        seg1: First line segment [point1, point2]
        seg2: Second line segment [point1, point2]
    
    Returns:
        list: State information where first element is state code (1-6),
              followed by relevant geometric data based on the state
    
    Note:
        It is very important to distinguish between integer values (1)
        and real values (1.0); the result can be disastrous.
    """

# 3D check
    # newSeg1 = [MakePoint3D(seg1[0]), MakePoint3D(seg1[1])]
    # newSeg2 = [MakePoint3D(seg2[0]), MakePoint3D(seg2[1])]
#

# -----
# NOTE
#   When this function is called from GetSegBoxState (aSeg, aBox),
#   seg1 is a segment from a fiberm and seg2 is a segment from a box.
#

    newSeg1 = seg1
    newSeg2 = seg2

    # print ('newseg1', seg1)
    # print ('newseg2', seg1)``

    state = []

    v1 = [x - y for x, y in zip(newSeg1[1], newSeg1[0])]
#  print 'v1=', v1
    v2 = [x - y for x, y in zip(newSeg2[1], newSeg2[0])]
#  print 'v2=', v2
    v1v2Cross = np.cross(v1, v2).tolist()
    v1v2Dot = GetDot(v1, v2)

    qp = [y - x for x, y in zip(newSeg1[0], newSeg2[0])]
#  print 'qp=', qp

    qpv1Cross = np.cross(qp, v1).tolist()

    if (GetVecLen(v1v2Cross) <= ZERO) & (GetVecLen(qpv1Cross) <= ZERO):
        #    print "collinear lines"
        try:
            t0 = GetDot(qp, v1) / GetDot(v1, v1)
        except ZeroDivisionError:
            print('*******************************')
            print('** Division by zero happened **')
            print('*******************************')
            print('v1 = ', v1)
            print('v2 = ', v2)
            print('seg1 =', seg1)
            print('seg2 =', seg2)
            # sys.exit(0)

        qps = [x + y for x, y in zip(qp, v2)]
#    print 'qps=, ', qps
        t1 = GetDot(qps, v1) / GetDot(v1, v1)
#    print 'qps.v1=, ', GetDot (qps, v1)
#    print 'qps.v1=, ', GetDot (qps, v1)

        if (v1v2Dot < 0.):
            #      print "v1, v2 in opposite directions"
            t0, t1 = t1, t0

#    print 't0 = ', t0
#   print 't1 = ', t1

# if not(((t0<=0.) & (t1<=0.)) | ((t1>=1) & (t0>=1))): # This lines was
# used earlier but the following if is enough
        if not ((t1 <= 0.) | (t0 >= 1)):
            #      print "segments overlap"
            if (t0 > 0) & (t1 < 1):
                #       Alpha-type overlap
                overlapLen = (t1 - t0) * GetVecLen(v1)
                overlapStart = [x + t0 * y for x, y in zip(newSeg1[0], v1)]
                overlapEnd = [x + t1 * y for x, y in zip(newSeg1[0], v1)]

                P1VecToOverLapStart = [t0 * x for x in v1]
                P1VecToOverLapEnd = [t1 * x for x in v1]
#        print 'alpha'
            elif ((t0 < 1) & (t0 > 0) & (t1 > 1)):
                #       Gamma-type overlap
                overlapLen = (1. - t0) * GetVecLen(v1)
                overlapStart = [x + t0 * y for x, y in zip(newSeg1[0], v1)]
                overlapEnd = [x + y for x, y in zip(newSeg1[0], v1)]

                P1VecToOverLapStart = [t0 * x for x in v1]
                P1VecToOverLapEnd = v1
#        print 'gamma'
            else:  # ( t1>0) & (t0<0):
                #       Beta-type overlap
                overlapLen = t1 * GetVecLen(v1)
                overlapStart = newSeg1[0]
                overlapEnd = [x + t1 * y for x, y in zip(newSeg1[0], v1)]

                P1VecToOverLapStart = [0, 0]
                P1VecToOverLapEnd = [t1 * x for x in v1]
#        print 'beta'
#      print "overlap length = ", overlapLen
            state.append(1)
            state.append(overlapStart)
            state.append(overlapEnd)

            state.append(P1VecToOverLapStart)
            state.append(P1VecToOverLapEnd)
#      print 'from state=', GetDistance(state[1],state[2])

        elif ((abs(t1) <= ZERO) | (abs(t0) - 1. <= ZERO)):
            #      print "two segments have only one common point"
            state.append(2)
            if (abs(t1) <= ZERO):
                state.append(newSeg1[0])
            else:
                state.append(newSeg1[1])
        else:
            #      print "segments are collinear but disjoint"
            state.append(3)
    elif (GetVecLen(v1v2Cross) <= ZERO) & (GetVecLen(qpv1Cross) > ZERO):
        #    print "parallel lines but not intersecting"
        state.append(4)
    elif not (GetVecLen(v1v2Cross) <= ZERO):
        #   The original formula always gives positive values if implemented as follows:
        #     tInter = GetVecLen (GetCross (qp, v2))/ GetVecLen (v1v2Cross)
        #     uInter = GetVecLen (GetCross (qp, v1))/ GetVecLen (v1v2Cross)
        # which creates a bug. Implementing with no GetVecLen resolves the
        # issue.

        # np.cross results in a scalar if the cross product becomes a number in
        # the case of colinear vectors
        qpv2Cross = np.cross(qp, v2).tolist()
        tInter = qpv2Cross / v1v2Cross
        uInter = qpv1Cross / v1v2Cross
# OBSOLETE LINES after changing to np.cross they did not work
#    tInter = qpv2Cross[2]/v1v2Cross[2]
#    uInter = qpv1Cross[2]/v1v2Cross[2]
        if ((tInter >= 0.) & (tInter <= 1.)) & (
                (uInter >= 0.) & (uInter <= 1.)):
            #      print "intersection at"

            xInter = [tInter * x + y for x, y in zip(v1, newSeg1[0])]
            xInter[0] = round(xInter[0], 3)
            xInter[1] = round(xInter[1], 3)
#      print xInter
            state.append(5)
            state.append(xInter)
        else:
            #      print 'Not parallel and not intersecting'
            state.append(6)
    return state


def GetSegBoxState(aSeg, aBox):
    """Analyzes the geometric relationship between a line segment and a 2D box.
    
    Returns detailed state information about how a line segment intersects
    or relates to a 2D bounding box. This is used for fiber analysis in RVE.
    
    State codes and returned data:
        1: Both endpoints inside box - Nil
        2: One endpoint inside, one on boundary - Nil
        3: Both endpoints on boundary (different edges) - Nil
        4: Both endpoints on same boundary - Nil
        5: Segment overlaps with box edge - Point1/Point2 On The Border + Separation Coordinate
        6: Segment completely outside box - Point is out of the box w/o any intersections
        7: One endpoint inside, one outside - Point1/Point2 Inside + Separation Coordinate + edge index
        8: Both endpoints outside, segment crosses box - Two Separation Coordinates
        9: One endpoint on boundary, one outside (one intersection) - Nil
        10: Reserved - Nil
        11: Point on boundary/vertex, other outside - Point is out of the box w/t intersections
    
    Box edge numbering:
        +--2--+
        |     |
        3     1  
        |     |
        +--0--+
    
    Box vertex numbering:
        3-----2
        |     |
        |     |
        0-----1
    
    Args:
        aSeg: Line segment defined by two endpoints
        aBox: 2D bounding box [[x_min, y_min], [x_max, y_max]]
    
    Returns:
        list: State information [state_code, ...additional_data]
              Additional data varies by state and may include intersection
              points and edge indices.
              
    Note:
        States 7 and 11 require additional data for MirrorPointInBox().
        Return formats:
        - going through segment: [state, intersection point, edgeindex]
        - going through vertex: [state, intersection point, [edgeindex1, edgeindex2]]
    """
    def handle_x():
        print('> GetSegBoxState(): the point is out of the box!')
        print('\taSeg = ', aSeg, '\n\taBox = ', aBox)
        print('\tp1BoxStat = ', p1_edge_stat, '\n\tp2BoxStat = ', p2_edge_stat)
        print('state = ', state)

    state = []
    minBox, maxBox = aBox

    # get the coordinates of the box vertices
    # vertex_lst = [minBox, [maxBox[0],minBox[1]], maxBox, [minBox[0],maxBox[1]]]

    allBoxSeg = GetBoxSeg(aBox)

    p1Coord = aSeg[0]
    p2Coord = aSeg[1]

    # get the state of the ends of the segment w/t respect to the box
    p1BoxStat = GetPointBoxState(p1Coord, aBox)
    p2BoxStat = GetPointBoxState(p2Coord, aBox)

    # The state of the points against the edges of the box is evaluated
    # NOTE the return value 2 and 3 both should be considered as "not on the
    # line"
    p1_edge_stat = [GetPointSegState(p1Coord, x) for x in allBoxSeg]
    p2_edge_stat = [GetPointSegState(p2Coord, x) for x in allBoxSeg]

    # get the state of the segment w/t respect to the edges of the box and
    # possible intersection point infor
    seg_edge_stat = [GetSegSegState(aSeg, x) for x in allBoxSeg]

    # this is the state of the segments without the extra information, e.g.,
    # the intersection points.
    only_seg_edge_stat = [x[0] for x in seg_edge_stat]

    # in the following nested-if, states are denoted by 'S' and if a point is on the vertex the state is exceptional is denoted by 'XC'.
    # The domain is decomposed as:
    # \omega+		points outside the box
    # \omega-		points inside the box
    # \gamma		points on the boundary
    # Thus, a point inside the box does not have any ends on the box segments.

    # I. first point is inside the box
    if (p1BoxStat == 1):

        # I1. second point inside the box -> S1
        if (p2BoxStat == 1):
            state.append(1)

        # I2. second point is on an edge -> S2
        # I3. or second point is on a vertex -> XS2
        elif (p2BoxStat >= 3):
            state.append(2)

        # I4. 2nd point is outside of the box -> S7 or XS7
        elif (p2BoxStat == 2):

            state.append(7)
            # I4a. 2nd point is outside of the box and passes through an edge
            # of the box -> 7
            if only_seg_edge_stat.count(5) == 1:
                # append the intersection point
                state.append(seg_edge_stat[only_seg_edge_stat.index(5)][1])
        # append the intersecting box edge
                state.append(only_seg_edge_stat.index(5))

        # OBSOLETE RETURN VALUES
                # return Point1 Inside + Separation Coordinate + index of the separating box segment
                # state.append (p1Coord)
        # append the intersection point
                # state.append (seg_edge_stat[only_seg_edge_stat.index (5)][1])
        # append the intersecting box edge
                # state.append (only_seg_edge_stat.index (5))

            # I4b. 2nd point is outside of the box and passes through a vertex of the box -> 7
            # In this case, two edges of the box will have only one
            # intersection with the segment.
            elif (only_seg_edge_stat.count(5) == 2):
                # append the intersection point
                state.append(seg_edge_stat[only_seg_edge_stat.index(5)][1])
        # append the intersecting box edge
                state.append([i for i, x in enumerate(
                    only_seg_edge_stat) if x == 5])

        # OBSOLETE RETURN VALUES
        # return Point1 Inside + Separation Coordinate + index of the separating box segment
                # state.append (p1Coord)
                # append the intersection point
                # state.append (seg_edge_stat[only_seg_edge_stat.index (5)][1])
        # append the intersecting box edge
                # state.append (only_seg_edge_stat.index (5))

        else:
            print(
                '> GetSegBoxState(): Error detecting the type of the second point at stage I!')
            print('\taSeg = ', aSeg, '\n\taBox = ', aBox)
            print(
                '\tp1BoxStat = ',
                p1_edge_stat,
                '\n\tp2BoxStat = ',
                p2_edge_stat)
            state.append(0)

    # II. first point is on an edge but not on a vertex
    elif (p1BoxStat == 3):

        # II1. second point inside the box -> S2
        if (p2BoxStat == 1):
            state.append(2)

        # II2. second point on an edge
        elif (p2BoxStat == 3):

            # get the index of the edge on which the second point resides
            # II2a. p2 is on the same edge is p1 -> S4
            if (p1_edge_stat.index(1) == p2_edge_stat.index(1)):
                state.append(4)

            # II2b. p2 is on another edge -> S3
            else:
                state.append(3)

        # II3. second point on a vertex
        elif (p2BoxStat == 4):

            # because the 2nd point is on a vertex, there will be two edges that include the vertex; if any of these edges is the same as the edge of the 1st point, they are on the same edge
            # extract the index of the edges where the second point is located
            # at their ends
            edges_index = [i for i, x in enumerate(p2_edge_stat) if x == 4]

            # II3a. the first and the second point are on the same edge -> XS4
            if (p1_edge_stat.index(1) == edges_index[0]) | (
                    p1_edge_stat.index(1) == edges_index[1]):
                state.append(4)
            # II3b. otherwise they are on different edges -> XS3
            else:
                state.append(3)
        # II4. second point is out of the box
        elif (p2BoxStat == 2):
            # get the number of intersection points
            intersect_count = only_seg_edge_stat.count(5)
            # II4d. one intersection -> 9
            if (intersect_count == 1):
                state.append(9)

            # II4a. two intersections -> S11
            elif (intersect_count == 2):
                state.append(11)

                # extract the index of the edges where the second point is
                # located at their ends
                edges_index = [i for i, x in enumerate(
                    only_seg_edge_stat) if x == 5]
                # get the index for the edge of the first point
                p1_edge_index = p1_edge_stat.index(1)
                # remove the index of the first point
                edges_index.pop(edges_index.index(p1_edge_index))
                # add the coordinate of the intersection
                state.append(seg_edge_stat[edges_index[0]][1])
                # return the index of the edges
                state.append(edges_index[0])

                # OBSOLETE RETURN VALUES
                # return Point1 Inside + Separation Coordinate + index of the separating box segment
                # state.append (p1Coord)
                # add the coordinate of the intersection
                # state.append (seg_edge_stat[edges_index[0]][1])
                # append the intersecting box edge
                # state.append (only_seg_edge_stat.index (5))

            # II4c. three intersections -> XS11
            elif (intersect_count == 3):
                state.append(11)

                # extract the index of the edges where the second point is
                # located at their ends
                edges_index = [i for i, x in enumerate(
                    only_seg_edge_stat) if x == 5]
                # get the index for the edge of the first point
                p1_edge_index = p1_edge_stat.index(1)
                # remove the index of the first point
                edges_index.pop(edges_index.index(p1_edge_index))
                # add the coordinate of the intersection [using either of the
                # remaining data]
                state.append(seg_edge_stat[edges_index[0]][1])
                # find & append the intersecting vertex
                for i in range(4):
                    if ((i % 4) in edges_index) & (
                            ((i + 1) % 4) in edges_index):
                        vertex_index = i
                # add the respective edges
                state.append([vertex_index, (vertex_index + 1) % 4])

            # II4b. overlap is the only remaining case -> S5
            else:
                state.append(5)
                try:
                    # find the overlapping edge index & add the overlap point
                    state.append(seg_edge_stat[only_seg_edge_stat.index(1)][1])
                except BaseException:
                    handle_x()

    # III. first point is on a vertex
    elif (p1BoxStat == 4):

        # III1.  2nd point inside -> XS2
        if (p2BoxStat == 1):
            state.append(2)

        # III2. 2nd point on an edge
        elif (p2BoxStat == 3):

            # get the index of the edge on which the second point resides
            p2_edge_index = p2_edge_stat.index(1)
            p1_edge_index = [i for i, x in enumerate(p1_edge_stat) if x == 4]

            # III2a. 2nd point on the adjacent edge -> XS4
            if (p2_edge_index == p1_edge_index[0]) | (
                    p2_edge_index == p1_edge_index[1]):
                state.append(4)

            # III2b. 2nd point on any edge other than the adjacent one -> XS3
            else:
                state.append(3)

        # III3. 2nd point on a vertex
        elif (p2BoxStat == 4):

            # get the index of the edge on which the second point resides
            p1_edge_index = [i for i, x in enumerate(p1_edge_stat) if x == 4]
            p2_edge_index = [i for i, x in enumerate(p2_edge_stat) if x == 4]

            # adjacent vertex has common edges with the first point vertex
            # III3a. 2nd point on a neighbouring vertex-> XS4
            if any([(x in p2_edge_index) for x in p1_edge_index]):
                state.append(4)

            # III3b. 2nd point on the opposite side vertex -> XS3
            else:
                state.append(3)
        # III4. 2nd point is outside of the box
        elif (p2BoxStat == 2):

            # get the number of intersection points
            intersect_count = only_seg_edge_stat.count(5)

            # III4a. 2nd point outside w/t no intersection -> XS12
            if (intersect_count == 2):
                state.append(6)
                handle_x()

            # III4b/c. 3 intersection points
            elif (intersect_count == 3):

                # III4b. 2nd point outside through the adjacent edge (= through neighbouring vertex) -> XS5
                # 3 intersection points + 1 overlapped edge
                if (1 in only_seg_edge_stat):
                    state.append(5)

                    # find the overlapping edge index & add the overlap point
                    state.append(seg_edge_stat[only_seg_edge_stat.index(1)][2])

                # III4c. 2nd point outside through any edge but the adjacent
                # one (no overlaps) -> XS11
                else:
                    state.append(11)
                    # return Point1 Inside + Separation Coordinate + index of the separating box segment
                    # state.append (p1Coord)

                    # extract the index of the 3 edges with intersection
                    edges_index = [
                        i for i, x in enumerate(only_seg_edge_stat) if x == 5]

                    # get the 2 indexes of the edges of the vertex of the 1st
                    # point
                    p1_edge_index = [
                        i for i, x in enumerate(p1_edge_stat) if x == 4]

                    # remove these 2 indexes from the 3 before that; the
                    # remaining is the edge of the intersection of second point
                    edges_index.pop(edges_index.index(p1_edge_index[0]))
                    edges_index.pop(edges_index.index(p1_edge_index[1]))

                    # add the coordinate of the intersection
                    state.append(seg_edge_stat[edges_index[0]][1])

                    # append the intersecting box edge, which is the only
                    # remaining component
                    state.append(edges_index[0])

            # III4d. 2nd point outside but through the opposite vertex-> XS11
            #        [The code is exactly the same as III4c.]
            elif (intersect_count == 4):
                state.append(11)
        # return Point1 Inside + Separation Coordinate + index of the separating box segment
                # state.append (p1Coord)

                # extract the index of the 4 edges with intersection
                edges_index = [i for i, x in enumerate(
                    only_seg_edge_stat) if x == 5]

                # get the 2 indexes of the edges of the vertex of the 1st point
                p1_edge_index = [
                    i for i, x in enumerate(p1_edge_stat) if x == 4]

                # remove these 2 indexes from the 3 before that; the remaining
                # is the edge of the intersection of second point
                edges_index.pop(edges_index.index(p1_edge_index[0]))
                edges_index.pop(edges_index.index(p1_edge_index[1]))

                # add the coordinate of the intersection
                state.append(seg_edge_stat[edges_index[0]][1])

                # find & append the intersecting vertex
                for i in range(4):
                    if ((i % 4) in edges_index) & (
                            ((i + 1) % 4) in edges_index):
                        vertex_index = i
                # add the respective edges
                state.append([vertex_index, (vertex_index + 1) % 4])

            # there is no such a case; something is wrong!
            else:
                print(
                    '> GetSegBoxState(): Error detecting the type of the second point at stage III!')
                print('\taSeg = ', aSeg, '\n\taBox = ', aBox)
                print(
                    '\tp1BoxStat = ',
                    p1_edge_stat,
                    '\n\tp2BoxStat = ',
                    p2_edge_stat)
                state.append(0)

    # IV. first point might be out of the box, which is wrong!
    else:
        print('> GetSegBoxState(): first point is out of the box!')
        print(
            '\taSeg = ',
            aSeg,
            '\n\taBox = ',
            aBox,
            '\n\tp1BoxStat = ',
            p1_edge_stat,
            '\n\tp2BoxStat = ',
            p2_edge_stat)
        state.append(6)

    return state

    # OBSOLETE CODE OF GetSegBoxState()
    #
    # S1: both ends are inside the box
    # if ((p1BoxStat == 1) & (p2BoxStat == 1)):
    #  state.append (1)
    # S2: point on border + point inside
    # XS2: point on vortex + point inside
    # elif ((p1BoxStat == 1) & (p2BoxStat >= 3)) | ((p1BoxStat >= 3) & (p2BoxStat == 1)):
    #  state.append (2)
    # if points are on the boundary or vertices, its either S4 (XS4) or S3(XS3)
    # elif ((p1BoxStat >= 3) & (p2BoxStat > 3)):
    #  # S4: both points on the same border
    #  # XS4: both points on the same border where one is on a vertex
    #  if any([((x==y)&(x==1)) for x,y in zip(p1_edge_stat, p2_edge_stat)]) | any([(((x==4)&(y==1)) | ((x==1)&(y==4))) for x,y in zip#(p1_edge_stat, p2_edge_stat)]):
    #    state.append (4)
    #  # S3 and XS3
    #  else:
    #    state.append (3)
    # if a point is out of the box and one is one the boundary or a vertex, it is either 5, 9, or 11
    # elif ((p1BoxStat == 2) & (p2BoxStat >= 3)) | ((p1BoxStat >= 3) & (p2BoxStat == 2)):
    #  # S5 and XS5: point on boundary/vertex + point outside (parallel line case)
    #  if (1 in only_seg_edge_stat) | (2 in only_seg_edge_stat):
    #    state.append (5)
    #    # returns the separation point coordinate
    #    # it is the vertex if a point is on it
    #    if (p1BoxStat == 4):
    #      state.append (p1Coord)
    #    # it is point2 if it is on the vertex
    #    elif (p2BoxStat == 4):
    #      state.append (p2Coord)
    #    # otherwise, the line is partially on the border and the separation point is calculated
    #    else:
    #      state.append (seg_edge_stat[only_seg_edge_stat.index (1)][1])
    #      state.append (seg_edge_stat[only_seg_edge_stat.index (1)][2])
    #  # S9/XS9: Exactly one intersection of the segment and the box is required.
    #  elif (only_seg_edge_stat.count(5) == 1):   #(2 in only_seg_edge_stat) | (5 in only_seg_edge_stat):
    #    state.append (9)
    #
    #  # There are two intersection points in state 11:  one intersection in the beginning/end + another intersection in the middle.
    #  elif (only_seg_edge_stat.count(5) == 2):
    #    state.append (11)
    #
# #    Indices for the intersections between two segments (state 5)
    #    indices = [i for i, x in enumerate(only_seg_edge_stat) if x == 5]
    #
# #    Add these two points since they make the portion of the segment which is inside the box
    #    for i in indices:
    #      state.append (segBoxSegStat[i][1])
    # this part addresses the case in which both ends of the segment are outside the box
    # elif ((p1BoxStat == 2) & (p2BoxStat == 2)):
    #  if (1 in only_seg_edge_stat):
    #    state.append (6)
    #    state.append (seg_edge_stat[only_seg_edge_stat.index (1)][1])
    #    state.append (seg_edge_stat[only_seg_edge_stat.index (1)][2])
    #  elif only_seg_edge_stat.count(5) == 2:
    #    state.append (8)
    #    indices = [i for i, x in enumerate(only_seg_edge_stat) if x == 5]
    #    for i in indices:
    #      state.append (seg_edge_stat[i][1])
    #  # this is the old line which made some bugs: elif (only_seg_edge_stat.count(6)==4):
    #  else:
    #    state.append(10)
    # elif ((p1BoxStat == 2) & (p2BoxStat == 1)):
    #    state.append (7)
    #    state.append (seg_edge_stat[only_seg_edge_stat.index (5)][1])
    #    state.append (p2Coord)
    #    state.append (only_seg_edge_stat.index (5))
    # elif ((p1BoxStat == 1) & (p2BoxStat == 2)):
    # return Point1/Point2 Inside + Separation Coordinate + index of the separating box segment
    #    state.append (7)
    #    state.append (seg_edge_stat[only_seg_edge_stat.index (5)][1])
    #    state.append (p1Coord)
    #    state.append (only_seg_edge_stat.index (5))
    # else:
    #  print ('> GetSegBoxState(): No special case detected')
    #  print ('\taSeg = ', aSeg,'\n\taBox = ', aBox)
    #  print ('\tp1BoxStat = ', p1_edge_stat,'\n\tp2BoxStat = ', p2_edge_stat)
    #  state.append (0)
    #
    #
    # return state


# ***********************************************************************
# SUBROUTINE InterLinePlane
#
# Objective:
#   Returns the intersection point of line with a plane.
# The line is introduced using its direction vector and an arbitrary
# point whereas the plane is introduced using its attitude numbers,
# i.e., for Ax+By+Cz+D=0 are A, B, C, and D.
#
# Input(s):
#   dirVec       REAL*8(3)           direction vector of the line
#   arbPoint     REAL*8(3)           arbitrary point on the line
#   attNum       REAL*8(4)           attitude number of the plane
#
# Output(s):
#                REAL*8(3)           intersection point
#
# Auxiliary variable(s):
#   temp         REAL*8              the t parameter which corresponds to the intersection point
#
# Required subprogram(s):
#   none
#
# Restrictions:
#   none
#
# Future Updates:
#   none
#
# Last update: 3/1/2017
# ***********************************************************************
def InterLinePlane(dirVec, arbPoint, attNum):
    """Returns the intersection point of line with a plane.
    
    The line is introduced using its direction vector and an arbitrary
    point whereas the plane is introduced using its attitude numbers,
    i.e., for Ax+By+Cz+D=0 are A, B, C, and D.
    
    Args:
        dirVec: Direction vector of the line (3D)
        arbPoint: Arbitrary point on the line (3D)
        attNum: Attitude numbers of the plane [A, B, C, D]
    
    Returns:
        list: Intersection point coordinates [x, y, z]
    """
    temp = -1. * (attNum[0] * arbPoint[0] + attNum[1] * arbPoint[1] + attNum[2] * arbPoint[2] +
                  attNum[3]) / (attNum(0) * dirVec(0) + attNum(1) * dirVec(1) + attNum(2) * dirVec(2))
    return [
        arbPoint[0] +
        temp *
        dirVec[0],
        arbPoint[1] +
        temp *
        dirVec[1],
        arbPoint[2] +
        temp *
        dirVec[2]]
# ***********************************************************************
# SUBROUTINE CalcClosePointPlane
#
# Objective:
#   Calculates the coordinates of the closest point on a plane with
# respect to a given point.
#
# Input(s):
#   arbPoint     REAL*8(3)           arbitrary point in the space
#   attNum       REAL*8(4)           attitude number of the plane
#
# Output(s):
#   closePoint   REAL*8(3)           closest point on the plane with
#                                    respect to the arbitrary point
#
# Auxiliary variable(s):
#   none
#
# Required subprogram(s):
#   InterLinePlane()
#
# Restrictions:
#   none
#
# Future Updates:
#   none
#
# Last update: 4/1/2017
# ***********************************************************************


def CalcClosePointPlane(arbPoint, attNum):
    """Calculates the coordinates of the closest point on a plane.
    
    Calculates the coordinates of the closest point on a plane with
    respect to a given point.
    
    Args:
        arbPoint: Arbitrary point in space (3D)
        attNum: Attitude numbers of the plane [A, B, C, D]
    
    Returns:
        list: Closest point on the plane with respect to the arbitrary point
    """
    return InterLinePlane(attNum[0:3], arbPoint, attNum)

# ***********************************************************************
# SUBROUTINE ReflectPointPlane
#
# Objective:
#   Reflects a point across a plane with attitude numbers of A,B,C, and D
#
# Input(s):
#   orgPoint     REAL*8(3)           original point
#   attNum       REAL*8(4)           attitude number of the plane
#
# Output(s):
#   refPoint     REAL*8(3)           reflected point
#
# Auxiliary variable(s):
#   closePoint   REAL*8(3)           closest point to the original point on the plane
#
# Required common-blocks:
#   none
#
# Required subprogram(s):
#   none
#
# Restrictions:
#   none
#
# Future Updates:
#   none
#
# Last update: 4/1/2017
# ***********************************************************************


def ReflectPointPlane(orgPoint, attNum):
    """Reflects a point across a plane with attitude numbers A, B, C, and D.
    
    Args:
        orgPoint: Original point (3D)
        attNum: Attitude numbers of the plane [A, B, C, D]
    
    Returns:
        list: Reflected point coordinates
    """
    closePoint = CalcClosePointPlane(orgPoint, attNum)
    return [2. * x - y for x, y in zip(closePoint, orgPoint)]


def MakeSegPeriodic(aSeg, state, aBox):
    """Processes a fiber segment to maintain periodicity in RVE analysis.
    
    Takes a segment and returns a list of processed segments that satisfy
    periodic boundary conditions. If no division is required, returns the
    original segment.
    
    This function is used in fiber generation for Representative Volume
    Elements (RVE) where periodic boundary conditions must be maintained.
    
    Args:
        aSeg: Input line segment [point1, point2]
        state: State information from GetSegBoxState
        aBox: RVE bounding box [[x_min, y_min], [x_max, y_max]]
    
    Returns:
        list: List of segment(s) that satisfy periodicity:
              - [0] if no update needed for periodicity
              - [n, [p1,p2], [p3,p4], ...] for n replacement segments
    
    Note:
        Fiber generation creates nodes in/on RVE border which can extend
        inside, outside, or on the RVE. Relevant states: 1,2,3,4,5,7,11
        
        States requiring action:
        - State 5: May have overlaps when fiber is long compared to RVE edge
        - State 4: (currently prohibited in generation algorithm)
        - States 7,11: Outside segment moved to opposite edge
        
        Error handling:
        - State 6 cases: Regenerate random start point (quick patch for
          vertex corner issues)
        
        The function implements periodic boundary conditions by:
        1. Cutting segments at box boundaries  
        2. Mirroring intersection points to opposite edges
        3. Continuing fiber segments from mirrored points
    """

    # create an empty return list
    segLst = []
    cur_seg = aSeg
    cur_state = state

    # Calculate the mid-axis of the box
    minCorner = aBox[0]
    maxCorner = aBox[1]

    # get the coordinates of the box vertices
    vertex_lst = [
        minCorner, [
            maxCorner[0], minCorner[1]], maxCorner, [
            minCorner[0], maxCorner[1]]]

    # just to push start the while loop
    cutIsRequired = True

    while cutIsRequired:
        # Only states 7 and 11 require action
        if cur_state[0] in [7, 11]:
            # p1, p2, and p3 are the first, intersection, and second points,
            # respectively.
            p1 = cur_seg[0]
            p2 = cur_state[1]
            p3 = cur_seg[1]

            # add the initial segment of the fibre to the list
            segLst.append([p1, p2])

            # mirror the intersection point to get the new p1 (update p1)
            p1 = MirrorPointInBox(p2, aBox)
            # Calculate the p3-p2 displacement vector
            disp = [round(x - y, 3) for x, y in zip(p3, p2)]
            # calculate the new p3 (update p3)
            p3 = [round(x + y, 3) for x, y in zip(p1, disp)]

            # update the current segment and its state to new points p1 & p3
            cur_seg = [p1, p3]
            cur_state = GetSegBoxState(cur_seg, aBox)

            # there is a possibility of having a vertex point while making the
            # material periodic; make sure such case does not happen
            while cur_state[0] == 6:
                print(
                    '>  MakeSegPeriodic(): State 6 is encountered...\n\tSeg lst:',
                    segLst,
                    '\n\tcurSeg: ',
                    cur_seg)
                print('\toriginal segment causing problem:\t', aSeg)
                print('> MakeSegPeriodic(): generating a new start coordinate while preserving the orinetation of the segment and the remaining length...')
                # Calculate the p3-p1 displacement vector
                disp = [round(x - y, 3) for x, y in zip(p3, p1)]
                # regenerate the first point [lines copied from
                # Create_full_random()]
                boxMin, boxMax = GetBoxMinMax(aBox)
                boxEdge = [x - y for x, y in zip(boxMax, boxMin)]
                # ensure the first point is in the box---excluding the edges
                p1 = [round(x + random.random() * y, 3)
                      for x, y in zip(boxMin, boxEdge)]
                while not IsPointInBox(p1, aBox):
                    p1 = [round(x + random.random() * y, 3)
                          for x, y in zip(boxMin, boxEdge)]
                # recalculate the new p3 (update p3)
                p3 = [round(x + y, 3) for x, y in zip(p1, disp)]
                cur_seg = [p1, p3]
                cur_state = GetSegBoxState(cur_seg, aBox)
                print(
                    '> the current seg is updated to ',
                    cur_seg,
                    '\n\t new state = ',
                    cur_state)
        else:
            cutIsRequired = False
            segLst.append(cur_seg)
    return segLst

# OBSOLETE MakeSegPeriodic()
#
#  # Get the state of the fiber
#  # TODO there is a bug here where sometimes GetSegBoxStates returns nil;
#  # it happens when the starting point of the segment is at one corner of the box.
#  #segState  = GetSegBoxState (cur_seg, aBox)
#
#  # Only states 7 and 11 require action
#  if state[0] in [7, 11]:
#    cutIsRequired = True
#  else:
#    cutIsRequired = False
#    segLst.append (cur_seg)
#
#
#
#
#
#  while cutIsRequired:
#    if (state[0] == 7):
# print "state 7"
# Detect the inside and intersect points
#      point1 = state[1]
#      point2 = state[2]
#
#
#      point1State = GetPointBoxState (point1[0:2], aBox)
#      if  point1State== 1:
#        inPoint    = point1
#        interPoint = point2
#      else:
#        inPoint    = point2
#        interPoint = point1
# Detect the outside point
#      if inPoint == cur_seg[0]:
#        outPoint = cur_seg[1]
#      else:
#        outPoint = cur_seg[0]
# Cut the old fiber and add it to the list
#      segLst.append([inPoint, interPoint])
#
# Calculate the remaining segment vector
#      remSegVec = [round(x-y,3) for x,y in zip (outPoint, interPoint)]
# Reflect the intersection point and get the start point of the new segment
#      newSegStart = MirrorPointInBox (interPoint, state, aBox)
#      newSegEnd   = [round(x+y,3) for x,y in zip (newSegStart, remSegVec)]
# Do not append it to the return list unless it does not need any divisions
#      cur_seg = [newSegStart, newSegEnd]
#    elif (state[0] == 11):
# print "state 11"
# Detect the start and intersect points
#      point1 = state[1]
#      point2 = state[2]
#
# the outside point
#      if (point1 == cur_seg[0]):
#        startPoint = point1
#        interPoint = point2
#        outPoint   = cur_seg[1]
#      elif (point1 == cur_seg[1]):
#        startPoint = point1
#        interPoint = point2
#        outPoint   = cur_seg[0]
#      elif (point2 == cur_seg[0]):
#        startPoint = point2
#        interPoint = point1
#        outPoint   = cur_seg[1]
#      elif (point2 == cur_seg[1]):
#        startPoint = point2
#        interPoint = point1
#        outPoint   = cur_seg[0]
#
#
# Cut the old fiber and add it to the list
#      segLst.append([startPoint, interPoint])
#
# Calculate the remaining segment vector
#      remSegVec = [round(x-y,3) for x,y in zip (outPoint, interPoint)]
# Reflect the intersection point and get the start point of the new segment
#
#      newSegStart = MirrorPointInBox (interPoint, aBox)
#      newSegEnd   = [round(x+y,3) for x,y in zip (newSegStart, remSegVec)]
# Do not append it to the return list unless it does not need any divisions
#      cur_seg = [newSegStart, newSegEnd]
#    # Check the state of the current segment
#    # 28/04/2019 TODO: There is a bug here where state is returned empty, and thus the following if# generates and exception. The exception statement is written to find that special case.
#    state  = GetSegBoxState (cur_seg, aBox)
# If check shows that no additional cut is required, append to list and flag an exit
#    try:
#        if state[0] in [7, 11]:
#          cutIsRequired = True
#
#        else:
#          cutIsRequired = False
#
#          segLst.append (cur_seg)
#    except:
#        print ('Exceptional case:\n\t cur_seg = ', cur_seg, '\n\taBox = ',aBox)
#  return segLst


# **********************************************************************************************
# MirrorPointOnBox ()
#   The reflection is done for all the points on the box border. It detects the border and
#   reflects the point to the opposite one.
#   The order of box segments are as the following: [southSeg, eastSeg, northSeg, westSeg]
#
#
# **********************************************************************************************

def MirrorPointInBox(aPoint, aBox):
    """Mirrors a point in a 2D box to the opposite edge/vertex.
    
    The reflection is done for all points on the box border. It detects the
    border and reflects the point to the opposite one.
    Code is only for points on the edges/vertices.
    
    Args:
        aPoint: Point on the box boundary (2D)
        aBox: 2D box defined by min and max corners
    
    Returns:
        list: Mirrored point coordinates
    """
    # Calculate the mid-axis of the box
    minCorner = aBox[0]
    maxCorner = aBox[1]

    # get the coordinates of the box vertices
    vertex_lst = [
        minCorner, [
            maxCorner[0], minCorner[1]], maxCorner, [
            minCorner[0], maxCorner[1]]]

    # detect if it is a vertex point to be mirrored or somewhere on the edge
    if aPoint in vertex_lst:
        if aPoint == vertex_lst[0]:
            mirrored_point = vertex_lst[2]
        elif aPoint == vertex_lst[1]:
            mirrored_point = vertex_lst[3]
        elif aPoint == vertex_lst[2]:
            mirrored_point = vertex_lst[0]
        else:
            mirrored_point = vertex_lst[1]

    # it is on the edge of the box but not on any vertex
    else:
        if aPoint[0] == minCorner[0]:
            mirrored_point = [maxCorner[0], aPoint[1]]
        elif aPoint[0] == maxCorner[0]:
            mirrored_point = [minCorner[0], aPoint[1]]
        elif aPoint[1] == minCorner[1]:
            mirrored_point = [aPoint[0], maxCorner[1]]
        elif aPoint[1] == maxCorner[1]:
            mirrored_point = [aPoint[0], minCorner[1]]

    return mirrored_point

# OBSOLETE CODE
#  '''
#  The reflection is done for all the points on the box border. It detects the border and reflects the point to the opposite one.
#  The order of box segments are [southSeg, eastSeg, northSeg, westSeg], which are enumerated to [0, 1, 2, 3].
#  States 7 & 11 will be considered.
#  '''
#  xMidAxis = [[minCorner[0], 0.5*(minCorner[1]+maxCorner[1])], [maxCorner[0], 0.5*(minCorner[1]+maxCorner[1])]]
#  yMidAxis = [[0.5*(minCorner[0]+maxCorner[0]), minCorner[1]], [0.5*(minCorner[0]+maxCorner[0]), maxCorner[1]]]
#
#  # the only differentiation required is between the vertex intersection and edge intersection
#  edge_index = state[2]
#
#  # edge intersection (one intersection)
#  if len(edge_index) == 1:
#    # Mirror the point depending on the edge
#    if (edge_index == 0) | (edge_index == 2):
#      mirrored_point = ReflectPointSeg (aPoint, xMidAxis)
#    else:
#      mirrored_point = ReflectPointSeg (aPoint, yMidAxis)
#
#  # vertex intersection (two intersections)
#  if len(edge_index) == 2:
#
#    # Numbering the edges of the box:
#    #   +--2--+
#    #   |     |
#    #   3     1
#    #   |     |
#    #   +--0--+
#    # Numbering of the vertices of the box:
#    #   3-----2
#    #   |     |
#    #   |     |
#    #   |     |
#    #   0-----1
#
#    vertex_index = (edge_index[1] + 1) % 4
#    mirrored_point = vertex_lst[vertex_index]
#  # any other case is an error
#  else:
#    handle_error()

#  # Nested function to report an error
#  def handle_error():
#    print ('> MirrorPointInBox() ERROR: invalid state.\n\tState = ', state,'\n\tpoint = ', aPoint,'\n\taBox = ', aBox)


# OBSOLETE CODE OF MirrorPointInBox
#
#  # [I4.] state 7 starts from inside
#  if state[0] == 7:
#    # [I4a.] one intersection
#    if len(state[2]) == 1:
#      edge_index = state[2]
#    # [I4b.] two intersections (through vertex)
#    if len(state[2]) == 2:
#      pass
#    # any other case is an error
#    else:
#      handle_error()
#
#  # [II4./III4.] state 11 is from a point on the edge (II) or vertex (III)
#  elif state[0]== 11:
#    # [II4a./III4c.] one intersection
#    if len(state[2]) == 1:
#      edge_index = state[2]
#    # [II4c./III4d.] two intersections (through vertex)
#    if len(state[2]) == 2:
#      pass
#    # any other case is an error
#    else:
#      handle_error()
#  # other states are not permitted
#  else:
#    handle_error()
#
#
#  # Find the edge of the box that includes the point
#  # Exceptional case happens for vertices.
#
#  # Find the number of intersection points
#
#  allBoxSeg  = GetBoxSeg (aBox)
#  pointState = [GetPointSegState(aPoint[0:2], x)  for x in allBoxSeg]
#
#
#
#  try:
#    SourceEdge = pointState.index(1)
#  except ValueError:
#    print ("Error 1 is not in the list")
#    print ("point: ", aPoint)
#    print ("pointState ", pointState)
#    print ("box ", aBox)
#    sys.exit(0)
#
#  # Mirror the point depending on the edge
#  if (SourceEdge == 0) | (SourceEdge == 2):
#    mirroredPoint = ReflectPointSeg (aPoint, xMidAxis)
#  else:
#    mirroredPoint = ReflectPointSeg (aPoint, yMidAxis)
#
#


# **********************************************************************************************
# ReflectPointSeg ()
#   The reflection is done by mirroring a point with respect to a an axis represented by a
#   segment in the 2D space.
#
#
# **********************************************************************************************
def ReflectPointSeg(aPoint, aSeg):
    """Reflects a point with respect to an axis represented by a segment in 2D space.
    
    Args:
        aPoint: Point to be reflected (2D)
        aSeg: Segment representing the reflection axis
    
    Returns:
        list: Reflected point coordinates (rounded to 5 decimal places)
    """
    s1 = aSeg[0]
    s2 = aSeg[1]
    vecD = [x - y for x, y in zip(s1, aPoint)]
    vecS = [x - y for x, y in zip(s2, s1)]
    vecSLen = GetVecLen(vecS)
    vecSNorm = [x / vecSLen for x in vecS]
    dotProduct = GetDot(vecD, vecSNorm)
    return [round(x + 2. * y - 2. * dotProduct * z, 5)
            for x, y, z in zip(aPoint, vecD, vecSNorm)]


# ********************************************************************************
# GetBoxMinMax ()
#   Returns the min and max corners of a box.
#
# ********************************************************************************
def GetBoxMinMax(aBox):
    """Returns the min and max corners of a 2D box.
    
    Args:
        aBox: 2D box defined by corner points
    
    Returns:
        list: [[x_min, y_min], [x_max, y_max]]
    """
    # just to make sure the order of coordinates is correct
    x1 = min([item[0] for item in aBox])
    x2 = max([item[0] for item in aBox])
    y1 = min([item[1] for item in aBox])
    y2 = max([item[1] for item in aBox])

    return [[x1, y1], [x2, y2]]


# ********************************************************************************
# CreatePart ()
#   Returns the partitioned box to nParts in each direction so nPart**2 parts will
#   be returned.
# ********************************************************************************

def CreatePart(aBox, nPart):
    """Returns the partitioned box into nPart x nPart sub-boxes.
    
    Partitions a 2D box into nPart subdivisions in each direction,
    resulting in nPart^2 total sub-boxes.
    
    Args:
        aBox: 2D box to partition
        nPart: Number of partitions in each direction
    
    Returns:
        list: List of sub-boxes, each defined by [min_corner, max_corner]
    """
    minBox, maxBox = GetBoxMinMax(aBox)

    xLen = maxBox[0] - minBox[0]
    yLen = maxBox[1] - minBox[1]

    xStep = xLen / nPart
    yStep = yLen / nPart

    partedBox = []

    for j in range(nPart):
        for i in range(nPart):
            partedBox.append([[minBox[0] + i * xStep,
                               minBox[1] + j * yStep],
                              [minBox[0] + (i + 1) * xStep,
                               minBox[1] + (j + 1) * yStep]])

    return partedBox
# ********************************************************************************
#
# Fiber contributed length to the RVE is calculated for a fiber, i.e., the
# contribution factor is applied to its length.
#
#
# ********************************************************************************


def GetFiberContLen(aSeg, aBox):
    """Calculates fiber contributed length to the RVE.
    
    Fiber contributed length to the RVE is calculated for a fiber, i.e., the
    contribution factor is applied to its length.
    
    Args:
        aSeg: Fiber segment defined by two endpoints
        aBox: Representative Volume Element (RVE) box
    
    Returns:
        float: Contributed length of the fiber segment to the RVE
    """
    fiberState = GetSegBoxState(aSeg, aBox)

#  a dictionary mapping may be used here to improve the efficiency of the code.
#  fiberCont = {0:0, 1:1, 2:1, 3:1, 4:0.5, 5:0.5, 6:0.5, 7:1, 8:1, 9:0, 10:0}
#  if fiberCont.get (fiberState[0], 0) != 0:
    fiberLen = GetDistance(aSeg[0], aSeg[1])

    if fiberState[0] in [1, 2, 3]:
        contLen = fiberLen
    elif fiberState[0] == 4:
        contLen = 0.5 * fiberLen
    elif fiberState[0] in [5, 6]:
        contLen = 0.5 * GetDistance(fiberState[1], fiberState[2])
    elif fiberState[0] in [7, 8, 11]:
        contLen = GetDistance(fiberState[1], fiberState[2])
    else:
        contLen = 0
    return contLen

# ********************************************************************************
# CalcPrincipalVals ()
#   Returns the partitioned box to nParts in each direction so nPart**2 parts will
#   be returned.
# ********************************************************************************


def CalcPrincipalVals_dontuse(aMat):
    """Calculates principal values for a 2x2 matrix (deprecated version).
    
    Usage:
    aMat = [[a_11,a_12], [a_21,a_22]]

    Note: negative values are possible
    
    Args:
        aMat: 2x2 matrix as list of lists
    
    Returns:
        list: [theta_max, theta_min, lambda_max, lambda_min]
    
    Note:
        This version is deprecated - use calc_principal_vals instead
    """

    [lambda_min, lambda_max], [eigenvec_min,
                               eigenvec_max] = eigh(np.array(aMat))

    theta_max = round(
        np.degrees(
            np.arctan2(
                eigenvec_max[1],
                eigenvec_max[0])),
        5)
    theta_min = round(
        np.degrees(
            np.arctan2(
                eigenvec_min[1],
                eigenvec_min[0])),
        5)

    # this used to be (theta, aMin, aMax)
    return [theta_max, theta_min, lambda_max, lambda_min]

# this is the same as CalcPrincipalVals but makes sure the angle is
# between 0 and 180


def calc_principal_vals(a_mat):
    """Calculates principal values and angles for a 2x2 matrix.
    
    Same as CalcPrincipalVals but ensures the angle is between 0 and 180 degrees.
    
    Args:
        a_mat: 2x2 matrix as [[a11, a12], [a21, a22]]
    
    Returns:
        list: [theta_max, theta_min, lambda_max, lambda_min]
              where theta values are angles in degrees (0-180)
              and lambda values are eigenvalues
    """

    [lambda_min, lambda_max], [eigenvec_min,
                               eigenvec_max] = eigh(np.array(a_mat))

    # print ('eigenvec max =', eigenvec_max)
    # print ('eigenvec min =', eigenvec_min)

    theta_max = round(
        np.degrees(
            np.arctan2(
                eigenvec_max[1],
                eigenvec_max[0])),
        5)
    theta_min = round(
        np.degrees(
            np.arctan2(
                eigenvec_min[1],
                eigenvec_min[0])),
        5)

    if (theta_max < 0):
        theta_max += 180.
        theta_min += 180.

    # this used to be (theta, aMin, aMax)
    return [theta_max, theta_min, lambda_max, lambda_min]


def CalcPrincipalVals_obsolete(aMat):
    """Calculates principal values for a 2x2 matrix (obsolete version).
    
    Usage:
    aMat = [[a_11,a_12], [a_21,a_22]]
    
    Args:
        aMat: 2x2 matrix as list of lists
    
    Returns:
        list: [theta_max, theta_min, lambda_max, lambda_min]
    
    Note:
        This version is obsolete - use calc_principal_vals instead
        Version: 2019/09/10
    """
#  a11 = aMat[0][0]
#  a12 = aMat[0][1]
#  a22 = aMat[1][1]
#
#
# it is possible to have a zero denominator
#  if ((a11-a22) == 0.):
#    alpha = 90.
#  elif (a11*a22-a12**2. == 0.):
#    alpha = 0.
#  else:
#    alpha = math.degrees(math.atan (2.*a12/a11-a22))
#
#  aMax = 0.5*(a11+a22) + math.sqrt (0.25*(a11-a22)**2.+a12**2.)
#  aMin = 0.5*(a11+a22) - math.sqrt (0.25*(a11-a22)**2.+a12**2.)
    anArray = np.array(aMat)
    eVals, eVecs = np.linalg.eig(anArray)

    # eVec1 corresponds to e1
    e1 = round(eVals[0], 5)
    e2 = round(eVals[1], 5)

    eVec1 = eVecs[:, 0]
    eVec2 = eVecs[:, 1]

    # sorts from bigger to smaller and returns the indexes; however, I think the absolute values should be sorted
    # abs_eVals = abs(eVals)
    idx = eVals.argsort()[::-1]

# old code which worked but I changed it to arctan2
    # alpha1 = round(np.degrees (np.arctan(eVec1[1]/eVec1[0])), 5)
    # alpha2 = round(np.degrees (np.arctan(eVec2[1]/eVec2[0])), 5)

    # NOTE: theta denotes the rotation required to obtain the principal axis
    # W/T RESPECT TO CURRENT ORIENTATION. Thus, this is not an absolute
    # value??
    lambda_max = round(eVals[idx[0]], 5)
    eigenvec_max = eVecs[:, idx[0]]
    # theta_max		= round(np.degrees (np.arctan(eigenvec_max[1]/eigenvec_max[0])), 5)
    theta_max = round(
        np.degrees(
            np.arctan2(
                eigenvec_max[1],
                eigenvec_max[0])),
        5)

    lambda_min = round(eVals[idx[1]], 5)
    eigenvec_min = eVecs[:, idx[1]]
    # theta_min		= round(np.degrees (np.arctan(eigenvec_min[1]/eigenvec_min[0])), 5)
    theta_min = round(
        np.degrees(
            np.arctan2(
                eigenvec_min[1],
                eigenvec_min[0])),
        5)
    # OBSOLETE CODE THAT I THINK WAS WRONG !!!
    # alpha1 = round(np.degrees (np.arctan2(eVec1[1],eVec1[0])), 5)
    # alpha2 = round(np.degrees (np.arctan2(eVec2[1],eVec2[0])), 5)
    #
    #
    # arrayVec1 = np.matmul (anArray, eVec1)
    # eign1Vec1 = e1*eVec1
    ##
    # arrayVec2 = np.matmul (anArray, eVec2)
    # eign1Vec2 = e2*eVec2
    #
    # if not (np.array_equal (np.matmul (anArray, eVec1), e1*eVec1) & np.array_equal (np.matmul (anArray, eVec2), e2*eVec2)):
    #   alpha1, alpha2 = alpha2, alpha1
    #
    # if e1 >= e2:
    #  alpha = alpha1
    #  aMax = e1
    #  aMin = e2
    # else:
    #  alpha = alpha2
    #  aMax = e2
    #  aMin = e1

    # this used to be (theta, aMin, aMax)
    return [theta_max, theta_min, lambda_max, lambda_min]


# Cohen–Sutherland Algorithm for line clipping
def clip_line(box, x1, y1, x2, y2):
    """Cohen-Sutherland Algorithm for line clipping against a rectangular box.
    
    Args:
        box: Clipping box defined by [[x_min, y_min], [x_max, y_max]]
        x1, y1: Start point coordinates
        x2, y2: End point coordinates
    
    Returns:
        float: Length of the clipped line segment (0 if rejected)
    """
    # Defining region codes
    INSIDE = 0  # 0000
    LEFT = 1  # 0001
    RIGHT = 2  # 0010
    BOTTOM = 4  # 0100
    TOP = 8  # 1000

    # Defining x_max,y_max and x_min,y_min for rectangle
    # Since diagonal points are enough to define a rectangle
    # Function to compute region code for a point(x,y)
    def computeCode(x_min, y_min, x_max, y_max, x, y):
        code = INSIDE
        if x < x_min:      # to the left of rectangle
            code |= LEFT
        elif x > x_max:    # to the right of rectangle
            code |= RIGHT
        if y < y_min:      # below the rectangle
            code |= BOTTOM
        elif y > y_max:    # above the rectangle
            code |= TOP

        return code

    x_max = box[1][0]
    y_max = box[1][1]
    x_min = box[0][0]
    y_min = box[0][1]

    # Compute region codes for P1, P2
    code1 = computeCode(x_min, y_min, x_max, y_max, x1, y1)
    code2 = computeCode(x_min, y_min, x_max, y_max, x2, y2)
    accept = False

    while True:

        # If both endpoints lie within rectangle
        if code1 == 0 and code2 == 0:
            accept = True
            break

        # If both endpoints are outside rectangle
        elif (code1 & code2) != 0:
            break

        # Some segment lies within the rectangle
        else:

            # Line Needs clipping
            # At least one of the points is outside,
            # select it
            x = 1.0
            y = 1.0
            if code1 != 0:
                code_out = code1
            else:
                code_out = code2

            # Find intersection point
            # using formulas y = y1 + slope * (x - x1),
            # x = x1 + (1 / slope) * (y - y1)
            if code_out & TOP:

                # point is above the clip rectangle
                x = x1 + (x2 - x1) * (y_max - y1) / (y2 - y1)
                y = y_max

            elif code_out & BOTTOM:

                # point is below the clip rectangle
                x = x1 + (x2 - x1) * (y_min - y1) / (y2 - y1)
                y = y_min

            elif code_out & RIGHT:

                # point is to the right of the clip rectangle
                y = y1 + (y2 - y1) * (x_max - x1) / (x2 - x1)
                x = x_max

            elif code_out & LEFT:

                # point is to the left of the clip rectangle
                y = y1 + (y2 - y1) * (x_min - x1) / (x2 - x1)
                x = x_min

            # Now intersection point x,y is found
            # We replace point outside clipping rectangle
            # by intersection point
            if code_out == code1:
                x1 = x
                y1 = y
                code1 = computeCode(x_min, y_min, x_max, y_max, x1, y1)

            else:
                x2 = x
                y2 = y
                code2 = computeCode(x_min, y_min, x_max, y_max, x2, y2)

    if accept:
        # print ("Line accepted from %.2f,%.2f to %.2f,%.2f" % (x1,y1,x2,y2))

        vec = np.array([x2, y2]) - np.array([x1, y1])
        vec_len = np.linalg.norm(vec)

        # Here the user can add code to display the rectangle
        # along with the accepted (portion of) lines

    else:
        # print("Line rejected")
        vec_len = 0.0

    return vec_len


def clip_line_ext(box, x1, y1, x2, y2):
    """Extended Cohen-Sutherland Algorithm with edge weighting.
    
    Cohen-Sutherland Algorithm extended so that lines on the edge are counted
    with half the length of the fully inside ones.
    
    Args:
        box: Clipping box defined by [[x_min, y_min], [x_max, y_max]]
        x1, y1: Start point coordinates
        x2, y2: End point coordinates
    
    Returns:
        float: Weighted length of the clipped line segment
    """
    # run the original
    virgin_len = clip_line(box, x1, y1, x2, y2)
    if (virgin_len <= ZERO):
        extended_len = 0.0
    else:
        [xmin, ymin], [xmax, ymax] = box
        if ((x1 == xmin and x2 == xmin) or (x1 == xmax and x2 == xmax) or (
                y1 == ymin and y2 == ymin) or (y1 == ymax and y2 == ymax)):
            extended_len = virgin_len * 0.5
        else:
            extended_len = virgin_len
    return extended_len
