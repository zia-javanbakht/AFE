      subroutine hooklw(m,nn,kcus,b,ngens,dt,dtdl,e,pr,ndi,nshear,
     * imod,rprops,iprops)
c* * * * * *
c
c     user subroutine for elastic constants for
c     anisotropic options.
c
c     m(1)         user element number
c     m(2)         internal element number
c     nn           integration point number
c     kcus(1)      layer number
c     kcus(2)      internal layer number
c     b            stress-strain law or compliance matrix
c     ngens        number of stress components
c     dt           state variable
c     dtdl         incremental state variable
c     e            Young's modulus if ISOTROPIC option used
c                  Three Youngs' modulus if Orthotropic option is user
c     pr           Poisson ratio if ISOTROPIC option used
c                  Three Poisson ratios if Orthotropic option is user
c     ndi          number of direct components
c     nshear       number of shear components
c     imod         set to 0 if user subroutine anelas is used
c                  set to 1 to indicate that stress-strain law has been given
c                  set to 2 to indicate that compliance matrix - strain-stess 
c                     matrix has been given
c     rprops       real parameters
c     iprops       integer parameters
c* * * * * *
#ifdef _IMPLICITNONE
      implicit none
#else
      implicit logical (a-z)
#endif
c     ** Start of generated type statements **
      real*8 b, dt, dtdl, e
      integer imod, iprops, kcus, m, ndi, ngens, nn, nshear
      real*8 pr, rprops
c     ** End of generated type statements **
      REAL*8 :: exx, eyy, vyx, gxy


      dimension b(ngens,ngens),dt(*),dtdl(*),rprops(*),iprops(*),
     *  kcus(2),m(2),e(*),pr(*)

      exx = 6.91D3
      eyy = 8.51D3
      vyx = 0.32D0
      gxy = 2.41D3
       

      
c   Compliance matrix
      imod = 2
      
      b(1,1) = 1.D0/exx
      b(1,2) =-vyx/eyy
      b(1,3) = 0.0D0
      b(2,1) = b(1,2)
      b(2,2) = 1.D0/eyy
      b(2,3) = 0.0D0
      b(3,1) = 0.0D0
      b(3,2) = 0.0D0
      b(3,3) = 1.D0/(2.D0*gxy)
 
      Write (6,*) '** HOOKLW'
      return
      end
      
      subroutine orient2(n,nn,kcus,material,matname,icomp,nodes,nnodes,
     $     coord,coordint,ncoord,dircos,icall,iply,ilocal,ifast,
     $     vec1,vec2,integer_data,real_data)
c* * * * * *
c
c     user subroutine for definition of material orientation
c
c
c        this routine is called for each element, integration point and layer.
c        an exception is when the fast integrated composite option is used,
c        for which case it is only called for the first layer.
c
c        it is called only at the start of the analysis. For subsequent
c        increments the stored calculated data is used, taking effects
c        of large rotations into account.
c
c        the user is expected to define the element material coordinate
c        system ("preferred element system") with vectors vec1 and vec2
c        as described below.
c        the coordinate system can be visualized by means of element post codes
c        691 and 694. for composites also post code 697 (ply angle) is needed.
c        
c        the material system defined can be defined either in the element
c        coordinate system (as defined by dircos) or in the global coordinate
c        system. for plane stress elements and composites, the two vectors
c        should be in the plane of the element, normal to the thickness
c        direction. they will be projected if this is not the case.
c
c        for composites (except fast integrated) it is possible to either
c        define an element based orientation and use the ply angles defined
c        with the COMPOSITE option to define the orientation of each ply, or
c        to define the orientation of each layer. in the latter case the ply
c        angles are not used in the analysis.
c
c        it is possible to use different orientation in each integration point
c        and also in each layer (also for multilayered shell elements).
c        however, it is not possible to visualize them separately since an average
c        element orientation is used for post processing. separate orientations
c        for each layer of a composite can be visualized.
c
c
c  input
c
c     n(1)         user element number
c     n(2)         internal element number
c     nn           integration point number
c     kcus(1)      layer number - 1 if continuum element
c     kcus(2)      internal layer number - 1 if continuum element
c     material(1)  material id of element, or current layer if composite
c     material(2)  internal material id of element, or current layer if composite
c     matname      material name
c     icomp        composite group id. if not a composite icomp=0
c     nodes        element nodes for the current element
c     nnodes       number of element nodes for the current element
c     coord        coordinates of the element nodes
c     coordint     coordinates of the current integration point
c     ncoord       number of coordinate directions
c     dircos       direction cosines. the transformation between
c                  the element coordinate system and the global
c                  coordinate system.
c                  dircos(1,1:3) gives the global x axis in the element
c                                coordinate system
c                  dircos(1:3,1) gives the local x axis in the global
c                                coordinate system
c     icall        currently not used
c     ifast        set to 1 if fast integrated composite is used (no thermal)
c                  set to 2 if fast integrated composite is used (thermal)
c                  for ifast>0 the iply=1 case is not available
c                  since this routine is then not called for every layer
c     integer_data array with integer numbers. for future expansion
c     real_data    array with real    numbers. for future expansion
c
c
c  output
c
c     iply         if set to 1 in this routine then the ply angles
c                  for a composite is taken into account here.
c                  otherwise this is done outside this routine using
c                  the ply angles from the input.
c                  if used then all layers and all integration points
c                  of an element need to use it since this option
c                  is only available on a per element basis.
c
c     ilocal       if set to 1 in this routine then vec1 and vec2 below
c                  are given in the element coordinate system.
c                  otherwise they are in the global coordinate system.
c
c
c     vec1         vectors to define the material orientation.
c     vec2         the 1st preferred material direction is given by vec1.
c                  the 3rd direction is obtained as the cross
c                    product of vec1 and vec2.
c                  the 2nd direction is obtained as the cross
c                    product between the 3rd direction and vec1.
c
c                  the vectors are in the element local coordinate system
c                  if ilocal=1 and in the global coordinate system
c                  otherwise
c
c* * * * * *
#ifdef _IMPLICITNONE
      implicit none
#else
      implicit logical (a-z)
#endif
      integer kcus,n,nn,material,nodes,nnodes,ncoord,icall,iply,ilocal
      integer icomp,ifast,integer_data
      real*8 coord,dircos,vec1,vec2,coordint,real_data
      character*24 matname
      dimension n(2),kcus(2),material(2),nodes(*)
      dimension coord(ncoord,*),dircos(3,3),vec1(3),vec2(3)
      dimension coordint(*),integer_data(*),real_data(*)
      
      WRITE (6,*) '** ORIENT2 SUBROUTINE'
      SELECT CASE (n(1))
        CASE (1:2)
          vec1 = [1.D0, 0.D0, 0.D0]
          vec2 = [0.D0, 1.D0, 0.D0]
        CASE (3)
          vec1 = [+0.866D0,-0.500D0, 0.D0]
          vec2 = [-0.500D0,-0.866D0, 0.D0]
        CASE (4)
          vec1 = [+0.866D0, 0.500D0, 0.D0]
          vec2 = [-0.500D0, 0.866D0, 0.D0]
        CASE (5)
          vec1 = [+0.500D0,-0.866D0, 0.D0]
          vec2 = [-0.866D0,-0.500D0, 0.D0]
        CASE (6)
          vec1 = [+0.500D0, 0.866D0, 0.D0]
          vec2 = [-0.866D0, 0.500D0, 0.D0]
        CASE (7)
          vec1 = [0.D0, -1.D0, 0.D0]
          vec2 = [-1.D0, 0.D0, 0.D0]
        CASE (8)
          vec1 = [0.D0, 1.D0, 0.D0]
          vec2 = [1.D0, 0.D0, 0.D0]
      END SELECT
      ilocal = 0

      return
      end
