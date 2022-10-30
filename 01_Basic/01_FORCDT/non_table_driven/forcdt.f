      subroutine forcdt(u,v,a,dp,du,time,dtime,ndeg,node,
     *                  ug,xord,ncrd,iacflg,inc,ipass)
c
c... Input of time dependent nodal quantities:
c...   -displacement, acceleration, velocity or force in a stress
c...    analysis
c...   -temperature or flux in a heat transfer analysis
c...   -current or voltage in the electrical pass of a Joule heating
c...    analysis
c...   -mass flux or pressure in a hydrodynamic bearing analysis
c...   -pressure or source in an acoustic analysis
c...   -charge or potential in an electrostatic analysis
c...   -current or potential in a magnetostatic analysis
c...   -current/charge or potential in an electromagnetic analysis
c...   -velocity or force in a fluid analysis
c... This routine is called each iteration and for each nodal point 
c... defined to use this routine
c... For non-table input this is done with the model definition 
c... option FORCDT.
c... For table input this is done with the POINT LOAD or 
c... FIXED DISP option
c... Please note that the values returned from this routine will
c... only be used in iteration 1 in the case of displacement, 
c... acceleration or velocity loading
c...
c... Some variables have a general meaning, others have a meaning
c... depending on the analysis type
c...
c...
c... The variables dp, du and iacflg can be defined in this routine.
c... The other variables are input variables.
c...
c... Variables with a general meaning:
c...     dtime  increment of time
c...     ndeg   number of degrees of freedom
c...     node   global node number
c...     xord   original nodal coordinates
c...     ncrd   number of coordinates per node
c...     inc    increment number
c...
c... Variables with a meaning depending on the analysis type:
c...     u      -stress analysis:
c...              total displacement
c...            -fluid analysis:
c...              velocity (fluid only; penalty formulation)
c...              velocity, pressure (fluid only; mixed formulation)
c...              velocity, temperature (fluid-thermal; penalty
c...              formulation)
c...              velocity, pressure, temperature (fluid-thermal;
c...              mixed formulation; strong coupling)
c...            -otherwise not used
c...     v      -stress analysis:
c...              total velocity (dynamics only)
c...            -otherwise not used
c...     a      -stress analysis:
c...              total acceleration (dynamics only)
c...            -heat transfer analysis:
c...              derivative of point flux with respect to temperature
c...              (to be defined optionally at degrees of freedom
c...               without temperature boundary conditions; this might
c...               improve the convergence behavior)
c...            -otherwise not used
c...     dp     -stress analysis:
c...              incremental point load; can be defined at degrees of
c...              freedom without kinematic boundary conditions
c...            -heat transfer analysis:
c...              external flux; can be defined at degrees of freedom
c...              without temperature boundary conditions
c...            -electrical part of a Joule heating analysis:
c...              external current; can be defined at degrees of freedom
c...              without voltage boundary conditions
c...            -hydrodynamic bearing analysis:
c...              mass flux; can be defined at degrees of freedom
c...              without pressure boundary conditions
c...            -acoustic analysis:
c...              external source; can be defined at degrees of freedom
c...              without pressure boundary conditions
c...            -electrostatic analysis:
c...              external charge; can be defined at degrees of freedom
c...              without potential boundary conditions
c...            -magnetostatic analysis:
c...              external current; can be defined at degrees of freedom
c...              without potential boundary conditions
c...            -electromagnetic analysis:
c...              external current/charge; can be defined at degrees of
c...              freedom without potential boundary conditions
c...            -fluid analysis:
c...              total point load; can be defined at degrees of
c...              freedom without kinematic boundary conditions
c...              (in case of strong coupling, also the total flux can
c...               be defined if the temperature degree of freedom has
c...               no boundary condition)
c...     du     -stress analysis:
c...              incremental displacement, or total acceleration or
c...              total velocity or total displacements 
c..                  (see definition of iacflg below)  
c...              can be defined at degrees of freedom with kinematic
c...              boundary conditions
c...            -heat transfer analysis:
c...              total temperature; can be defined at degrees of
c...              freedom with temperature boundary conditions (the
c...              value of du passed as input is the estimated
c...              temperature)
c...            -electrical part of a Joule heating analysis:
c...              total voltage; can be defined at degrees of freedom
c...              with voltage boundary conditions
c...            -hydrodynamic bearing or acoustic analysis:
c...              total pressure; can be defined at degrees of freedom
c...              with pressure boundary conditions
c...            -electrostatic, magnetostatic or electromagnetic
c...             analysis:
c...              total potential; can be defined at degrees of freedom
c...              with potential boundary conditions
c...            -fluid analysis:
c...              velocity; can be defined at degrees of freedom with
c...              velocity boundary conditions
c...              (in case of strong coupling, also the temperature can
c...               be defined if the temperature degree of freedom has
c...               a boundary condition)
c...     time   -stress, transient acoustic, transient electromagnetic
c...             or transient fluid analysis:
c...              total time at the beginning of the increment
c...            -heat transfer analysis:
c...              total time at the end of the increment
c...            -otherwise not used
c...     ug     -stress analysis:
c...              total displacement in the global system
c...            -otherwise not used
c...     iacflg -stress analysis:
c..        For non-table input 
c...             must be defined as 1 if accelerations are prescribed
c...             and as 2 if velocities are prescribed in a dynamic
c...             analysis
c...            -otherwise not used
c...       For table input 
c...             set to 0  if user returns incremental displacement of the increment in du
c                set to -1 if user returns total displacement at the end of the increment in du
c...     ipass   0: conventional analysis
c...             1: stress portion of coupled analysis
c...             2: heat portion of coupled analysis
c...             3: fluid pass of a coupled fluid structural analysis
c...             4: electrical part of a Joule heating analysis
c
#ifdef _IMPLICITNONE
      implicit none
#else
      implicit logical (a-z)
#endif
c     ** Start of generated type statements **
      real*8 a, dp, dtime, du
      integer iacflg, inc, ipass, ncrd, ndeg, node
      real*8 time, u, ug, v, xord
c     ** End of generated type statements **
      dimension u(ndeg),v(ndeg),a(ndeg),dp(ndeg),du(ndeg),ug(ndeg),
     *          xord(ncrd)
c
      iacflg = 0
!      du = [0.0, -10.0, 0.0]
      write (6,*) 'dtime=',dtime
      write (6,*) 'time =',time
      
      IF (inc .EQ. node) THEN
	      dp = [0.0, -10.0, 0.0]
      END IF
      
      return
      end
