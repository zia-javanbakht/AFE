c
c  The user subroutine permits the introduction of further modification of
c  non-linear spring constants for use with the SPRINGS and/or FOUNDATION
c  options and input of nonlinear dampings.
c    
c  Before coding this subroutine, User should refer to the detailed description 
c  in the MSC.Marc manual volume D regarding the usage of this subroutine.
c
c  Input:
c     datak(1)  --- the data value if spring constant (or foundation stiffness),
c                   as defined by the user in SPRINGS/FOUNDATION options data 
c                   input.
c                   (a) For springs: it is the mechanical stiffness.
c                   (b) For thermal links: it is the thermal conduction.
c                   (c) For electrical links: it is electrical conduction.
c     datak(2)  --- the data value of the damping constant as defined in the
c                   SPRINGS option data input.
c
c     For Elastic Foundation (only static contributions):
c
c     u(1)      --- for elastic foundation: u(1)=U_n, (positvie in the
c                   direction specified by face identification given in the
c                   FOUNDATION option).
c     u(2)-u(4) --- not used.
c
c     For Springs/Dashpots (static and/or dynamic contribution):
c
c     u(1)      --- for mechanical springs: u(1) = U_2-U_1
c                   for thermal links:      u(1) = T_2-T_1
c                   for electrical links:   u(1) = V_2-V_1
c     u(2)      --- for dynamic spring/dashpot U(2) = Udot_2-Udot_1
c     u(3)      --- for mechanical spring in coupled analysis and for electrical
c                   links in Joule heating analysis: u(3) = average temperature
c                   of spring or it is not used.
c     u(4)      --- not used.
c
c     For springs/Dashpot (harmonic analysis):
c
c     u(1)      --- u(1) = U_2-U_1 static predeformation.
c     u(2)      --- not used.
c     u(3)      --- u(3) = U_2-U_1 real part of harmonic deformation.
c     u(4)      --- u(4) = U_2-U_1 imaginary part of harmonic deformation.
c
c     time(1)   --- the total time (for dynamic or creep analysis).
c     time(2)   --- the frequency (for harmonic analysis with springs/dashpots).
c
c     n(1)      --- the element number (for elastic foundation).
c                   the first user-node number (for spring).
c     n(2)      --- the face number (for elastic foundation).
c                   the second user-node number (for spring).
c     nn        --- the integration point number (only for elastic foundaton).
c
c     nsprng(1) --- the spring number, the position of the spring in the input
c                   data list (only for springs).
c     nsprng(2) --- = 1, mechanical analysis or stress part of coupled analsis
c                       (only for springs).
c                   = 2, heat transfer analysis or thermal part of coupled
c                        analysis (only for springs).
c                   = 3, electrical analysis (only for springs).
c     nsprng(3) --- user spring id
c
c  Required output:
c     ratk(1)   --- the ratio of the present value if spring stiffness to the
c                   data value given in the option input; to be defined by the
c                   user.
c     ratk(2)   --- the ratio of the present value of the damping coefficient
c                   to the data value given in the input; to be defined by the
c                   user. This applies to SPRINGS in the dynamic analysis only.
c     f(1)      --- the force to be defined by the user (only needed for
c                   mechanical analysis).
c                   (a) For springs: f(1) = spring force.
c                   (b) For elastic foundation: f(1) = pressure per unit area.
c                   (c) For harmonics: f(1) = real part of harmonic force.
c     f(2)      --- the force to be defined by user (only needed for mechanical
c                   analysis).
c                   (a) For springs: f(2) = the damping force.
c                   (b) For harmonics: f(2) = imaginary part of the harminc force.
c
      MODULE CommonData
        IMPLICIT NONE
        
        INTEGER, PARAMETER :: SPRINGS = 5
        REAL*8, DIMENSION (SPRINGS) :: SpringForces
      END MODULE CommonData
      
      subroutine usprng(ratk,f,datak,u,time,n,nn,nsprng)
        USE CommonData
  
        IMPLICIT NONE
      
c     ** Start of generated type statements **
        REAL*8 datak, f
        INTEGER n, nn, nsprng
        REAL*8 ratk, time, u
c     ** End of generated type statements **
        DIMENSION ratk(*),datak(*),u(*),time(*),n(*),f(2),
     *  nsprng(*)
          REAL*8 :: k = 50.D0

          WRITE (6,*), '** USPRNG'
          WRITE (6,*), 'Spring Number         :', nsprng(1)
          WRITE (6,*), 'Spring Constant       :', datak(1)
          WRITE (6,*), 'Relative Displacement :', u(1)

          ratk(1) = k
          f(1) = k*u(1)
          
!          ratk(1) = (-5.D0*u(1)) + 22.5D0
!          f(1) = -2.5D0*(u(1)**2.D0) + 22.5D0*u(1)
          
          SpringForces(nsprng(1)) = f(1)
          
          RETURN
        END
        
      subroutine uedinc (inc,incsub)
        USE CommonData

        IMPLICIT NONE
c     ** Start of generated type statements **
        INTEGER inc, incsub
c     ** End of generated type statements **
        INTEGER, PARAMETER :: fileUnit = 10
        INTEGER :: i
        CHARACTER(LEN=250), PARAMETER :: fileName = 'Results.txt'
        LOGICAL :: fileExist, fileEnded, unitConnected
        

        IF (inc .EQ. 0) THEN
          INQUIRE (UNIT = fileUnit, OPENED = unitConnected)
          IF (unitConnected .EQV. .FALSE.) THEN
            INQUIRE (FILE = fileName, EXIST=fileExist)
            IF (fileExist .EQV. .TRUE.) THEN
              OPEN (UNIT = fileUnit, FILE = fileName, STATUS = 'OLD')
              CLOSE (UNIT = fileUnit, STATUS = 'DELETE')
            END IF
          END IF
          OPEN (UNIT = fileUnit, File = fileName, ACCESS = 'SEQUENTIAL',
     &         STATUS = 'NEW', ACTION = 'READWRITE', FORM = 'FORMATTED')
          WRITE (fileUnit, 102)
          WRITE (fileUnit, 100)
          WRITE (fileUnit, 102)
        ELSE
          WRITE (fileUnit,101) inc, (SpringForces(i), i = 1, SPRINGS)
          IF (inc .EQ. 10) THEN
            WRITE (fileUnit, 102)
            CLOSE (UNIT = fileUnit)
          END IF
        END IF 
        RETURN
100   FORMAT ('INC Spring1 Spring2 Spring3 Spring4 Spring5')
101   FORMAT (I3,1X,5(F7.4,1X))
102   FORMAT (44('-'))
      END
      
      
