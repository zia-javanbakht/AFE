      subroutine ubreakglue(t1,t2,signorm,sigtan,sn,st,
     $       expn,expt,info,ibodc,ibodt,inc,time,timeinc)
c
c---------------------------------------------------------------------
c user subroutine for defining the breaking glue criterion
c
c   glued contact will break  if t1+t2 > 1.0
c
c this routine is called for each point in glued contact where the
c breaking glue option is used.
c
c    t1  -  first  term in breaking criterion
c    t2  -  second term in breaking criterion
c t1 and t2 are upon entry calculated using the standard criterion
c and can be modified in this routine
c
c    t1    will appear on the post file as nodal post code 66, Breaking Index (Normal)
c    t2    will appear on the post file as nodal post code 67, Breaking Index (Tangential)
c    t1+t2 will appear on the post file as nodal post code 68, Breaking Index
c
c   signorm  - normal     stress at point
c   sigtan   - tangential stress at point
c
c   sn   - user input limit normal     stress (from contact table option)
c   st   - user input limit tangential stress (from contact table option)
c   expn - user input exponent for first  term
c   expt - user input exponent for second term
c      these four parameters have been scaled if table variation is used
c
c   info - node    to segment contact :(1) = current user node id
c          segment to segment contact :(1) = element id of ibodc         
c                                      (2) = face of this element     
c                                      (3) = element id of ibodt         
c                                      (4) = face of this element     
c
c   ibodc- body number that this point belongs to, i.e. contacting body
c   ibodt- touched body number
c   inc  - current increment number
c   time - time at the begining of increment
c   timeinc - is the time increment
c
c
c   the following calculation has been done prior to calling this routine:
c      if (sn.gt.0.d0.and.signorm.gt.0.d0) then
c        t1=(signorm/sn)**expn
c      else
c        t1=0.d0
c      endif
c      if (st.gt.0.d0) then
c        t2=(abs(sigtan)/st)**expt
c      else
c        t2=0.d0
c      endif
c
c---------------------------------------------------------------------


#ifdef _IMPLICITNONE
      implicit none
#else
      implicit logical (a-z)
#endif

      real*8 t1,t2,signorm,sigtan,sn,st,expn,expt,time,timeinc
      integer info,inc,ibodc,ibodt
c
      dimension info(*)
      
      WRITE (6,*) '** UBREAKGLUE'
      WRITE (6,*) 'Node ID           :', info(1)
      WRITE (6,*) 'Face              :', info(2)
      WRITE (6,*) 'Normal stress     :', signorm
      WRITE (6,*) 'sn                :', sn
      WRITE (6,*) 't1                :', t1
      WRITE (6,*) 'expn              :', expn
      WRITE (6,*) 'Tangential stress :', sigtan
      WRITE (6,*) 'st                :', st
      WRITE (6,*) 't2                :', t2
      WRITE (6,*) 'expt              :', expt
      WRITE (6,*) 'Touching body     :', ibodt
      WRITE (6,*) 'Contact body      :', ibodc
      
!      GOTO 99
      IF (SN .GT. 0.D0 .AND. signorm .GT. 0.D0) THEN
        SELECT CASE (info(1))
          CASE (22)
            sn = 3.D0
          CASE (21)
            sn = 5.D0
          CASE (19)
            sn = 15.D0
          CASE (17)
            sn = 30.D0
          CASE (15)
            sn = 35.D0
          CASE (13)
            sn = 40.D0
        END SELECT
        t1=(signorm/sn)**expn
              WRITE (6,*) 'new t1            :', t1
      ELSE
        t1 = 0.D0
      END IF

99    CONTINUE
      return
      end
