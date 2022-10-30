      subroutine usdata ( kin,kou,ic)
        IMPLICIT NONE
        REAL*8 :: emod, pois
        common /usdacm/emod, pois
        ! The following line gives the first value of the variable once it is executed and it is just a declartaion
        ! for the first time it is executed not that every time the subroution is running the value is assigned.
        data emod/0/,pois/0/
        ! common variables is defined here and in order to use in other subroutines the following commands must be used:
        ! common /usdacm/emod,pois
        ! I though the declaration must be used as well, I mean the part "real*8 emod,pois" but I think this part is not mandatory.
        write(kou,*) 'ic= ',ic
        write(kou,*) 'kin= ',kin
        write(kou,*) 'emod,pois = ',emod,pois
        
        if (ic.eq.2) then
          read(kin,*) emod,pois
        end if
        
        write(kou,*) '2-I am here'
        write(kou,*) 'ic= ',ic
        write(kou,*) 'kin= ',kin
        write(kou,*) 'emod,pois = ',emod,pois
        RETURN
      end
      
            ! This subroutine runs for each spring and each time for the spring number nsprng(1)
      subroutine usprng (ratk,f,datak,u,time,n,nn,nsprng)
        !include 'usprng.cbl'
        IMPLICIT NONE
c     ** Start of generated type statements **
        real*8 datak, f
        integer n, nn, nsprng
        real*8 ratk, time, u
c     ** End of generated type statements **
        dimension ratk(*),datak(*),u(*),time(*),n(*),f(2),
     *  nsprng(*)
c
        real*8 emod,pois
        common/usdacm/emod,pois
        
        integer, parameter :: fu = 40
        character(*) :: log_filename      
        parameter (log_filename='log.txt')
        real, parameter, dimension(3) :: delta=(/0.01,0.5,1./)
        real, parameter, dimension(3) :: stiff=(/10000.,5000.,25000./)   
        real myforce,disp,stif
        
        if (i==0) then
          Open(unit=fu,file=log_filename,status='unknown')
          close(unit=fu,status='delete')      
        end if 
        i=i+1
        Open(unit=fu,file=log_filename,status='unknown',access='append')
         
         
         disp=u(1)
         stif=stiff(1)
         myforce=disp*stif
               
         if ((disp.gt.delta(1)).and.(disp.le.delta(2))) then
          stif=stiff(2) 
         end if       
         if (disp.gt.delta(2)) then
          stif=stiff(3)       
         end if       
         if (nsprng(1).eq.4) then stif=stif/2.
         myforce=disp*stif
      
      ! This part is for debugging purposes.
        write(fu,*),i
        write(fu,*),'disp= ',disp
        write(fu,*),'stif= ',stif
        write(fu,*),'force=  ',myforce
        write(fu,*),'datak= ',datak(1)
        write(fu,*),'nsprng= ',nsprng(1)
        write(fu,*),'n1= ',n(1)
        write(fu,*),'n2= ',n(2)
        write(fu,*),'E=',emod,'Pois= ',pois
         
         ratk(1)=stif
         ratk(2)=0.
         f(1)=myforce

        close(fu) 
        return
      end
      
      subroutine uedinc (inc,incsub)
        IMPLICIT NONE
       write(6,20) 'inc#',inc,'subinc#',incsub
20    format(A,1X,i5,1X,A,i5)
        RETURN
      END
      
      