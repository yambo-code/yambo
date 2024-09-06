!
! License-Identifier: GPL
!
! Copyright (C) 2024 the Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
subroutine RT_Runge_Kutta_Fehlberg(E)
 !
 use pars,           ONLY:SP,schlen,cI
 use real_time,      ONLY:RT_bands,RT_step,NE_tot_time,NE_i_time,RT_dyn_step,&
&                         RK_integrators,N_RK_integrators
 use stderr,         ONLY:STRING_same
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:nXkibz
 use fields,         ONLY:gauge_field,Efield,n_ext_fields,Efield_strength
 use units,          ONLY:AUT2FS,SPEED_OF_LIGHT
 use vec_operate,    only:normalize_v
 !
#include<memory.h>
 !
 type(levels)      :: E
 !
 ! Work Space 
 !
 type(gauge_field)   :: A
 real(SP)            :: Time_total,Damp,delta_E_max,delta_E
 real(SP),allocatable:: F(:),Time(:),Ef(:),dT_correction(:,:)
 integer             :: N_steps,it,i_f,ik,ib,ibp
 !
 ! .. Runge-Kutta
 integer                :: i_Selected_RK_int(4),n,m,i_int
 character(schlen)      :: Selected_RK_int(4)
 complex(SP),allocatable:: Y(:,:)
 real(SP)               :: T_eval
 complex(SP)            :: K(4),Y_RK(4)
 !
 call section('=','Time-Step evaluation via Runge-Kutta-Fehlber')
 !################################################################
 !
 ! Initial step & damping
 !------------------------
 RT_step=1./AUT2FS/1.E3_SP ! 1as
 RT_dyn_step=RT_step
 Damp=0._SP
 !Damp=100.0_SP/HA2EV/1000. ! 50 meV
 !Time_total=HBAR_eVfs/Damp/HA2EV/AUT2FS
 Time_total=NE_tot_time
 !
 ! Fields init
 !-------------
 do i_f=1,n_ext_fields
   if (STRING_same(Efield(i_f)%ef_name,'none')) cycle
   !Time_total=max(Time_total,4*Efield(i_f)%FWHM)
   Efield(i_f)%versor        =normalize_v(Efield(i_f)%versor)
   Efield(i_f)%amplitude     =Efield_strength(Efield(i_f)%intensity,'AU')
   Efield(i_f)%t_initial_indx=max(nint(Efield(i_f)%t_initial/RT_step)+1,2)
   Efield(i_f)%t_initial     =real(Efield(i_f)%t_initial_indx-1)*RT_step
 enddo
 !
 ! Step & Allocs
 !---------------
 N_steps=Time_Total/RT_step
 YAMBO_ALLOC(F,(N_steps+1))
 YAMBO_ALLOC(Time,(N_steps+1))
 YAMBO_ALLOC(Ef,(N_steps+1))
 YAMBO_ALLOC(dT_correction,(3,N_steps+1))
 YAMBO_ALLOC(Y,(4,N_steps+1))
 Y=0._SP
 dT_correction=0._SP
 !
 ! Elemental frequencies
 !=======================
 !
 ! .. Electronic transitions
 !----------------------------
 delta_E_max=-1.
 do ik=1,nXkibz
   do ib=RT_bands(1),RT_bands(2)
     do ibp=RT_bands(1),RT_bands(2)
       if (ib==ibp) cycle
       delta_E=abs(E%E(ib,ik,1)-E%E(ibp,ik,1))
       delta_E_max=max(delta_E,delta_E_max)
     enddo
   enddo
 enddo 
 !
 ! .. Efield
 !-----------
 NE_i_time=0
 do it=1,N_steps+1
   Time(it)=RT_step*(it-1)
   NE_i_time=NE_i_time+1
   call RT_Ext_fields(A,Time(it),-1)
   Ef(it)=dot_product(real(A%vecpot_vel),(/1.,1.,1./))/SPEED_OF_LIGHT/sqrt(3._SP)
 enddo
 !
 ! RK integrators
 !----------------
 Selected_RK_int(1)  ='EULER'
 Selected_RK_int(2:3)=(/'RK2','RK3'/)
 Selected_RK_int(4)  ='RK41'
 !
 call Runge_Kutta_integrators(.FALSE.)
 !
 do it=1,N_RK_integrators
   do n=1,4
     if (STRING_same(Selected_RK_int(n),RK_integrators(it)%short)) i_Selected_RK_int(n)=it
   enddo
 enddo
 !
 ! RK integrations
 !-----------------
 do i_int=1,4
   !
   Y(i_int,1)=1._SP
   !
   do it=1,N_steps
     !
     Y_RK=0._SP
     K   =0._SP
     !
     do n=1,RK_integrators( i_Selected_RK_int(i_int) )%N
       !
       Y_RK(n)=Y(i_int,it)
       if (n>1) then
         do m=1,n-1
           Y_RK(n)=Y_RK(n)+RT_step*RK_integrators( i_Selected_RK_int(i_int) )%a(n,m)*K(m)
         enddo
       endif
       !
       T_eval=Time(it)+RK_integrators( i_Selected_RK_int(i_int) )%c(n)*RT_step
       K(n) =-cI*S(T_eval)*Y_RK(n)
       !
     enddo
     !
     Y(i_int,it+1)=Y(i_int,it)
     do n=1,RK_integrators( i_Selected_RK_int(i_int) )%N
       Y(i_int,it+1)=Y(i_int,it+1)+RK_integrators( i_Selected_RK_int(i_int) )%b(n)*K(n)*RT_step
     enddo
     !
   enddo
   !
 enddo
 !
 do it=1,N_steps
   do i_int=1,3
     dT_correction(i_int,it)=abs( Y(i_int,it)/ (  Y(i_int+1,it)- Y(i_int,it) ) )
   enddo
   write (200,*) it,dT_correction(:,it)
 enddo
 !
 do it=1,N_steps+1 
   write (100,'(10f30.10)') Time(it)*AUT2FS,aimag(Y(:4,it))
 enddo
 stop
 !
 contains
   !
   real(SP) function S(T)
     !
     real(SP)::T
     integer ::itp
     !
     do itp=1,N_steps+1
       if (T>=Time(itp).and.T<Time(itp+1)) then
         S=Ef(itp)*sin(delta_E_max*Time(itp))*exp(-Damp*Time(itp))
       endif
     enddo
     !
   end function
   !
end subroutine RT_Runge_Kutta_Fehlberg
