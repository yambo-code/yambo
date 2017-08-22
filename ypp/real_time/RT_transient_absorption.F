!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine RT_transient_absorption(Xbsk,Xen,Xk,Xq,BS_H_dim,BS_E,BS_R0)
 !
 use pars,            ONLY:SP,pi,cZERO
 use units,           ONLY:AUT2FS,HA2EV,SPEED_OF_LIGHT
 use R_lattice,       ONLY:bz_samp,q0_def_norm,d3k_factor,q_norm,bare_qpg
 use D_lattice,       ONLY:alat
 use electrons,       ONLY:levels,n_sp_pol,spin,spin_occ
 use LIVE_t,          ONLY:live_timing
 use X_m,             ONLY:X_t,DIP_projected
 use RT_control,      ONLY:RT_output,RT_apply
 use IO_m,            ONLY:io_DIP
 use YPP_real_time,   ONLY:RT_conf
 use X_m,             ONLY:Epsilon_ii
 use BS,              ONLY:BSS_eh_table,BS_mat,BS_bands
 use frequency,       ONLY:w_samp
 use real_time,       ONLY:RT_bands
 use parser_m,        ONLY:parser
 !
#include<memory.h>
 !
 type(X_t)         :: Xbsk
 type(levels)      :: Xen
 type(bz_samp)     :: Xk,Xq
 integer           :: BS_H_dim
 complex(SP)       :: BS_E(BS_H_dim),BS_R0(BS_H_dim)
 !
 ! Work space
 !
 type(w_samp)      :: W_bss
 !
 integer              :: n_T_steps,i_T,neh,i_v,i_c,i_kbz,i_kibz,i_spin,i_freq,i_l,i_vp,i_cp,io_err
 real(SP)             :: io_Time,Co,f_cv,f_vpv,f_ccp,delta_f_cv,output(4),BS_R_tmp
 complex(SP)          :: IP_E,IP_R,BS_R1(BS_H_dim)
 logical              :: l_TR_transm,l_TR_reflec,l_include_IP
 !
 real(SP),allocatable    :: X_absorption_coeff(:,:)
 complex(SP),allocatable :: X_reflectance(:,:)
 !
 ! Pre-setup of carrier components and Time evolution parameters
 !
 call RT_apply(Xbsk%ib,Xen,Xk)
 !
 ! Setup of Time-range & Report
 !
 !call RT_IO_carriers_setup(Xen,Xk,"TIME_RANGE REPORT",n_T_steps) 
 !
 call parser('Transmission',l_TR_transm)
 call parser('Reflectivity',l_TR_reflec)
 call parser('IncludeIP'   ,l_include_IP)
 !
 Xbsk%ib=BS_bands
 !
 call PARALLEL_global_indexes(Xen,Xk,Xq," ",RESET=.TRUE.)
 call PARALLEL_global_indexes(Xen,Xk,Xq,"DIPOLES")
 !
 io_DIP=.TRUE.
 call DIPOLE_IO(Xk,Xen,Xbsk,'read',io_err)
 call DIPOLE_expand_and_project(Xk,X_oscillators,BSS_q0)
 !
 YAMBO_ALLOC(Epsilon_ii,(RT_conf%W_n_steps,2))
 if(l_TR_reflec) then
   YAMBO_ALLOC(X_reflectance,(RT_conf%W_n_steps,2))
 endif
 if(l_TR_transm) then
   YAMBO_ALLOC(X_absorption_coeff,(RT_conf%W_n_steps,2))
 endif
 !
 ! Frequency setup
 !
 W_bss%er=RT_conf%W_range
 W_bss%n_freqs=RT_conf%W_n_steps
 W_bss%per_memstps=100._SP
 W_bss%dr=RT_conf%damp_factor
 !
 call FREQUENCIES_setup(W_bss)
 !
 ! Output
 !
 call RT_output(what="initialize")
 call RT_output("open trans_eps_real trans_eps_imag eps_eq trans_residual")
 if (l_TR_reflec) call RT_output("open trans_refl_rel trans_refl_abs refl_eq")
 if (l_TR_transm) call RT_output("open trans_transm_rel trans_transm_abs transm_eq")
 !
 Co=real(spin_occ)/(2._SP*pi)**3._SP*d3k_factor*4._SP*pi/q0_def_norm**2
 !
 do i_T=1,n_T_steps
   !
   io_Time=min(RT_conf%T_range(1)+(i_T-1)*RT_conf%delta_T,RT_conf%T_range(2))
   !
   call RT_apply(Xbsk%ib,Xen,Xk,TIME=io_Time)
   !
   if (i_T==1.) call live_timing('Transient Absorption',n_T_steps)
   !
   Epsilon_ii=cZERO
   !
   ! Compute the contribution due to exissting transitions
   !
   BS_R0=cZERO
   BS_R1=cZERO
   !
   do i_l=1,BS_H_dim
     !
     do neh = 1,BS_H_dim
       !
       i_kbz   = BSS_eh_table(neh,1)
       i_kibz  = Xk%sstar(i_kbz,1)
       i_v     = BSS_eh_table(neh,2)
       i_c     = BSS_eh_table(neh,3)
       i_spin  = spin(BSS_eh_table(neh,:))
       !
       if(i_v<RT_bands(1) .or. i_c>RT_bands(2) ) cycle
       !
       f_cv=      ( Xen%f(i_v,i_kibz,i_spin)- Xen%f(i_c,i_kibz,i_spin))/spin_occ
       delta_f_cv=(Xen%df(i_v,i_kibz,i_spin)-Xen%df(i_c,i_kibz,i_spin))/spin_occ
       !
       BS_R0(i_l)=BS_R0(i_l)-DIP_projected(i_c,i_v,i_kbz,i_spin)*BS_mat(neh,i_l)*sqrt(f_cv)
       BS_R1(i_l)=BS_R1(i_l)-DIP_projected(i_c,i_v,i_kbz,i_spin)*BS_mat(neh,i_l)*delta_f_cv/sqrt(f_cv)
       !
     enddo 
     !
     Epsilon_ii(:,1)=Epsilon_ii(:,1)-     BS_R0(i_l)*conjg(BS_R0(i_l))       /(W_bss%p(:)-BS_E(i_l))
     Epsilon_ii(:,2)=Epsilon_ii(:,2)-0.5*(BS_R1(i_l)*conjg(BS_R0(i_l))+conjg(BS_R1(i_l))*BS_R0(i_l))/(W_bss%p(:)-BS_E(i_l))
     !
     Epsilon_ii(:,1)=Epsilon_ii(:,1)+     BS_R0(i_l)*conjg(BS_R0(i_l))       /(W_bss%p(:)+BS_E(i_l))
     Epsilon_ii(:,2)=Epsilon_ii(:,2)+0.5*(BS_R1(i_l)*conjg(BS_R0(i_l))+conjg(BS_R1(i_l))*BS_R0(i_l))/(W_bss%p(:)+BS_E(i_l))
     !
     BS_R_tmp=-0.5*real((BS_R1(i_l)*conjg(BS_R0(i_l))+conjg(BS_R1(i_l))*BS_R0(i_l)))
     !
     if( RT_conf%W_range(1)<real(BS_E(i_l)) .and. real(BS_E(i_l))<RT_conf%W_range(2) ) call RT_output("trans_residual",&
&        VALUEs=(/BS_R_tmp/),E=real(BS_E(i_l))*HA2EV,TIME=io_Time*AUT2FS)
     !
   enddo
   !
   ! Now sum the contributions due to new transitions assuming the IP situation
   !
   if(l_include_IP) then
     do i_kbz=1,Xk%nbz
       i_kibz=Xk%sstar(i_kbz,1)
       do i_spin=1,n_sp_pol
         !
         ! First valence
         do i_v=RT_bands(1),Xen%nbf
           do i_vp=RT_bands(1),Xen%nbf
             !
             if(i_v==i_vp) cycle
             !
             f_vpv=-Xen%f(i_v,i_kibz,i_spin)*(Xen%df(i_vp,i_kibz,i_spin))/spin_occ/spin_occ
             IP_R=-conjg(DIP_projected(i_vp,i_v,i_kbz,i_spin))*sqrt(f_vpv)
             IP_E=Xen%E(i_vp,i_kibz,i_spin)-Xen%E(i_v,i_kibz,i_spin)
             ! Photo-induced absorption and stimulated emission in v-v'
             Epsilon_ii(:,2)=Epsilon_ii(:,2)-IP_R*conjg(IP_R)/(W_bss%p(:)-IP_E)
             Epsilon_ii(:,2)=Epsilon_ii(:,2)+IP_R*conjg(IP_R)/(W_bss%p(:)+IP_E)
             !
           enddo
         enddo
         !
         ! Then conduction
         do i_cp=Xen%nbf+1,RT_bands(2)
           do i_c=Xen%nbf+1,RT_bands(2)
             !
             if(i_c==i_cp) cycle
             !
             f_ccp= (Xen%df(i_cp,i_kibz,i_spin))*(spin_occ-Xen%f(i_c,i_kibz,i_spin))/spin_occ/spin_occ
             IP_R=-conjg(DIP_projected(i_c,i_cp,i_kbz,i_spin))*sqrt(f_ccp)
             IP_E=Xen%E(i_c,i_kibz,i_spin)-Xen%E(i_cp,i_kibz,i_spin)
             Epsilon_ii(:,2)=Epsilon_ii(:,2)-IP_R*conjg(IP_R)/(W_bss%p(:)-IP_E)
             Epsilon_ii(:,2)=Epsilon_ii(:,2)+IP_R*conjg(IP_R)/(W_bss%p(:)+IP_E)
             !
           enddo
         enddo
         !
       enddo
     enddo
   endif
   !
   Epsilon_ii(:,1)=1._SP+Epsilon_ii(:,1)*Co
   Epsilon_ii(:,2)=      Epsilon_ii(:,2)*Co
   !
   if(l_TR_reflec) then
     ! Reflectance is assumed for a 3D system
     ! r=(sqrt(eps)-1)/(sqrt(eps)+1)
     X_reflectance(:,1)=(sqrt(Epsilon_ii(:,1))-1._SP)/(sqrt(Epsilon_ii(:,1))+1._SP)
     ! der r/der eps = 1/(sqrt(eps)+1)**2/ sqrt(eps)
     X_reflectance(:,2)=Epsilon_ii(:,2)/(sqrt(Epsilon_ii(:,1))+1._SP)**2._SP/sqrt(Epsilon_ii(:,1)) 
   endif
   !
   if(l_TR_transm) then
     ! 3D systems
     ! alpha= 2/c aimag(sqrt(eps))
     !X_absorption_coeff(:,1)=1._SP-exp(-real(W_bss%p(:))/SPEED_OF_LIGHT*aimag(sqrt(Epsilon_ii(:,1)))*thickness)
     !X_absorption_coeff(:,2)=
     ! MAU Nano-letters: 2D systems
     X_absorption_coeff(:,1)=real(W_bss%p(:))/SPEED_OF_LIGHT*aimag(Epsilon_ii(:,1))*alat(3)
     X_absorption_coeff(:,2)=real(W_bss%p(:))/SPEED_OF_LIGHT*aimag(Epsilon_ii(:,2))*alat(3)
   endif
   !
   do i_freq=1,W_bss%n_freqs
     output(4)=real(Epsilon_ii(i_freq,1))
     output(3)=aimag(Epsilon_ii(i_freq,1))
     output(2)=aimag(Epsilon_ii(i_freq,2))
     output(1)=real(Epsilon_ii(i_freq,2))
     call RT_output("trans_eps_real",VALUEs=(/output(1)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time*AUT2FS)
     call RT_output("trans_eps_imag",VALUEs=(/output(2)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time*AUT2FS)
     if(i_T==1) call RT_output("eps_eq",VALUEs=(/output(3:4)/),E=real(W_bss%p(i_freq))*HA2EV)
     if(l_TR_reflec) then
       !  R=|r|^2
       output(3)=X_reflectance(i_freq,1)*conjg(X_reflectance(i_freq,1))
       !output(2)=X_reflectance(i_freq,2)*conjg(X_reflectance(i_freq,2))-output(3)
       output(2)=X_reflectance(i_freq,2)*conjg(X_reflectance(i_freq,1))+X_reflectance(i_freq,1)*conjg(X_reflectance(i_freq,2))
       output(1)=output(2)/output(3)
       call RT_output("trans_refl_rel",VALUEs=(/output(1)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time*AUT2FS)
       call RT_output("trans_refl_abs",VALUEs=(/output(2)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time*AUT2FS)
       if(i_T==1) call RT_output("refl_eq",VALUEs=(/output(3)/),E=real(W_bss%p(i_freq))*HA2EV)
     endif
     ! I need to check some factor
     if(l_TR_transm) then
       ! 3D systems
       !output(3)=1._SP-X_reflectance(i_freq,1)*conjg(X_reflectance(i_freq,1))-X_absorption_coeff(i_freq,1)
       !output(2)=-X_reflectance(i_freq,1)*conjg(X_reflectance(i_freq,2))-conjg(X_reflectance(i_freq,1))*X_reflectance(i_freq,2) &
       !         -X_absorption_coeff(i_freq,2)
       ! 2D systems (reflectance is assumed to be zero)
       output(3)=1._SP-X_absorption_coeff(i_freq,1)
       output(2)=-X_absorption_coeff(i_freq,2)
       output(1)=output(2)/output(3)
       call RT_output("trans_transm_rel",    VALUEs=(/output(1)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time*AUT2FS)
       call RT_output("trans_transm_abs",    VALUEs=(/output(2)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time*AUT2FS)
       if(i_T==1) call RT_output("transm_eq",VALUEs=(/output(3)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time*AUT2FS)
     endif
   enddo
   !
   call RT_output("space trans")
   !
   call live_timing(steps=1)
   !
 enddo
 !
 YAMBO_FREE(Epsilon_ii)
 YAMBO_FREE(X_reflectance)
 YAMBO_FREE(X_absorption_coeff)
 !
 call live_timing()
 !
 return
 !
end subroutine RT_transient_absorption
