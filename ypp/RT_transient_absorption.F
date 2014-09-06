!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine RT_transient_absorption(Xbsk,Xen,Xk,Xq,BS_H_dim,BS_E,BS_R)
 !
 use pars,            ONLY:SP,pi
 use units,           ONLY:FS2AUT,HA2EV
 use R_lattice,       ONLY:bz_samp,q0_def_norm,d3k_factor,q_norm,bare_qpg
 use electrons,       ONLY:levels,n_sp_pol,spin,spin_occ
 use LIVE_t,          ONLY:live_timing
 use X_m,             ONLY:X_t,DIP_q_dot_iR
 use rt_ctl,          ONLY:RT_OCC_LIFE_and_REF_IO,RT_output
 use real_time,       ONLY:rt
 use wave_func,       ONLY:wf_ng,wf_norm_test
 use IO_m,            ONLY:io_DIP
 use YPP,             ONLY:OCC_T_range,OCC_deltaT,X_rt
 use X_m,             ONLY:X_epsilon
 use BS,              ONLY:BSS_eh_table
 use frequency,       ONLY:w_samp
 use interfaces,      ONLY:PARALLEL_global_indexes
 use parallel_m,      ONLY:PAR_Xk_bz_index
 use SC,              ONLY:SC_bands,SC_QP_setup
 use parser_m,        ONLY:parser
 !
 implicit none
 !
 type(X_t)         :: Xbsk
 type(levels)      :: Xen
 type(bz_samp)     :: Xk,Xq
 integer           :: BS_H_dim
 complex(SP)       :: BS_E(BS_H_dim),BS_R(BS_H_dim),IP_E,IP_R,Wd
 !
 ! Work space
 !
 type(W_samp)      :: W_bss
 !
 integer              :: n_T_steps,i_T,neh,i_v,i_c,i_kbz,i_kibz,i_kmem,i_spin,i_freq
 character(3)         :: message
 real(SP)             :: io_Time,Co,change_factor,f_cv,delta_f_cv,output(3)
 logical              :: l_TR_transm,l_TR_reflec
 !
 real(SP),allocatable    :: delta_f(:,:,:)
 complex(SP),allocatable :: X_reflectance(:,:)
 !
 call RT_variables_load(Xen,Xk,n_T_steps)
 !
 ! I need to compute the dipoles, however they rely on the parallel structure
 ! I would need a structure defined for ypp 
 !
 call parser('Transmission',l_TR_transm)
 call parser('Reflectivity',l_TR_reflec)
 !
 Xbsk%ib=SC_bands
 !Xbsk%ng=wf_ng
 Xbsk%whoami=5
 allocate(q_norm(1),bare_qpg(1,1))
 !call SC_QP_setup(Xen)
 !
 call PARALLEL_global_indexes(Xen,Xk,Xq," ",        RESET=.TRUE. ,X=Xbsk)
 call PARALLEL_global_indexes(Xen,Xk,Xq,"Real_Time",RESET=.FALSE.,X=Xbsk)
 !
 io_DIP=.FALSE.
 call Dipole_driver(Xen,Xk,Xbsk,Xbsk%q0)
 !
 deallocate(q_norm,bare_qpg)
 !
 allocate(delta_f(rt%nb(1):rt%nb(2),Xk%nibz,n_sp_pol))
 allocate(X_epsilon(2,X_rt%energy_steps))
 if(l_TR_transm.or.l_TR_reflec) allocate(X_reflectance(2,X_rt%energy_steps))
 !
 W_bss%er(1)=X_rt%energy_range(1)
 W_bss%er(2)=X_rt%energy_range(2)
 !
 W_bss%n_freqs=X_rt%energy_steps
 W_bss%per_memstps=100._SP
 !
 W_bss%dr(1)=X_rt%damp_factor
 W_bss%dr(2)=X_rt%damp_factor
 !
 call FREQUENCIES_setup(W_bss)
 !
 call RT_output(what="initialize")
 call RT_output("open trans_eps_rel trans_eps_abs trans_eps_eq")
 if (l_TR_reflec) call RT_output("open trans_refl_rel trans_refl_abs trans_refl_eq")
 !if(l_TR_transm) call RT_output("open trans_transm1 trans_transm2 trans_transm3")
 !
 Co=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/q0_def_norm**2
 !
 if (n_T_steps> 1) call live_timing('Transient Absorption',n_T_steps,SERIAL=.true.)
 !
 do i_T=1,n_T_steps
   !
   io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   !
   call RT_OCC_LIFE_and_REF_IO(Xen,'G',k=Xk,Time=io_Time)
   !
   call RT_alloc_def_or_free(rt,Xen,Xk,2)
   !
   message='RT '
   if(i_T==1) message='RT0'
   !
   call Nearest_kpt_interpolation(rt%nk,Xk%nibz,rt%nb,rt%nstates,1,1,rt%table,rt%k,Xk%pt,rt%df,delta_f,message)
   !
   X_epsilon=(0._SP,0._SP)
   !
   ! Compute the contribution due to exissting transitions
   !
   do neh = 1,BS_H_dim
     !
     i_kbz   = BSS_eh_table(neh,1)
     i_kibz  = Xk%sstar(i_kbz,1)
     i_v     = BSS_eh_table(neh,2)
     i_c     = BSS_eh_table(neh,3)
     i_spin  = spin(BSS_eh_table(neh,:))
     !
     if(i_v<rt%nb(1) .or. i_c>rt%nb(2) ) cycle
     !
     f_cv=Xen%f(i_v,i_kibz,i_spin)*(spin_occ-Xen%f(i_c,i_kibz,i_spin))/spin_occ/spin_occ
     !
     delta_f_cv=delta_f(i_v,i_kibz,i_spin)*(spin_occ-Xen%f(i_c,i_kibz,i_spin))/spin_occ/spin_occ- &
&               Xen%f(i_v,i_kibz,i_spin)*          delta_f(i_c,i_kibz,i_spin) /spin_occ/spin_occ
     !
     change_factor=0._SP
     if(f_cv>0) change_factor=0.5*delta_f_cv/f_cv
     !
     X_epsilon(1,:)=X_epsilon(1,:)-                 BS_R(neh)*conjg(BS_R(neh))/(W_bss%p(:)-BS_E(neh))
     X_epsilon(2,:)=X_epsilon(2,:)-2.*change_factor*BS_R(neh)*conjg(BS_R(neh))/(W_bss%p(:)-BS_E(neh))
     !
   enddo
   !
   ! Now sum the contributions due to new transitions assuming the IP stuation
   do i_kbz=1,Xk%nbz
     i_kibz=Xk%sstar(i_kbz,1)
     i_kmem= PAR_Xk_bz_index(i_kbz)
     do i_spin=1,n_sp_pol
       !
       ! First valence
       do i_v=rt%nb(1),Xen%nbf
         do i_c=rt%nb(1),Xen%nbf
           !
           if(i_v==i_c) cycle
           !
           f_cv=-Xen%f(i_v,i_kibz,i_spin)*delta_f(i_c,i_kibz,i_spin)/spin_occ/spin_occ
           IP_R=-conjg(DIP_q_dot_iR(i_c,i_v,i_kmem,i_spin))*sqrt(f_cv)
           IP_E=Xen%E(i_c,i_kibz,i_spin)-Xen%E(i_v,i_kibz,i_spin)
           X_epsilon(2,:)=X_epsilon(2,:)-IP_R*conjg(IP_R)/(W_bss%p(:)-IP_E)
           X_epsilon(2,:)=X_epsilon(2,:)+IP_R*conjg(IP_R)/(W_bss%p(:)+IP_E)
           !
         enddo
       enddo
       !
       ! Then conduction
       do i_v=Xen%nbf+1,rt%nb(2)
         do i_c=Xen%nbf+1,rt%nb(2)
           !
           if(i_v==i_c) cycle
           !
           f_cv= delta_f(i_v,i_kibz,i_spin)*(spin_occ-Xen%f(i_c,i_kibz,i_spin))/spin_occ/spin_occ
           IP_R=-conjg(DIP_q_dot_iR(i_c,i_v,i_kmem,i_spin))*sqrt(f_cv)
           IP_E=Xen%E(i_c,i_kibz,i_spin)-Xen%E(i_v,i_kibz,i_spin)
           X_epsilon(2,:)=X_epsilon(2,:)-IP_R*conjg(IP_R)/(W_bss%p(:)-IP_E)
           X_epsilon(2,:)=X_epsilon(2,:)+IP_R*conjg(IP_R)/(W_bss%p(:)+IP_E)
           !
         enddo
       enddo
       !
     enddo
   enddo
   !
   X_epsilon(1,:)=1.+X_epsilon(1,:)*Co
   X_epsilon(2,:)=   X_epsilon(2,:)*Co
   !
   if(l_TR_reflec.or.l_TR_transm) then
     X_reflectance(1,:)=(sqrt(X_epsilon(1,:))-1)/(sqrt(X_epsilon(1,:))+1)
     !X_reflectance(2,:)=(sqrt(X_epsilon(1,:)+X_epsilon(2,:))-1)/(sqrt((X_epsilon(1,:)+X_epsilon(2,:)))+1)
     X_reflectance(2,:)=0.5_SP*X_epsilon(2,:)*(1._SP/(X_epsilon(1,:)+sqrt(X_epsilon(2,:))) - &
&            (sqrt(X_epsilon(1,:))-1)/(sqrt(X_epsilon(1,:))+1)**2._SP/sqrt(X_epsilon(1,:)) )
   endif
   !
   do i_freq=1,W_bss%n_freqs
     output(1)=aimag(X_epsilon(2,i_freq))/aimag(X_epsilon(1,i_freq))
     output(2)=aimag(X_epsilon(2,i_freq))
     output(3)=aimag(X_epsilon(1,i_freq))
     call RT_output("trans_eps_rel",VALUEs=(/output(1)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time/FS2AUT)
     call RT_output("trans_eps_abs",VALUEs=(/output(2)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time/FS2AUT)
     if(i_T==1) call RT_output("trans_eps_eq",VALUEs=(/output(3)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time/FS2AUT)
     if(l_TR_reflec) then
       output(3)=X_reflectance(1,i_freq)*conjg(X_reflectance(1,i_freq))
       !output(2)=X_reflectance(2,i_freq)*conjg(X_reflectance(2,i_freq))-output(3)
       output(2)=X_reflectance(2,i_freq)*conjg(X_reflectance(1,i_freq))+X_reflectance(1,i_freq)*conjg(X_reflectance(2,i_freq))
       output(1)=output(2)/output(3)
       call RT_output("trans_refl_rel",VALUEs=(/output(1)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time/FS2AUT)
       call RT_output("trans_refl_abs",VALUEs=(/output(2)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time/FS2AUT)
       if(i_T==1) call RT_output("trans_refl_eq",VALUEs=(/output(3)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time/FS2AUT)
     endif
     ! I need to check some factor
     !if(l_TR_transm) then
     !  output(3)=1-X_reflectance(1,i_freq)*conjg(X_reflectance(1,i_freq))-aimag(X_epsilon(1,i_freq))
     !  output(2)=1-X_reflectance(1,i_freq)*conjg(X_reflectance(1,i_freq))-aimag(X_epsilon(1,i_freq)+X_epsilon(2,i_freq))
     !  output(1)=output(2)/output(3)
     !  call RT_output("trans_transm_rel",VALUEs=(/output(1)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time/FS2AUT)
     !  call RT_output("trans_transm_abs",VALUEs=(/output(2)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time/FS2AUT)
     !  call RT_output("trans_transm_eq",VALUEs=(/output(3)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=io_Time/FS2AUT)
     !endif
   enddo
   !
   call RT_output("space trans")
   !
   call live_timing(steps=1)   
   !
 enddo
 !
 call RT_alloc_def_or_free(rt,Xen,Xk,3)
 !
 deallocate(delta_f)
 !
 call live_timing()
 !
 return
 !
end subroutine RT_transient_absorption

