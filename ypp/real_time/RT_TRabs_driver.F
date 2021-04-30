!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_transient_absorption(Xen,Xk,Xq)
 !
 use pars,            ONLY:SP,pi,cZERO,rZERO
 use R_lattice,       ONLY:bz_samp,q0_def_norm,d3k_factor
 use electrons,       ONLY:levels,spin,spin_occ
 use LIVE_t,          ONLY:live_timing
 use vec_operate,     ONLY:v_norm,v_rotate
 use RT_control,      ONLY:RT_apply,CARR_RT_IO_t
 use YPP_real_time,   ONLY:RT_conf,RT_time,TRabsDIP_dir,TRabsWHAT,TRabsDIP_plane,&
&                          TRabs_RES_left,TRabs_RES_right,TRabsMODE,TRabs_E
 use YPPm,            ONLY:DIPs
 use X_m,             ONLY:Epsilon_ii,global_gauge
 use IO_m,            ONLY:LOG
 use BS,              ONLY:BS_bands,BS_H_dim
 use frequency,       ONLY:w_samp
 use parallel_int,    ONLY:PARALLEL_global_indexes
 use real_time,       ONLY:RT_bands,l_TRabs_YPP
 use stderr,          ONLY:STRING_match,STRING_same
 use com,             ONLY:msg
 !
#include<memory.h>
 !
 type(levels)      :: Xen
 type(bz_samp)     :: Xk,Xq
 !
 ! Work space
 !
 type(w_samp)            :: W_bss
 integer                 :: N_cv_trans,N_cc_trans,N_vv_trans,N_trans,N_trans_done,i_T,io_err,i_trans,&
&                           N_col,nbf_eq,N_dirs,i_dir
 real(SP)                :: DIP_dir(4,3),Co
 complex(SP)             :: RES
 logical                 :: l_IP,l_BS,l_OFF,l_cv,l_cc,l_vv
 !
 l_TRabs_YPP=.TRUE.
 !
 ! Pre-setup of carrier components and Time evolution parameters
 !
 call RT_apply(DIPs%ib,Xen,Xk,IO_COM=LOG,TIMES_ONLY=.TRUE.)
 !
 ! Setup of Time-range & Report
 !
 call RT_time_configuration_setup("CARRIERS",CARR_RT_IO_t)
 !
 ! Dimensions, Constant, Logicals and allocs
 !
 l_BS      = BS_H_dim> 0.and.STRING_match(TRabsWHAT,"abs")
 l_IP      = (STRING_match(TRabsMODE,"ip").or..not.l_BS).and.STRING_match(TRabsWHAT,"abs")
 l_OFF     = STRING_match(TRabsWHAT,"kerr").and..not.STRING_same(TRabsDIP_plane,"none")
 l_cv      = STRING_match(TRabsMODE,"cv")
 l_cc      = STRING_match(TRabsMODE,"cc")
 l_vv      = STRING_match(TRabsMODE,"vv")
 if (l_BS) then
   DIPs%ib   =BS_bands
   N_cv_trans=BS_H_dim
 else
   DIPs%ib   =RT_bands
   N_cv_trans=(RT_bands(2)-Xen%nbf)*(Xen%nbf-RT_bands(1)+1)*Xk%nbz
 endif
 N_cc_trans=(Xen%nbf-RT_bands(1)+1)*(Xen%nbf-RT_bands(1)+1)*Xk%nbz
 N_vv_trans=(RT_bands(2)-Xen%nbf)*(RT_bands(2)-Xen%nbf)*Xk%nbz
 N_trans=0
 nbf_eq=Xen%nbf
 !
 if (l_cv) N_trans=N_cv_trans
 if (l_vv) N_trans=N_trans+N_vv_trans
 if (l_cc) N_trans=N_trans+N_cc_trans
 !
 call msg('s','[TR-ABS] cv (EQ)  transitions :',N_cv_trans)
 call msg('s','[TR-ABS] cc (NEQ)             :',N_cc_trans)
 call msg('s','[TR-ABS] vv (NQE)             :',N_vv_trans)
 !
 Co=real(spin_occ)/(2._SP*pi)**3*d3k_factor*4._SP*pi/q0_def_norm**2 
 !
 ! Par Setup
 !
 call PARALLEL_global_indexes(Xen,Xk,Xq," ",RESET=.TRUE.)
 call PARALLEL_global_indexes(Xen,Xk,Xq,"DIPOLES",Dip=DIPs)
 !
 ! Dipoles...
 !------------
 ! ... q->0
 DIP_dir=rZERO
 TRabsDIP_dir=TRabsDIP_dir/v_norm( TRabsDIP_dir )
 if (trim(global_gauge)=='length'  ) TRabsDIP_dir=TRabsDIP_dir*q0_def_norm
 DIPs%q0=TRabsDIP_dir
 DIP_dir(1,:)=TRabsDIP_dir
 !
 ! ... I/O
 call DIPOLE_IO(Xk,Xen,DIPs,'read',io_err,'DIP')
 !
 ! ... rotation
 if (l_OFF) then
   N_dirs=2
   call v_rotate(trim(TRabsDIP_plane),-pi/2._SP,DIPs%q0,v_out=DIP_dir(2,:))
 endif
 !
 ! OBS alloc
 !
 YAMBO_ALLOC(TRabs_E,(N_trans))
 N_col=0 
 if (l_BS)  N_col=N_col+1 ! eps
 if (l_IP)  N_col=N_col+1 ! eps0
 if (l_OFF) N_col=N_col+1 ! kerr
 YAMBO_ALLOC(Epsilon_ii,(RT_conf%W_n_steps,N_col)) 
 N_dirs=0
 if (l_BS.or.l_IP) N_dirs=1
 if (l_OFF) N_dirs=N_dirs+1
 YAMBO_ALLOC(TRabs_RES_left ,(N_trans,N_dirs,2))
 YAMBO_ALLOC(TRabs_RES_right,(N_trans,N_dirs,2))
 !
 ! Frequency setup
 !
 W_bss%er=RT_conf%X_W_range
 W_bss%n_freqs=RT_conf%W_n_steps
 W_bss%per_memstps=100._SP
 W_bss%dr=RT_conf%damp_factor
 !
 call FREQUENCIES_setup(W_bss)
 !
 ! Output (init)
 call RT_Trabs_output("open",W_bss,0._SP,l_BS,l_IP,l_OFF)
 !
 do i_T=1,RT_conf%T_n_steps
   !
   call RT_apply(DIPs%ib,Xen,Xk,TIME=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   if (i_T==1)  call live_timing('Transient Absorption',RT_conf%T_n_steps)
   !
   TRabs_RES_left =cZERO
   TRabs_RES_right=cZERO
   !
   do i_dir=1,N_dirs
     N_trans_done=0
     if (l_BS) call RT_TRabs_residuals("BSE",N_trans_done,i_dir,DIP_dir(i_dir,:),Xen,Xk,nbf_eq)
     if (l_IP) then
       if (l_cv.and..not.l_BS) call RT_TRabs_residuals("cv IP",N_trans_done,i_dir,DIP_dir(i_dir,:),Xen,Xk,nbf_eq)
       if (l_vv) call RT_TRabs_residuals("vv IP",N_trans_done,i_dir,DIP_dir(i_dir,:),Xen,Xk,nbf_eq)
       if (l_cc) call RT_TRabs_residuals("cc IP",N_trans_done,i_dir,DIP_dir(i_dir,:),Xen,Xk,nbf_eq)
     endif
   enddo
   !
   N_col=0 
   Epsilon_ii=cZERO
   !
   if (l_BS) then
!     N_col=N_col+1
!     do i_trans=1,BS_H_dim
!       RES=Co*( TRabs_iRES(i_trans,2)*conjg(TRabs_iRES(i_trans,1)) +&
!&               TRabs_iRES(i_trans,1)*conjg(TRabs_iRES(i_trans,2)) )
!       Epsilon_ii(:,N_col)=Epsilon_ii(:,N_col)-RES/(W_bss%p(:)-TRabs_E(i_trans))
!     enddo
   endif
   if (l_IP) then
     N_col=N_col+1
     do i_trans=BS_H_dim+1,N_trans
       RES=Co*TRabs_RES_right(i_trans,1,1)*TRabs_RES_left(i_trans,1,1)
       Epsilon_ii(:,N_col)=Epsilon_ii(:,N_col)-RES/(W_bss%p(:)-TRabs_E(i_trans))
     enddo
   endif
   !
   ! Output (write)
   call RT_Trabs_output("write",W_bss,RT_time(i_T),l_BS,l_IP,l_OFF)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 ! Output (close)
 call RT_Trabs_output("close",W_bss,0._SP,l_BS,l_IP,l_OFF)
 !
 ! Free...
 YAMBO_FREE(Epsilon_ii)
 YAMBO_FREE(TRabs_E)
 YAMBO_FREE(TRabs_RES_left)
 YAMBO_FREE(TRabs_RES_right)
 !
 call live_timing()
 !
end subroutine RT_transient_absorption
