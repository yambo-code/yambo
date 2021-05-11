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
 use pars,            ONLY:SP,pi,cZERO,rZERO,schlen
 use R_lattice,       ONLY:bz_samp,q0_def_norm,d3k_factor
 use electrons,       ONLY:levels,spin,spin_occ
 use LIVE_t,          ONLY:live_timing
 use vec_operate,     ONLY:v_norm,v_rotate
 use RT_control,      ONLY:RT_apply,CARR_RT_IO_t
 use YPP_real_time,   ONLY:RT_conf,RT_time,TRabsDIP_dir,TRabsWHAT,TRabsDIP_plane,&
&                          TRabs_RES_left,TRabs_RES_right,TRabsMODE,TRabs_E,Epsilon_ij,B_Hall
 use YPPm,            ONLY:DIPs
 use X_m,             ONLY:global_gauge
 use IO_m,            ONLY:LOG
 use BS,              ONLY:BS_bands,BS_K_dim,BS_H_dim
 use frequency,       ONLY:w_samp
 use parallel_int,    ONLY:PARALLEL_global_indexes,PP_redux_wait,PARALLEL_index
 use parallel_m,      ONLY:PAR_COM_DIPk_ibz_INDEX,PAR_COM_DIPk_ibz_A2A,PP_indexes,PP_indexes_reset
 use real_time,       ONLY:RT_bands,l_TRabs_YPP
 use stderr,          ONLY:STRING_match,STRING_same
 use com,             ONLY:msg
 use interpolate,     ONLY:INTERPOLATE_is_quiet
 !
#include<memory.h>
 !
 type(levels)      :: Xen
 type(bz_samp)     :: Xk,Xq
 !
 ! Work space
 !
 type(w_samp)            :: W_bss
 integer                 :: N_cv_trans,N_cc_trans,N_vv_trans,N_cc_vv_trans,N_trans,N_trans_done,i_T,io_err,i_trans,&
&                           nb(2),N_dirs,i_dir,j_dir,N_res,N_files
 real(SP)                :: DIP_dir(3,3),Co
 logical                 :: l_BS,l_OFF,l_cv,l_cc,l_vv,l_ABS,l_EQ
 character(2)            :: label
 character(schlen)       :: ofile(20)
 type(PP_indexes)        :: PX_bs,PX_ip
 !
 call section("+","Transient Optical Properties")
 !
 if (BS_K_dim(1)>0 .and. BS_H_dim/=BS_K_dim(1)) then
   call warning("TR-optical properties implemented only for TDA BSE")
   return
 endif
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
 l_BS      = BS_K_dim(1)> 0
 l_ABS     = STRING_match(TRabsWHAT,"abs")
 l_OFF     = STRING_match(TRabsWHAT,"kerr").and..not.STRING_same(TRabsDIP_plane,"none")
 l_cv      = STRING_match(TRabsMODE,"cv")
 l_cc      = STRING_match(TRabsMODE,"cc")
 l_vv      = STRING_match(TRabsMODE,"vv")
 l_EQ      = STRING_match(TRabsMODE,"eq")
 !
 !  |   | <- RT_bands(2)
 !  |   |
 !  |   |
 !  |   |
 !  |x  | <- nbm
 !  |x  |
 !  |xxx| <- nbf
 !  |xxx|
 !  |xxx|
 !  |xxx| <- RT_bands(1)
 !
 ! cv ( RT_bands(1) - nbm ) -> ( nbf+1 - RT_bands(2) )
 ! vv ( RT_bands(1) - nbf ) -> ( same )
 ! cc ( nbm+1 - RT_bands(2) ) -> ( same )
 !
 if (l_BS) then
   DIPs%ib   =BS_bands
   N_cv_trans=BS_K_dim(1)
 else
   DIPs%ib   =RT_bands
   N_cv_trans=(RT_bands(2)-Xen%nbf)*(Xen%nbm-RT_bands(1)+1)*Xk%nbz
 endif
 label="IP"
 if (l_BS) then
   label="BS"
 endif
 N_vv_trans=(Xen%nbf-RT_bands(1)+1)*(Xen%nbf-RT_bands(1)+1)*Xk%nbz
 N_cc_trans=(RT_bands(2)-Xen%nbm)*(RT_bands(2)-Xen%nbm)*Xk%nbz
 nb=(/Xen%nbf,Xen%nbm/)
 !
 N_trans=0
 N_cc_vv_trans=0
 if (l_cv) N_trans=N_cv_trans
 if (l_vv) N_cc_vv_trans=N_cc_vv_trans+N_vv_trans
 if (l_cc) N_cc_vv_trans=N_cc_vv_trans+N_cc_trans
 N_trans=N_trans+N_cc_vv_trans
 call PP_indexes_reset(PX_bs)
 call PP_indexes_reset(PX_ip)
 if (l_BS) then
   call PARALLEL_index(PX_bs,(/N_cv_trans/))
   call PARALLEL_index(PX_ip,(/N_cc_vv_trans/))
 else
   call PARALLEL_index(PX_ip,(/N_trans/))
 endif
 !
 if (l_cv) call msg('s','[TR-ABS] cv (EQ)  transitions :',N_cv_trans)
 if (l_cc) call msg('s','[TR-ABS] cc (NEQ) transitions :',N_cc_trans)
 if (l_vv) call msg('s','[TR-ABS] vv (NEQ) transitions :',N_vv_trans)
 call msg('s','[TR-ABS] Total    transitions :',N_trans)
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
 if (STRING_same(TRabsDIP_plane,"all")) TRabsDIP_dir=(/1._SP,0._SP,0._SP/)
 TRabsDIP_dir=TRabsDIP_dir/v_norm( TRabsDIP_dir )
 if (trim(global_gauge)=='length'  ) TRabsDIP_dir=TRabsDIP_dir*q0_def_norm
 DIPs%q0=TRabsDIP_dir
 DIP_dir(1,:)=TRabsDIP_dir
 !
 ! ... I/O
 call DIPOLE_IO(Xk,Xen,DIPs,'read',io_err,'DIP')
 !
 ! ... rotation
 N_dirs=1
 if (l_OFF) then
   if (STRING_same(TRabsDIP_plane,"all")) then
     N_dirs=3
     call v_rotate("xy",-pi/2._SP,DIP_dir(1,:),v_out=DIP_dir(2,:))
     call v_rotate("xz",-pi/2._SP,DIP_dir(1,:),v_out=DIP_dir(3,:))
   else
     N_dirs=2
     call v_rotate(trim(TRabsDIP_plane),-pi/2._SP,DIP_dir(1,:),v_out=DIP_dir(2,:))
   endif
 endif
 !
 ! Allocations
 !
 ! ... Energies
 YAMBO_ALLOC(TRabs_E,(N_trans))
 !
 ! ... Residuals 
 N_res=N_res+1
 if (l_BS) N_res=N_res+1
 YAMBO_ALLOC(TRabs_RES_left ,(N_trans*N_res,N_dirs,2)) ! (ABS/IP, i_dir Res/delta_Res)
 YAMBO_ALLOC(TRabs_RES_right,(N_trans*N_res,N_dirs,2))
 !
 ! ... Observables 
 YAMBO_ALLOC(Epsilon_ij,(RT_conf%W_n_steps,N_dirs,N_dirs)) 
 if (l_OFF) then
   YAMBO_ALLOC(B_Hall,(N_dirs,N_dirs)) 
 endif
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
 n_files=0
 call RT_Trabs_output("init",label,ofile,N_dirs,N_files,W_bss,0._SP,l_OFF,l_ABS)
 !
 ! Output (open)
 call RT_Trabs_output("open","all",ofile,N_dirs,N_files,W_bss,0._SP,l_OFF,l_ABS)
 !
 do i_T=1,RT_conf%T_n_steps
   !
   YAMBO_FREE(Xen%df)
   !
   call RT_apply(DIPs%ib,Xen,Xk,TIME=RT_time(i_T),VERBOSE=i_T==1)
   !
   if (i_T==1)  then
     call live_timing('Transient Absorption',RT_conf%T_n_steps)
     INTERPOLATE_is_quiet=.TRUE.
   endif
   !
   TRabs_E        =rZERO
   TRabs_RES_left =cZERO
   TRabs_RES_right=cZERO
   if (l_OFF) B_Hall =cZERO
   Epsilon_ij=cZERO
   !
   do i_dir=1,N_dirs
     N_trans_done=0
     if (l_BS) call RT_TRabs_residuals("BSE",N_trans_done,i_dir,DIP_dir(i_dir,:),Xen,Xk,nb,l_EQ)
     if (l_cv.and..not.l_BS) call RT_TRabs_residuals("cv IP",N_trans_done,i_dir,DIP_dir(i_dir,:),Xen,Xk,nb,l_EQ)
     if (l_vv) call RT_TRabs_residuals("vv IP",N_trans_done,i_dir,DIP_dir(i_dir,:),Xen,Xk,nb,l_EQ)
     if (l_cc) call RT_TRabs_residuals("cc IP",N_trans_done,i_dir,DIP_dir(i_dir,:),Xen,Xk,nb,l_EQ)
   enddo
   ! 
   call PP_redux_wait(TRabs_RES_right,COMM=PAR_COM_DIPk_ibz_INDEX%COMM)
   call PP_redux_wait(TRabs_RES_left,COMM=PAR_COM_DIPk_ibz_A2A%COMM)
   call PP_redux_wait(TRabs_RES_left,COMM=PAR_COM_DIPk_ibz_INDEX%COMM)
   call PP_redux_wait(TRabs_RES_right,COMM=PAR_COM_DIPk_ibz_A2A%COMM)
   call PP_redux_wait(TRabs_E,COMM=PAR_COM_DIPk_ibz_INDEX%COMM)
   call PP_redux_wait(TRabs_E,COMM=PAR_COM_DIPk_ibz_A2A%COMM)
   !
   do i_dir=1,N_dirs
     do j_dir=1,N_dirs
       if (l_ABS.and.i_dir==j_dir) call build_up_eps_ij("EPS-"//label,i_dir,j_dir)
       if (l_OFF.and.i_dir <j_dir) call build_up_eps_ij("EPS-"//label,i_dir,j_dir)
       if (l_OFF.and.i_dir <j_dir) call build_up_eps_ij("AH-"//label,i_dir,j_dir)
     enddo
   enddo
   !
   call PP_redux_wait(Epsilon_ij)
   if (l_OFF) call PP_redux_wait(B_Hall)
   !
   ! Output (write)
   call RT_Trabs_output("write",label,ofile,N_dirs,N_files,W_bss,RT_time(i_T),l_OFF,l_ABS)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 INTERPOLATE_is_quiet=.FALSE.
 !
 ! Output (close)
 call RT_Trabs_output("close",label,ofile,N_dirs,N_files,W_bss,0._SP,l_OFF,l_ABS)
 !
 ! Free...
 YAMBO_FREE(Epsilon_ij)
 YAMBO_FREE(B_Hall)
 YAMBO_FREE(TRabs_E)
 YAMBO_FREE(TRabs_RES_left)
 YAMBO_FREE(TRabs_RES_right)
 !
 call live_timing()
 !
 contains
   !
   subroutine build_up_eps_ij(what,I,J)
     character(*) :: what
     integer      :: I,J
     complex(SP)  :: RES,GF(W_bss%n_freqs)
     if (what=="EPS-IP".or.what=="AH-IP") then
       do i_trans=BS_K_dim(1)+1,N_trans
         if (.not.PX_ip%element_1D(i_trans-BS_K_dim(1))) cycle
         RES=Co*TRabs_RES_right(i_trans,I,1)*TRabs_RES_left(i_trans,J,1)
         if (what=="EPS-IP") then
           Epsilon_ij(:,i,j)=Epsilon_ij(:,i,j)-RES/(W_bss%p(:)-TRabs_E(i_trans))+conjg(RES)/(W_bss%p(:)+TRabs_E(i_trans))
         endif
         if (what=="AH-IP")  B_Hall(i,j)=B_Hall(i,j)+RES-conjg(RES)
       enddo
     endif
     if (what=="EPS-BS".or.what=="AH-BS") then
       ! ANTI-RES part to be coded
       do i_trans=1,BS_K_dim(1)
         if (.not.PX_bs%element_1D(i_trans)) cycle
         RES=Co*( TRabs_RES_right(i_trans,I,1)*TRabs_RES_left(i_trans,J,2) +&
&                 TRabs_RES_right(i_trans,I,2)*TRabs_RES_left(i_trans,J,1) )
         if (l_EQ) RES=RES+Co*TRabs_RES_right(i_trans,I,1)*TRabs_RES_left(i_trans,J,1)
         if (what=="EPS-BS") Epsilon_ij(:,i,j)=Epsilon_ij(:,i,j)-RES/(W_bss%p(:)-TRabs_E(i_trans))
         if (what=="AH-BS")  B_Hall(i,j)=B_Hall(i,j)+RES-conjg(RES)
       enddo
     endif
   end subroutine
   !
end subroutine RT_transient_absorption
