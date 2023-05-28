!
!        Copyright (C) 2000-2023 the YAMBO team
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
 use drivers,         ONLY:l_rt_carriers_in_use
 use pars,            ONLY:SP,pi,cZERO,rZERO,schlen,DP,cI
 use R_lattice,       ONLY:bz_samp,q0_def_norm,d3k_factor
 use electrons,       ONLY:levels,spin,spin_occ
 use LIVE_t,          ONLY:live_timing
 use vec_operate,     ONLY:v_norm,v_rotate
 use RT_control,      ONLY:RT_apply,CARR_RT_IO_t
 use YPP_real_time,   ONLY:RT_conf,RT_time,TRabsDIP_dir,TRabsWHAT,TRabsDIP_plane,&
&                          TRabs_RES_left,TRabs_RES_right,TRabsMODE,TRabs_E,Epsilon_ij,&
&                          B_Hall,TRabs_lEPS,TRabs_EPS_label
 use YPPm,            ONLY:DIPs
 use X_m,             ONLY:global_gauge
 use IO_m,            ONLY:LOG
 use BS,              ONLY:BS_bands,BS_K_dim,BS_H_dim
 use frequency,       ONLY:w_samp
 use parallel_int,    ONLY:PARALLEL_global_indexes,PP_redux_wait,PARALLEL_index
 use parallel_m,      ONLY:PAR_COM_DIPk_ibz_INDEX,PAR_COM_DIPk_ibz_A2A,PP_indexes,PP_indexes_reset
 use real_time,       ONLY:RT_bands,l_TRabs_YPP
 use stderr,          ONLY:STRING_match,STRING_same,STRING_remove
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
 integer                 :: N_cv_trans,N_cc_trans,N_vv_trans,N_cc_vv_trans,N_trans,N_trans_real,i_T,io_err,i_trans,&
&                           nb(2),N_DIP_dirs,i_dir,j_dir,N_res,N_files,DIP_dir_table(3),ic,jc
 real(SP)                :: DIP_dir(3,3),Co
 logical                 :: l_BS,l_OFF,l_cv,l_cc,l_vv,l_ABS,l_EQ,l_KERR,l_TRANS,l_REFL
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
 if (.not.l_rt_carriers_in_use) return
 !
 ! Setup of Time-range & Report
 !
 call RT_time_configuration_setup("CARRIERS",CARR_RT_IO_t)
 !
 ! Dimensions, Constant, Logicals and allocs
 !
 ! ...logicals
 l_BS      = BS_K_dim(1)> 0
 l_ABS     = STRING_match(TRabsWHAT,"abs")
 l_KERR    = STRING_match(TRabsWHAT,"kerr")
 l_TRANS   = STRING_match(TRabsWHAT,"trans")
 l_REFL    = STRING_match(TRabsWHAT,"refl")
 if (any((/l_REFL,l_TRANS/))) then
   call warning("Reflectivity and Trasnmission still not implemented")
   l_TRANS=.FALSE.
   l_REFL =.FALSE.
   TRabsWHAT=STRING_remove(TRabsWHAT,"trans")
   TRabsWHAT=STRING_remove(TRabsWHAT,"refl")
 endif
 !
 ! ...transitions
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
   N_cv_trans=(RT_bands(2)-Xen%nbf(1))*(Xen%nbm(1)-RT_bands(1)+1)*Xk%nbz
 endif
 label="IP"
 if (l_BS) label="BS"
 N_vv_trans=(Xen%nbf(1)-RT_bands(1)+1)*(Xen%nbf(1)-RT_bands(1)+1)*Xk%nbz
 N_cc_trans=(RT_bands(2)-Xen%nbm(1))*(RT_bands(2)-Xen%nbm(1))*Xk%nbz
 nb=(/Xen%nbf(1),Xen%nbm(1)/)
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
 if (l_cv) call msg('s','[TR-OBS] cv (EQ)  transitions :',N_cv_trans)
 if (l_cc) call msg('s','[TR-OBS] cc (NEQ) transitions :',N_cc_trans)
 if (l_vv) call msg('s','[TR-OBS] vv (NEQ) transitions :',N_vv_trans)
 call msg('s','[TR-OBS] Total    transitions :',N_trans)
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
 !
 ! ... EPS_ij components
 l_OFF=.FALSE.
 do i_dir=1,3
   do j_dir=1,3
     TRabs_lEPS(i_dir,j_dir)=STRING_match(TRabsDIP_plane,TRabs_EPS_label(i_dir,j_dir))
     if (STRING_match(TRabsDIP_plane,"all"))  TRabs_lEPS(i_dir,j_dir)=.TRUE.
     if (i_dir/=j_dir.and. TRabs_lEPS(i_dir,j_dir)) l_OFF = .TRUE.
   enddo
 enddo
 if (STRING_same(TRabsDIP_plane,"none"))  TRabs_lEPS(1,1)=.TRUE.
 if (.not.l_OFF) then
   call warning("No off-diagonal components => Kerr switched-off")
   l_KERR=.FALSE.
   TRabsWHAT=STRING_remove(TRabsWHAT,"kerr")
 endif
 !
 ! ... # of DIPOLE directions to compute
 N_DIP_dirs=0
 DIP_dir_table=0
 do i_dir=1,3
   if (any(TRabs_lEPS(i_dir,:)).or.any(TRabs_lEPS(:,i_dir))) then
     N_DIP_dirs=N_DIP_dirs+1
     DIP_dir_table(N_DIP_dirs)=i_dir
   endif
 enddo
 !
 ! ... computing directions
 DIP_dir=rZERO
 call msg('s','Creating '//trim(TRabsDIP_plane)//' related dipole directions')
 call msg('s','Dipole direction seed (defines `x` axis)',TRabsDIP_dir)
 TRabsDIP_dir=TRabsDIP_dir/v_norm( TRabsDIP_dir )
 if (trim(global_gauge)=='length'  ) TRabsDIP_dir=TRabsDIP_dir*q0_def_norm
 do i_dir=1,N_DIP_dirs
   if (DIP_dir_table(i_dir)==1) DIP_dir(i_dir,:)=TRabsDIP_dir
   if (DIP_dir_table(i_dir)==2) call v_rotate("xy",-pi/2._SP,TRabsDIP_dir,v_out=DIP_dir(i_dir,:))
   if (DIP_dir_table(i_dir)==3) call v_rotate("xz",-pi/2._SP,TRabsDIP_dir,v_out=DIP_dir(i_dir,:))
 enddo
 !
 ! ... I/O
 call DIPOLE_IO(Xk,Xen,DIPs,'read',io_err,'DIP')
 if (io_err/=0) return
 !
 ! Allocations
 !
 ! ... Energies
 YAMBO_ALLOC(TRabs_E,(N_trans))
 !
 ! ... Residuals 
 N_res=1
 if (l_BS) N_res=N_res+1
 YAMBO_ALLOC(TRabs_RES_left ,(N_trans*N_res,N_DIP_dirs,2)) ! (ABS/IP, i_dir Res/delta_Res)
 YAMBO_ALLOC(TRabs_RES_right,(N_trans*N_res,N_DIP_dirs,2))
 !
 ! ... Observables 
 if (l_ABS) then
   YAMBO_ALLOC(Epsilon_ij,(RT_conf%W_n_steps,3,3)) 
 endif
 if (l_OFF) then
   YAMBO_ALLOC(B_Hall,(3,3)) 
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
 ofile=" "
 n_files=0
 call RT_Trabs_output("init",label,ofile,N_files,W_bss,0._SP)
 !
 ! Output (open)
 call RT_Trabs_output("open","all",ofile,N_files,W_bss,0._SP)
 !
 do i_T=1,RT_conf%T_n_steps
   !
   YAMBO_FREE(Xen%df)
   !
   call RT_apply(DIPs%ib,Xen,Xk,TIME=RT_time(i_T),VERBOSE=i_T==1)
   !
   if (i_T==1)  then
     call live_timing('Transient '//trim(TRabsWHAT),RT_conf%T_n_steps)
     INTERPOLATE_is_quiet=.TRUE.
   endif
   !
   TRabs_E        =rZERO
   TRabs_RES_left =cZERO
   TRabs_RES_right=cZERO
   if (l_KERR) B_Hall    =cZERO
   if (l_ABS ) Epsilon_ij=cZERO
   !
   N_trans_real=0
   if (l_BS) call RT_TRabs_residuals("BSE",N_trans_real,N_DIP_dirs,DIP_dir,Xen,Xk,nb,l_EQ)
   if (l_cv.and..not.l_BS) call RT_TRabs_residuals("cv IP",N_trans_real,N_DIP_dirs,DIP_dir,Xen,Xk,nb,l_EQ)
   if (l_vv) call RT_TRabs_residuals("vv IP",N_trans_real,N_DIP_dirs,DIP_dir,Xen,Xk,nb,l_EQ)
   if (l_cc) call RT_TRabs_residuals("cc IP",N_trans_real,N_DIP_dirs,DIP_dir,Xen,Xk,nb,l_EQ)
   ! 
   call PP_redux_wait(TRabs_RES_right,COMM=PAR_COM_DIPk_ibz_INDEX%COMM)
   call PP_redux_wait(TRabs_RES_left,COMM=PAR_COM_DIPk_ibz_A2A%COMM)
   call PP_redux_wait(TRabs_RES_left,COMM=PAR_COM_DIPk_ibz_INDEX%COMM)
   call PP_redux_wait(TRabs_RES_right,COMM=PAR_COM_DIPk_ibz_A2A%COMM)
   call PP_redux_wait(TRabs_E,COMM=PAR_COM_DIPk_ibz_INDEX%COMM)
   call PP_redux_wait(TRabs_E,COMM=PAR_COM_DIPk_ibz_A2A%COMM)
   !
   do ic=1,N_DIP_dirs
     do jc=1,N_DIP_dirs
       if (l_ABS)             call build_up_eps_ij("EPS-"//label,ic,jc)
       if (l_KERR.and.ic/=jc) call build_up_eps_ij("AH-"//label,ic,jc)
     enddo
   enddo
   !
   if (l_ABS)  call PP_redux_wait(Epsilon_ij)
   if (l_KERR) call PP_redux_wait(B_Hall)
   !
   ! Output (write)
   call RT_Trabs_output("write",label,ofile,N_files,W_bss,RT_time(i_T))
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 INTERPOLATE_is_quiet=.FALSE.
 !
 ! Output (close)
 call RT_Trabs_output("close",label,ofile,N_files,W_bss,0._SP)
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
     complex(DP)  :: ACC(W_bss%n_freqs)
     ACC=0._DP
     i_dir=DIP_dir_table(I)
     j_dir=DIP_dir_table(J)
     if (.not.TRabs_lEPS(i_dir,j_dir)) return
     if (what=="EPS-IP".or.what=="AH-IP") then
       do i_trans=BS_K_dim(1)+1,N_trans_real
         if (.not.PX_ip%element_1D(i_trans-BS_K_dim(1))) cycle
         if (what=="EPS-IP") then
           RES=Co*TRabs_RES_right(i_trans,I,1)*TRabs_RES_left(i_trans,J,1)
           ACC(:)=ACC(:)-RES/(W_bss%p(:)-TRabs_E(i_trans))+conjg(RES)/(W_bss%p(:)+TRabs_E(i_trans))
         endif
         if (what=="AH-IP") then
           RES=2.*Co*aimag(TRabs_RES_right(i_trans,I,1)*TRabs_RES_left(i_trans,J,1))
           ACC(1)=ACC(1)+RES
         endif
       enddo
       if (what=="EPS-IP")  Epsilon_ij(:,i_dir,j_dir)=Epsilon_ij(:,i_dir,j_dir)+ACC(:)
       if (what=="AH-IP" )  B_Hall(i_dir,j_dir)=B_Hall(i_dir,j_dir)+cI*ACC(1)
     endif
     if (what=="EPS-BS".or.what=="AH-BS") then
       do i_trans=1,BS_K_dim(1)
         if (.not.PX_bs%element_1D(i_trans)) cycle
         RES=Co*( TRabs_RES_right(i_trans,I,1)*TRabs_RES_left(i_trans,J,2) +&
&                 TRabs_RES_right(i_trans,I,2)*TRabs_RES_left(i_trans,J,1) )
         if (l_EQ) RES=RES+Co*TRabs_RES_right(i_trans,I,1)*TRabs_RES_left(i_trans,J,1)
         ! ANTI-RES part to be coded for EPS-BS.
         if (what=="EPS-BS") Epsilon_ij(:,i_dir,j_dir)=Epsilon_ij(:,i_dir,j_dir)-RES/(W_bss%p(:)-TRabs_E(i_trans))
         if (what=="AH-BS")  B_Hall(i_dir,j_dir)=B_Hall(i_dir,j_dir)+RES-conjg(RES)
       enddo
     endif
   end subroutine
   !
end subroutine RT_transient_absorption
