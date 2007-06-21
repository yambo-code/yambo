!
! Copyright (C) 2000-2005 A. Marini and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
 subroutine std_presets(INSTR,IND,OD,JS)
 !
 use drivers
 use X_m,        ONLY:iq_now,self_detect_E_range,half_X_mat_ONLY,eps_2_alpha,&
&                     alpha_dim,use_X_RIM,long_path,X_RIM_nkpts
 use QP_m,       ONLY:pp_taylor_terms,QP_rep_string,QP_dSc_steps,QP_n_W_freqs,&
&                     QP_Sx_low_band,QP_dSc_test,QP_solver,QP_G_damp,QP_dSc_delta,&
&                     QP_W_er,QP_W_dr,QP_cg_percent,QP_xc_FUNCTIONAL,&
&                     QP_ctl_db,QP_ctl_interp_neigh,QP_ctl_E,QP_ctl_W,QP_ctl_Z,&
&                     QP_ctl_short_descr,QP_Sc_steps,QP_G_er,QP_G_dr,&
&                     QP_W_partially_done,GWo_SC_done,GWo_iterations
 use functions,  ONLY:bose_E_cut
 use D_lattice,  ONLY:i_space_inv,n_atoms_max,n_atom_species,pp_n_l_comp_max,pp_kbv_dim
 use R_lattice,  ONLY:n_g_shells,ng_closed,bse_scattering,&
&                     Xk_grid_is_uniform,RIM_id_epsm1_reference,RIM_epsm1,&
&                     RIM_anisotropy,RIM_ng,RIM_n_rand_pts,nqibz,q0_def_norm,&
&                     cyl_ph_radius,box_length,cyl_length,cut_geometry,CUTOFF_plus_RIM
 use electrons,  ONLY:n_spin,n_sp_pol,n_spinor,BZ_RIM_nbands,BZ_RIM_tot_nkpts,&
&                     filled_tresh, spin_orbit
 use par_proc_m, ONLY:myid,master_node
 use com,        ONLY:isec,depth,secnm,previous_secmode,of,opened_of,&
&                     of_unit,of_unit,write_to_of,write_to_report,max_open_ofs,&
&                     out_path,in_path,jobstr,db_wr_path
 use stderr,     ONLY:win_size,tty_size,write_to_log,logfile
 use timing,     ONLY:log_line_to_dump,log_line,nhash,live_timing_is_on,&
&                     cput_seg,cput_sec,cput_tot,parallel_timing,ct
 use wave_func,  ONLY:wf_b,wf_k,wf_ng,wf_space,wf_norm_test
 use FFT_m,      ONLY:fft_dim_loaded,fft_size,fft_dim
 use IO_m,       ONLY:io_reset,max_io_units,write_to_dbs,dbs_fragment,serial_number
 use BS,         ONLY:BS_n_g_W,BS_eh_en,BS_identifier,BS_q,BS_eh_win,&
&                     BS_res_mode,BS_K_dim,BS_cpl_mode,BS_not_const_eh_f,&
&                     BSS_mode,BSS_n_freqs,BSS_n_descs,BSS_er,BSS_dr,&
&                     BSS_q0,Haydock_treshold,BS_K_is_ALDA,BSS_uses_RIM
 use TDDFT,      ONLY:FXC_description,FXC_type,FXC_n_descs,FXC_n_g_corr,&
&                     FXC_per_memstps,FXC_LRC_alpha,FXC_SVD_cutoff,&
&                     FXC_is_causal
 use ACFDT,      ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
 use ELPH,       ONLY:ph_modes,elph_nb,ph_damping,PH_null_E_rgn
 use memory_m,   ONLY:mem_reset
 use zeros,      ONLY:zero_norm,k_iku_zero,k_rlu_zero,G_iku_zero,zero_dfl
 use xc_functionals,  ONLY:GS_xc_FUNCTIONAL,GS_xc_KIND
 !
 implicit none
 !
 character(*) :: INSTR,IND,OD,JS
 integer      :: i1
 !
 ! ZEROs 
 !
 zero_norm =zero_dfl
 k_iku_zero=zero_dfl
 k_rlu_zero=zero_dfl
 G_iku_zero=zero_dfl
 !
 ! PATHS 
 !
 in_path=IND
 out_path=OD
 db_wr_path=OD
 jobstr=JS
 !
 ! CALLS
 !
 master_node=myid==0
 call win_size(tty_size)
 call ct(INIT=.TRUE.)
 !
 !com
 !
 isec=0
 depth=-1
 secnm=' '
 previous_secmode=' '
 of=' '
 opened_of=' '
 of_unit=0
 of_unit(max_open_ofs)=-11
 write_to_of=master_node
 write_to_report=master_node
 !
 !stderr
 !
 write_to_log=master_node
 write (logfile,'(2a)') trim(out_path),'/l_stderr'
 !
 !Timing
 !
 log_line_to_dump=.false.
 log_line=' '
 nhash=20
 parallel_timing=.false.
 live_timing_is_on=.true.
 !
 !functions
 !
 bose_E_cut=0.1
 !
 !R_lattice
 !
 n_g_shells=0
 nqibz=0
 ng_closed=0
 bse_scattering=.false.
 Xk_grid_is_uniform=.true.
 RIM_id_epsm1_reference=0
 RIM_epsm1=0.
 RIM_anisotropy=0.
 RIM_ng=0
 RIM_n_rand_pts=0
 Box_length=0.
 cyl_ph_radius=0.
 box_length=0.
 cyl_length=0.
 cut_geometry='sphere'
 CUTOFF_plus_RIM=.false.
 q0_def_norm=1.E-5
 !
 !D_lattice
 !
 n_atoms_max=0
 n_atom_species=0
 pp_n_l_comp_max=0
 pp_kbv_dim=0
 i_space_inv=-1
 !
 !drivers
 !
 list_dbs=.false.
 infile_editing=.false.
 l_setup=.false.
 l_rim=.false.
 l_col_cut=.false.
 l_optics=.false.
 l_xxvxc=.false.
 l_em1d=.false.
 l_em1s=.false.
 l_ppa=.false.
 l_gw0=.false.
 l_bse=.false.
 l_lr=.false.
 l_bss=.false.
 l_bs_fxc=.false.
 l_acfdt=.false.
 l_el_corr=.false.
 l_ph_corr=.false.
 gpl_version=.false.
 !
 !electrons
 !
 n_spin=1
 n_sp_pol=1
 n_spinor=1
 spin_orbit       = .false.
 BZ_RIM_nbands=0
 BZ_RIM_tot_nkpts=0
 filled_tresh=0.00001
 !
 !memory
 !
 call mem_reset()
 !
 !wave_func
 !
 wf_b=0
 wf_k=0
 wf_ng=0
 wf_space=' '
 wf_norm_test=.true.
 !
 !FFT
 !
 fft_dim_loaded=0
 fft_size=0
 fft_dim=0
 !
 !IO
 !
 serial_number=0
 write_to_dbs=master_node
 dbs_fragment=index(INSTR,'dbfrag')>0
 do i1=1,max_io_units
   call io_reset(i1)
 enddo
 !
 !X
 !
 iq_now=0
 X_RIM_nkpts=0
 self_detect_E_range=.false.
 half_X_mat_only=.false.
 use_X_RIM=.false.
 eps_2_alpha=1._SP
 alpha_dim='adim'
 long_path='none'
 !
 !QPm
 !
 pp_taylor_terms=0
 QP_rep_string='kpbne0ees0'
 QP_dSc_steps=2
 QP_Sc_steps=100
 QP_n_W_freqs=100
 QP_Sx_low_band=1
 QP_dSc_test=.false.
 QP_solver=' '
#if defined PJ_SC
 QP_solver='n'
#endif
 QP_G_damp=0.1/HARTREE
 QP_dSc_delta=0.1/HARTREE
 QP_W_er=0.
 QP_W_dr=0.1/HARTREE
 QP_W_partially_done=.false.
 QP_G_er=(/0._SP,10._SP/HARTREE/)
 QP_G_dr=0.1/HARTREE
 QP_cg_percent=100.
 QP_xc_FUNCTIONAL=' '
 GWo_iterations=0
 GWo_SC_done=.false.
 !
 QP_ctl_db="none"
 QP_ctl_interp_neigh=1
 QP_ctl_E(1,:)=(/0.,1.,1./)
 QP_ctl_E(2,:)=(/0.,1.,1./)
 QP_ctl_E(3,:)=(/0.,1.,1./)
 QP_ctl_W=0.
 QP_ctl_Z=(1.,0.)
 QP_ctl_short_descr=' '
 !
 ! BS/BSS
 !
 BS_n_g_W=1
 BS_eh_en=(/-1.,-1./)/HARTREE
 BS_identifier=0
 BS_q=1
 BS_eh_win=100.
 BS_res_mode='xc'
 BS_cpl_mode='none'
 BS_K_dim=0
 BSS_mode=' '
 BSS_n_freqs=100
 BSS_n_descs=0
 BSS_er=(/0.,10./)/HARTREE
 BSS_dr=.1/HARTREE
 BSS_q0=(/1.,0.,0./)
 BSS_uses_RIM=.FALSE.
 BS_K_is_ALDA=.FALSE.
 BS_not_const_eh_f=.FALSE.
 Haydock_treshold=-0.02
 !
 ! TDDFT
 !
 FXC_description=""
 FXC_type='-rpa'
 FXC_n_descs=0
 FXC_n_g_corr=1
 FXC_per_memstps=100.
 FXC_LRC_alpha=0.
 FXC_SVD_cutoff=0.
 FXC_is_causal=.false.
 !
 ! ACFDT
 !
 ACFDT_n_lambda=1
 ACFDT_n_freqs=10
 ACFDT_E_range=(/100.,1000./)/HARTREE
 !
 ! xc_functionals  
 !
 GS_xc_FUNCTIONAL=-1             ! unknow 
 GS_xc_KIND=-1                   ! unknow 
 !
 ! ELPH
 !
 ph_modes=0
 elph_nb=0
 PH_damping=0.
 PH_null_E_rgn=0.1/HARTREE
 !
 end subroutine
