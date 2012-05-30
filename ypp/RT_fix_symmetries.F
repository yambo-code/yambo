!
! Copyright (C) 2000-2012 C. Attaccalite, A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_fix_symmetries(E,k)
 !
 use YPP,                 ONLY:E_field,B_theta,B_psi,B_field,Magnetization
 use drivers,             ONLY:l_sc_run
 use pars,                ONLY:IP,LP,SP,DP,lchlen
 use com,                 ONLY:msg,warning,core_io_path,more_io_path
 use memory_m,            ONLY:mem_est
 use electrons,           ONLY:levels,E_reset,n_spin,n_sp_pol,n_spinor
 use vec_operate,         ONLY:v_norm,v_is_zero,i_sort
 use matrix_operate,      ONLY:m3det
 use D_lattice,           ONLY:nsym,dl_sop,spin_sop,i_time_rev,a,alat,mag_syms,&
&                              i_space_inv,sop_inv,inv_index,symmetry_group_table
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz,g_rot,&
&                              ng_closed,bz_samp_duplicate,rl_sop
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,OP_WR,OP_RD,VERIFY,&
&                              OP_APP,cp_file,rm_file,cp_directory,Fragmented_IO,mk_dir, &
&                              io_fragmented,OP_RD_CL,RD_CL,RD,DUMP,RD_CL_IF_END,WR_CL_IF_END
 use wave_func,           ONLY:ioWF,wf_nc_k, wf_igk,wf_ncx,wf_nb_io, &
&                              wf_ng,wf_state,WF_load,wf_state,wf,WF_free,wf_ng ,wf_nb_io_groups
 use real_time,           ONLY:kpoints_map
 use pseudo,              ONLY:pp_kb,pp_kbd,n_atomic_species,pp_n_l_times_proj_max,PP_free
 use SC,                  ONLY:local_V,SC_R,H_nl_sc,SC_bands
 use stderr,              ONLY:intc,set_real_printed_length
 use parser_m,            ONLY:parser
 use zeros,               ONLY:k_iku_zero
 use timing,              ONLY:live_timing
 !
 implicit none
 type(levels)                 :: E,E_expanded
 type(bz_samp)                :: k
 !
 integer, external :: ioDB1,io_SC_components,ioKB_PP
 !
 ! Work Space
 !
 integer               :: ik_save,iv(2),ierr,ib,i_wf,n_steps,i_spin,&
&                         sop_inv_save(nsym),ID,io_err,i1,i2,is,ik
 integer               :: spatial_nsym,old_spatial_nsym,old_trev_nsym
 integer,allocatable   :: wf_nc_k_save(:),wf_igk_save(:,:),ic_rot_table(:)
 real(SP)              :: v1(3),v2(3),m(3,3)
 real(SP)              :: B_cart(3)
 integer               :: symm_M_factor,symm_B_factor,ig,ig_rot,ic,ic_rot
 real(SP), allocatable :: pp_kb_store(:,:,:,:,:),pp_kbd_store(:,:,:,:,:)
 logical               :: Rm_all_symm,use_E_field,use_B_field,has_magnetization,reject_symmetry,Rm_TR,&
&                         S_contains_TR(nsym),Trev_2_spatial=.false.,Rm_inv_BM_axis
 type(bz_samp)         :: k_save
 type(levels)          :: en_sc,en_sc_expanded
 character(lchlen)     :: core_io_path_save,fragment_name
 integer               :: ACTION_,ioKB_err,io_SC_E_err,io_SC_V_err,io_SC_WF_err
 complex(SP), allocatable :: full_H(:,:,:,:)
 real(SP),    allocatable :: wf_disk(:,:,:,:)
 real(SP),    allocatable :: dl_sop_tmp(:,:,:)
 complex(SP), allocatable :: wf_tmp(:,:)
 !
 mag_syms=.FALSE.
 use_E_field=.FALSE.
 use_B_field=.FALSE.
 has_magnetization=.FALSE.
 if (any(E_field/=0.)) use_E_field=.TRUE.
 if (B_field/=0.)      use_B_field=.TRUE.
 if (any(Magnetization/=0.))  has_magnetization=.TRUE.
 !
 call parser('RmAllSymm',Rm_all_symm)
 call parser('RmTimeRev',Rm_TR)
 !
 ! Do not consider symmtries which invert the axis along the magnetization or the field B
 call parser('RmInvMBaxis',Rm_inv_BM_axis)
 !
 ! Check if we are using mag symmetries
 !
 if ( (use_B_field.or.(n_spinor==2.and.has_magnetization)).and. &
&      i_time_rev==1.and..not.Rm_TR) mag_syms=.true.
 if (mag_syms) call msg('s','There are a magnetic field or SOC in a magnetic system with T-rev')
 !
 if (Rm_all_symm) then
   call section('*',"== Remove all Symmetries ==")
   use_E_field=.false.
   use_B_field=.false.
   has_magnetization=.false.
   Rm_TR=.false.
   E_field=0.
   B_field=0.
   Magnetization=0.
 endif
 !
 if (use_E_field) call section('*',"== Electric Fields: BZ sampling remapping ==")
 !
 if (use_B_field) then
   call section('*',"== Magnetic fields: BZ sampling remapping ==")
   B_cart(1)=B_field*sin(B_theta)*cos(B_psi)
   B_cart(2)=B_field*sin(B_theta)*sin(B_psi) 
   B_cart(3)=B_field*cos(B_theta)
 endif
 if(has_magnetization) call section('*',"== Magnetized system: BZ sampling remapping ==")
 !
 if (i_time_rev==0) Rm_TR=.false.
 if (Rm_TR)       call section('*',"== Remove Time Reversal ==")
 !
 if (.not.any((/use_B_field,use_E_field,has_magnetization,Rm_all_symm,Rm_TR/))) return 
 !
 if (use_B_field.and.has_magnetization) call warning('Magnetic field in magnetic system!')
 !
 if (wf_nb_io/=E%nb) then
   call warning('Splitted wavefunctions still not supported')
   return
 endif
 !
 call section('+',"Symmetries")
 !=============================
 !
 ! Seek for all possible shells
 !
 if (wf_ncx>ng_closed) then
   ng_closed=wf_ncx
   call G_shells_finder()
 endif
 !
 ! Expansion of default k-points
 !
 call k_ibz2bz(k,'i',.true.)
 call k_reduce(k)
 !
 call bz_samp_reset(k_save)
 call bz_samp_duplicate(k,k_save)
 !
 ! Check presence of SC wave-functions
 !
 call E_reset(en_sc)
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
 io_SC_WF_err=io_SC_components('WF',en_sc,ID)
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
 io_SC_E_err=io_SC_components('E',en_sc,ID)
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
 io_SC_V_err=io_SC_components('V',en_sc,ID)
 !
 if (io_SC_E_err/=0.or.io_SC_WF_err/=0.or.io_SC_V_err/=0) io_SC_E_err=-1 
 !
 ! Read PseudoPotentials
 !
 call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID)
 ioKB_err=ioKB_PP(ID) 
 !
 if(ioKB_err==0) then
   ! 
   call section('=',"PseudoPotentials")
   allocate(pp_kb_store(wf_ng,n_atomic_species,pp_n_l_times_proj_max,n_spin,k%nibz))
   allocate(pp_kbd_store(wf_ng,n_atomic_species,pp_n_l_times_proj_max,n_spin,k%nibz))
   call mem_est("pp_kb_store pp_kbd_store",(/size(pp_kb),size(pp_kbd)/),elements_kind=(/SP,SP/))
   !
   do ik=1,k_save%nibz
     !
     if (ik< k_save%nibz) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==k_save%nibz) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)      
     ioKB_err=ioKB_PP(ID)
     !
     pp_kb_store(:,:,:,:,ik) =pp_kb(:,:,:,:)
     pp_kbd_store(:,:,:,:,ik)=pp_kbd(:,:,:,:)
     !
   enddo
   !
 endif
 !
 ! time reversal-related Symmetries
 !
 forall (is=1:nsym) S_contains_TR(is)=is>nsym/(1+i_time_rev)
 !
 ! Construct spin_sop in spinor-space
 !
 call build_spin_sop()
 !
 ! Remove Symmetries
 !
 spatial_nsym=nsym/(1+i_time_rev)
 !
 old_spatial_nsym=spatial_nsym
 old_trev_nsym=nsym-spatial_nsym
 !
 if(Rm_TR) then   ! No time reversal 
   if(i_space_inv==1.and.inv_index>spatial_nsym.and..not.mag_syms) then
     call msg('s',trim(intc(spatial_nsym))//' T-rev are now spatial symm')
     spatial_nsym=2*spatial_nsym
     nsym=spatial_nsym
     Trev_2_spatial=.true.
   else
     nsym=nsym/(1+i_time_rev)
   endif
   i_time_rev=0
 endif
 !
 if(Rm_all_symm) then
   nsym=1
   spatial_nsym=1
   i_time_rev=0
 else
   is=2
   !
   do while (is<=nsym)
     !
     reject_symmetry=.false.
     if (use_E_field) then
       v1(:) = matmul( dl_sop(:,:,is), E_field(1,:) )      
       v2(:) = matmul( dl_sop(:,:,is), E_field(2,:) )      
       ! Ok Spatial symmetries that leave E invariant
       if(is<=spatial_nsym) reject_symmetry=v_norm(v1(:)-E_field(1,:))>1.E-5.or.v_norm(v2(:)-E_field(2,:))>1.E-5
       ! E is invariant under T-rev
       if(is> spatial_nsym) reject_symmetry=v_norm(v1(:)+E_field(1,:))>1.E-5.or.v_norm(v2(:)+E_field(2,:))>1.E-5
     endif
     !
     if (use_B_field.and..not.reject_symmetry) then
       symm_B_factor=0
       v1(:) = matmul( dl_sop(:,:,is), B_cart )       
       if(all( abs(v1(:)-B_cart(:))<epsilon(1.)) ) symm_B_factor=+1
       if(all( abs(v1(:)+B_cart(:))<epsilon(1.)) ) symm_B_factor=-1
       reject_symmetry=(symm_B_factor==0)
       if(.not.reject_symmetry.and.Rm_inv_BM_axis) reject_symmetry=(symm_B_factor==-1)
       if(.not.reject_symmetry) then
         ! We check what happens in the plane perpendicular to B: 
         ! 1) B is invariant under spatial inversion
         if(is<=spatial_nsym) reject_symmetry= ( abs ( m3det(dl_sop(:,:,is))/symm_B_factor - 1.)>1.E-5)
         ! 2) T-rev inverts B
         if(is> spatial_nsym) reject_symmetry= ( abs ( m3det(dl_sop(:,:,is))/symm_B_factor + 1.)>1.E-5)       
       endif
     endif
     !
     if(has_magnetization.and.n_spinor==2.and..not.reject_symmetry) then
       symm_M_factor=0
       v1(:) = matmul( dl_sop(:,:,is), Magnetization )
       if(all( abs(v1(:)-Magnetization(:))<epsilon(1.)) ) symm_M_factor=+1
       if(all( abs(v1(:)+Magnetization(:))<epsilon(1.)) ) symm_M_factor=-1
       reject_symmetry=(symm_M_factor==0)
       if(.not.reject_symmetry.and.Rm_inv_BM_axis) reject_symmetry=(symm_M_factor==-1)
       if(.not.reject_symmetry) then
         ! We check what happens in the plane perpendicular to M: 
         ! 1) M is invariant under space inv
         if(is<=spatial_nsym) reject_symmetry= ( abs ( m3det(dl_sop(:,:,is))/symm_M_factor - 1.)>1.E-5)
         ! 2) T-rev inverts M
         if(is> spatial_nsym) reject_symmetry= ( abs ( m3det(dl_sop(:,:,is))/symm_M_factor + 1.)>1.E-5)
       endif
       !
     endif
     !
     if(reject_symmetry) then
       dl_sop(:,:,is:nsym-1)=dl_sop(:,:,is+1:nsym)
       nsym=nsym-1
       if(is<=spatial_nsym) spatial_nsym=spatial_nsym-1
     else
       is=is+1
     endif
     !
   enddo
 endif
 !
 if(.not.RM_all_symm)&
& call msg('s',trim(intc(old_spatial_nsym))//' spatial symmetries reduced to ...'//trim(intc(spatial_nsym)))
 !
 if(i_time_rev==1)&
& call msg('s',trim(intc(old_trev_nsym))//' T-rev symmetries reduced to ...'//trim(intc(nsym-spatial_nsym)))
 !
 allocate(dl_sop_tmp(3,3,nsym))
 dl_sop_tmp(1:3,1:3,1:nsym)=dl_sop(1:3,1:3,1:nsym)
 deallocate(dl_sop)
 allocate(dl_sop(3,3,nsym))
 dl_sop=dl_sop_tmp
 deallocate(dl_sop_tmp)
 !
 call set_real_printed_length(f_length=7,g_length=8)
 call msg('s','[Symm] Spatial simmetries now are:')  
 do is=1,spatial_nsym
   call msg('s','[S'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))
 enddo
 if (i_time_rev==1) then
   call msg('s','[Symm] T-rev derived simmetries now are:')  
   do is=spatial_nsym+1,nsym
     call msg('s','[S*'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))  
   enddo
 endif
 call set_real_printed_length()
 !
 ! Remap Inversion symmetry index
 !
 inv_index=-1
 do is=1,nsym
   forall (i1=1:3,i2=1:3) rl_sop(i1,i2,is)=dl_sop(i1,i2,is)*alat(i1)/alat(i2)
   if ( all(nint(reshape(dl_sop(:,:,is),(/9/)))==(/-1.,0.,0.,0.,-1.,0.,0.,0.,-1./)) ) inv_index=is
 enddo
 if (inv_index<0) i_space_inv=0
 !
 if(i_time_rev==1 .and. inv_index>0) then
   if(inv_index<=spatial_nsym) then
     i_space_inv=0
     call msg('s','[SYMs] Spatial inversion '//trim(intc(inv_index))//' is a symmetry')
   else
     call msg('s','[SYMs] Spatial inversion '//trim(intc(inv_index))//' is NOT a symmetry')
   endif
 endif
 !
 ! Check group table
 !
 sop_inv_save=sop_inv
 !
 call symmetry_group_table('s')
 !
 call section('=',"K-points")
 !===========================
 !
 call msg('s',trim(intc(k%nibz))//' k-points are now expanded to ...')
 deallocate(k%pt)
 call k_reduce(k)
 call msg('l'," "//trim(intc(k%nibz)))
 !
 ! Map k-point using the save BZ
 !
 ! k_points_map(1, ? ) = ik
 ! k_points_map(2, ? ) = is
 !
 ! with ik,is relative to the unreduced set of k-points and symmetries
 !
 allocate(kpoints_map(2,k%nibz))
 !
 kpoints_map=0
 do ik=1,k%nibz 
   do ik_save=1,k%nbz
     if (v_is_zero(k%pt(ik,:)-k_save%ptbz(ik_save,:),zero_=k_iku_zero)) then
       kpoints_map(:, ik ) = k_save%sstar(ik_save,:)
       exit 
     endif
   enddo
 enddo
 !
 ! swap ik and i1 i.e.
 ! sort first all the points in the ibz
 ! 
 main_loop: do ik=1,k%nibz 
   if ( kpoints_map(2, ik )/=1 ) then
     do i1=ik+1,k%nibz
       if ( kpoints_map(2, i1 )==1 ) then
         iv=kpoints_map(:, ik )
         v1=k%pt(ik,:)
         kpoints_map(:,ik)=kpoints_map(:,i1)
         kpoints_map(:,i1)=iv
         k%pt(ik,:)=k%pt(i1,:)                 
         k%pt(i1,:)=v1
         cycle main_loop
       endif
     enddo
   endif
 enddo main_loop
 !
 call section('=',"Energies")
 !===========================
 !
 call map_energies(E,E_expanded,k,kpoints_map,S_contains_TR)
 !
 if (trim(core_io_path)/=trim(more_io_path)) then
   call mk_dir(more_io_path)
   call mk_dir(trim(more_io_path)//'/SAVE')
 endif
 !
 if(io_SC_E_err/=-1) then
   call section('=',"SC wavefunctions and energies")
   !===============================
   !
   ! Energies
   !
   en_sc%nb=SC_bands(2)
   en_sc%nk=k_save%nibz
   allocate(en_sc%E(en_sc%nb,en_sc%nk,n_sp_pol))  
   !
   nkibz=k_save%nibz
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1,2/),ID=ID)
   io_SC_E_err=io_SC_components('E',en_sc,ID)
   !
   call map_energies(en_sc,en_sc_expanded,k,kpoints_map,S_contains_TR)
   !
   nkibz=k%nibz
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,MODE=DUMP,SEC=(/1,2/),ID=ID)
   io_SC_E_err=io_SC_components('E',en_sc_expanded,ID)
   !
   !  Wave-function
   !
   allocate(SC_R(SC_bands(2),SC_bands(2),k%nibz,n_sp_pol))
   call mem_est("SC_R",(/size(SC_R)/))
   !
   nkibz=k_save%nibz
   !
   call io_control(ACTION=OP_RD,COM=NONE,MODE=VERIFY,SEC=(/1/),ID=ID)
   io_SC_WF_err=io_SC_components('WF',en_sc,ID)
   !
   do ik=1,k_save%nibz
     call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID)
     io_SC_WF_err=io_SC_components('WF',en_sc,ID)
   enddo
   !
   nkibz=k%nibz
   !
   call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=ID)
   io_SC_WF_err=io_SC_components('WF',en_sc,ID)
   !
   do ik=1,k_save%nibz
     call io_control(ACTION=WR_CL_IF_END,SEC=(/ik+1/),ID=ID)
     io_SC_WF_err=io_SC_components('WF',en_sc,ID)
   enddo
   !
   do ik=k_save%nibz+1,k%nibz
     SC_R(:,:,ik,:)=SC_R(:,:,kpoints_map(1,ik),:)
     call io_control(ACTION=WR_CL_IF_END,SEC=(/ik+1/),ID=ID)
     io_SC_WF_err=io_SC_components('WF',en_sc,ID)
   enddo
   !
   deallocate(SC_R)
   call mem_est("SC_R")
   !
   ! Copy the potential
   !
   call XC_switch()
   if(.not.local_V) then
     !
     allocate(H_nl_sc(SC_bands(2),SC_bands(2),n_sp_pol))
     call mem_est("H_nl_sc",(/size(H_nl_sc)/))
     !
     allocate(full_H(SC_bands(2),SC_bands(2),k_save%nibz,n_sp_pol))
     call mem_est("full_H",(/size(full_H)/))
     !
     nkibz=k_save%nibz
     !
     call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
     io_SC_V_err=io_SC_components('V',en_sc,ID)
     !
     do ik=1,k_save%nibz
       call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID)
       io_SC_V_err=io_SC_components('V',en_sc,ID)
       full_H(:,:,ik,:)=H_nl_sc(:,:,:)
     enddo
     !
     nkibz=k%nibz
     !
     call io_control(ACTION=OP_WR,COM=NONE,SEC=(/1/),ID=ID)
     io_SC_V_err=io_SC_components('V',en_sc,ID)
     !
     do ik=1,k_save%nibz
       H_nl_sc(:,:,:)=full_H(:,:,ik,:)
       call io_control(ACTION=WR_CL_IF_END,SEC=(/ik+1/),ID=ID)
       io_SC_V_err=io_SC_components('V',en_sc,ID)
     enddo
     !
     do ik=k_save%nibz+1,k%nibz
       H_nl_sc(:,:,:)=full_H(:,:,kpoints_map(1,ik),:)
       call io_control(ACTION=WR_CL_IF_END,SEC=(/ik+1/),ID=ID)
       io_SC_V_err=io_SC_components('V',en_sc,ID)
     enddo    
     !
     deallocate(H_nl_sc)
     call mem_est("H_nl_sc")
     !
     deallocate(full_H)
     call mem_est("full_H")
     !
   endif
   !
 endif
 !
 call section('=',"Wavefunctions")
 !===============================
 !
 if(io_SC_E_err/=-1) l_sc_run=.true. ! to not read again SC wavefunctions
 !
 call WF_load(wf_ng,1,(/1,E%nb/),(/1,k_save%nibz/),space='C',title='')
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID)  
 Fragmented_IO=io_fragmented(ID)
 io_err=ioWF(ID)
 !
 if (trim(core_io_path)/=trim(more_io_path)) then
   !
   call msg('s',':: Copying existing database ...')
   !
   call cp_file(trim(core_io_path)//"/SAVE/s.kb_pp",trim(more_io_path)//"/SAVE",ierr)
   call cp_file(trim(core_io_path)//"/SAVE/ns.kb_pp",trim(more_io_path)//"/SAVE",ierr)
   call cp_file(trim(core_io_path)//"/SAVE/s.wf",trim(more_io_path)//"/SAVE",ierr)
   call cp_file(trim(core_io_path)//"/SAVE/ns.wf",trim(more_io_path)//"/SAVE",ierr)
   do ik=1,k%nibz
     do i1=1,wf_nb_io_groups
       fragment_name='ns.wf_fragments_'//trim(intc(ik))//'_'//trim(intc(i1))
       call cp_directory(trim(core_io_path)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
       fragment_name='s.wf_fragments_'//trim(intc(ik))//'_'//trim(intc(i1))
       call cp_directory(trim(core_io_path)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
       fragment_name='ns.kb_pp_fragments_'//trim(intc(ik))//'_'//trim(intc(i1))
       call cp_directory(trim(core_io_path)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
       fragment_name='s.kb_pp_fragments_'//trim(intc(ik))//'_'//trim(intc(i1))
       call cp_directory(trim(core_io_path)//"/SAVE/"//trim(fragment_name),trim(more_io_path)//"/SAVE",ierr)
     enddo
   enddo
   !
   call msg('l','done')
   !
 else
   call warning(':: Existing database will be overwritten')
 endif
 !
 ! Rotating wavefunctions components
 !
 allocate(wf_nc_k_save(k_save%nibz),wf_igk_save(wf_ncx,k_save%nibz))
 call mem_est("wf_nc_k_save wf_igk_save",(/size(wf_nc_k_save),size(wf_igk_save)/),(/IP,IP/))
 wf_nc_k_save=wf_nc_k
 wf_igk_save =wf_igk
 !
 deallocate(wf_nc_k,wf_igk)
 allocate(wf_nc_k(k%nibz),wf_igk(wf_ncx,k%nibz))
 !
 wf_igk=-1
 do i1=1,k_save%nibz
   ik=kpoints_map(1,i1)
   !
   wf_igk(1:wf_ncx,i1) = wf_igk_save(1:wf_ncx,ik)
   wf_nc_k(i1)         = min(ng_closed,wf_nc_k_save(ik))
   !
 enddo
 !
 do i1=k_save%nibz+1,k%nibz 
   !
   ik=kpoints_map(1,i1)
   is=kpoints_map(2,i1)
   !
   wf_nc_k(i1)              = min(ng_closed,wf_nc_k_save(ik))
   wf_igk(1:wf_nc_k(i1),i1) = g_rot(is,wf_igk_save(1:wf_nc_k(i1),ik))
   !
   call i_sort( wf_igk(1:wf_nc_k(i1),i1) )
   !
 enddo
 !
 deallocate(wf_igk_save,wf_nc_k_save)
 call mem_est("wf_nc_k_save wf_igk_save")
 !
 wf_ng=maxval(wf_igk)
 !
 call section('=',"Final I/O")
 !============================
 !
 core_io_path_save=core_io_path
 core_io_path=more_io_path
 !
 n_steps=(k%nibz-k_save%nibz)*E%nb
 call live_timing('WFs rotation',n_steps)
 !
 allocate(ic_rot_table(wf_ncx))
 allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
 allocate(wf_tmp(wf_ncx,n_spin))
 !
 wf_disk=(0.,0.)
 !
 ACTION_=OP_APP_WR_CL
 if(Fragmented_IO) ACTION_=OP_WR_CL
 !
 do i1=k_save%nibz+1,k%nibz 
   !      
   ik=kpoints_map(1,i1)
   is=kpoints_map(2,i1)
   !
   call io_control(ACTION=ACTION_,COM=NONE,SEC=(/i1+1,1/),ID=ID)
   !
   ic_rot_table=-1
   do ic=1,wf_nc_k(ik)
     ig_rot=g_rot(is,wf_igk(ic,ik))
     if(ig_rot>wf_ng) cycle
     do ic_rot=1,wf_nc_k(i1)
       ig=wf_igk(ic_rot,i1)
       if(ig==ig_rot) exit
     enddo
     ic_rot_table(ic)=ic_rot
   enddo
   !
   do ib=1,E%nb
     !
     wf_tmp=(0._SP,0._SP)
     !
     do i_spin=1,n_spin
       !
       i_wf=wf_state(ib,ik,i_spin)
       !
       forall(ic=1:wf_nc_k(ik)) wf_tmp(ic_rot_table(ic),i_spin)=wf(ic,i_wf)
       !
     enddo
     !
     forall(ic=1:wf_nc_k(i1)) wf_tmp(ic,:)=matmul(spin_sop(:,:,is),wf_tmp(ic,:))
     if ( S_contains_TR(is) ) wf_tmp=conjg(wf_tmp)
     !
     wf_disk(1,ib,1:wf_ncx,:)= real(wf_tmp(:wf_ncx,:))
     wf_disk(2,ib,1:wf_ncx,:)= aimag(wf_tmp(:wf_ncx,:))
     !
     call live_timing(steps=1)
     !
   enddo
   !
   io_err=ioWF(ID,wf=wf_disk)
   !
 enddo
 !
 call live_timing()
 ! 
 if(Fragmented_IO) then
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
   io_err=ioWF(ID)
 endif
 !
 deallocate(ic_rot_table,wf_disk)
 call mem_est("wf_disk")
 deallocate(wf_tmp)
 !
 if(ioKB_err==0) then 
   !
   call msg('s',':: PseudoPotentials ...')
   !
   do i1=k_save%nibz+1,k%nibz 
     !
     ik=kpoints_map(1,i1)
     !
     call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/i1+1/),ID=ID)      
     ioKB_err=ioKB_PP(ID)
     !
   enddo
   !
   deallocate(pp_kb_store,pp_kbd_store)
   call mem_est("pp_kb_store pp_kbd_store")
   !
   call PP_free()
   !
   call msg('l','done')
   !
 endif
 !
 call msg('s',':: DB1 ...')
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=ioDB1(E_expanded,k,ID)
 !
 core_io_path=core_io_path_save
 !
 call msg('l','done')
 !
 ! (n)db clean
 !=============
 !
 if (trim(core_io_path)==trim(more_io_path)) then
   call msg('s',':: Cleaning ...')
   call rm_file((trim(core_io_path))//"/SAVE/ndb.kindx")
   call rm_file((trim(core_io_path))//"/SAVE/ndb.gops")
   call rm_file((trim(core_io_path))//"/SAVE/ndb.dipoles")
   call rm_file((trim(core_io_path))//"/SAVE/db.dipoles")
   call rm_file((trim(core_io_path))//"/SAVE/ndb.P_and_P2")
   call rm_file((trim(core_io_path))//"/SAVE/db.P_and_P2")
   call rm_file((trim(core_io_path))//"/SAVE/db.kindx")
   call rm_file((trim(core_io_path))//"/SAVE/db.gops")
   call msg('l','done')
 endif
 !
 ! CLEAN
 !=======
 deallocate(kpoints_map)
 call bz_samp_reset(k_save)
 call E_reset(E_expanded)
 call WF_free()
 !
end subroutine
 
subroutine map_energies(E,E_expanded,k,kpoints_map,S_contains_TR)
 !
 use electrons,           ONLY:levels,E_reset,n_sp_pol
 use R_lattice,           ONLY:bz_samp
 !
 implicit none
 !
 type(levels), intent(in)     :: E
 type(levels), intent(out)    :: E_expanded
 type(bz_samp)                :: k
 integer,       intent(in)    :: kpoints_map(2,k%nibz)
 logical,       intent(in)    :: S_contains_TR(100)
 !
 ! Work Space
 !
 integer :: ik,is
 !
 call E_reset(E_expanded)
 E_expanded%nb=E%nb
 E_expanded%nk=k%nibz
 allocate(E_expanded%E(E%nb,k%nibz,n_sp_pol))
 !
 do ik=1,k%nibz
   if(n_sp_pol==1) then
     E_expanded%E(:,ik,1)=E%E(:,kpoints_map(1,ik),1)
     cycle
   endif
   is=kpoints_map(2,ik)
   if(.not.S_contains_TR(is)) E_expanded%E(:,ik,:)=E%E(:,kpoints_map(1,ik),:)
   ! T-rev flips the spin
   if(     S_contains_TR(is)) then
     E_expanded%E(:,ik,1)=E%E(:,kpoints_map(1,ik),2)
     E_expanded%E(:,ik,2)=E%E(:,kpoints_map(1,ik),1)
   endif
 enddo
 !
end subroutine map_energies
