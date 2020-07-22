!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine setup(en,Xen,Ken,k,Xk)
 !
 use pars,           ONLY:SP,pi,schlen
 use drivers,        ONLY:l_setup
 use C_driver,       ONLY:code_bin
 use parser_m,       ONLY:parser
 use wave_func,      ONLY:wf_ng,wf_ncx
 use electrons,      ONLY:levels,n_met_bands,n_full_bands,E_duplicate,E_reset
 use D_lattice,      ONLY:a,alat,nsym,i_time_rev,DL_vol,Tel,dl_sop,&
&                         atoms_spatial_inversion,i_space_inv,input_Tel_is_negative,&
&                         inv_index,atoms_string,load_PT_elements,n_atoms_species,Z_species,&
&                         n_atomic_species,PT_elements,non_periodic_directions,lattice,&
&                         symmetry_group_table,mag_syms,idt_index,symmetry_equiv_atoms
 use R_lattice,      ONLY:RL_vol,b,n_g_shells,ng_in_shell,Xk_grid_new_mapping,&
&                         rl_sop,bz_samp,g_vec,E_of_shell,ng_closed,bz_samp_duplicate_Fg,BZ_FineGd_mode
 use com,            ONLY:msg,fat_log
 use stderr,         ONLY:intc
 use IO_m,           ONLY:REP,DUMP,NONE,io_control,OP_RD_CL
 use vec_operate,    ONLY:c2a,define_b_and_DL_vol
 use zeros,          ONLY:k_iku_zero,k_rlu_zero,G_iku_zero,define_zeros
 use interfaces,     ONLY:OCCUPATIONS_Fermi
 use wave_func,      ONLY:wf_ng,wf_ng_1st_BZ
#if defined _SC
 use wave_func,      ONLY:WF_buffered_IO 
 use drivers,        ONLY:l_sc_run
 use electrons,      ONLY:n_sp_pol
 use SC,             ONLY:load_SC_components,found_SC_DB
 use global_XC,      ONLY:G_kind,G_xc_functional,X_kind,&
&                         X_xc_functional,setup_global_XC,K_kind,K_xc_functional,loaded_WF_xc_string,&
&                         X_perturbation,G_perturbation,K_perturbation,WF_kind,WF_xc_functional,WF_perturbation
#endif
#if defined _HDF5_IO
 ! For I/O debug, see below
 !use IO_m,           ONLY:netcdf_call,nf90_set_log_level
#endif
 !
#include <memory.h>
 !
 type(levels)    ::en,Xen,Ken
 type(bz_samp)   ::k,Xk
 !
 ! Work Space
 !
 real(SP)          :: Tel_SAVE,tmp_V(3)
 integer           :: i1,i2,i3,is,i_err,nbf_m_SAVE(2),ID,n_to_print
 type(levels)      :: Xen_COPY
 character(schlen) :: dumb_ch
 integer, external :: k_lattice,io_Double_Grid
 !
#if defined _HDF5_IO
 ! This is very useful for I/O debug
 ! NETCDF library need to be compiled with --enable-loggning flag
 ! call netcdf_call(nf90_set_log_level(3),1)
#endif
 !
 call section('*','CORE Variables Setup') 
 !########################################
 !
 call section('+','Unit cells') 
 !=============================
 call crystal_lattice()
 !
 call msg('r','Cell kind             ',' '//trim(lattice))
 !
 call load_PT_elements()
 !
 atoms_string=' '
 if (allocated(Z_species)) then
   call msg('r','Atoms in the cell     ',' '//PT_elements(Z_species(:)))
   do is=1,n_atomic_species
     atoms_string=trim(atoms_string)//trim(intc(n_atoms_species(is)))//PT_elements(Z_species(is))
     if (is<n_atomic_species) atoms_string=atoms_string//"+"
     call msg('r','number of '//PT_elements(Z_species(is))//' atoms    ',n_atoms_species(is))
   enddo
 endif
 !
 ! DL vol / RL
 !
 call define_b_and_DL_vol()
 call msg('r','Alat factors          ',alat,'[a.u.]')
 !
 call msg('nr','Direct lattice volume ',DL_vol,'[a.u.]') !
 call msg('r','Direct lattice vectors',(/' A[ 1 ]  ',' A[ 2 ]  ',' A[ 3 ]  '/))
 tmp_V=(/a(1,1)/alat(1),a(1,2)/alat(2),a(1,3)/alat(3)/)
 call msg('r',' A[ 1 ]',tmp_V, ' [iru] ')
 if(fat_log.or.l_setup) call msg('r','        ',a(1,:),' [cc] ')
 tmp_V=(/a(2,1)/alat(1),a(2,2)/alat(2),a(2,3)/alat(3)/)
 call msg('r',' A[ 2 ]',tmp_V, ' [iru] ')
 if(fat_log.or.l_setup) call msg('r','        ',a(2,:),' [cc] ')
 tmp_V=(/a(3,1)/alat(1),a(3,2)/alat(2),a(3,3)/alat(3)/)
 call msg('r',' A[ 3 ]',tmp_V, ' [iru] ')
 if(fat_log.or.l_setup) call msg('r','        ',a(3,:),' [cc] ')
 !
 RL_vol=(2._SP*pi)**3/DL_vol
 call msg('nr','Recip. lattice volume ',RL_vol,'[a.u.]')
 call msg('r','Recip. lattice vectors',(/' B[ 1 ]  ',' B[ 2 ]  ',' B[ 3 ]  '/))
 tmp_V=b(1,:)
 call c2a(b_in=b,v_in=tmp_V,mode='kc2i')
 call msg('r',' B[ 1 ]',tmp_V, ' [iku] ')
 if(fat_log.or.l_setup) call msg('r','        ',b(1,:),' [cc] ')
 tmp_V=b(2,:)
 call c2a(b_in=b,v_in=tmp_V,mode='kc2i')
 call msg('r',' B[ 2 ]',tmp_V, ' [iku] ')
 if(fat_log.or.l_setup) call msg('r','        ',b(2,:),' [cc] ')
 tmp_V=b(3,:)
 call c2a(b_in=b,v_in=tmp_V,mode='kc2i')
 call msg('r',' B[ 3 ]',tmp_V, ' [iku] ')
 if(fat_log.or.l_setup) call msg('r','        ',b(3,:),' [cc] ')
 !
 ! ZERO's SETUP
 !
 call define_zeros(vector_=g_vec,zero_=G_iku_zero)
 call define_zeros(vector_=k%pt, zero_=k_iku_zero)
 call define_zeros(vector_=k%pt, zero_=k_rlu_zero,RLU=.TRUE.)
 !
 ! Symmetries and moltiplication table:
 !
 !  R_i*R_j=R_stab(i,j)
 !
 call section('=','Symmetries') 
 !=============================
 !
 ! Spin symmetries & xc spin for libxc
 !
 call build_spin_sop()
 !
 call build_xc_spin_map()
 !
 YAMBO_ALLOC(rl_sop,(3,3,nsym))
 !
 do is=1,nsym
   forall (i2=1:3,i3=1:3) rl_sop(i2,i3,is)=dl_sop(i2,i3,is)*alat(i2)/alat(i3)
 enddo
 !
 ! Indentity index
 !
 idt_index=-1
 do is=1,nsym
   if (all(nint(reshape(dl_sop(:,:,is),(/9/)))==(/1,0,0,0,1,0,0,0,1/)) ) idt_index=is
 enddo
 !
 if (idt_index==-1) then
   call warning('Identity not found among the given symmetry list')
 endif
 !
 ! Inversion index
 !
 inv_index=-1
 do is=1,nsym
   if ( all(nint(reshape(dl_sop(:,:,is),(/9/)))==(/-1,0,0,0,-1,0,0,0,-1/)) ) inv_index=is
 enddo
 !
 ! Defines i_space_inv value
 call atoms_spatial_inversion()
 !
 call msg('r','Inversion symmetry    ',inv_index>0)
 call msg('r','Spatial inversion     ',i_space_inv==1)
 if (inv_index>0) call msg('r','Inversion index       ',inv_index)
 !
 ! Time reversal
 !
 call msg('r','K-space Time-reversal ',(i_time_rev==1))
 call msg('r','Magnetic symmetries   ',mag_syms)
 if (i_time_rev==1) call msg('r','Time-reversal derived K-space symmetries',(/nsym/2+1,nsym/))
 !
 ! Symmetries Multiplication Table
 !
 call symmetry_group_table('r')
 call symmetry_equiv_atoms()
 !
 ! Symmetries list
 !
 call msg('rn','Symmetries units      ',' [cc]')
 do is=1,nsym
   dumb_ch=trim(intc(is))//'] '
   i1=9
   do while (i1<nsym)
     if(is<=i1) dumb_ch=' '//trim(dumb_ch)
     i1=i1*10+9
   enddo
   if (is<=nsym/(1+i_time_rev)) dumb_ch=' [S '//trim(dumb_ch)
   if (is> nsym/(1+i_time_rev)) dumb_ch=' [S*'//trim(dumb_ch)
   call msg('r',trim(dumb_ch),reshape(dl_sop(:,:,is),(/9/)))  
 enddo
 !
 call section('=','Reciprocal space')
 !============================
 !
 call G_shells_finder()
 !
 call msg('r','nG shells         ',n_g_shells)
 call msg('r','nG charge         ',ng_closed)
 call msg('r','nG WFs            ',wf_ng)
 call msg('r','nC WFs            ',wf_ncx)
 !
 ! Indexes of -G. minus_G_index(iG)| G_{minus_G_index(iG)}=-G. When there is no Spatial inversion
 ! the map is built in G_shells_finder
 !
 if (inv_index>0) call eval_minus_G()
 !
 n_to_print=min(800,n_g_shells)
 if (.not.fat_log) n_to_print=n_to_print/10
 !
 call msg('r','G-vecs. in first '//trim(intc(n_to_print))//' shells',' [ Number ] ')
 do i1=1,n_to_print,9
   call msg('r',' ',ng_in_shell(i1:min(i1+8,n_to_print)) )
 enddo
 if (n_to_print<n_g_shells) call msg('r',"...")
 !
 call msg('r','Shell energy in first '//trim(intc(n_to_print))//' shells',' [ mHa ] ')
 do i1=1,n_to_print,9
   call msg('r',' ',E_of_shell(i1:min(i1+8,n_to_print))*1000._SP)
 enddo
 if (n_to_print<n_g_shells) call msg('r',"...")
 !
 ! First simplified implementation of wf_ng_1st_BZ & wf_ng_2st_BZ
 ! A more correct implementation would involve a rotation of the shells
 ! and the evaluation of the resulting wf_ng
 !
 wf_ng_1st_BZ=wf_ng
 call PARSER_close_G(wf_ng_1st_BZ,'tRL')
 !
 call section('=','K-grid lattice')
 !=================================
 !
 Xk_grid_new_mapping=.false.
 !
 call parser('NoDiagSC',Xk_grid_new_mapping)
 if(Xk_grid_new_mapping) call msg('sr','Using the new bz sampling setup')
 !
 i_err=k_lattice(k,Xk,1,.TRUE.)
 if (i_err /= 0 ) then
   call warning('Trying to expand the k-grid')
   call msg('r','')
   i1=2
   i2=min(20,n_g_shells) 
   do while ( i_err /= 0 .and. i1<=i2)
     if (i1/=i2) i_err=k_lattice(k,Xk,i1,.FALSE.)
     if (i1==i2) i_err=k_lattice(k,Xk,i1,.TRUE.)
     i1=i1+1
   enddo
   if (i_err/=0) call error('Impossible to determine the K-grid lattice')
 endif
 !
 if (l_setup.or.fat_log.or.index(code_bin,'ypp')>0) call REPORT_Energies(en%E,k,Xk,en%nb,(/1,k%nibz/),'K',.TRUE.,.FALSE.)
 ! 
 ! E_dbgd
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=DUMP,ID=ID)
 i_err=io_Double_Grid(en,k,ID)
 !
 call bz_samp_duplicate_Fg(k%FGibz,Xk%FGibz,k%nibz) 
 call bz_samp_duplicate_Fg(k%FGbz,Xk%FGbz,k%nbz) 
 call bz_samp_duplicate_Fg(k%FGbare,Xk%FGbare,k%nibz) 
 !
 if (En%FG%nb>0.and.allocated(en%FG%E)) then
   !
   call section('=','Double K-grid')
   !=======================================
   !
   call msg('r', 'Symmetry Mode                         ',BZ_FineGd_mode)
   call msg('r', 'Kpts (IBZ): Coarse,Fine,Duplicated    ',(/k%nibz,k%FGibz%N,k%FGibz%N_duplicate/))
   call msg('r', 'Kpts ( BZ): Coarse,Fine,Duplicated    ',(/k%nbz, k%FGbz%N, k%FGbz%N_duplicate/))
   call msg('r', 'Bands                                 ',en%FG%nb)
   !
 endif
 !
 if (trim(non_periodic_directions)/="none") then
   !
   call section('=','Non periodic geometry')
   !========================================
   !
   call check_periodic_directions((/.false.,.false.,.false./))
   !
 endif
 !
 call section('=','Energies & Occupations')
 !==============================================
 !
 !call msg('rn','Energy unit   ',' [eV]')
 !
 Xen%nk=Xk%nibz
 ! 
 call E_duplicate(en,Xen)
 !
 ! SC Energies 
 !-------------
#if defined _SC
 !
 ! First check if it possible to load the SC energies
 !
 call load_SC_components('E',E=en,COM_=REP,MODE_=DUMP,ik=0,&
&                         kind=G_kind,xc_functional=G_xc_functional,perturbation=G_perturbation)
 if (found_SC_DB) call setup_global_XC('G',G_kind,G_xc_functional,G_perturbation)
 call load_SC_components('E',E=Xen,COM_=NONE,MODE_=DUMP,ik=0,&
&                         kind=X_kind,xc_functional=X_xc_functional,perturbation=X_perturbation)
 if (found_SC_DB) call setup_global_XC('X',X_kind,X_xc_functional,X_perturbation)
 if (found_SC_DB.and.WF_buffered_IO) then
   call warning(' SC WFs and WF_buffered not supported. Turning off buffering')
   WF_buffered_IO=.false.
 endif
#endif
 !
 ! Negative Temperature indicates that such Temperature is held
 ! fixed in the calculation (not overwritten in io_header.F, for
 ! example).
 !
 ! Moreover also %nbf and %nbm are kept at the value obtained at zero 
 ! temperature
 !
 input_Tel_is_negative=Tel<0.
 Tel=abs(Tel)
 Tel_SAVE=Tel
 !
 if (input_Tel_is_negative) then
   call E_duplicate(Xen,Xen_COPY)
   Tel=0._SP
   call OCCUPATIONS_Fermi(Xen_COPY,Xk,mode="OCCUPATIONS")
   nbf_m_SAVE=(/Xen_COPY%nbf,Xen_COPY%nbm/)
   call E_reset(Xen_COPY)
 endif
 !
 Tel=Tel_SAVE
 call OCCUPATIONS_Fermi(Xen,Xk,OBJ="X")
 call OCCUPATIONS_Extend(Xen,en)
 !
 if (input_Tel_is_negative) then
   Xen%nbf=nbf_m_SAVE(1)
   Xen%nbm=nbf_m_SAVE(2)
   en%nbf =nbf_m_SAVE(1)
   en%nbm =nbf_m_SAVE(2)
 endif
 !
 n_met_bands =en%nbm
 n_full_bands=en%nbf
 !
 ! Energies report
 !
 if (l_setup.or.fat_log.or.index(code_bin,'ypp')>0) call REPORT_Energies(en%E,k,Xk,en%nb,(/1,k%nibz/),'E',.FALSE.,.TRUE.)
 !
 ! Once the occupations have been evaluated I dump Xen in Ken
 !
 call E_duplicate(Xen,Ken)
 !
#if defined _SC
 K_kind=X_kind
 K_xc_functional=X_xc_functional
 K_perturbation=X_perturbation
 call setup_global_XC('K',K_kind,K_xc_functional,K_perturbation)
 !
 if (.not.l_sc_run) then
   !
   ! SC wavefunctions
   !
   ! Here I only check for the existence of the SC WF DB.
   !
   call load_SC_components('check_WF_DB',n_bands=0,&
&                          kind=WF_kind,xc_functional=WF_xc_functional,perturbation=WF_perturbation)
   !
   if (found_SC_DB) then
     call setup_global_XC('WF',WF_kind,WF_xc_functional,WF_perturbation)
     call warning(trim(loaded_WF_xc_string)//' wavefunctions found')
   endif
   !
 endif
 !
 ! Copy Ho eigenvalues in Xen%Eo for dipoles
 !
 if (l_sc_run .and. .not.allocated(Xen%Eo) .and. found_SC_DB) then
   YAMBO_ALLOC(Xen%Eo,(Xen%nb,Xen%nk,n_sp_pol))
   Xen%Eo(:,:,:)=en%Eo(:,:en%nk,:)
 endif
 !
#endif
 !
end subroutine
