!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use parser_m,       ONLY:parser
 use electrons,      ONLY:levels,n_met_bands,n_full_bands,E_duplicate,E_reset,&
&                         BZ_DbGd_nbands,DbGd_is_symmetry_expanded,BZ_DbGd_tot_nk_levels
 use D_lattice,      ONLY:a,alat,nsym,i_time_rev,DL_vol,Tel,dl_sop,&
&                         atoms_spatial_inversion,i_space_inv,input_Tel_is_negative,&
&                         inv_index,atoms_string,load_PT_elements,n_atoms_species,Z_species,&
&                         n_atomic_species,PT_elements,non_periodic_directions,lattice,&
&                         symmetry_group_table,mag_syms,idt_index,symmetry_equiv_atoms
 use R_lattice,      ONLY:RL_vol,b,n_g_shells,ng_in_shell,Xk_grid_new_mapping,&
&                         rl_sop,bz_samp,g_vec,E_of_shell
 use com,            ONLY:msg,error,warning
 use stderr,         ONLY:intc,real2ch
 use IO_m,           ONLY:REP,DUMP,NONE
 use vec_operate,    ONLY:c2a,define_b_and_DL_vol
 use zeros,          ONLY:k_iku_zero,k_rlu_zero,G_iku_zero,define_zeros
 use interfaces,     ONLY:OCCUPATIONS_Fermi
#if defined _SC
 use wave_func,      ONLY:WF_buffered_IO 
 use drivers,        ONLY:l_sc_run
 use electrons,      ONLY:n_sp_pol
 use SC,             ONLY:load_SC_components,found_SC_DB
 use global_XC,      ONLY:G_kind,G_xc_functional,X_kind,&
&                         X_xc_functional,setup_global_XC,K_kind,K_xc_functional,loaded_WF_xc_string,&
&                         X_perturbation,G_perturbation,K_perturbation,WF_kind,WF_xc_functional,WF_perturbation
#endif
 !
#include <memory.h>
 !
 type(levels)    ::en,Xen,Ken
 type(bz_samp)   ::k,Xk
 !
 ! Work Space
 !
 real(SP)          :: rv(3),Tel_SAVE
 integer           :: i1,i2,i3,is,i_err,nbf_m_SAVE(2)
 type(levels)      :: Xen_COPY
 character(schlen) :: dumb_ch1,dumb_ch2
 integer, external :: k_lattice
 !
 call section('*','CORE Variables Setup') 
 !########################################
 !
 call section('+','Unit cells') 
 !=============================
 call crystal_lattice()
 !
 call msg('rn','Unit cell is '//trim(lattice))
 !
 call load_PT_elements()
 !
 atoms_string=' '
 !
 if (allocated(Z_species)) then
   do is=1,n_atomic_species
     dumb_ch1=trim(intc(n_atoms_species(is)))//PT_elements(Z_species(is))
     atoms_string=trim(atoms_string)//trim(dumb_ch1)
   enddo
   call msg('rn','... containing '//trim(atoms_string)//' atoms')
 endif
 !
 call msg('r','... with scaling factors [a.u.]:',alat)
 call msg('nr','Direct Lattice(DL) unit cell [iru  /  cc(a.u.)]')
 dumb_ch1='A1 ='//trim(real2ch(a(1,1)/alat(1)))//' '//trim(real2ch(a(1,2)/alat(2)))//' '//&
&                 trim(real2ch(a(1,3)/alat(3)))//'     '//&
&                 trim(real2ch(a(1,1)))//' '//trim(real2ch(a(1,2)))//' '//trim(real2ch(a(1,3)))
 call msg('r',dumb_ch1)
 dumb_ch1='A2 ='//trim(real2ch(a(2,1)/alat(1)))//' '//trim(real2ch(a(2,2)/alat(2)))//' '//&
&                 trim(real2ch(a(2,3)/alat(3)))//'     '//&
&                 trim(real2ch(a(2,1)))//' '//trim(real2ch(a(2,2)))//' '//trim(real2ch(a(2,3)))
 call msg('r',dumb_ch1)
 dumb_ch1='A3 ='//trim(real2ch(a(3,1)/alat(1)))//' '//trim(real2ch(a(3,2)/alat(2)))//' '//&
&                 trim(real2ch(a(3,3)/alat(3)))//'     '//&
&                 trim(real2ch(a(3,1)))//' '//trim(real2ch(a(3,2)))//' '//trim(real2ch(a(3,3)))
 call msg('rn',dumb_ch1)
 !
 ! DL vol
 !
 call define_b_and_DL_vol()
 call msg('r','DL volume [au]:',DL_vol)
 RL_vol=(2._SP*pi)**3/DL_vol
 !
 call msg('nr','Reciprocal Lattice(RL) unit cell [iku  /  cc]')
 rv=b(1,:)
 call c2a(b_in=b,v_in=rv,mode='kc2i')
 dumb_ch1='B1 ='//trim(real2ch(rv(1)))//' '//trim(real2ch(rv(2)))//' '//trim(real2ch(rv(3)))//'     '//&
&                 trim(real2ch(b(1,1)))//' '//trim(real2ch(b(1,2)))//' '//trim(real2ch(b(1,3)))
 call msg('r',dumb_ch1)
 rv=b(2,:)
 call c2a(b_in=b,v_in=rv,mode='kc2i')
 dumb_ch1='B2 ='//trim(real2ch(rv(1)))//' '//trim(real2ch(rv(2)))//' '//trim(real2ch(rv(3)))//'     '//&
&                 trim(real2ch(b(2,1)))//' '//trim(real2ch(b(2,2)))//' '//trim(real2ch(b(2,3)))
 call msg('r',dumb_ch1)
 rv=b(3,:)
 call c2a(b_in=b,v_in=rv,mode='kc2i')
 dumb_ch1='B3 ='//trim(real2ch(rv(1)))//' '//trim(real2ch(rv(2)))//' '//trim(real2ch(rv(3)))//'     '//&
&                 trim(real2ch(b(3,1)))//' '//trim(real2ch(b(3,2)))//' '//trim(real2ch(b(3,3)))
 call msg('rn',dumb_ch1)
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
 call msg('r','DL (S)ymmetries [cc]')
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
   if (is<=nsym/(1+i_time_rev))&
&    call msg('r','[S'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))  
   if (is>nsym/(1+i_time_rev).and.mag_syms)&
&    call msg('r','[S*'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))
 enddo
 !
 ! Time Reversal
 !
 inv_index=-1
 select case(i_time_rev)
   case(1)
     call msg('nr','[SYMs] Time-reversal derived K-space symmetries:',(/nsym/2+1,nsym/))
     if(.not.mag_syms) inv_index=nsym/2+1
     if(mag_syms) then
       do is=1,nsym
         if ( all(nint(reshape(dl_sop(:,:,is),(/9/)))==(/-1,0,0,0,-1,0,0,0,-1/)) ) inv_index=is
       enddo
     endif
   case(0)
     call msg('nr','[SYMs] K-space Time-reversal not included')
     do is=1,nsym
       if ( all(nint(reshape(dl_sop(:,:,is),(/9/)))==(/-1,0,0,0,-1,0,0,0,-1/)) ) inv_index=is
     enddo
 end select
 !
 ! Indentity index
 !
 idt_index=-1
 do is=1,nsym
   if (all(nint(reshape(dl_sop(:,:,is),(/9/)))==(/1,0,0,0,1,0,0,0,1/)) ) idt_index=is
 enddo
 !
 !
 ! Space inversion
 !
 call atoms_spatial_inversion()
 if (inv_index>0) then
   if (i_space_inv==1) call msg('r','[SYMs] Spatial inversion '//trim(intc(inv_index))//' is a symmetry')
   if (i_space_inv==0) call msg('r','[SYMs] Spatial inversion '//trim(intc(inv_index))//' is NOT a symmetry')
 else
#if !defined _RT
   call warning('Spatial Inversion not found among the given symmetry list')
#endif
 endif
 !
 ! Symmetries Multiplication Table
 !
 call symmetry_group_table('r')
 call symmetry_equiv_atoms()
 !
 call section('=','RL shells')
 !============================
 !
 call G_shells_finder()
 call msg('rn','Shells, format: [S#] G_RL(mHa)')
 !
 ! Indexes of -G. minus_G_index(iG)| G_{minus_G_index(iG)}=-G. When there is no Spatial inversion
 ! the map is built in G_shells_finder
 !
 if (inv_index>0) call eval_minus_G()
 !
 do i1=n_g_shells,max(n_g_shells-27,1),-4
   dumb_ch1=' '
   do i2=i1,max(i1-3,1),-1
     dumb_ch2=trim(dumb_ch1)//' [S'//trim(intc(i2))//']:'//trim(intc(ng_in_shell(i2)))//&
&             '('//trim(real2ch(E_of_shell(i2)*1000._SP))//')'
     dumb_ch1=dumb_ch2
   enddo
   call msg('r',trim(dumb_ch2))
 enddo
 call msg('r',' ...')
 do i1=min(12,n_g_shells),1,-4
   dumb_ch1=' '
   do i2=i1,max(i1-3,1),-1
     dumb_ch2=trim(dumb_ch1)//' [S'//trim(intc(i2))//']:'//trim(intc(ng_in_shell(i2)))//&
&             '('//trim(real2ch(E_of_shell(i2)*1000._SP))//')'
     dumb_ch1=dumb_ch2
   enddo
   call msg('r',trim(dumb_ch2))
 enddo
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
 if (BZ_DbGd_nbands>0.and.allocated(en%E_DbGd)) then
   !
   call section('=','Double K-grid')
   !=======================================
   !
   call msg('r', 'K-points             :',BZ_DbGd_tot_nk_levels)
   call msg('r', 'Bands                :',BZ_DbGd_nbands)
   call msg('rn','Is the grid expanded :',DbGd_is_symmetry_expanded)
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
 call section('=','Energies [ev] & Occupations')
 !==============================================
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
 ! K points / Energies report
 !
 call msg('rn','X BZ K-points :',Xk%nbz)
 !
 call REPORT_Energies(en%E,k,Xk,en%nb,(/1,k%nibz/),'E',.TRUE.)
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
