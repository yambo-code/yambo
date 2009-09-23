!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine setup(en,Xen,Ken,k,Xk)
 !
 use drivers,        ONLY:l_sc_run
 use pars,           ONLY:SP,pi,schlen
 use electrons,      ONLY:levels,n_met_bands,n_full_bands,E_duplicate
 use D_lattice,      ONLY:a,alat,nsym,i_time_rev,DL_vol,Tel,dl_sop,&
&                         atoms_spatial_invertion,i_space_inv,input_Tel_fixed,&
&                         inv_index,atoms_string,load_PT_elements,n_atoms_species,Z_species,&
&                         n_atomic_species,PT_elements,non_periodic_directions,lattice,&
&                         symmetry_group_table
 use R_lattice,      ONLY:RL_vol,b,n_g_shells,ng_in_shell,&
&                         rl_sop,bz_samp,g_vec,E_of_shell
 use com,            ONLY:msg,error,warning
 use stderr,         ONLY:intc,real2ch
 use IO_m,           ONLY:REP,DUMP,NONE
 use vec_operate,    ONLY:cross_product,c2a
 use zeros,          ONLY:k_iku_zero,k_rlu_zero,G_iku_zero,define_zeros
#if defined _SC || defined _RT || defined _MAGNETIC
 use SC,             ONLY:load_SC_components,found_SC_DB
 use global_XC,      ONLY:G_kind,G_xc_functional,X_kind,&
&                         X_xc_functional,setup_global_XC,MORE_NONE,K_kind,K_xc_functional,loaded_WF_xc_string,&
&                         X_perturbation,G_perturbation,K_perturbation,WF_kind,WF_xc_functional,WF_perturbation
#endif
 !
 implicit none
 type(levels)    ::en,Xen,Ken
 type(bz_samp)   ::k,Xk
 !
 ! Work Space
 !
 real(SP)          :: rv(3),m(3,3)
 integer           :: i1,i2,i3,is,ia,i_err
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
     dumb_ch1=trim(intc(n_atoms_species(is)))//'--'
     if (Z_species(is)/=0) dumb_ch1=trim(intc(n_atoms_species(is)))//PT_elements(Z_species(is))
     atoms_string=trim(atoms_string)//trim(dumb_ch1)
   enddo
   call msg('rn','... containing '//trim(atoms_string)//' atoms')
 endif
 !
 call msg('r','... with scaling factors [a.u.]:',alat)
 call msg('nr','Direct Lattice(DL) unit cell [iru]')
 call msg('r','A1 =',a(1,:)/alat(:))
 call msg('r','A2 =',a(2,:)/alat(:))
 call msg('rn','A3 =',a(3,:)/alat(:))
 !
 ! DL vol
 !
 DL_vol=0.
 rv=cross_product(a(2,:),a(3,:))
 do i1=1,3
   DL_vol=DL_vol+a(1,i1)*rv(i1)
 enddo
 call msg('r','DL volume [au]:',DL_vol)
 RL_vol=(2.*pi)**3./DL_vol
 !
 ! RL unit vectors 
 !
 b(1,:)=cross_product(a(2,:),a(3,:))*2.*pi/DL_vol
 b(2,:)=cross_product(a(3,:),a(1,:))*2.*pi/DL_vol
 b(3,:)=cross_product(a(1,:),a(2,:))*2.*pi/DL_vol
 !
 call msg('nr','Reciprocal Lattice(RL) unit cell [iku]')
 call c2a(b,b(1,:),rv,'kc2i')
 call msg('r','B1 =',rv)
 call c2a(b,b(2,:),rv,'kc2i')
 call msg('r','B2 =',rv)
 call c2a(b,b(3,:),rv,'kc2i')
 call msg('rn','B3 =',rv)
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
 allocate(rl_sop(3,3,nsym))
 do is=1,nsym
   forall (i2=1:3,i3=1:3) rl_sop(i2,i3,is)=dl_sop(i2,i3,is)*alat(i2)/alat(i3)
   if (i_time_rev==1.and.is>nsym/2) cycle
   call msg('r','[S'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))  
 enddo
 !
 ! Time Reversal
 !
 inv_index=-1
 select case(i_time_rev)
   case(1)
     call msg('nr','[SYMs] Time-reversal derived K-space symmetries:',(/nsym/2+1,nsym/))
     inv_index=nsym/2+1
   case(0)
     call msg('nr','[SYMs] K-space Time-reversal not included')
     do is=1,nsym
       if ( all(nint(reshape(dl_sop(:,:,is),(/9/)))==(/-1.,0.,0.,0.,-1.,0.,0.,0.,-1./)) ) inv_index=is
     enddo
 end select
 !
 ! Space inversion
 !
 call atoms_spatial_invertion()
 if (inv_index>0) then
   if (i_space_inv==1) call msg('r','[SYMs] Spatial inversion '//trim(intc(inv_index))//' is a symmetry')
   if (i_space_inv==0) call msg('r','[SYMs] Spatial inversion '//trim(intc(inv_index))//' is NOT a symmetry')
 else
   call warning('Spatial Inversion not found among the given symmetry list')
 endif
 !
 ! Symmetries Multiplication Table
 !
 call symmetry_group_table('r')
 !
 call section('=','RL shells')
 !============================
 !
 call G_shells_finder()
 call msg('rn','Shells, format: [S#] G_RL(mHa)')
 !
 do i1=n_g_shells,max(n_g_shells-27,1),-4
   dumb_ch1=' '
   do i2=i1,max(i1-3,1),-1
     dumb_ch2=trim(dumb_ch1)//' [S'//trim(intc(i2))//']:'//trim(intc(ng_in_shell(i2)))//&
&             '('//trim(real2ch(E_of_shell(i2)*1000.))//')'
     dumb_ch1=dumb_ch2
   enddo
   call msg('r',trim(dumb_ch2))
 enddo
 call msg('r',' ...')
 do i1=min(12,n_g_shells),1,-4
   dumb_ch1=' '
   do i2=i1,max(i1-3,1),-1
     dumb_ch2=trim(dumb_ch1)//' [S'//trim(intc(i2))//']:'//trim(intc(ng_in_shell(i2)))//&
&             '('//trim(real2ch(E_of_shell(i2)*1000.))//')'
     dumb_ch1=dumb_ch2
   enddo
   call msg('r',trim(dumb_ch2))
 enddo
 !
 call section('=','K-grid lattice')
 !=================================
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
 !
 if (trim(non_periodic_directions)/="none") then
   !
   call section('=','Non periodic geometry')
   !========================================
   !
   call check_periodic_directions()
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
#if defined _SC || defined _RT || _MAGNETIC
 !
 ! First check if it possible to load the SC energies
 !
 call load_SC_components('E',E=en,COM_=REP,MODE_=DUMP,ik=0,&
&                         kind=G_kind,xc_functional=G_xc_functional,perturbation=G_perturbation)
 if (found_SC_DB) call setup_global_XC('G',G_kind,MORE_NONE,G_xc_functional,G_perturbation)
 call load_SC_components('E',E=Xen,COM_=NONE,MODE_=DUMP,ik=0,&
&                         kind=X_kind,xc_functional=X_xc_functional,perturbation=X_perturbation)
 if (found_SC_DB) call setup_global_XC('X',X_kind,MORE_NONE,X_xc_functional,X_perturbation)
#endif
 !
 ! Negative Temperature indicates that such Temperature is held
 ! fixed in the calculation (not overwritten in io_header.F, for
 ! example).
 !
 input_Tel_fixed=Tel<0.
 Tel=abs(Tel)
 !
 call fermi_level(Xen,Xk,Tel,2)
 call extend_occupations(Xen,en)
 !
 n_met_bands =en%nbm
 n_full_bands=en%nbf
 !
 ! K points / Energies report
 !
 call msg('rn','X BZ K-points :',Xk%nbz)
 !
 call report_energies(en%E,k,Xk,en%nb,(/1,k%nibz/),'E',.TRUE.)
 !
 ! Once the occupations have been evaluated I dump Xen in Ken
 !
 call E_duplicate(Xen,Ken)
#if defined _SC || defined _RT || defined _MAGNETIC
 K_kind=X_kind
 K_xc_functional=X_xc_functional
 K_perturbation=X_perturbation
 call setup_global_XC('K',K_kind,MORE_NONE,K_xc_functional,K_perturbation)
 !
 if (.not.l_sc_run) then
   !
   ! SC wavefunctions
   !
   ! Here I only check for the existence of the SC WF DB. Band checking will be done later
   !
   call load_SC_components('check_WF_DB',n_bands=0,&
&                          kind=WF_kind,xc_functional=WF_xc_functional,perturbation=WF_perturbation)
   !
   if (found_SC_DB) then
     call setup_global_XC('WF',WF_kind,MORE_NONE,WF_xc_functional,WF_perturbation)
     call warning(trim(loaded_WF_xc_string)//' wavefunctions found. Bands check at loading time.')
   endif
   !
 endif
 !
#endif
 !
end subroutine
