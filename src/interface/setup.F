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
 use pars,           ONLY:SP,IP,pi,schlen
 use electrons,      ONLY:levels,n_met_bands,n_full_bands,E_duplicate
 use memory_m,       ONLY:mem_est
 use D_lattice,      ONLY:a,alat,nsym,i_time_rev,DL_vol,Tel,dl_sop,sop_inv,&
&                         sop_tab,atoms_spatial_invertion,i_space_inv,input_Tel_fixed,&
&                         inv_index,atoms_string,load_PT_elements,n_atoms,znucl,&
&                         n_atom_species,PT_elements
 use R_lattice,      ONLY:RL_vol,b,n_g_shells,ng_in_shell,&
&                         rl_sop,bz_samp,g_vec,E_of_shell
 use com,            ONLY:msg,error
 use stderr,         ONLY:intc,real2ch
 use IO_m,           ONLY:REP,DUMP,NONE
 use vec_operate,    ONLY:cross_product,c2a
 use zeros,          ONLY:k_iku_zero,k_rlu_zero,G_iku_zero,define_zeros
#if defined _SC
 use SC,             ONLY:load_SC_components
 use global_XC,      ONLY:G_kind,G_xc_functional,X_kind,&
&                         X_xc_functional,setup_global_XC,MORE_NONE,K_kind,K_xc_functional
#endif
 !
 implicit none
 type(levels)    ::en,Xen,Ken
 type(bz_samp)   ::k,Xk
 !
 ! Work Space
 !
 real(SP)          :: rv(3),m(3,3)
 integer           :: i1,i2,i3,is,ia
 character(schlen) :: dumb_ch1,dumb_ch2
 !
 call section('*','Input variables setup') 
 !========================================
 !
 call load_PT_elements()
 atoms_string=' '
 if (allocated(znucl)) then
   do is=1,n_atom_species
     dumb_ch1=trim(intc(n_atoms(is)))//PT_elements(znucl(is))
     atoms_string=trim(atoms_string)//trim(dumb_ch1)
   enddo
   call msg('rn','Unit cell contains '//trim(atoms_string)//' atoms')
 endif
 !
 call msg('r','Unit cell scaling factors [a.u.]:',alat)
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
        if ( all(reshape(dl_sop(:,:,is),(/9/))==(/-1.,0.,0.,0.,-1.,0.,0.,0.,-1./)) ) inv_index=is
     enddo
 end select
 !
 ! Space inversion
 !
 call atoms_spatial_invertion()
 if (inv_index>0) then
   if (i_space_inv==1) call msg('r','[SYMs] Spatial inversion '//trim(intc(inv_index))//' is a symmetry')
   if (i_space_inv==0) call msg('r','[SYMs] Spatial inversion '//trim(intc(inv_index))//' is NOT a symmetry')
 endif
 !
 ! Symmetries Multiplication Table
 !
 allocate(sop_tab(nsym,nsym),sop_inv(nsym))
 call mem_est("SOP-adds SOPs",(/3*3*nsym,nsym**2+nsym/),(/SP,IP/))
 !
 sop_tab=0
 sop_inv=0
 do i1=1,nsym
   do i2=1,nsym
     m=matmul(dl_sop(:,:,i1),dl_sop(:,:,i2))
     do i3=1,nsym
       if (all(abs(m-dl_sop(:,:,i3))<=1.E-5)) then
         if (sop_tab(i1,i2)/=0) call error('Error (stab 1): check the input symmetries!')        
         sop_tab(i1,i2)=i3
         if (sop_tab(i1,i2)==1) sop_inv(i1)=i2                
       endif
     enddo
     if (sop_tab(i1,i2)==0) call error('[SYMs] check the input symmetries!')    
   enddo
 enddo
 if (any(sop_inv==0)) call error('[SYMs] check the input symmetries!')
 call msg('r','[SYMs] Group table built correctly')
 !
 call section('+','K-grid lattice')
 !=================================
 !
 call k_lattice(k,Xk) 
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
 call section('=','Input (E)nergies[ev] & Occupations')
 !=====================================================
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
&                         kind=G_kind,xc_functional=G_xc_functional)
 call setup_global_XC('G',G_kind,MORE_NONE,G_xc_functional)
 call load_SC_components('E',E=Xen,COM_=NONE,MODE_=DUMP,ik=0,&
&                         kind=X_kind,xc_functional=X_xc_functional)
 call setup_global_XC('X',X_kind,MORE_NONE,X_xc_functional)
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
 n_met_bands=en%nbm
 n_full_bands=en%nbf
 !
 ! K points / Energies report
 !
 call msg('rn','X BZ K-points :',Xk%nbz)
 !
 call report_energies(en%E,k,en%nb,(/1,k%nibz/),'E',.TRUE.)
 !
 ! Once the occupations have been evaluated I dump Xen in Ken
 !
 call E_duplicate(Xen,Ken)
#if defined _SC
 K_kind=X_kind
 K_xc_functional=X_xc_functional
 call setup_global_XC('K',K_kind,MORE_NONE,K_xc_functional)
#endif
 !
end subroutine
