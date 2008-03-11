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
 module interfaces
   ! 
   use pars,      ONLY:SP,PI,lchlen,schlen,DP
   implicit none
   !
   logical :: force_noTR
   logical :: force_noSYM
   logical :: force_noWFs
   !
   real(SP):: alat_mult_factor
   !
   integer               :: n_band_block, maxband
   integer, allocatable  :: blocks(:,:)
   !
 contains
   !
   subroutine make_real_wf(ib,ik,wf_,nc)
     use stderr,    ONLY:intc
     use com,       ONLY:warning
     integer     :: ib,ik,nc
     real (SP)   :: wf_(2,nc)
     !
     ! Work Space
     !
     integer     :: ic,ic_start,ic_check
     real(SP)    :: c1,c2,c_norm_sq,ph_sin(nc),ph_cos(nc),ph_cos_sq(nc)
     complex(SP) :: phase,wf_c
     ic_start=-1
     ic_check=0
     do ic=1,nc
       c1=wf_(1,ic)
       c2=wf_(2,ic)
       c_norm_sq=c1**2.+c2**2.
       if (c_norm_sq>1.E-5) then
         ic_check=ic_check+1
         ph_cos_sq(ic)=1./(1.+(c2/c1)**2.)
         ph_cos(ic)=c1/sqrt(c_norm_sq)
         ph_sin(ic)=c2/sqrt(c_norm_sq)
         if (ic_start<0) ic_start=ic
       else
         cycle
       endif
       if (abs(ph_cos_sq(ic)-ph_cos_sq(ic_start))>0.1) then
         call warning('No phase @ band '//trim(intc(ib))//' k-point '//trim(intc(ik)))
       endif
       if (ic_check>10) exit
     enddo
     phase=cmplx(ph_cos(ic_start),-ph_sin(ic_start))
     do ic=1,nc
       wf_c=cmplx(wf_(1,ic),wf_(2,ic))
       wf_(1,ic)=real(phase*wf_c)
       wf_(2,ic)=aimag(phase*wf_c)
     enddo
     !
   end subroutine
   !
   subroutine interface_presets(in_string)
     !
     use stderr, ONLY:string_split
     character(*)   :: in_string
     !
     character(schlen)::str_piece(100)
     integer          ::i1
     !
     ! Split the string in pieces
     !
     call string_split(in_string,str_piece)
     !
     force_noTR =index(in_string,'notr')>0
     force_noSYM=index(in_string,'nosym')>0
     force_noWFs=index(in_string,'nowf')>0
     !
     alat_mult_factor=1.
     do i1=1,100
       if ( trim(str_piece(i1))=='alat_f' )  read(str_piece(i1+1),*) alat_mult_factor
     enddo
     !
   end subroutine
   !
   subroutine symmetries_check_and_load(int_sop,nsym_)
     !
     use com,       ONLY:msg,error
     use electrons, ONLY:spin_orbit
     use D_lattice, ONLY:nsym,dl_sop,i_time_rev,a,&
&                        atoms_spatial_invertion,i_space_inv
     use R_lattice, ONLY:b
     !
     integer :: nsym_
     integer :: int_sop(3,3,nsym_) ! Symmetries in RLU
     !
     integer :: is,is_inv
     logical :: l_identity,l_inversion
     real(SP):: sop_mat(3,3)
     !
     if (force_noSYM) then
       i_time_rev=0
       nsym=1
       allocate(dl_sop(3,3,1))
       dl_sop(1,:,1)=(/1.,0.,0./)
       dl_sop(2,:,1)=(/0.,1.,0./)
       dl_sop(3,:,1)=(/0.,0.,1./)
       call msg("l","no")
       return
     endif
     !
     i_time_rev=1
     !
     ! Space Invertion (possible only for v >= 3.0.4)
     !
     call atoms_spatial_invertion()
     !
     ! Search for identity and inversion symmetries.
     !
     l_identity=.false.
     l_inversion=.false.
     do is=1,nsym_
       if (int_sop(1,1,is)==1.and.int_sop(1,2,is)==0.and.int_sop(1,3,is)==0.and.&
&          int_sop(2,1,is)==0.and.int_sop(2,2,is)==1.and.int_sop(2,3,is)==0.and.&
&          int_sop(3,1,is)==0.and.int_sop(3,2,is)==0.and.int_sop(3,3,is)==1) &
&          l_identity=.true.
       if (int_sop(1,1,is)==-1.and.int_sop(1,2,is)==0.and.int_sop(1,3,is)==0.and.&
&          int_sop(2,1,is)==0.and.int_sop(2,2,is)==-1.and.int_sop(2,3,is)==0.and.&
&          int_sop(3,1,is)==0.and.int_sop(3,2,is)==0.and.int_sop(3,3,is)==-1) then
         l_inversion=.true.
         is_inv=is
       endif 
     enddo
     if (.not.l_identity) call msg("l","[ID no]...")
     if (l_identity)      call msg("l","[ID yes]...")
     !
     ! Special case where I,-I present: no TR used.
     !
     if (l_identity.and.l_inversion) then
       !
       ! If -I is a space inversion I keep both I and -I
       ! and switch off TR
       !
       if (i_space_inv==1) i_time_rev=0
       if (i_space_inv==0) call &
&         error('-I is in the symmetries list but it is not a proper operation')
     endif
     !
     ! Identity was stripped in outkss: swap all
     ! symmetries with their inversions
     !
     if (.not.l_identity.and.l_inversion) int_sop=-int_sop
     !
     if (.not.l_inversion) call msg("l","[INV no]...")
     if (l_inversion)      call msg("l","[INV yes]...")
     !
     ! TR ?
     !
     if (spin_orbit) then 
       i_time_rev=0
       call msg("l","[SO yes => TR no]...")
     endif
     if (force_noTR) then 
       i_time_rev=0
       call msg("l","[TR no]...")
     endif
     if (.not.spin_orbit.and..not.force_noTR) then
       if (i_time_rev==1) call msg("l","[TR yes]...")
       if (i_time_rev==0) call msg("l","[TR no]...")
     endif
     !
     ! Double the simmetries if TR is added
     !
     nsym=nsym_
     if (i_time_rev==1) nsym=2.*nsym_
     !
     !
     if (i_space_inv==0) call msg("l","[SI no]")
     if (i_space_inv==1) call msg("l","[SI yes]")
     !
     !From abinit help ...
     !
     !The relation between the above symmetry matrices symrel,
     !expressed in the basis of primitive translations, and the same symmetry
     !matrices expressed in cartesian coordinates, is as follows.
     !Denote the matrix whose columns are the primitive
     !translations as A, and denote the cartesian symmetry matrix as R. Then
     !R_rlu = A(inverse) * R * A
     !
     !In my case A=transpose(a)
     !
     !R= transpose(a) R_rlu inverse[transpose(a)]
     !
     !but inverse[transpose(a)]= b/2./pi
     !
     allocate(dl_sop(3,3,nsym))
     do is=1,nsym_
       sop_mat=matmul(transpose(a),int_sop(:,:,is))
       dl_sop(:,:,is)=matmul(sop_mat,b)/2./pi
       if (i_time_rev==1) dl_sop(:,:,is+nsym_)=-dl_sop(:,:,is)
     enddo
     !
   end subroutine
   !
   subroutine print_interface_dimensions(en,k)
     use com,                   ONLY : msg
     use electrons,             ONLY : levels, default_nel, n_spin, n_sp_pol, &
&                                      n_spinor, spin_orbit
     use R_lattice,             ONLY : bz_samp, ng_vec, g_vec
     use D_lattice,             ONLY : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                      DL_vol, a, alat, default_Tel, &
&                                      n_atoms_max,n_atom_species,n_atoms, atom_pos, &
&                                      znucl
     use wave_func,             ONLY : wf_nc_k, wf_ncx, wf_igk, wf_ng
     use xc_functionals,        ONLY : GS_xc_KIND,GS_xc_FUNCTIONAL, xc_string
     implicit none
     type(levels),     intent(in)   :: en     ! Energies
     type(bz_samp),    intent(in)   :: k      ! K/Q points
     !
     call msg('s',' :: Electrons             :',default_nel)
     call msg('s',' :: Temperature       [ev]:',default_Tel)
     call msg('s',' :: Lattice factors [a.u.]:',alat)
     call msg('s',' :: K-points              :',k%nibz)
     call msg('s',' :: Bands                 :',en%nb)
     call msg('s',' :: Spinor components     :',n_spinor)
     call msg('s',' :: Spin polarizations    :',n_sp_pol)
     call msg('s',' :: Spin orbit coupling   : ',spin_orbit)
     call msg('s',' :: Symmetries   [spatial]:',nsym/(i_time_rev+1))
     call msg('s',' ::                [T-rev]: ',i_time_rev==1)
     call msg('s',' :: Max WF components     :',wf_ncx)
     call msg('s',' :: RL vectors        (WF):',wf_ng)
     call msg('s',' :: RL vectors    (CHARGE):',ng_vec)
     call msg('s',' :: XC potential          : ',xc_string(GS_xc_KIND,GS_xc_FUNCTIONAL))
     call msg('s',' :: Atomic species        :',n_atom_species)
     call msg('s',' :: Max atoms/species     :',n_atoms_max)
     !
     return
     !
   end subroutine print_interface_dimensions
   !
 end module
