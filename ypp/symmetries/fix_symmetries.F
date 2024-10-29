!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): CA DS
!
subroutine fix_symmetries(old_nsym,old_dl_sop,old_sop_inv,S_contains_TR)
 !
 use pars,                ONLY:SP
 use com,                 ONLY:msg
 use parser_m,            ONLY:parser
 use matrix_operate,      ONLY:m3det
 use stderr,              ONLY:intc
 use YPPm,                ONLY:E_field,B_theta,B_psi,B_field
 use electrons,           ONLY:n_spinor,Spin_magn
 use R_lattice,           ONLY:rl_sop
 use vec_operate,         ONLY:v_norm
 use D_lattice,           ONLY:nsym,dl_sop,i_time_rev,alat,mag_syms,&
&                              i_space_inv,sop_inv,inv_index,symmetry_group_table
 !
#include<memory.h>
 !
 integer, intent(out)   :: old_nsym
 real(SP),intent(out)   :: old_dl_sop(3,3,nsym)
 integer, intent(out)   :: old_sop_inv(nsym)
 logical, intent(out)   :: S_contains_TR(nsym)
 !
 ! Work space
 !
 integer                :: spatial_nsym,old_spatial_nsym,old_trev_nsym
 integer                :: symm_M_factor,symm_B_factor
 real(SP), allocatable  :: dl_sop_tmp(:,:,:)
 real(SP)               :: B_cart(3)
 logical                :: Reduce_syms
 logical                :: Rm_all_syms,Rm_TR,Rm_INV
 logical                :: use_E_field,use_B_field,has_magnetization
 !
 ! Dummies
 !
 integer                :: i1,i2
 integer                :: is
 logical                :: reject_symmetry
 real(SP)               :: v1(3),v2(3)
 !
 use_E_field=.FALSE.
 use_B_field=.FALSE.
 has_magnetization=.FALSE.
 if (any(E_field/=0.)) use_E_field=.TRUE.
 if (B_field/=0.)      use_B_field=.TRUE.
 if (any(Spin_magn>1.E-3).and.n_spinor==2)  has_magnetization=.TRUE.
 !
 call parser('RmAllSymm',Rm_all_syms)
 !
 call parser('RmTimeRev',Rm_TR)
 if (i_time_rev==0) Rm_TR=.false.
 !
 call parser('RmSpaceInv',Rm_INV)
 if (i_space_inv==0) Rm_INV=.false.
 !
 Reduce_syms=any((/use_B_field,use_E_field,has_magnetization,Rm_all_syms,Rm_TR,Rm_INV/))
 !
 ! Check if we are using mag symmetries
 !
 mag_syms= ( use_B_field .or. (n_spinor==2.and.has_magnetization)) &
&           .and. (i_time_rev==1) .and. (.not.Rm_TR)
 !
 if (mag_syms) call msg('s','There is T-rev with (a) [a magnetic field] or (b) [SOC in a magnetic system]')
 !
 if (Rm_all_syms) then
   call section('*',"== Remove all Symmetries ==")
   use_E_field=.false.
   use_B_field=.false.
   has_magnetization=.false.
   Rm_TR=.false.
   E_field=0.
   B_field=0.
 endif
 !
 if (use_E_field) call section('*',"== Electric Fields: BZ sampling remapping ==")
 !
 if (use_B_field) call section('*',"== Magnetic fields: BZ sampling remapping ==")
 !
 if (has_magnetization.and.n_spinor==2) call section('*',"== Magnetic system: BZ sampling remapping ==")
 !
 if (     Rm_TR.and..not.RM_INV) call section('*',"== Remove Time Reversal ==")
 !
 if (.not.Rm_TR.and.     RM_INV) call section('*',"== Remove Spatial Inversion ==")
 !
 if (     Rm_TR.and.     RM_INV) call section('*',"== Remove Time Reversal and Spatial Inversion ==")
 !
 if (use_B_field.and.has_magnetization) call warning('Magnetic field in magnetic system')
 !
 ! Time-reversal related Symmetries
 !
 forall (is=1:nsym) S_contains_TR(is)=is>nsym/(1+i_time_rev)
 !
 spatial_nsym=nsym/(1+i_time_rev)
 !
 old_nsym=nsym
 old_spatial_nsym=spatial_nsym
 old_trev_nsym=nsym-spatial_nsym
 old_dl_sop=dl_sop
 old_sop_inv=sop_inv
 !
 if (use_B_field) then
   B_cart(1)=B_field*sin(B_theta)*cos(B_psi)
   B_cart(2)=B_field*sin(B_theta)*sin(B_psi) 
   B_cart(3)=B_field*cos(B_theta)
 endif
 !
 if(.not.Reduce_syms) return
 !
 call section('+',"Symmetries")
 !=============================
 !
 !*********************
 ! Remove Symmetries  *
 !*********************
   !
   ! First replace (or remove) Time reversal
   if(Rm_TR) then
     if(i_space_inv==1.and.inv_index>spatial_nsym.and..not.mag_syms) then
       call msg('s',trim(intc(spatial_nsym))//' T-rev are now spatial symm')
       spatial_nsym=2*spatial_nsym
       nsym=spatial_nsym
     else
       nsym=nsym/(1+i_time_rev)
     endif
     i_time_rev=0
     mag_syms=.false.
   endif
   !
   if(Rm_inv.and..not.(inv_index>spatial_nsym)) then
     nsym=nsym/2
     spatial_nsym=spatial_nsym/2
     call msg('s','Removed inversion. Symmetries are now '//trim(intc(spatial_nsym)))
   endif
   !
   ! Then remove other symmetries according to input
   if(Rm_all_syms) then
     nsym=1
     spatial_nsym=1
     i_time_rev=0
     mag_syms=.false.
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
         v1(:) = matmul( dl_sop(:,:,is), Spin_magn )
         if(all( abs(v1(:)-Spin_magn(:))<epsilon(1.)) ) symm_M_factor=+1
         if(all( abs(v1(:)+Spin_magn(:))<epsilon(1.)) ) symm_M_factor=-1
         reject_symmetry=(symm_M_factor==0)
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
   YAMBO_ALLOC(dl_sop_tmp,(3,3,nsym))
   dl_sop_tmp(1:3,1:3,1:nsym)=dl_sop(1:3,1:3,1:nsym)
   YAMBO_FREE(dl_sop)
   YAMBO_ALLOC(dl_sop,(3,3,nsym))
   dl_sop=dl_sop_tmp
   YAMBO_FREE(dl_sop_tmp)
   !
 !
 if(.not.RM_all_syms)&
& call msg('s',trim(intc(old_spatial_nsym))//' spatial symmetries reduced/increased to ...'//trim(intc(spatial_nsym)))
 !
 if(nsym==spatial_nsym) then
   i_time_rev=0
   mag_syms=.false.
 endif
 !
 if(i_time_rev==1) then
   call msg('s',trim(intc(old_trev_nsym))//' T-rev symmetries reduced/increased to ...'//trim(intc(nsym-spatial_nsym)))
 endif
 !
 call msg('s','[Symm] Spatial simmetries now are')  
 do is=1,spatial_nsym
   call msg('s','[S'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))
 enddo
 if (i_time_rev==1) then
   call msg('s','[Symm] T-rev derived simmetries now are')  
   do is=spatial_nsym+1,nsym
     call msg('s','[S*'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))  
   enddo
 endif
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
 call symmetry_group_table('s')
 !
end subroutine
!
subroutine atoms_check_symm(symm_to_check,n_syms_to_check,found_symm)
 use pars,           ONLY:zero_dfl
 use matrix_operate, ONLY:m3inv
 use pars,           ONLY:SP
 use D_lattice,      ONLY:atom_pos,a,n_atomic_species,n_atoms_species
 !
 implicit none
 !
 integer  :: n_syms_to_check
 real(SP) :: symm_to_check(3,3,n_syms_to_check)
 logical  :: found_symm
 !
 ! Work Space
 !
 integer  :: ia,iap,is,isymm,n_syms
 real(SP) :: at_diff(3),a_m1(3,3),tmp_pos(3,2),shift(3)
 !
 found_symm=.false.
 n_syms=n_syms_to_check
 !
 call m3inv(transpose(a),a_m1)
 !
 shift=2*(a(1,:)+a(2,:)+a(3,:))
 !
 symm_loop: do isymm=1,n_syms_to_check
   symm_to_check(:,:,1)=symm_to_check(:,:,isymm)
   do is=1,n_atomic_species
     atoms_loop: do ia=1,n_atoms_species(is)
       do iap=1,n_atoms_species(is)
         !
         tmp_pos(:,1)=atom_pos(:,ia,is)
         tmp_pos(:,2)=matmul(symm_to_check(:,:,1),atom_pos(:,iap,is)+shift)
         !
         at_diff=matmul(a_m1,tmp_pos(:,1)-tmp_pos(:,2))
         !
         at_diff(1)=abs(at_diff(1)-nint(at_diff(1)))     
         at_diff(2)=abs(at_diff(2)-nint(at_diff(2)))     
         at_diff(3)=abs(at_diff(3)-nint(at_diff(3)))     
         !
         if (all(at_diff<=zero_dfl)) cycle atoms_loop
         !
       enddo
       !
       if(iap==n_atoms_species(is)+1) then
         n_syms=n_syms-1
         cycle symm_loop
       endif
       !
     enddo atoms_loop
   enddo
 enddo symm_loop
 !
 found_symm= (n_syms>0)
 !
end subroutine atoms_check_symm
