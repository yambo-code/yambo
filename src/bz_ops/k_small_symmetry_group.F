!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
subroutine k_small_symmetry_group(k,k_symm_grp)
 !
 ! Outputs:
 ! 
 !   k_symm_grp%nsymm, k_symm_grp%indx
 !
 !Find the small symmetry group for each k-point
 !
 use pars,           ONLY:SP
 use com,            ONLY:msg
 use zeros,          ONLY:k_rlu_zero
 use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:bz_samp,k_symm_small_grp,rl_sop
 !     
#include<y_memory.h>
 !
 type(bz_samp), intent(in) ::k
 type(k_symm_small_grp), intent(out) :: k_symm_grp
 !
 ! Work space
 !
 integer   :: ik,is,i1,idx
 real(SP)  :: v_rot(3),v_rlu(3)
 !
 if (.not.allocated(k%sstar)) call k_expand(k)
 !
 if(allocated(k_symm_grp%nsym)) then
   YAMBO_FREE(k_symm_grp%nsym)
   YAMBO_FREE(k_symm_grp%indx)
 endif
 !
 YAMBO_ALLOC(k_symm_grp%nsym,(k%nibz))
 YAMBO_ALLOC(k_symm_grp%indx,(k%nibz,nsym))
 !
 ! Initialize with only the idensity
 !
 !
 call msg('rs','Build small-kpoints symmetry group ')
 do ik=1,k%nibz
   !
   k_symm_grp%nsym(ik)=1
   k_symm_grp%indx(ik,k_symm_grp%nsym(ik))=1     
   !
   ! Check if a symmetry is not in the star
   ! in this case add it to the small group
   !
   call c2a(v_in=k%pt(ik,:),v_out=v_rlu,mode='ki2a')
   !
   do is=2,nsym
     v_rot=matmul(rl_sop(:,:,is),k%pt(ik,:))
     call k2bz(v_in=v_rot)
     call c2a(v_in=v_rot,mode='ki2a')
     if (rlu_v_is_zero(v_rot-v_rlu,zero_=k_rlu_zero)) then 
       k_symm_grp%nsym(ik)=k_symm_grp%nsym(ik)+1
       k_symm_grp%indx(ik,k_symm_grp%nsym(ik))=is
     endif
   enddo
   !
 enddo
 !
 ! Check small symmetry group
 !
! do ik=1,k%nibz
!  call c2a(v_in=k%pt(ik,:),v_out=v_rlu,mode='ki2a')
!  write(*,*) ' k ',ik,' nsym ',k_symm_grp%nsym(ik), ' = ',v_rlu
!  do i1=1,k_symm_grp%nsym(ik)
!     is=k_symm_grp%indx(ik,i1)
!     v_rot=matmul(rl_sop(:,:,is),k%pt(ik,:))
!     call k2bz(v_in=v_rot)
!     call c2a(v_in=v_rot,mode='ki2a')
!   enddo	   
! enddo
 !
end subroutine
