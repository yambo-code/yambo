!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function k_the_nearest(k_ref,k,n_k,k_dist,ROTATE_k_ref,k_symm,i_err,border_point)
 !
 ! On output : - k(k_the_nearest,:) is the nearest to k_ref
 !             - k(k_the_nearest,:)==k_ref when i_err=0
 !
 ! All k-points must be in IKU coordinates
 !
 use pars,           ONLY:SP,zero_dfl
 use vec_operate,    ONLY:iku_v_norm,k2bz
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:rl_sop
 !
 implicit none
 !
 integer,  intent(in) :: n_k
 real(SP), intent(in) :: k_ref(3)
 real(SP), intent(in) :: k(n_k,3)
 real(SP), intent(out),  optional :: k_dist(n_k)
 logical,  intent(out),  optional :: border_point
 logical,  intent(in) ,  optional :: ROTATE_k_ref
 integer,  intent(out),  optional :: k_symm
 integer,  intent(out),  optional :: i_err
 !
 ! Work Space
 !
 integer :: ik,is,nsym_loop
 real(SP):: v(3),k_dist_(n_k),k_rotated(3,nsym),MAX_dist
 logical :: ROTATE_k_ref_
 !
 k_the_nearest=0
 if (present(i_err)) then
   i_err=-1
 endif
 if (present(border_point)) then
   border_point=.FALSE.
 endif
 !
 ROTATE_k_ref_=.FALSE.
 if (present(ROTATE_k_ref)) then
   ROTATE_k_ref_=ROTATE_k_ref
 endif
 !
 if (ROTATE_k_ref_) then
   nsym_loop=nsym
   do is=1,nsym
     k_rotated(:,is)=matmul(rl_sop(:,:,is),k_ref)
   enddo
 else
   nsym_loop=1
   k_rotated(:,1)=k_ref
 endif
 ! 
 MAX_dist=10000._SP
 !
 do ik=1,n_k
   do is=1,nsym_loop
     v=k_rotated(:,is)-k(ik,:)
     call k2bz(v)
     k_dist_(ik)=iku_v_norm(v)
     if ( k_dist_(ik) < MAX_dist-zero_dfl) then
       k_the_nearest=ik
       MAX_dist=k_dist_(ik)
       if (present(k_symm)) then
         k_symm=is
       endif
     endif
     if (k_dist_(ik)<zero_dfl) then
       if (present(i_err)) i_err=0
       exit
     endif
     !
   enddo
 enddo
 !
 if (k_the_nearest==0) call error('in k_the_nearest. Nearest point not found.')
 !
 if(present(k_dist)) then
   k_dist=k_dist_
 endif
 !
 if (.not.present(border_point)) return
 !
 do ik=1,n_k
   if (ik==k_the_nearest) cycle
   if ( abs(k_dist_(ik)-k_dist_(k_the_nearest))<zero_dfl) then
     border_point=.TRUE.
     return
   endif
 enddo
 !
end function
