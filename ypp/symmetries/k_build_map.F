!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): CA DS
!
subroutine k_build_map(k,k_save,kpoints_map,action_kind)
 !
 use pars,                ONLY:SP
 use zeros,               ONLY:k_rlu_zero
 use vec_operate,         ONLY:rlu_v_is_zero,c2a
 use R_lattice,           ONLY:bz_samp
 !
 implicit none
 !
 type(bz_samp),intent(inout) :: k
 type(bz_samp),intent(in)    :: k_save
 integer,intent(out)         :: kpoints_map(2,k%nibz)
 !
 integer,intent(in)          :: action_kind
 ! 
 ! Work space
 !
 integer               :: i1,ik,ik_save
 integer               :: iv(2),nk
 real(SP)              :: v1(3),v2(3)
 ! 
 !
 ! Map k-point using the save BZ
 !
 ! kpoints_map(1, ik ) = ik_save
 ! kpoints_map(2, ik ) = is
 !
 ! with ik_save,is relative to the original set of k-points and symmetries
 !
 if(action_kind==1) nk=k_save%nbz
 if(action_kind==2) nk=k_save%nibz
 !
 kpoints_map=0
 do ik=1,k%nibz 
   do ik_save=1,k_save%nbz
     if(action_kind==1) v1=k_save%ptbz(ik_save,:)
     if(action_kind==2) v1=k_save%pt(ik_save,:)
     !
     call c2a(v_in=v1,mode='ki2a')
     call c2a(v_in=k%pt(ik,:),v_out=v2,mode='ki2a')
     !
     if (rlu_v_is_zero(v2-v1,zero_=k_rlu_zero)) then
       if(action_kind==1) kpoints_map(:, ik ) = k_save%sstar(ik_save,:)
       if(action_kind==2) kpoints_map(1, ik ) = ik_save
       exit 
     endif
   enddo
 enddo
 !
 if(action_kind==2) return
 !
 ! swap ik and i1 to sort
 ! first all the points in the ibz
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
end subroutine
