!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine QP_apply_DB_interpolation(band_range,qp,en,k,l_plan_todo_EWZI,&
&                                    DB_corrected,n_neigh)
 !
 ! Using qp% elements (all must be defined) I QP correct
 ! the en type
 !
 ! l_plan_todo_EWZI=( E W Z Interp )
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:n_sp_pol,spin
 use com,           ONLY:msg,warning
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp,k_the_nearest
 use vec_operate,   ONLY:sort
 use QP_m,          ONLY:QP_t,QP_ctl_applied
 !
 implicit none
 type(levels)::en
 type(QP_t)  ::qp
 type(bz_samp)::k
 integer :: n_neigh,band_range(2),DB_corrected(en%nb,en%nk,n_sp_pol)
 logical :: l_plan_todo_EWZI(5),wrong_width_sign_warn
 !
 ! Work Space
 !
 integer  :: i1,i2,i_spin,ikbz,order(qp%nk),k_near(n_neigh),nk_interpolated,&
&            nk_exact,i_err,ik_ref,ib_ref
 real(SP) :: idist(qp%nk),odist(qp%nk),k_dist(n_neigh)
 !
 logical, external :: QP_check_if_corrected
 !
 ! IBZ -> BZ (iku)
 !
 call k_ibz2bz(k,'i',.true.)
 !
 ! Transfer & interpolation
 !
 wrong_width_sign_warn=.false.
 nk_interpolated=0
 nk_exact=0
 !
 if (.not.l_plan_todo_EWZI(4).and..not.l_plan_todo_EWZI(5)) then
  l_plan_todo_EWZI(4)=.TRUE.
 endif
 !
 if (l_plan_todo_EWZI(4)) then
   !
   ! Nearest K-point interpolation
   !===============================
   !
   do i1=1,k%nibz
     !
     k_near=0
     k_dist=1.E5
     idist=1.E5
     !
     do i2=1,qp%nk
       do ikbz=1+sum(k%nstar(:i1-1)),sum(k%nstar(:i1))
         idist(i2)=min(idist(i2),kmin_distance(qp%k(i2,:),k%ptbz(ikbz,:)))
       enddo
     enddo
     !
     if (qp%nk>1) then
       call sort(idist,odist,order)
       k_near(:min(qp%nk,n_neigh))=order(:min(qp%nk,n_neigh))
       k_dist(:min(qp%nk,n_neigh))=odist(:min(qp%nk,n_neigh))
     else
       k_near(1)=1
       k_dist(1)=idist(1)
     endif
     !
     ! Transfer OR interpolate
     !
     ! First I check if there is an exact correposndance
     ! between k%pt and qp%k. This exists when i_err==0
     !
     k_near(1)=k_the_nearest(k%pt(i1,:),qp%k,qp%nk,ROTATE_k_ref=.TRUE.,i_err=i_err)
     if (i_err==0.and.n_neigh==1) then
       call qp_transfer(k_near(1),i1,1)
       nk_exact=nk_exact+1
     else if (l_plan_todo_EWZI(4)) then
       call qp_transfer(k_near,i1,n_neigh)
       nk_interpolated=nk_interpolated+1
     endif
     !
   enddo
   !
   call msg('sr','[QP] Kpts covered exactly  [o/o]:',real(nk_exact)/real(k%nibz)*100._SP)
   if (l_plan_todo_EWZI(4).and.nk_interpolated>0) then
     call msg('r','[Interpolate] Nighbours         :',n_neigh)
     call msg('r','[Interpolate] Kpts covered [o/o]:',real(nk_interpolated)/real(k%nibz)*100._SP)
   endif
   !
 else if (l_plan_todo_EWZI(5)) then
   !
   ! Nearest Level interpolation
   !===============================
   !
   do i1=1,k%nibz
     do i2=band_range(1),band_range(2)
       do i_spin=1,n_sp_pol
         !
         call Nearest_level_interpolation(En%Eo(i2,i1,i_spin)-minval(En%Eo(:,:,:)),&
&                                         qp%E_bare(:),qp%n_states,1,ib_ref,ik_ref,.TRUE.)
         !
         if (l_plan_todo_EWZI(1)) then
           if (QP_check_if_corrected((/i2,i2/),(/i1,i1/),(/i_spin,i_spin/),en,'E')) cycle
           en%E(i2,i1,i_spin)=en%Eo(i2,i1,i_spin)+qp%E(ib_ref)-qp%E_bare(ib_ref)
           DB_corrected(i2,i1,i_spin)=1
         endif
         !
         if (l_plan_todo_EWZI(2)) then
           if (QP_check_if_corrected((/i2,i2/),(/i1,i1/),(/i_spin,i_spin/),en,'W')) cycle
           if (real(qp%E(ib_ref))>0..and.aimag(qp%E(ib_ref))>0) then
             if (.not.wrong_width_sign_warn) call warning(' Wrong QP width sign fixed')
             en%W(i2,i1,i_spin)=-aimag(qp%E(ib_ref))
             wrong_width_sign_warn=.true.
           else
             en%W(i2,i1,i_spin)=aimag(qp%E(ib_ref))
           endif
           DB_corrected(i2,i1,i_spin)=1
         endif
         !
         if (l_plan_todo_EWZI(3)) then
           if (QP_check_if_corrected((/i2,i2/),(/i1,i1/),(/i_spin,i_spin/),en,'E')) cycle
           en%Z(i2,i1,i_spin)=qp%Z(ib_ref)
           DB_corrected(i2,i1,i_spin)=1
         endif
         !
       enddo
     enddo
   enddo
   !
 endif
 !
 if (all(DB_corrected(band_range(1):band_range(2),:,:)==1)) l_plan_todo_EWZI(:3)=.false.
 !
 QP_ctl_applied=.true.
 !
 ! Clean
 !
 call k_ibz2bz(k,'d',.false.)
 !
 contains
   !
   subroutine qp_transfer(ikqp,ik,n_neigh_)
     integer :: n_neigh_,ikqp(n_neigh_),ik
     !
     ! Work Space
     !
     real(SP)   :: Dl(band_range(2),n_sp_pol),Wl(band_range(2),n_sp_pol)
     complex(SP):: Zl(band_range(2),n_sp_pol)
     integer    :: i3,i4,bqp
     !
     Dl=0.
     Wl=0.
     Zl=(0.,0.)
     !
     do i4=1,n_neigh_
       do i3=1,qp%n_states
         if (qp%table(i3,3)/=ikqp(i4)) cycle
         bqp=qp%table(i3,1)
         !
         i_spin=spin(qp%table(i3,:))
         !
         if (bqp<band_range(1).or.bqp>band_range(2)) cycle
         !
         if (l_plan_todo_EWZI(1)) Dl(bqp,i_spin)=Dl(bqp,i_spin)+real(qp%E(i3)-qp%E_bare(i3))
         !
         if (l_plan_todo_EWZI(2))then
           if (real(qp%E(i3))>0..and.aimag(qp%E(i3))>0) then
             if (.not.wrong_width_sign_warn) call warning(' Wrong QP width sign fixed')
             Wl(bqp,i_spin)=Wl(bqp,i_spin)-aimag(qp%E(i3))
             wrong_width_sign_warn=.true.
           else
             Wl(bqp,i_spin)=Wl(bqp,i_spin)+aimag(qp%E(i3))
           endif
         endif
         !
         if (l_plan_todo_EWZI(3)) Zl(bqp,i_spin)=Zl(bqp,i_spin)+qp%Z(i3)
       enddo
     enddo
     do i3=band_range(1),band_range(2)
       if (l_plan_todo_EWZI(1)) then
         !
         if (any(Dl(i3,:)==0._SP)) cycle
         do i_spin=1,n_sp_pol
           !
           if (QP_check_if_corrected((/i3,i3/),(/ik,ik/),(/i_spin,i_spin/),en,'E')) cycle
           !
           en%E(i3,ik,i_spin)=en%Eo(i3,ik,i_spin)+Dl(i3,i_spin)/real(n_neigh_)
           !
         enddo
         !
         DB_corrected(i3,ik,:)=1
         !
       endif
     enddo
     do i3=band_range(1),band_range(2)
       if (l_plan_todo_EWZI(2)) then
         !
         do i_spin=1,n_sp_pol
           !
           if (QP_check_if_corrected((/i3,i3/),(/ik,ik/),(/i_spin,i_spin/),en,'W')) cycle
           !
           en%W(i3,ik,i_spin)=Wl(i3,i_spin)/real(n_neigh_)
           !
         enddo
         !
         DB_corrected(i3,ik,:)=1
         !
       endif
     enddo
     do i3=band_range(1),band_range(2)
       if (l_plan_todo_EWZI(3)) then
         !
         do i_spin=1,n_sp_pol
           !
           if (QP_check_if_corrected((/i3,i3/),(/ik,ik/),(/i_spin,i_spin/),en,'Z')) cycle
           !
           en%Z(i3,ik,i_spin)=Zl(i3,i_spin)/real(n_neigh_)
           !
         enddo
         !
         DB_corrected(i3,ik,:)=1
         !
       endif
     enddo
     !
   end subroutine
   !
   real(SP) function kmin_distance(k,p) ! k/p are iku
     use pars
     use vec_operate,  ONLY:iku_v_norm,c2a
     use R_lattice,    ONLY:b
     implicit none
     real(SP) :: k(3),p(3) 
     !
     ! Work Space
     !
     integer :: i1,i2,i3
     integer,parameter :: ni=2
     real(SP) :: di(3),da(3)
     di=k-p
     call c2a(b,di,da,'ki2a')
     kmin_distance=iku_v_norm(di)
     do i1=-ni,ni
       do i2=-ni,ni
         do i3=-ni,ni
           call c2a(b,da(:)-(/i1,i2,i3/),di,'ka2c')
           kmin_distance=min(kmin_distance,iku_v_norm(di))
         enddo
       enddo
     enddo
   end function
   !
end subroutine
