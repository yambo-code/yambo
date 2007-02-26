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
 subroutine QP_apply_interpolate(band_range,qp,en,k,l_qp,&
&                                DB_corrected,n_neigh)
 !
 ! Using qp% elements (all must be defined) I QP correct
 ! the en type
 !
 ! l_qp=( E W Z Interp )
 !
 use pars
 use com,           ONLY:msg
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use vec_operate,   ONLY:sort
 use QP_m,          ONLY:QP_t
 !
 implicit none
 type(levels)::en
 type(QP_t)  ::qp
 type(bz_samp)::k
 integer :: n_neigh,band_range(2),DB_corrected(en%nb,en%nk) 
 logical :: l_qp(4)
 !
 ! Work Space
 !
 integer  :: i1,i2,ikbz,order(qp%nk),k_near(n_neigh),nk_interpolated,nk_exact
 real(SP) :: idist(qp%nk),odist(qp%nk),v1(3),k_dist(n_neigh)
 !
 ! IBZ -> BZ (iku)
 !
 call k_ibz2bz(k,'i',.true.)
 !
 ! Transfer & interpolation
 !
 nk_interpolated=0
 nk_exact=0
 do i1=1,k%nibz
   k_near=0
   k_dist=1.E5
   idist=1.E5
   do i2=1,qp%nk
     do ikbz=1+sum(k%nstar(:i1-1)),sum(k%nstar(:i1))
       idist(i2)=min(idist(i2),kmin_distance(qp%k(i2,:),k%ptbz(ikbz,:)))
     enddo
   enddo
   if (qp%nk>1) then
     call sort(idist,odist,order)
     k_near(:min(qp%nk,n_neigh))=order(:min(qp%nk,n_neigh))
     k_dist(:min(qp%nk,n_neigh))=odist(:min(qp%nk,n_neigh))
   else
     k_near(1)=1
     k_dist(1)=idist(1)
   endif
   if (.not.l_qp(4)) then
     if (abs(k_dist(1))>1.E-5) cycle
     call qp_transfer(k_near(:1),i1,1)
     nk_exact=nk_exact+1
   else
     call qp_transfer(k_near,i1,n_neigh)
     nk_interpolated=nk_interpolated+1
   endif
 enddo
 call msg('r','[QP] Kpts covered exactly  [o/o]:',&
&              real(nk_exact)/real(k%nibz)*100._SP)
 if (.not.l_qp(4).and.nk_interpolated>0) then
   call msg('r','[Interpolate] Nighbours         :',n_neigh)
   call msg('r','[Interpolate] Kpts covered [o/o]:',&
&               real(nk_interpolated)/real(k%nibz)*100._SP)
 endif
 !
 if (all(DB_corrected(band_range(1):band_range(2),:)==1)) l_qp(:3)=.false.
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
     real(SP)   :: Dl(band_range(2)),Wl(band_range(2))
     complex(SP):: Zl(band_range(2))
     integer    :: i3,i4,bqp
     logical    :: QP_apply_done
     Dl=0.
     Wl=0.
     Zl=(0.,0.)
     do i4=1,n_neigh_
       do i3=1,qp%n_states
         if (qp%table(i3,3)/=ikqp(i4)) cycle
         bqp=qp%table(i3,1)
         if (bqp<band_range(1).or.bqp>band_range(2)) cycle
         Dl(bqp)=Dl(bqp)+real(qp%E(i3)-qp%E_bare(i3))
         Wl(bqp)=Wl(bqp)+aimag(qp%E(i3))
         Zl(bqp)=Zl(bqp)+qp%Z(i3)
       enddo
     enddo
     do i3=band_range(1),band_range(2)
       if (l_qp(1)) then
         if (QP_apply_done((/i3,i3/),(/ik,ik/),en,'E')) cycle
         if (Dl(i3)==0._SP) cycle
         en%E(i3,ik,1)=en%Eo(i3,ik,1)+Dl(i3)/real(n_neigh_)
         DB_corrected(i3,ik)=1
       endif
       if (l_qp(2)) then
         if (QP_apply_done((/i3,i3/),(/ik,ik/),en,'W')) cycle
         if (Wl(i3)==0._SP) cycle
         en%W(i3,ik)=Wl(i3)/real(n_neigh_)
         DB_corrected(i3,ik)=1
       endif
       if (l_qp(3)) then
         if (QP_apply_done((/i3,i3/),(/ik,ik/),en,'Z')) cycle
         if (Zl(i3)==(0._SP,0._SP)) cycle
         en%Z(i3,ik)=Zl(i3)/real(n_neigh_)
         DB_corrected(i3,ik)=1
       endif
     enddo
     !
   end subroutine
   !
   real function kmin_distance(k,p) ! k/p are iku
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
