
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
module vec_operate
 !
 use pars,      ONLY:SP,pi,zero_dfl
 !
 contains
   ! 
   function normalize_v(v,zero_norm_)
     !
     implicit none
     real(SP), intent(in) :: v(3)
     real(SP), optional   :: zero_norm_
     !
     real(SP) :: normalize_v(3)
     real(SP) :: norm
     !
     norm = v_norm(v)
     if (present(zero_norm_)) then
       if(norm <= zero_norm_) return
     else
       if(norm <= zero_dfl) return
     endif
     normalize_v(:) = v(:)/norm
     !
   end function normalize_v
   !
   !
   function v_norm(v)
     implicit none
     real(SP) :: v(3),v_norm
     v_norm=sqrt(dot_product(v,v))
   end function v_norm
   !
   function iku_v_norm(v)
     use D_lattice,      ONLY:alat
     implicit none
     real(SP) :: v(3),u(3),iku_v_norm
     u(:)=v(:)*2.*pi/alat(:)
     iku_v_norm=sqrt(dot_product(u,u))
   end function iku_v_norm
   !
   function cross_product(a,b)
     implicit none
     real(SP), intent(in) :: a(3),b(3)
     real(SP)             :: cross_product(3)
     cross_product(1) = a(2) * b(3) - a(3) * b(2)
     cross_product(2) = a(3) * b(1) - a(1) * b(3)
     cross_product(3) = a(1) * b(2) - a(2) * b(1)
   end function cross_product
   !
   logical function v_is_zero(v,zero_)
     implicit none
     real(SP)          :: v(3),local_zero(3)
     real(SP),optional :: zero_(3)
     !
     local_zero=zero_dfl
     if (present(zero_)) local_zero=zero_
     !
     v_is_zero=all((/abs(v(1))<=local_zero(1),&
&                    abs(v(2))<=local_zero(2),&
&                    abs(v(3))<=local_zero(3)/))
   end function v_is_zero
   !
   logical function rlu_v_is_zero(v,zero_)
     implicit none
     real(SP)          :: v(3),u(3)
     real(SP),optional :: zero_(3)
     u=v-nint(v)
     if (.not.present(zero_)) rlu_v_is_zero=v_is_zero(u)
     if (     present(zero_)) rlu_v_is_zero=v_is_zero(u,zero_)
   end function rlu_v_is_zero
   !
   subroutine c2a(b_in,v_in,v_out,mode)
     !
     ! mode = 'k/rc2a' 'k/ra2c' (cc  <-> rlu)
     ! mode = 'k/ri2a' 'k/ra2i' (iku <-> rlu)
     ! mode = 'k/ri2c' 'k/rc2i' (cc  <-> iku)
     !
     use D_lattice,      ONLY:alat
     use R_lattice,      ONLY:b
     use matrix_operate, ONLY:m3inv
     implicit none
     real(SP)          :: v_in(3)
     real(SP), optional:: b_in(3,3),v_out(3)
     character(4)::mode
     !
     ! Work Space
     !
     real(SP) a_here(3,3),mat(3,3),n(3),u(3)
     !
     a_here=b
     if (present(b_in)) a_here=b_in
     !
     if (index(mode,'r')/=0) n(:)=alat(:)
     if (index(mode,'k')/=0) n(:)=2.*pi/alat(:)
     u=v_in
     if (index(mode,'i2')/=0) u(:)=v_in(:)*n(:) ! rlu -> cc
     mat=reshape((/1.,0.,0.,0.,1.,0.,0.,0.,1./),(/3,3/))
     if (index(mode,'a2c')/=0.or.index(mode,'a2i')/=0) mat=transpose(a_here)
     if (index(mode,'c2a')/=0.or.index(mode,'i2a')/=0) call m3inv(transpose(a_here),mat)
     if (present(v_out)) then
       v_out=matmul(mat,u) 
       if (index(mode,'2i')/=0) v_out(:)=v_out(:)/n(:) ! * -> rlu
     else
       v_in=matmul(mat,u) 
       if (index(mode,'2i')/=0) v_in(:)=v_in(:)/n(:) ! * -> rlu
     endif
   end subroutine c2a
   !
   subroutine k2bz(v_in,v_out,b_in) 
     !
     ! k is iku 
     !
     use R_lattice,  ONLY:b
     implicit none
     real(SP)           :: v_in(3)
     real(SP), optional :: v_out(3),b_in(3,3)
     ! 
     ! Work Space
     ! 
     real(SP):: b_here(3,3)
     real(SP):: p(3),q(3),u(3),dist
     integer :: i1,i2,i3
     integer,parameter :: ni=2
     !
     if (present(b_in))      b_here=b_in
     if (.not.present(b_in)) b_here=b
     !
     call c2a(b_here,v_in,q,'ki2a')
     call c2a(b_here,v_in,p,'ki2c')
     dist=v_norm(p)
     do i1=-ni,ni
       do i2=-ni,ni
         do i3=-ni,ni
           call c2a(b_here,q(:)-(/i1,i2,i3/),u,'ka2c')
           if (v_norm(u)<dist-1.E-5) p=u
           if (v_norm(u)<dist-1.E-5) dist=v_norm(u)
         enddo
       enddo
     enddo
     call c2a(b_here,p,q,'kc2i')
     !
     if (present(v_out))      v_out=q
     if (.not.present(v_out)) v_in=q
     !
   end subroutine k2bz
   !
   real(SP) function ak2bz(k) 
     !
     ! k is rlu 
     !
     ! Output is the module the k in the IBZ, ak2bz=|k|
     !
     implicit none
     real(SP) :: k(3)
     real(SP) :: k_i(3) !ws
     !
     call c2a(v_in=k,v_out=k_i,mode='ka2i')
     call k2bz(v_in=k_i)
     ak2bz=iku_v_norm(k_i)
     call c2a(v_in=k_i,v_out=k,mode='ki2a')
     !
   end function ak2bz
   !
   subroutine sort(arrin,arrout,indx)                             
     !
     ! Sort real(dt) values from arrin into array 
     ! arrout and give permutations in indx.
     ! Content of indx is destroyed.
     !
     implicit none
     real(SP)::  arrin(:)
     integer,  optional::  indx(:)
     real(SP), optional::  arrout(:)
     !
     ! local variables
     !
     integer j, i,n, ir, l, indxt
     real(SP)  q
     integer, allocatable:: l_indx(:)
     real(SP),allocatable:: l_arrout(:)
     !
     n=size(arrin)
     allocate(l_indx(n),l_arrout(n))
     !
     if(n.eq.1) then
       l_arrout(1) = arrin(1)
       l_indx(1) = 1
       if (present(arrout)) arrout=l_arrout
       if (.not.present(arrout)) arrin=l_arrout
       if (present(indx)) indx=l_indx
       deallocate(l_indx,l_arrout)
       return
     endif
     do j=1,n
       l_indx(j)=j
     enddo
     l=n/2+1
     ir=n
  1  continue
     if (l.gt.1)then
       l=l-1
       indxt=l_indx(l)
       q=arrin(indxt)
     else
       indxt=l_indx(ir)
       q=arrin(indxt)
       l_indx(ir)=l_indx(1)
       ir=ir-1
       if (ir.eq.1)then
         l_indx(1)=indxt
         goto 3
       endif
     endif
     i=l
     j=l+l
  2  if (j.le.ir)then
      if (j.lt.ir) then
        if (arrin(l_indx(j)).lt.arrin(l_indx(j+1)))j=j+1
      endif
      if (q.lt.arrin(l_indx(j))) then
        l_indx(i)=l_indx(j)
        i=j
        j=j+j
      else
        j=ir+1
      endif
      go to 2
     endif
     l_indx(i)=indxt
     go to 1
  3  continue
     do i=1,n
       l_arrout(i) = arrin(l_indx(i))
     enddo
     if (present(arrout)) arrout=l_arrout
     if (.not.present(arrout)) arrin=l_arrout
     if (present(indx)) indx=l_indx
     deallocate(l_indx,l_arrout)
   end subroutine sort
   !
   subroutine degeneration_finder(E,n,first_deg_el,deg_elmnts,deg_grps,deg_accuracy)
     !
     ! E = ( 0 1 1 2 3 3 3 ) , n = 7
     !
     ! first_deg_el = ( 2 5 0 0 0 0 0 )
     ! deg_elmnts =   ( 1 2 0 0 0 0 0 )
     !
     ! deg_grps = 2
     !
     implicit none
     !
     integer, intent(in)  :: n
     integer, intent(out) :: first_deg_el(n),deg_elmnts(n),deg_grps
     real(SP),intent(in)  :: E(n),deg_accuracy
     !
     ! Work Space
     !
     integer :: igrp,iref,i1
     logical :: l_flag
     !
     deg_grps=0
     first_deg_el(2:n)=0
     deg_elmnts(2:n)=0
     first_deg_el(1)=0
     deg_elmnts(1)=0
     !
     iref=1
     do while (iref < n ) 
       !
       l_flag=.true.
       !
       do i1=iref+1,n
         if (abs(E(iref)-E(i1)).le.deg_accuracy) then
           if (l_flag) then
             deg_grps=deg_grps+1
             first_deg_el(deg_grps)=iref
             deg_elmnts(deg_grps)=1
             l_flag=.false.
           endif
           deg_elmnts(deg_grps)=deg_elmnts(deg_grps)+1
         else
           exit
         endif
       enddo
       ! 
       if (l_flag) iref=iref+1
       if (.not.l_flag) iref=iref+deg_elmnts(deg_grps)
       !
     enddo
     !
   end subroutine
   !
end module vec_operate
