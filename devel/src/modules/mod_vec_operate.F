
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
module vec_operate
 !
 use pars,      ONLY:SP,pi,zero_dfl
 implicit none
 !
 contains
   ! 
   function normalize_v(v,zero_norm_)
     !
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
   pure real(SP) function v_norm(v)
     real(SP), intent(in) :: v(3)
     v_norm=sqrt(dot_product(v,v))
   end function v_norm
   !
   pure real(SP) function iku_v_norm(v)
     use D_lattice,      ONLY:alat
     real(SP), intent(in) :: v(3)
     real(SP) :: u(3)
     u(:)=v(:)*2._SP*pi/alat(:)
     iku_v_norm=sqrt(dot_product(u,u))
   end function iku_v_norm
   !
   function cross_product(a,b)
     real(SP), intent(in) :: a(3),b(3)
     real(SP)             :: cross_product(3)
     cross_product(1) = a(2) * b(3) - a(3) * b(2)
     cross_product(2) = a(3) * b(1) - a(1) * b(3)
     cross_product(3) = a(1) * b(2) - a(2) * b(1)
   end function cross_product
   !
   pure logical function v_is_zero(v,zero_)
     real(SP), intent(in) :: v(3)
     real(SP), intent(in), optional :: zero_(3)
     real(SP)          :: local_zero(3)
     !
     local_zero=zero_dfl
     if (present(zero_)) local_zero=zero_
     !
     v_is_zero=all((/abs(v(1))<=local_zero(1),&
&                    abs(v(2))<=local_zero(2),&
&                    abs(v(3))<=local_zero(3)/))
   end function v_is_zero
   !
   pure logical function rlu_v_is_zero(v,zero_)
     real(SP), intent(in), optional :: zero_(3)
     real(SP), intent(in)           :: v(3)
     real(SP)          :: u(3)
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
     real(SP)          :: v_in(3)
     real(SP), optional:: b_in(3,3),v_out(3)
     character(4)::mode
     !
     ! Work Space
     !
     real(SP) a_here(3,3),mat(3,3),n(3),u(3)
     !
     ! Define local unit cell vectors
     !
     a_here=b
     if (present(b_in)) a_here=b_in
     !
     ! Scale factor if input vector is in iku
     !
     if (index(mode,'r')/=0) n(:)=alat(:)
     if (index(mode,'k')/=0) n(:)=2.*pi/alat(:)
     !
     ! u is rlu or cc (no iku)
     !
     u=v_in
     if (index(mode,'i2')/=0) u(:)=v_in(:)*n(:) ! iku -> cc
     !
     ! i2c/c2i
     mat=reshape((/1.,0.,0.,0.,1.,0.,0.,0.,1./),(/3,3/))
     !
     ! a2c/a2i
     if (index(mode,'a2c')/=0.or.index(mode,'a2i')/=0) mat=transpose(a_here)
     !
     ! c2a/i2a
     if (index(mode,'c2a')/=0.or.index(mode,'i2a')/=0) call m3inv(transpose(a_here),mat) 
     !
     if (present(v_out)) then
       v_out=matmul(mat,u) 
       if (index(mode,'2i')/=0) v_out(:)=v_out(:)/n(:) ! * -> iku
     else
       v_in=matmul(mat,u) 
       if (index(mode,'2i')/=0) v_in(:)=v_in(:)/n(:) ! * -> iku
     endif
   end subroutine c2a
   !
   subroutine k2bz(v_in,v_out,b_in) 
     !
     ! k is iku 
     !
     use R_lattice,  ONLY:b
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
   subroutine rlu_k2bz(v_in,v_out) 
     !
     real(SP)           :: v_in(3)
     real(SP), optional :: v_out(3)
     !
     if (present(v_out)) then
       v_out=v_in-nint(v_in)                    
     else
       v_in =v_in-nint(v_in)                    
     endif
     !
   end subroutine rlu_k2bz
   !
   real(SP) function rlu_v_norm(k) 
     !
     ! Input k is rlu 
     !
     ! Output is the module of the k shifted in the IBZ, ak2bz=|k|
     !
     real(SP) :: k(3)
     real(SP) :: k_i(3) !ws
     !
     call c2a(v_in=k,v_out=k_i,mode='ka2i')
     call k2bz(v_in=k_i)
     rlu_v_norm=iku_v_norm(k_i)
     !
   end function rlu_v_norm
   !
   subroutine sort(arrin,arrout,indx,indx_m1)                             
     !
     ! Sort real(dt) values from arrin into array 
     ! arrout and give permutations in indx, indx_m1.
     ! Content of indx is destroyed.
     ! indx_m1: j went to   position indx_m1(j)
     ! indx   : i came from position indx(i) 
     !
     real(SP)::  arrin(:)
     real(SP), optional::  arrout(:)
     integer,  optional::  indx(:)
     integer,  optional::  indx_m1(:)
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
     if (present(indx_m1)) forall( i=1:n) indx_m1(l_indx(i))=i
     deallocate(l_indx,l_arrout)
   end subroutine sort
   !
   subroutine i_sort(arrin,arrout,indx)                             
     integer           ::  arrin(:)
     integer,  optional::  indx(:)
     real(SP), optional::  arrout(:)
     !
     integer :: n
     !
     real(SP), allocatable :: l_arrin(:)
     real(SP), allocatable :: l_arrout(:)
     !
     n=size(arrin)
     !
     allocate(l_arrin(n),l_arrout(n))
     !
     l_arrin=real(arrin)
     if(present(arrout)) then
       l_arrout=real(arrout)
       if(     present(indx)) call sort(l_arrin,arrout=l_arrout,indx=indx)
       if(.not.present(indx)) call sort(l_arrin,arrout=l_arrout)
       arrout=int(l_arrout)
     else
       if(     present(indx)) call sort(l_arrin,indx=indx)
       if(.not.present(indx)) call sort(l_arrin)
       arrin=int(l_arrin)
     endif
     !
     return
     !
   end subroutine i_sort
   !
   subroutine degeneration_finder(E,n,first_deg_el,deg_elmnts,deg_grps,&
&                                 deg_accuracy,Include_single_values)
     !
     ! E = ( 0 1 1 2 3 3 3 ) , n = 7
     !
     ! first_deg_el = ( 2 5 0 0 0 0 0 )
     ! deg_elmnts =   ( 1 2 0 0 0 0 0 )
     !
     ! deg_grps = 2
     !
     integer, intent(in)  :: n
     integer, intent(out) :: first_deg_el(n),deg_elmnts(n),deg_grps
     real(SP),intent(in)  :: E(n),deg_accuracy
     logical, optional    :: Include_single_values
     !
     ! Work Space
     !
     integer :: iref,i1
     real(SP):: E_diff(2)
     logical :: l_flag,l_singles
     !
     deg_grps=0
     first_deg_el(2:n)=0
     deg_elmnts(2:n)=0
     first_deg_el(1)=0
     deg_elmnts(1)=0
     !
     l_singles=.FALSE.
     if (present(Include_single_values)) l_singles=Include_single_values
     !
     iref=1
     !
     do while (iref < n ) 
       !
       if (l_singles) then
         !
         E_diff=2.*deg_accuracy
         !
         if (iref>1) E_diff(1)=abs(E(iref)-E(iref-1))
         E_diff(2)=abs(E(iref)-E(iref+1))
         !
         if (all(E_diff>deg_accuracy)) then
           deg_grps=deg_grps+1
           first_deg_el(deg_grps)=iref
           deg_elmnts(deg_grps)=1
         endif
         !
         if (iref==n-1.and.E_diff(2)>deg_accuracy) then
           deg_grps=deg_grps+1
           first_deg_el(deg_grps)=iref+1
           deg_elmnts(deg_grps)=1
         endif
       endif
       !
       l_flag=.true.
       !
       do i1=iref+1,n
         !
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
