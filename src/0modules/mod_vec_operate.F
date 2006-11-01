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
 module vec_operate
!==================
 use pars
!
 contains
! 
 function v_module(v)
!--------------------
 implicit none
 real(SP) :: v(3),v_module
 v_module=sqrt(dot_product(v,v))
 end function
!
 function iku_v_module(v)
!------------------------
 use D_lattice,      ONLY:alat
 implicit none
 real(SP) :: v(3),u(3),iku_v_module
 u(:)=v(:)*2.*pi/alat(:)
 iku_v_module=sqrt(dot_product(u,u))
 end function

 function internal_product(u,v,ic)
!---------------------------------
 implicit none
 integer :: ic
 real(SP):: u(3),v(3),internal_product
 if (ic==1) internal_product=u(2)*v(3)-v(2)*u(3)
 if (ic==2) internal_product=u(3)*v(1)-v(3)*u(1)
 if (ic==3) internal_product=u(1)*v(2)-v(1)*u(2)
 end function
!
 logical function v_is_zero(v)
!-----------------------------
 use R_lattice, ONLY:RL_v_comp_acc
 implicit none
 real(SP) :: v(3)
 v_is_zero=all((/abs(v(1))<=RL_v_comp_acc,abs(v(2))<=RL_v_comp_acc,&
&                abs(v(3))<=RL_v_comp_acc/))
 end function

 logical function rlu_v_is_zero(v)
!--------------------------------
 implicit none
 real(SP) :: v(3),u(3)
 u=v-nint(v)
 rlu_v_is_zero=v_is_zero(u)
 end function
!
 subroutine c2a(b_in,v_in,v_out,mode)
!------------------------------------
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
!ws
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
 end subroutine

 subroutine k2bz(v_in,v_out,b_in) ! k is iku !
!================================
 use pars,       ONLY:SP
 use R_lattice,  ONLY:b
 implicit none
 real(SP)           :: v_in(3)
 real(SP), optional :: v_out(3),b_in(3,3)
!ws
 real(SP):: b_here(3,3)
 real(SP):: p(3),q(3),u(3),dist
 integer :: i1,i2,i3
 integer,parameter :: ni=2

 if (present(b_in))      b_here=b_in
 if (.not.present(b_in)) b_here=b

 call c2a(b_here,v_in,q,'ki2a')
 call c2a(b_here,v_in,p,'ki2c')
 dist=v_module(p)
 do i1=-ni,ni
   do i2=-ni,ni
     do i3=-ni,ni
       call c2a(b_here,q(:)-(/i1,i2,i3/),u,'ka2c')
       if (v_module(u)<dist-1.E-5) p=u
       if (v_module(u)<dist-1.E-5) dist=v_module(u)
     enddo
   enddo
 enddo
 call c2a(b_here,p,q,'kc2i')

 if (present(v_out))      v_out=q
 if (.not.present(v_out)) v_in=q

 end subroutine
!
 real(SP) function ak2bz(k) ! k is rlu !
!==========================
!
! Output is the module the k in the IBZ and
! ak2bz=|k|
!
 use pars, ONLY:SP
 implicit none
 real(SP) :: k(3)
 real(SP) :: k_i(3) !ws

 call c2a(v_in=k,v_out=k_i,mode='ka2i')
 call k2bz(v_in=k_i)
 ak2bz=iku_v_module(k_i)
 call c2a(v_in=k_i,v_out=k,mode='ki2a')

 end function
!
 logical function k_is_in_bz(k,lb) ! k is iku !
!=================================
 use pars
 implicit none
 real(SP) :: k(3),p(3),lb(3,3)
 call k2bz(k,p,lb)
 k_is_in_bz=v_is_zero(k-p)
 end function
!
 subroutine sort(arrin,arrout,indx)                             
!==================================
!
! Sort real(dt) values from arrin into array 
! arrout and give permutations in indx.
! Content of indx is destroyed.
!
!----------------------------------------
 use pars
 implicit none
 real(SP)::  arrin(:)
 integer,  optional::  indx(:)
 real(SP), optional::  arrout(:)
!
! local variables here --->
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
1 continue
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
     goto 150
   endif
 endif
 i=l
 j=l+l
2 if (j.le.ir)then
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
150 continue
 do i=1,n
   l_arrout(i) = arrin(l_indx(i))
 enddo
 if (present(arrout)) arrout=l_arrout
 if (.not.present(arrout)) arrin=l_arrout
 if (present(indx)) indx=l_indx
 deallocate(l_indx,l_arrout)
 end subroutine
!
 end module
