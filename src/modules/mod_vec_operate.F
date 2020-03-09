!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 implicit none
 !
 interface sort
   module procedure r_sort,i_sort
 end interface
 !
 interface cross_product
   module procedure r_cross_product,c_cross_product
 end interface
 !
 contains
   ! 
   integer function min_index(N,v)
     !
     integer,  intent(in) :: N
     real(SP), intent(in) :: v(N)
     !
     real(SP) :: v_min
     integer  :: i_v
     !
     v_min=minval(v)
     min_index=0
     do i_v=1,N
       if (abs(v(i_v)-v_min)<1.E-5) min_index=i_v
     enddo
     !
   end function min_index
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
   pure real(SP) function v_norm2(v)
     real(SP), intent(in) :: v(3)
     v_norm2=v(1)*v(1)+v(2)*v(2)+v(3)*v(3)
   end function v_norm2
   !
   real(SP) function iku_v_norm(v,inside_bz)
     !
     ! shifted in the BZ if inside_bz is set to true, ak2bz=|k|
     !
     use D_lattice,      ONLY:alat
     real(SP), intent(in)           :: v(3)
     logical,  intent(in), optional :: inside_bz
     !
     real(SP) :: u(3)
     logical  :: inside_bz_
     !
     inside_bz_=.false.
     if(present(inside_bz)) inside_bz_=inside_bz
     !
     u(:)=v(:)
     if(inside_bz_) call k2bz(v_in=u)
     !
     u(:)=u(:)*2._SP*pi/alat(:)
     iku_v_norm=sqrt(dot_product(u,u))
     !
   end function iku_v_norm
   !
   real(SP) function rlu_v_norm(k,inside_bz) 
     !
     ! Input k is rlu 
     !
     ! Output is the module of the k 
     ! shifted in the BZ if inside_bz is set to true
     !
     real(SP), intent(in)           :: k(3)
     logical , intent(in), optional :: inside_bz
     !
     ! Work space
     !
     real(SP) :: k_i(3)
     logical  :: inside_bz_
     !
     inside_bz_=.false.
     if(present(inside_bz)) inside_bz_=inside_bz
     !
     call c2a(v_in=k,v_out=k_i,mode='ka2i')
     rlu_v_norm=iku_v_norm(k_i,inside_bz=inside_bz_)
     !
   end function rlu_v_norm
   !
   function r_cross_product(a,b)
     real(SP), intent(in) :: a(3),b(3)
     real(SP)             :: r_cross_product(3)
     r_cross_product(1) = a(2) * b(3) - a(3) * b(2)
     r_cross_product(2) = a(3) * b(1) - a(1) * b(3)
     r_cross_product(3) = a(1) * b(2) - a(2) * b(1)
   end function r_cross_product
   !
   function c_cross_product(a,b)
     complex(SP), intent(in) :: a(3),b(3)
     complex(SP)             :: c_cross_product(3)
     c_cross_product(1) = conjg(a(2)) * b(3) - conjg(a(3)) * b(2)
     c_cross_product(2) = conjg(a(3)) * b(1) - conjg(a(1)) * b(3)
     c_cross_product(3) = conjg(a(1)) * b(2) - conjg(a(2)) * b(1)
   end function c_cross_product
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
   subroutine rot_v_xy_plane(t,v_in,v_out)
     !
     ! Rotates v_in of theta in the xy plane
     !
     use pars,     ONLY:rZERO,rONE
     !
     real(SP)          :: v_in(3),t
     real(SP), optional:: v_out(3)
     !
     real(SP)          :: M(3,3),v_loc(3)
     !
     !     ( cos(t) -sin(t)  0 )
     ! M = ( sin(t)  cos(t)  0 )
     !     (   0       0     1 )
     !
     M=reshape((/cos(t),-sin(t),rZERO,sin(t),cos(t),rZERO,rZERO,rZERO,rONE/),(/3,3/))
     v_loc=matmul(M,v_in)
     if (     present(v_out)) v_out=v_loc
     if (.not.present(v_out)) v_in =v_loc
     !
   end subroutine rot_v_xy_plane
   !
   subroutine c2a(b_in,v_in,v_out,mode)
     !
     ! mode = 'k/r c2a' 'k/r a2c' (cc  <-> rlu)
     ! mode = 'k/r i2a' 'k/r a2i' (iku <-> rlu)
     ! mode = 'k/r i2c' 'k/r c2i' (cc  <-> iku)
     !
     use D_lattice,      ONLY:alat,a
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
     if (index(mode,'r')/=0) then
       a_here=a
       if (present(b_in)) a_here=b_in
     else
       a_here=b
       if (present(b_in)) a_here=b_in
     endif
     !
     ! Scale factor if input vector is in iku
     !
     if (index(mode,'r')/=0) n(:)=alat(:)
     if (index(mode,'k')/=0) n(:)=2._SP*pi/alat(:)
     !
     ! u is rlu or cc (no iku)
     !
     u=v_in
     if (index(mode,'i2')/=0) u(:)=v_in(:)*n(:) ! iku -> cc
     !
     ! i2c/c2i
     mat=reshape((/1._SP,0._SP,0._SP,0._SP,1._SP,0._SP,0._SP,0._SP,1._SP/),(/3,3/))
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
   subroutine define_b_and_DL_vol()
     !
     use pars,        ONLY:pi
     use D_lattice,   ONLY:a,DL_vol
     use R_lattice,   ONLY:b
     use com,         ONLY:msg
     !
     real(SP)  :: cp(3)
     integer   :: i1
     !
     cp = cross_product(a(2,:),a(3,:))
     DL_vol=0._SP
     do i1=1,3
       DL_vol= DL_vol+a(1,i1)*cp(i1)
     enddo
     b(1,:)=cross_product(a(2,:),a(3,:))*2.0_SP*pi/DL_vol
     b(2,:)=cross_product(a(3,:),a(1,:))*2.0_SP*pi/DL_vol
     b(3,:)=cross_product(a(1,:),a(2,:))*2.0_SP*pi/DL_vol
     if ( DL_vol <0._SP) then
       call msg('rsn',"Axis vectors are left handed")
       DL_vol=abs(DL_vol)
     endif
     !
   end subroutine
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
     real(SP):: b_here(3,3),p(3),q(3),u(3),dist
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
   pure function k_periodic_idx(idx,k_map)
    use R_lattice,      ONLY:bz_map
    implicit none
    type(bz_map)  , intent(in) :: k_map
    integer,        intent(in) :: idx(3)
    integer                    :: k_periodic_idx(3)
    !
    k_periodic_idx(:)=modulo(idx(:)-1,k_map%max_kdir(:)) + 1
    !
   end function
   !
   integer function get_neighbor(step,ik,k_map)
     use R_lattice,      ONLY:bz_map
     implicit none
     !
     integer,       intent(in) :: step(3)
     integer,       intent(in) :: ik
     type(bz_map),  intent(in) :: k_map
     !
     ! Workspace
     !
     integer :: idx(3)
     !
     if(all(step(:)==0)) then
       get_neighbor=ik
       return 
     endif
     !
     idx=k_map%k_map_inv(ik,:)
     idx(:)=idx(:)+step(:)
     idx=k_periodic_idx(idx,k_map)
     get_neighbor=k_map%k_map_dir(idx(1),idx(2),idx(3))
     !
   end function get_neighbor
   !
   subroutine get_id_perpendicular(id1,id2,id3)
     implicit none
     integer, intent(in)  :: id1
     integer, intent(out) :: id2,id3
     !
     if(id1==1) then
       id2=2
       id3=3
     elseif(id1==2) then
       id2=1
       id3=3
     elseif(id1==3) then
       id2=1
       id3=2
     endif
     !
   end subroutine get_id_perpendicular
   !
   subroutine r_sort(arrin,arrout,indx,indx_m1)                             
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
     integer  :: j, i,n, ir, l, indxt
     real(SP) :: q
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
         go to 3
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
   end subroutine r_sort
   !
   integer function GreatestCommonDivisor(a,b)
     !
     implicit  none
     integer :: a, b, c
     if (a < b) then       ! since a >= b must be true, they
       c = a               ! are swapped if a < b
       a = b
       b = c
     end if
     do                    ! now we have a <= b
       c = mod(a, b)       !    compute c, the reminder
       if (c == 0) exit    !    if c is zero, we are done.  gcd = b
       a = b               !    otherwise, b becomes a
       b = c               !    and c becomes b
     end do                !    go back
     !
     GreatestCommonDivisor = b
     !
   end function GreatestCommonDivisor
   !
   subroutine i_sort(arrin,arrout,indx,indx_m1)                             
     !
     ! Sort integer values from arrin into array 
     ! arrout and give permutations in indx, indx_m1.
     ! Content of indx is destroyed.
     ! indx_m1: j went to   position indx_m1(j)
     ! indx   : i came from position indx(i) 
     !
     integer           ::  arrin(:)
     integer,  optional::  arrout(:)
     integer,  optional::  indx(:)
     integer,  optional::  indx_m1(:)
     !
     ! local variables
     !
     integer  :: j, i,n, ir, l, indxt, q
     integer, allocatable:: l_indx(:)
     integer, allocatable:: l_arrout(:)
     !
     n=size(arrin)
     allocate(l_indx(n),l_arrout(n))
     !
     if(n.eq.1) then
       l_arrout(1) = arrin(1)
       l_indx(1) = 1
       if (     present(arrout)) arrout=l_arrout
       if (.not.present(arrout)) arrin =l_arrout
       if (     present(indx)  ) indx  =l_indx
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
         go to 3
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
     if (     present(arrout) ) arrout=l_arrout
     if (.not.present(arrout) ) arrin =l_arrout
     if (     present(indx)   ) indx  =l_indx
     if (     present(indx_m1)) forall( i=1:n) indx_m1(l_indx(i))=i
     deallocate(l_indx,l_arrout)
   end subroutine i_sort
   !
   subroutine degeneration_finder(E,n,first_deg_el,deg_elmnts,deg_grps,&
&                                 deg_accuracy,Include_single_values)
     !
     ! Assumes energy are sorted
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
     first_deg_el=0
     deg_elmnts=0
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
         E_diff=2._SP*deg_accuracy
         !
         if (deg_grps>0) E_diff(1)=abs(E(iref)-E(first_deg_el(deg_grps)))
         if (iref<n    ) E_diff(2)=abs(E(iref)-E(iref+1))
         !
         if (all(E_diff>=deg_accuracy)) then
           deg_grps=deg_grps+1
           first_deg_el(deg_grps)=iref
           deg_elmnts(deg_grps)=1
         endif
         !
       endif
       !
       l_flag=.true.
       !
       do i1=iref+1,n
         !
         if (abs(E(iref)-E(i1))<deg_accuracy) then
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
       if (     l_flag) iref=iref+1
       if (.not.l_flag) iref=iref+deg_elmnts(deg_grps)
       !
     enddo
     !
     if(deg_grps> 0) i1=first_deg_el(deg_grps)
     if(deg_grps==0) i1=1
     !
     if(l_singles .and.  abs(E(n)-E(i1))>deg_accuracy) then
       deg_grps=deg_grps+1
       first_deg_el(deg_grps)=n
       deg_elmnts(deg_grps)=1
     endif
     !
   end subroutine
   !
end module vec_operate
