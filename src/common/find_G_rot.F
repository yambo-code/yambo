!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
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
subroutine find_G_rot(is,call_kind)
 !
 use pars,         ONLY:SP,schlen
 use com,          ONLY:error
 use R_lattice,    ONLY:ng_vec,ng_closed,g_vec,minus_G
 use D_lattice,    ONLY:dl_sop
 use zeros,        ONLY:G_iku_zero
 use vec_operate,  ONLY:v_is_zero
 !
 integer               :: is
 character(schlen)     :: call_kind
 !
 ! Work space
 ! 
 integer               :: ig,igp,ng
 logical               :: inversion
 logical               :: extend_grid
 logical               :: set_table
 real(SP)              :: g_rot_tmp(3) 
 real(SP),allocatable  :: g_vec_tmp(:,:)
 logical, allocatable  :: g_rot_found(:)
 !
 if(trim(call_kind)=='extend_grid') extend_grid = .true.
 if(trim(call_kind)=='set_table')   set_table   = .true.
 !
 ng=-1
 if(extend_grid) ng=ng_vec
 if(set_table)   ng=ng_closed
 !
 if(ng==-1) call error('find_g_rot: ng=-1')
 if(ng==1)  return
 !
 if(is==-1) inversion=.true.
 !
 allocate(g_vec_tmp(2*ng,3),g_rot_found(2*ng))
 g_vec_tmp(1:ng_vec,:)=g_vec
 g_rot_found=.false.
 if(extend_grid) deallocate(g_vec)
 !
 g_rot_found(1)=.true.
 if(set_table) minus_G(1)=1
 !
 do ig=2,ng
   if(g_rot_found(ig)) cycle
   if(     inversion) g_rot_tmp=-g_vec_tmp(ig,:)
   if(.not.inversion) g_rot_tmp=matmul(dl_sop(:,:,is),g_vec_tmp(ig,:))
   !
   do igp=ig+1,ng
     if(g_rot_found(igp)) cycle
     if( v_is_zero(g_rot_tmp-g_vec_tmp(igp,:),zero_=G_iku_zero)) then
       g_rot_found(ig)=.true.
       g_rot_found(igp)=.true.
       if(set_table) then
         minus_G(ig)=igp
         minus_G(igp)=ig
       endif
       exit
     endif
   enddo
   !
   if(.not.g_rot_found(ig).and..not.extend_grid) call error('find_g_rot: g_rot not found')
   !
   if(.not.g_rot_found(ig).and.extend_grid) then
     ng_vec=ng_vec+1
     ng=ng_vec
     g_vec_tmp(ng_vec,:)=g_rot_tmp
     g_rot_found(ig)=.true.
     g_rot_found(ng_vec)=.true.
   endif
 enddo
 !
 if(extend_grid) then
   allocate(g_vec(ng_vec,3))
   g_vec=g_vec_tmp(1:ng_vec,:)
 endif
 !
 deallocate(g_vec_tmp,g_rot_found)
 !
end subroutine
