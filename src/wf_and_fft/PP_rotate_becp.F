!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, IM
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
subroutine PP_rotate_becp(isc,ibec,obec)
 !
 ! isc(1), ibnd
 ! isc(2), ikpt
 ! isc(3), isymm
 ! isc(4), ispin
 !
 ! ibec    becp corresponding to (ikpt, ispin)
 ! obec    becp corresponding to (isym*ikpt, ispin)
 !
 use pars,          ONLY:SP
 use pseudo,        ONLY:pp_is_uspp
 use D_lattice,     ONLY:nsym,spin_sop,dl_sop,i_time_rev,idt_index,i_space_inv,inv_index
 use R_lattice,     ONLY:rl_sop
 use qe_pseudo_m,   ONLY:bec_type,allocate_bec_type,deallocate_bec_type,&
&                        beccopy,d1,d2,d3
 !
#include<memory.h>
 !
 ! vars
 !
 integer :: isc(4)
 type(bec_type), intent(in)    :: ibec
 type(bec_type), intent(inout) :: obec

 !
 ! Workspace
 !
 integer     :: isym
 real(SP)    :: rvkpt_c(3), arg
 complex(SP) :: c1(3,3), c2(5,5), c3(7,7), phase
 

 if (.not.pp_is_uspp) return
 !
 isym=isc(3)
 !
 if (isym==idt_index) then
   call beccopy(ibec, obec)
   return
 endif
 !
!! XXX
! write(0,*) "shape dl_sop", shape(dl_sop)
! write(0,*) "shape spin_sop", shape(spin_sop)
! write(0,"(/,2x,'DL_SOP')") 
! do isym = 1, nsym
!   write(0,*) "   isym = ", isym
!   write(0,*) "spin_op = ", spin_sop(:,:,isym)
!   write(0,"(3f15.9)") dl_sop(:,:,isym)
!   write(0,*)
! enddo
! write(0,"(/,2x,'RL_SOP')") 
! do isym = 1, nsym
!   write(0,*) "   isym = ", isym
!   write(0,*) "spin_op = ", spin_sop(:,:,isym)
!   write(0,"(3f15.9)") rl_sop(:,:,isym)
!   write(0,*)
! enddo
 
 !
 return
 !
end subroutine PP_rotate_becp

