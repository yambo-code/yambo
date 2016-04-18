!
!        Copyright (C) 2000-2016 the YAMBO team
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
integer function io_out_of_date(ID)
 !
 use IO_m,          ONLY:IO_OUTDATED_DB,io_file,ver_is_gt_or_eq
 !
 implicit none
 integer               :: ID
 !
 ! Work Space
 !
 integer :: REF_rev,REF_ver(3)
 !
 io_out_of_date=IO_OUTDATED_DB
 !
 ! Global revision check (mostly based on io_header)
 !===================================================
 !
 if (.not.ver_is_gt_or_eq(ID,(/3,0,15/))) return
 !
 ! DB specific
 !=============
 !
 REF_rev=5380
 if (index(io_file(ID),'RT_carriers')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 if (index(io_file(ID),'RT_J_and_P')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 if (index(io_file(ID),'RT_REF')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 if (index(io_file(ID),'RT_G')/=0.and..not.ver_is_gt_or_eq(ID,revision=REF_rev)) return
 !
 REF_ver=(/3,2,1/)
 if (index(io_file(ID),'elph_gkkp')/=0.and..not.ver_is_gt_or_eq(ID,REF_ver)) return
 if (index(io_file(ID),'elph_gFsq')/=0.and..not.ver_is_gt_or_eq(ID,REF_ver)) return
 !
 ! No error => Clean return
 !==========================
 !
 io_out_of_date=0
 !
end function
