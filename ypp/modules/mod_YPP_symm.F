!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
module YPP_symm
 !
 implicit none
 !
 logical               :: l_wf_nc_k_red
 !
 integer               :: ng_vec_save,ng_vec_new
 integer               :: wf_ng_save,wf_ng_new
 integer               :: wf_ncx_save,wf_ncx_new
 !
 integer,allocatable   :: wf_nc_k_save(:),wf_nc_k_reduced(:),wf_nc_k_new(:)
 integer,allocatable   :: wf_igk_save(:,:),wf_igk_reduced(:,:),wf_igk_new(:,:)
 !
end module YPP_symm
