!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA DS
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
subroutine fix_WFs_Gshells(k,k_save,kpoints_map,old_nsym,action_kind)
 !
 use com,                 ONLY:msg
 use memory_m,            ONLY:mem_est
 use vec_operate,         ONLY:v_norm,i_sort
 use wave_func,           ONLY:wf_nc_k, wf_igk,wf_ncx,wf_ng
 use R_lattice,           ONLY:bz_samp,g_rot,ng_closed
 use vec_operate,         ONLY:v_norm
 use D_lattice,           ONLY:nsym
 !
 implicit none
 !
 type(bz_samp),intent(inout) :: k
 type(bz_samp),intent(in)    :: k_save
 integer,intent(in)          :: kpoints_map(2,k%nibz)
 integer,intent(in)          :: old_nsym
 !
 integer,intent(in)          :: action_kind
 !
 ! Work space
 !
 integer                     :: wf_ng_save
 integer                     :: wf_ncx_save
 integer                     :: wf_nc_k_save(k_save%nibz)
 integer                     :: wf_igk_save(wf_ncx,k_save%nibz)
 !
 integer                     :: ik,ik_save,ic,is
 !
 wf_nc_k_save=wf_nc_k
 wf_igk_save =wf_igk
 wf_ncx_save =wf_ncx
 wf_ng_save  =wf_ng
 !
 deallocate(wf_nc_k,wf_igk)
 call mem_est("wf_nc_k wf_igk")
 !
 ! Check which of the wfc components can be rotated
 ! and discard the ones above ng_closed
 ! This could be removed by increasing the total number of g_vectors
 do ik=1,k_save%nibz
   do ic=1,wf_nc_k_save(ik)
     if( wf_igk_save(ic,ik)<=ng_closed ) cycle
     wf_igk_save(ic:wf_nc_k_save(ik),ik)=-1
     wf_nc_k_save(ik)=ic-1
     exit
   enddo
 enddo
 !
 wf_ncx=maxval(wf_nc_k_save)
 !
 allocate(wf_nc_k(k%nibz),wf_igk(wf_ncx,k%nibz))
 !
 select case(action_kind)
 case(1)
   !
   wf_igk=-1
   !
   do ik=1,k_save%nibz
     !
     ik_save=kpoints_map(1,ik)
     !
     wf_nc_k(ik) = wf_nc_k_save(ik_save)
     wf_igk(1:wf_nc_k(ik),ik) = wf_igk_save(1:wf_nc_k_save(ik_save),ik_save)
     !
   enddo
   !
   do ik=k_save%nibz+1,k%nibz
     !
     ik_save=kpoints_map(1,ik)
     is=kpoints_map(2,ik)
     !
     wf_nc_k(ik) = wf_nc_k_save(ik_save)
     wf_igk(1:wf_nc_k(ik),ik) = g_rot(is,wf_igk_save(1:wf_nc_k_save(ik_save),ik_save))
     call i_sort( wf_igk(1:wf_nc_k(ik),ik) )
     !
   enddo
   !
 case(2)
   !
   wf_igk=-1
   do ik=1,k%nibz
     wf_igk(1:wf_ncx,ik) = wf_igk_save(1:wf_ncx,kpoints_map(1,ik))
     wf_nc_k(ik)         = wf_nc_k_save(kpoints_map(1,ik))
   enddo
   !
   !
 end select
 !
 wf_ng=maxval(wf_igk)
 !
end subroutine
