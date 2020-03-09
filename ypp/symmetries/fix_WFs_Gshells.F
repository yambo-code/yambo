!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use vec_operate,         ONLY:v_norm,sort
 use wave_func,           ONLY:wf_nc_k, wf_igk,wf_ncx,wf_ng
 use R_lattice,           ONLY:bz_samp,g_rot,ng_closed,ng_vec
 use vec_operate,         ONLY:v_norm
 use D_lattice,           ONLY:nsym
 use stderr,              ONLY:intc
 use YPP_symm,            ONLY:wf_nc_k_save,wf_igk_save,wf_nc_k_reduced,wf_igk_reduced,&
&                              wf_nc_k_new,l_wf_nc_k_red,ng_vec_new,ng_vec_save,wf_igk_new,&
&                              wf_nc_k_new,wf_ncx_new,wf_ncx_save,wf_ng_new,wf_ng_save,wf_ng_cut
 !
#include<memory.h>
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
 integer                     :: ik,ik_save,ic,is
 !
 !
 ! The old SAVE tables
 !
 YAMBO_ALLOC(wf_nc_k_save,(k_save%nibz))
 YAMBO_ALLOC(wf_igk_save,(wf_ncx,k_save%nibz))
 wf_nc_k_save=wf_nc_k
 wf_igk_save =wf_igk
 wf_ncx_save =wf_ncx
 wf_ng_save  =wf_ng
 ng_vec_save =ng_vec
 !
 ! Check which of the wfc components can be rotated
 ! and discard the ones above ng_closed, or above wf_ng_cut if a cutoff is provided in input 
 !
 l_wf_nc_k_red=.false.
 if(wf_ng_cut==0) wf_ng_cut=ng_closed 
 do ik=1,k_save%nibz
   do ic=1,wf_nc_k(ik)
     if( wf_igk(ic,ik)<=wf_ng_cut ) cycle
     l_wf_nc_k_red=.true.
     wf_igk(ic:wf_nc_k(ik),ik)=-1
     wf_nc_k(ik)=ic-1
     exit
   enddo
 enddo
 !
 wf_ncx=maxval(wf_nc_k)
 !
 if(l_wf_nc_k_red) call warning(' cut-off reduced on some WFs')
 !
 ! The old SAVE tables reduced
 !
 YAMBO_ALLOC(wf_nc_k_reduced,(k_save%nibz))
 YAMBO_ALLOC(wf_igk_reduced,(wf_ncx,k_save%nibz))
 wf_nc_k_reduced=wf_nc_k
 wf_igk_reduced =wf_igk(:wf_ncx,:)
 !
 YAMBO_FREE(wf_nc_k)
 YAMBO_FREE(wf_igk)
 !
 YAMBO_ALLOC(wf_nc_k,(k%nibz))
 YAMBO_ALLOC(wf_igk,(wf_ncx,k%nibz))
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
     wf_nc_k(ik) = wf_nc_k_reduced(ik_save)
     wf_igk(1:wf_nc_k(ik),ik) = wf_igk_reduced(1:wf_nc_k_reduced(ik_save),ik_save)
     !
   enddo
   !
   do ik=k_save%nibz+1,k%nibz
     !
     ik_save=kpoints_map(1,ik)
     is=kpoints_map(2,ik)
     !
     wf_nc_k(ik) = wf_nc_k_reduced(ik_save)
     wf_igk(1:wf_nc_k(ik),ik) = g_rot(wf_igk_reduced(1:wf_nc_k_reduced(ik_save),ik_save),is)
     call sort( wf_igk(1:wf_nc_k(ik),ik) )
     !
   enddo
   !
 case(2)
   !
   wf_igk=-1
   do ik=1,k%nibz
     wf_igk(1:wf_ncx,ik) = wf_igk_reduced(1:wf_ncx,kpoints_map(1,ik))
     wf_nc_k(ik)         = wf_nc_k_reduced(kpoints_map(1,ik))
   enddo
   !
   ! Close the G-vectors shell
   !
   if(nsym>old_nsym) then
     !
     call msg('s','Closing shells...')
     !============================
     !
     do is=old_nsym+1,nsym
       call G_rot_grid(is,'extend_grid')
     enddo
     !
   endif
   !
 end select
 !
 wf_ng=maxval(wf_igk)
 !
 ! The new SAVE table
 !
 YAMBO_ALLOC(wf_nc_k_new,(k%nibz))
 YAMBO_ALLOC(wf_igk_new,(wf_ncx,k%nibz))
 wf_nc_k_new=wf_nc_k
 wf_igk_new =wf_igk
 wf_ncx_new =wf_ncx
 wf_ng_new  =wf_ng
 ng_vec_new =ng_vec
 !
 if(wf_ncx_save/=wf_ncx_new) call msg('s',':: wf_ncx was reduced from '//trim(intc(wf_ncx_save))//' to '//trim(intc(wf_ncx_new)))
 if(wf_ng_save /=wf_ng_new ) call msg('s',':: wf_ng changed from '//trim(intc(wf_ng_save))//' to '//trim(intc(wf_ng_new)))
 if(ng_vec_save/=ng_vec_new) call msg('s',':: ng_vec was increased to close the G-shell')
 !
end subroutine
