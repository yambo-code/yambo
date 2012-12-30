!
! Copyright (C) 2000-2012 D. Sangalli, C. Attaccalite and the YAMBO team 
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
subroutine fix_kpts_and_gvec(k,k_save,kpoints_map,wf_ncx_save,action_kind)
 !
 use pars,                ONLY:IP,SP
 use com,                 ONLY:msg
 use memory_m,            ONLY:mem_est
 use zeros,               ONLY:k_iku_zero
 use stderr,              ONLY:intc
 use vec_operate,         ONLY:v_norm,v_is_zero,i_sort
 use electrons,           ONLY:levels
 use wave_func,           ONLY:wf_nc_k, wf_igk,wf_ncx,wf_ng
 use R_lattice,           ONLY:bz_samp,nkibz,g_rot,ng_closed,ng_vec
 !
 implicit none
 !
 type(bz_samp)             :: k
 type(bz_samp),intent(in)  :: k_save
 integer,intent(out)       :: kpoints_map(2,k%nibz)
 integer,intent(out)       :: wf_ncx_save
 integer,intent(in)        :: action_kind
 !
 ! Work space
 !
 integer,allocatable   :: wf_nc_k_save(:),wf_igk_save(:,:)
 ! 
 ! Dummies
 !
 integer               :: i1,ik,ik_save,ic,is
 integer               :: iv(2)
 real(SP)              :: v1(3),v2(3)
 ! 
 call section('=',"K-points")
 !===========================
 !
 call msg('s',trim(intc(k%nibz))//' k-points are now expanded/reduced to ...')
 deallocate(k%pt)
 call k_reduce(k)
 call msg('l'," "//trim(intc(k%nibz)))
 !
 allocate(wf_nc_k_save(k_save%nibz),wf_igk_save(wf_ncx,k_save%nibz))
 call mem_est("wf_nc_k_save wf_igk_save",(/size(wf_nc_k_save),size(wf_igk_save)/),(/IP,IP/))
 wf_nc_k_save=wf_nc_k
 wf_igk_save =wf_igk
 wf_ncx_save =wf_ncx
 !
 deallocate(wf_nc_k,wf_igk)
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
 allocate(wf_nc_k(k%nibz),wf_igk(wf_ncx,k%nibz))
 !
 ! Map k-point using the save BZ
 !
 ! kpoints_map(1, ik ) = ik_save
 ! kpoints_map(2, ik ) = is
 !
 ! with ik_save,is relative to the original set of k-points and symmetries
 !
 kpoints_map=0
 do ik=1,k%nibz 
   do ik_save=1,k%nbz
     if (v_is_zero(k%pt(ik,:)-k_save%ptbz(ik_save,:),zero_=k_iku_zero)) then
       if(action_kind==1) kpoints_map(:, ik ) = k_save%sstar(ik_save,:)
       if(action_kind==2) kpoints_map(1, ik ) = ik_save
       exit 
     endif
   enddo
 enddo
 !
 select case(action_kind)
 case(1)
   !
   ! swap ik and i1 to sort
   ! first all the points in the ibz
   ! 
   main_loop: do ik=1,k%nibz 
     if ( kpoints_map(2, ik )/=1 ) then
       do i1=ik+1,k%nibz
         if ( kpoints_map(2, i1 )==1 ) then
           iv=kpoints_map(:, ik )
           v1=k%pt(ik,:)
           kpoints_map(:,ik)=kpoints_map(:,i1)
           kpoints_map(:,i1)=iv
           k%pt(ik,:)=k%pt(i1,:)                 
           k%pt(i1,:)=v1
           cycle main_loop
         endif
       enddo
     endif
   enddo main_loop
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
 end select
 !
 wf_ng=maxval(wf_igk)
 !
 ! CLEAN
 !=======
 deallocate(wf_igk_save,wf_nc_k_save)
 call mem_est("wf_nc_k_save wf_igk_save")
 !
end subroutine
