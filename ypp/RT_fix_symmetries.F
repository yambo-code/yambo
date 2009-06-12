!
! Copyright (C) 2000-2008 C. Attaccalite, A. Marini and the YAMBO team 
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
subroutine RT_fix_symmetries(E,k)
 !
 use YPP
 use pars,                ONLY :SP,lchlen
 use com,                 ONLY :msg,error,warning,core_io_path 
 use memory_m,            ONLY :mem_est
 use electrons,           ONLY :levels,E_reset,n_spin
 use vec_operate,         ONLY :v_norm
 use D_lattice,           ONLY :nsym,dl_sop,i_time_rev,a,atoms_spatial_invertion&
&                              ,i_space_inv,alat,sop_inv,sop_tab
 use R_lattice,           only :bz_samp,bz_samp_reset,nkibz,rl_sop,nkibz,nXkibz,g_vec&
&                              ,ng_vec,g_rot,ng_closed
 use IO_m,                only :io_control,OP_WR_CL,NONE,OP_APP_WR_CL,OP_RD_CL,OP_WR &
&                              ,DUMP,serial_number,RD_CL_IF_END,RD_CL,OP_RD,OP_APP,REP  &
&                              ,VERIFY,io_disconnect
 use wave_func,           ONLY :ioWF,wf_nc_k, wf_igk,wf_ncx,wf_nb_io,wf_nb_io_groups &
&                              ,wf_ng
 use stderr,              ONLY :intc
 use timing,              ONLY :live_timing
 use parser_m,            ONLY :parser
 !
 use wave_func,           ONLY:wf_load,wf_state,wf,WF_free,wf_ng
 use IO_m,                ONLY:mk_dir
 use zeros,               ONLY:k_iku_zero
 use vec_operate,         ONLY:v_is_zero
 use stderr,              ONLY:set_real_printed_length
 use R_lattice,           ONLY:bz_samp_duplicate
 use D_lattice,           ONLY:inv_index,symmetry_group_table
 !
 implicit none
 type(levels)                 :: E,E_expanded
 type(bz_samp)                :: k
 !
 integer, external :: ioDB1
 !
 ! Work Space
 !
 integer               :: ID,io_err,ioWF_err
 integer               :: new_space_inv
 real(SP)              :: v1(3),v2(3),m(3,3)
 integer,allocatable   :: old_wf_nc_k(:),old_wf_igk(:,:)
 logical               :: Rm_all_symm,found    
 integer               :: i1,i2,i3,is,isym,itmp(2),ik
 integer               :: ikibz,ib_grp
 integer               :: ng_closed_save,wf_nb_io_save
 real(SP), allocatable :: wf_disk(:,:,:,:),tmp_wf(:,:,:,:,:,:)
 character(lchlen)     :: core_io_path_save
 !
 type(bz_samp)         :: k_save
 integer               :: kpoints_map(2,k%nibz),ik_save,iv(2)
 real(SP)              :: v(3)
 !
 call section('*',"== Real Time: BZ sampling remapping ==")
 !
 call section('+',"Symmetries")
 !=============================
 !
 call parser('RmAllSymm',Rm_all_symm)
 !
 ! Seek for all possible shells
 !
 if (wf_ncx>ng_closed) then
   ng_closed=wf_ncx
   call G_shells_finder()
 endif
 !
 ! Expansion of default k-points
 !
 call k_ibz2bz(k,'i',.true.)
 call k_reduce(k)
 !
 call bz_samp_reset(k_save)
 call bz_samp_duplicate(k,k_save)
 !
 ! Remove Symmetries
 !
 nsym=nsym/(1+i_time_rev)
 i_time_rev=0 ! No time reversal
 call msg('s',trim(intc(nsym))//' spatial symmetries reduced to ...')
 !
 if(Rm_all_symm) then
   nsym=1
 else
   is=2
   do while (is<=nsym)
     v1(:) = matmul( dl_sop(:,:,is), efield(1,:) )      
     v2(:) = matmul( dl_sop(:,:,is), efield(2,:) )      
     if((v_norm(v1(:)-efield(1,:)))>1.E-5.or.(v_norm(v2(:)-efield(2,:)))>1.E-5) then
       dl_sop(:,:,is:nsym-1)=dl_sop(:,:,is+1:nsym)
       nsym=nsym-1
     else
       is=is+1
     endif
   enddo
 endif
 !
 call msg('l'," "//trim(intc(nsym)))
 !
 call set_real_printed_length(f_length=6,g_length=6)
 do is=1,nsym
   call msg('s','[S'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))  
 enddo
 call set_real_printed_length()
 !
 ! Remap Inversion symmetry index
 !
 inv_index=-1
 do is=1,nsym
   if ( all(nint(reshape(dl_sop(:,:,is),(/9/)))==(/-1.,0.,0.,0.,-1.,0.,0.,0.,-1./)) ) inv_index=is
 enddo
 if (inv_index<0) i_space_inv=0
 !
 ! Check group table
 !
 call symmetry_group_table('s')
 !
 call section('=',"K-points")
 !===========================
 !
 call msg('s',trim(intc(k%nibz))//' k-points are now expanded to ...')
 deallocate(k%pt)
 call k_reduce(k)
 call msg('l',trim(intc(k%nibz)))
 !
 ! Map k-point using the save BZ 
 !
 ! k_points_map(1, ? ) = ik
 ! k_points_map(2, ? ) = is
 !
 ! with ik,is relative to the unreduced set of k-points and symmetries
 !
 kpoints_map=0
 do ik=1,k%nibz 
   do ik_save=1,k%nbz
     if (v_is_zero(k%pt(ik,:)-k_save%ptbz(ik_save,:),zero_=k_iku_zero)) then
       kpoints_map(:, ik ) = k_save%sstar(ik_save,:)
       exit 
     endif
   enddo
 enddo
 main_loop: do ik=1,k%nibz 
   if ( kpoints_map(2, ik )/=1 ) then
     do i1=ik+1,k%nibz
       if ( kpoints_map(2, i1 )==1 ) then
         iv=kpoints_map(:, ik )
         v =k%pt(ik,:)
         kpoints_map(:,ik)=kpoints_map(:,i1)
         kpoints_map(:,i1)=iv
         k%pt(ik,:)=k%pt(i1,:)
         k%pt(i1,:)=v
         cycle main_loop
       endif
     enddo
   endif
 enddo main_loop
 !
 !DEBUG >
 !do ik=1,k%nibz 
 !  write (*,*) ik
 !  write (*,*) k%pt(ik,:)
 !  if (ik<=k_save%nibz) write (*,*) k_save%pt(ik,:)
 !  write (*,*) kpoints_map(:,ik)
 !enddo
 !DEBUG >
 !
 call section('=',"Energies")
 !===========================
 !
 call E_reset(E_expanded)
 E_expanded%nb=E%nb
 E_expanded%nk=k%nibz
 allocate(E_expanded%E(E%nb,k%nibz,n_spin))
 !
 deallocate(wf_nc_k,wf_igk)
 allocate(wf_nc_k(k%nibz),wf_igk(wf_ncx,k%nibz))
 !
 do ik=1,k%nibz
   E_expanded%E(:,ik,:)=E%E(:,kpoints_map(1,ik),:)
 enddo
 !
 call section('=',"Rebuilding DB1")
 !=================================
 !
 ng_closed_save =ng_closed  ! ng_closed is overwritten from ioDB1 !!
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=ioDB1(E_expanded,k,ID)
 !
 ng_closed=ng_closed_save
 !
 call section('=',"Wavefunctions")
 !===============================
 !
 call wf_load(wf_ng,1,E%nb,(/1,k_save%nibz/),space='G',title='-RT')
 write (*,*) allocated(wf)

 stop
 !
 allocate(old_wf_nc_k(k%nibz),old_wf_igk(wf_ncx,k%nibz))
 !
 old_wf_nc_k= wf_nc_k
 old_wf_igk = wf_igk
 wf_igk(1:wf_ncx,1:k_save%nibz)       = old_wf_igk(1:wf_ncx,1:k_save%nibz)
 wf_nc_k(1:k_save%nibz)               = old_wf_nc_k(1:k_save%nibz)
 !
 do i1=k_save%nibz+1,k%nibz 
   !      
   isym=sop_inv(kpoints_map(2,i1))
   !
   wf_igk (1:ng_closed,i1)   = old_wf_igk(g_rot(isym,1:ng_closed),kpoints_map(1,i1))
   wf_nc_k(i1)               = ng_closed
   !
   call msg('s','== For k '//intc(i1)// " number of G set to :"//intc(ng_closed))
   !
 enddo
 !
 nXkibz = k%nibz
 !
 !
 call msg('s','== Reading/Writing WF ...')
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=NONE,MODE=DUMP,ID=ID)
 ioWF_err=ioWF(ID) 
 !
 allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
 !
 allocate(tmp_wf(2,wf_nb_io,wf_ncx,n_spin,wf_nb_io_groups,nkibz-k_save%nibz))
 call mem_est("tmp_wf",(/size(tmp_wf)/),(/SP/))
 !
 call live_timing(' Reading old WF :',k_save%nibz) 
 !
 call io_disconnect(ID)
 !
 tmp_wf=0.
 !
 nkibz  = k_save%nibz  ! in order to close the file !!!
 !
 do ikibz=1,k_save%nibz
   !
   do ib_grp=1,wf_nb_io_groups
     !
     ! Read an old k-point
     !
     if (ikibz==1.and.ib_grp==1) call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1,2,1/),ID=ID)
     if (ikibz >1 .or. ib_grp>1) call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ikibz+1,ib_grp/),ID=ID)
     ioWF_err=ioWF(ID,wf_disk)
     !
     do i1=k_save%nibz+1,k%nibz
       !      
       if(kpoints_map(1,i1)==ikibz) then
         !      
         isym=sop_inv(kpoints_map(2,i1))
         !
         tmp_wf(1:2,1:wf_nb_io,1:ng_closed,1:n_spin,ib_grp,i1-k_save%nibz) &
  &    =wf_disk(1:2,1:wf_nb_io,1:ng_closed,1:n_spin)
         !
       endif
       !
     enddo
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 deallocate(wf_disk)
 call mem_est("wf_disk")
 !
 call live_timing()
 !
 call live_timing(' Writing new WF :',nkibz) 
 !
 nkibz  = k%nibz  ! in order to close the file 
 !
 ! Append the new k-points
 !
 do ik=k_save%nibz+1,nkibz
   !      
   do ib_grp=1,wf_nb_io_groups
   !
   call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID)
   io_err=ioWF(ID,wf=tmp_wf(:,:,:,:,ib_grp,ik-k_save%nibz))
   !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 deallocate(tmp_wf)
 call mem_est("tmp_wf")
 !
end subroutine
