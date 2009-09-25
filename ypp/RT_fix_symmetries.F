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
 use pars,                ONLY:SP,lchlen
 use com,                 ONLY:msg,warning,core_io_path,more_io_path
 use memory_m,            ONLY:mem_est
 use electrons,           ONLY:levels,E_reset,n_spin
 use vec_operate,         ONLY:v_norm,v_is_zero
 use matrix_operate,      ONLY:m3det
 use D_lattice,           ONLY:nsym,dl_sop,i_time_rev,a,alat,&
&                              i_space_inv,sop_inv,inv_index,symmetry_group_table
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,&
&                              g_rot,ng_closed,bz_samp_duplicate,rl_sop
 use IO_m,                ONLY:io_control,OP_WR_CL,NONE,OP_APP_WR_CL,OP_WR,&
&                              OP_APP,cp_file,rm_file,cp_directory
 use wave_func,           ONLY:ioWF,wf_nc_k, wf_igk,wf_ncx,wf_nb_io, &
&                              wf_ng,wf_state,wf_load,wf_state,wf,WF_free,wf_ng ,wf_nb_io_groups
 use real_time,           ONLY:kpoints_map
 use stderr,              ONLY:intc,set_real_printed_length
 use parser_m,            ONLY:parser
 use zeros,               ONLY:k_iku_zero
 !
 implicit none
 type(levels)                 :: E,E_expanded
 type(bz_samp)                :: k
 !
 integer, external :: ioDB1
 !
 ! Work Space
 !
 integer               :: ik_save,iv(2),ierr,ib,i_wf,i_spin,&
&                         sop_inv_save(nsym),ID,io_err,i1,i2,is,ik,i_elmt
 integer,allocatable   :: wf_nc_k_save(:),wf_igk_save(:,:)
 real(SP)              :: v1(3),v2(3),m(3,3)
 real(SP), allocatable :: wf_disk(:,:,:,:)
 logical               :: Rm_all_symm,use_E_field,use_B_field,reject_symmetry
 type(bz_samp)         :: k_save
 character(lchlen)     :: core_io_path_save,wf_fragment
 !
 use_E_field=.FALSE.
 use_B_field=.FALSE.
 if (any(E_field/=0.)) use_E_field=.TRUE.
 if (any((/"X","x","Y","y","Z","z"/)==B_field_direction)) use_B_field=.TRUE.
 !
 if (use_E_field) call section('*',"== Electric Fields in real time: BZ sampling remapping ==")
 if (use_B_field) call section('*',"== Magnetic field: BZ sampling remapping ==")
 !
 if (.not.use_B_field.and..not.use_E_field) return
 !
 if (wf_nb_io/=E%nb) then
   call warning('Splitted wavefunctions still not supported')
   return
 endif
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
 if (use_B_field) then
   select case (B_field_direction)
     case ("X","x")
       i_elmt=1
     case ("Y","y")
       i_elmt=2
     case ("Z","z")
       i_elmt=3
   end select
 endif
 !
 if(Rm_all_symm) then
   nsym=1
 else 
   is=2
   do while (is<=nsym)
     !
     if (use_E_field) then
       v1(:) = matmul( dl_sop(:,:,is), E_field(1,:) )      
       v2(:) = matmul( dl_sop(:,:,is), E_field(2,:) )      
       reject_symmetry=v_norm(v1(:)-E_field(1,:))>1.E-5.or.v_norm(v2(:)-E_field(2,:))>1.E-5
     else if (use_B_field) then
       reject_symmetry= abs(dl_sop(i_elmt,i_elmt,is))-1.>1.E-5
       if (.not.reject_symmetry) then
         do i1=1,3
           if (i1/=i_elmt.and.abs(dl_sop(i1,i_elmt,is))>1.E-5 ) reject_symmetry=.TRUE.
           if (i1/=i_elmt.and.abs(dl_sop(i_elmt,i1,is))>1.E-5 ) reject_symmetry=.TRUE.
         enddo
       endif
       if (.not.reject_symmetry) then
         if ( abs ( m3det(dl_sop(:,:,is))/dl_sop(i_elmt,i_elmt,is) - 1.)>1.E-5)  reject_symmetry=.TRUE.
       endif
     endif
     !
     if(reject_symmetry) then
       dl_sop(:,:,is:nsym-1)=dl_sop(:,:,is+1:nsym)
       nsym=nsym-1
     else
       is=is+1
     endif
     !
   enddo
 endif
 !
 call msg('l'," "//trim(intc(nsym)))
 !
 call set_real_printed_length(f_length=7,g_length=7)
 do is=1,nsym
   call msg('s','[S'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))  
 enddo
 call set_real_printed_length()
 !
 ! Remap Inversion symmetry index
 !
 inv_index=-1
 do is=1,nsym
   forall (i1=1:3,i2=1:3) rl_sop(i1,i2,is)=dl_sop(i1,i2,is)*alat(i1)/alat(i2)
   if ( all(nint(reshape(dl_sop(:,:,is),(/9/)))==(/-1.,0.,0.,0.,-1.,0.,0.,0.,-1./)) ) inv_index=is
 enddo
 if (inv_index<0) i_space_inv=0
 !
 ! Check group table
 !
 sop_inv_save=sop_inv
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
 allocate(kpoints_map(2,k%nibz))
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
 call section('=',"Energies")
 !===========================
 !
 call E_reset(E_expanded)
 E_expanded%nb=E%nb
 E_expanded%nk=k%nibz
 allocate(E_expanded%E(E%nb,k%nibz,n_spin))
 !
 do ik=1,k%nibz
   E_expanded%E(:,ik,:)=E%E(:,kpoints_map(1,ik),:)
 enddo
 !
 call section('=',"Wavefunctions")
 !===============================
 !
 call wf_load(wf_ng,1,(/1,E%nb/),(/1,k_save%nibz/),space='C',title='')
 !
 if (trim(core_io_path)/=trim(more_io_path)) then
   !
   call msg('s',':: Copying existing database ...')
   !
   call cp_file(trim(core_io_path)//"/SAVE/ns.wf",trim(more_io_path)//"/SAVE",ierr)
   call cp_file(trim(core_io_path)//"/SAVE/s.wf",trim(more_io_path)//"/SAVE",ierr)
   do ik=1,k%nibz
     do i1=1,wf_nb_io_groups
        write (wf_fragment,'(a,i5.5,a,i5.5)') 'wf_',ik,'_',i1
        call cp_directory(trim(core_io_path)//"/SAVE/"//trim(wf_fragment),trim(more_io_path)//"/SAVE",ierr)
     enddo
   enddo
   !
   call msg('l','done')
   !
 else
   call warning(':: Existing database will be overwritten')
 endif
 !
 ! Rotating wavefunctions components
 !
 call msg('s',':: Rotating wavefunctions')
 !
 allocate(wf_nc_k_save(k_save%nibz),wf_igk_save(wf_ncx,k_save%nibz))
 wf_nc_k_save=wf_nc_k
 wf_igk_save =wf_igk
 !
 deallocate(wf_nc_k,wf_igk)
 allocate(wf_nc_k(k%nibz),wf_igk(wf_ncx,k%nibz))
 !
 wf_igk(1:wf_ncx,1:k_save%nibz)       = wf_igk_save(1:wf_ncx,1:k_save%nibz)
 wf_nc_k(1:k_save%nibz)               = ng_closed
 !
 do i1=k_save%nibz+1,k%nibz 
   !      
   is=sop_inv_save(kpoints_map(2,i1))
   ik=kpoints_map(1,i1)
   !
   wf_igk (1:wf_nc_k(ik),i1) = wf_igk_save(g_rot(is,1:wf_nc_k(ik)),ik)
   wf_nc_k(i1)               = wf_nc_k(ik)
   !
 enddo
 !
 allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
 !
 call section('=',"Final I/O")
 !============================
 !
 core_io_path_save=core_io_path
 core_io_path=more_io_path
 !
 call msg('s',':: Wavefunctions ...')
 !
 do i1=k_save%nibz+1,k%nibz 
   !      
   ik=kpoints_map(1,i1)
   call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/i1+1,1/),ID=ID)
   !
   do ib=1,E%nb
     do i_spin=1,n_spin
       i_wf=wf_state(ib,ik,i_spin)
       wf_disk(1,ib,:,i_spin)= real(wf(:,i_wf))
       wf_disk(2,ib,:,i_spin)=aimag(wf(:,i_wf))
     enddo
   enddo
   !
   io_err=ioWF(ID,wf=wf_disk)
   !
 enddo
 !
 call msg('l','done')
 !
 call msg('s',':: DB1 ...')
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=ioDB1(E_expanded,k,ID)
 !
 core_io_path=core_io_path_save
 !
 call msg('l','done')
 !
 ! (n)db clean
 !=============
 !
 if (trim(core_io_path)==trim(more_io_path)) then
   call msg('s',':: Cleaning ...')
   call rm_file((trim(core_io_path))//"/SAVE/ndb.kindx")
   call rm_file((trim(core_io_path))//"/SAVE/ndb.gops")
   call rm_file((trim(core_io_path))//"/SAVE/db.kindx")
   call rm_file((trim(core_io_path))//"/SAVE/db.gops")
   call rm_file((trim(core_io_path))//"/SAVE/ns.kb_pp")
   call rm_file((trim(core_io_path))//"/SAVE/s.kb_pp")
   call msg('l','done')
 endif
 !
 ! CLEAN
 !=======
 deallocate(wf_nc_k_save,wf_igk_save,wf_disk,kpoints_map)
 call bz_samp_reset(k_save)
 call E_reset(E_expanded)
 call WF_free()
 call mem_est("wf_disk")
 !
end subroutine
