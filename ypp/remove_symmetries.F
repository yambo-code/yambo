!
! Copyright (C) 2000-2008 C. Attaccalite and the YAMBO team 
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
subroutine remove_symmetries(en,k)
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
 implicit none
 type(levels)                 :: en,new_en
 type(bz_samp)                :: k
 !
 integer, external :: ioDB1
 !
 ! Work Space
 !
 integer               :: ID,io_err,ioWF_err
 real(SP), allocatable :: new_dl_sop(:,:,:),old_kpt(:,:)
 integer               :: new_nsym,new_space_inv
 real(SP)              :: v1(3),v2(3),m(3,3)
 integer, allocatable  :: old_sstar(:,:),old_sop_inv(:),map_kpoints(:,:)
 integer,allocatable   :: old_wf_nc_k(:),old_wf_igk(:,:)
 logical               :: Rm_all_symm,found    
 integer               :: i1,i2,i3,is,isym,itmp(2),ik
 integer               :: old_nibz,ikibz,ib_grp
 integer               :: ng_closed_save,wf_nb_io_save
 real(SP), allocatable :: wf_disk(:,:,:,:),old_ptbz(:,:),tmp_wf(:,:,:,:,:,:)
 character(lchlen)     :: core_io_path_save
 !
 call parser('RmAllSymm',Rm_all_symm)
 !
 call k_ibz2bz(k,'i',.true.)
 call k_reduce(k)
 !
 ng_closed=wf_ncx
 call G_shells_finder()
 !
 allocate(old_sop_inv(nsym),old_sstar(k%nbz,2),old_ptbz(k%nbz,3),old_kpt(k%nibz,3))
 allocate(old_wf_nc_k(k%nibz),old_wf_igk(wf_ncx,k%nibz))
 !
 old_sop_inv= sop_inv
 old_sstar  = k%sstar
 old_wf_nc_k= wf_nc_k
 old_wf_igk = wf_igk
 old_nibz   = k%nibz
 old_ptbz   = k%ptbz
 old_kpt    = k%pt
 !
 ! Remove Symmetries
 !
 call msg('s','== Initial symmetries number : '//intc(nsym))
 !
 allocate(new_dl_sop(3,3,nsym))
 !
 new_nsym=0
 new_space_inv=0
 new_dl_sop=0.
 !
 if(Rm_all_symm) then
   new_nsym=1
   new_dl_sop(1,1,1)=1.
   new_dl_sop(2,2,1)=1.
   new_dl_sop(3,3,1)=1.
 else
   do is=1,nsym/(1+i_time_rev)
     v1(:) = matmul( dl_sop(:,:,is), efield(1,:) )      
     v2(:) = matmul( dl_sop(:,:,is), efield(2,:) )      
     if((v_norm(v1(:)-efield(1,:)))<1.E-5.and.(v_norm(v2(:)-efield(2,:)))<1.E-5) then
       new_nsym=new_nsym+1
       new_dl_sop(:,:,new_nsym)=dl_sop(:,:,is)
       if(is==i_space_inv) new_space_inv=new_nsym
     endif
   enddo
 endif
 !
 nsym=new_nsym
 call msg('s','== New symmetries number : '//intc(nsym))
 !
 deallocate(dl_sop,rl_sop)
 allocate(dl_sop(3,3,nsym),rl_sop(3,3,nsym))
 !
 dl_sop(:,:,1:nsym)=new_dl_sop(:,:,1:nsym)
 !
 deallocate(new_dl_sop)
 !
 i_time_rev =0
 i_space_inv=new_space_inv
 !
 do is=1,nsym
   forall (i2=1:3,i3=1:3) rl_sop(i2,i3,is)=dl_sop(i2,i3,is)*alat(i2)/alat(i3)
   if (i_time_rev==1.and.is>nsym/2) cycle
   call msg('r','[S'//trim(intc(is))//']',reshape(dl_sop(:,:,is),(/9/)))  
 enddo
 !
 deallocate(sop_tab,sop_inv)
 allocate(sop_tab(nsym,nsym),sop_inv(nsym))
 !
 sop_tab=0
 sop_inv=0
 !
 do i1=1,nsym
   do i2=1,nsym
     m=matmul(dl_sop(:,:,i1),dl_sop(:,:,i2))
     do i3=1,nsym
       if (all(abs(m-dl_sop(:,:,i3))<=1.E-5)) then
         if (sop_tab(i1,i2)/=0) call error('Error (stab 1): check the input symmetries!')        
         sop_tab(i1,i2)=i3
         if (sop_tab(i1,i2)==1) sop_inv(i1)=i2                
       endif
     enddo
     if (sop_tab(i1,i2)==0) call error('[SYMs] check the input symmetries!')    
   enddo
 enddo
 if (any(sop_inv==0)) call error('[SYMs] check the input symmetries!')
 call msg('r','[SYMs] Group table built correctly')
 !
 deallocate(k%pt)
 call k_reduce(k)
 !
 deallocate(k%sstar,k%nstar,k%weights)
 call k_expand(k)
 !
 call msg('s','== New nibz : '//intc(k%nibz))
 !
 allocate(map_kpoints(2,k%nibz))
 !
 call E_reset(new_en)
 !
 new_en%nb=en%nb
 !
 deallocate(wf_nc_k,wf_igk)
 allocate(wf_nc_k(k%nibz),wf_igk(wf_ncx,k%nibz))
 allocate(new_en%E(en%nb,k%nibz,n_spin))
 !
 ! Map k-point using full bz
 !
 do i1=1,k%nibz 
   i2=1
   found=.false.
   do while(i2 <=k%nbz .and. .not.found) 
     if(v_norm(k%pt(i1,:)-old_ptbz(i2,:))<1.E-5) then
       map_kpoints(1,i1)=old_sstar(i2,1)
       map_kpoints(2,i1)=old_sstar(i2,2)
       call msg('s','== Point '//intc(i1)// " from :"//intc(map_kpoints(1,i1)) &
    &  //" with sym "//intc(map_kpoints(2,i1))//" inv_sym "//intc(old_sop_inv(map_kpoints(2,i1)))) 
       found=.true.
     endif
     i2=i2+1
   enddo
   if(.not.found) call error(" k-point not found ")
 enddo
 !
 ! Re-order k-points in such a way that the first old_nikz
 ! are the same of the old set
 !
 do i1=1,old_nibz
   found=.false.
   if(v_norm(old_kpt(i1,:)-k%pt(i1,:))>1E-5) then
     !      
     i2=i1+1
     !
     do while(i2<=k%nibz.and..not.found)
       if(v_norm(old_kpt(i1,:)-k%pt(i2,:))<1E-5) then
         v1(:)     =k%pt(i2,:)
         k%pt(i2,:)=k%pt(i1,:)
         k%pt(i1,:)=v1(:)
         itmp(:)   =map_kpoints(:,i2)
         map_kpoints(:,i2)=map_kpoints(:,i1)
         map_kpoints(:,i1)=itmp(:)
         found=.true.
       endif      
       i2=i2+1
     enddo
     !
     if(.not.found)  call error('Error : k-point not found!')       
     !
   endif
 enddo
 !
 wf_igk(1:wf_ncx,1:old_nibz)       = old_wf_igk(1:wf_ncx,1:old_nibz)
 wf_nc_k(1:old_nibz)               = old_wf_nc_k(1:old_nibz)
 new_en%E(:,1:old_nibz,1:n_spin)   = en%E(:,1:old_nibz,1:n_spin)
 !
 do i1=old_nibz+1,k%nibz 
   !      
   isym=old_sop_inv(map_kpoints(2,i1))
   !
   wf_igk (1:ng_closed,i1)   = old_wf_igk(g_rot(isym,1:ng_closed),map_kpoints(1,i1))
   wf_nc_k(i1)               = ng_closed
   new_en%E(:,i1,1:n_spin)   = en%E(:,map_kpoints(1,i1),1:n_spin)
   !
   call msg('s','== For k '//intc(i1)// " number of G set to :"//intc(ng_closed))
   !
 enddo
 !
 nXkibz = k%nibz
 !
! core_io_path_save=core_io_path
! core_io_path     =NEW_WF_path
 !
 k%pt(:,3)=0
 !
 call msg('s','== Writing new DB1 ...')
 !
 ng_closed_save =ng_closed  ! ng_closed is overwritten from ioDB1 !!
 wf_nb_io_save  =wf_nb_io
 !
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=ioDB1(new_en,k,ID)
 !
 ng_closed=ng_closed_save
 wf_nb_io =wf_nb_io_save
 !
 call msg('s','== Reading/Writing WF ...')
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=NONE,MODE=DUMP,ID=ID)
 ioWF_err=ioWF(ID) 
 !
 allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
 !
 allocate(tmp_wf(2,wf_nb_io,wf_ncx,n_spin,wf_nb_io_groups,nkibz-old_nibz))
 call mem_est("tmp_wf",(/size(tmp_wf)/),(/SP/))
 !
 call live_timing(' Reading old WF :',old_nibz) 
 !
 call io_disconnect(ID)
 !
 tmp_wf=0.
 !
 nkibz  = old_nibz  ! in order to close the file !!!
 !
 do ikibz=1,old_nibz
   !
   do ib_grp=1,wf_nb_io_groups
     !
     ! Read an old k-point
     !
     if (ikibz==1.and.ib_grp==1) call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1,2,1/),ID=ID)
     if (ikibz >1 .or. ib_grp>1) call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ikibz+1,ib_grp/),ID=ID)
     ioWF_err=ioWF(ID,wf_disk)
     !
     do i1=old_nibz+1,k%nibz
       !      
       if(map_kpoints(1,i1)==ikibz) then
         !      
         isym=old_sop_inv(map_kpoints(2,i1))
         !
         tmp_wf(1:2,1:wf_nb_io,1:ng_closed,1:n_spin,ib_grp,i1-old_nibz) &
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
 do ik=old_nibz+1,nkibz
   !      
   do ib_grp=1,wf_nb_io_groups
   !
   call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/ik+1,ib_grp/),ID=ID)
   io_err=ioWF(ID,wf=tmp_wf(:,:,:,:,ib_grp,ik-old_nibz))
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
