!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): CA DS
!
#include<dev_defs.h>
!
subroutine write_rotated_WFs(E,k)
 !
 use pars,                ONLY:SP,lchlen,cZERO
 use com,                 ONLY:msg,core_io_path,more_io_path
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_WR_CL,NONE,OP_APP_CL,OP_WR,OP_RD,&
&                              OP_APP,OP_RD_CL,RD_CL,RD,frag_WF
 use wave_func,           ONLY:io_WF,wf_nc_k, wf_igk,DEV_VAR(wf_igk),wf_ncx,wf_nb_io,wf_ng, &
 &                             WF,wf_nb_io_groups
 use electrons,           ONLY:levels,E_reset,n_sp_pol,n_spinor
 use LIVE_t,              ONLY:live_timing
 use R_lattice,           ONLY:bz_samp,g_rot,ng_closed,nkibz
 use D_lattice,           ONLY:spin_sop
 use interfaces,          ONLY:WF_load,WF_free
 use YPP_symm,            ONLY:wf_nc_k_save,wf_igk_save,wf_nc_k_reduced,wf_igk_reduced,&
&                              wf_nc_k_new,wf_igk_new,&
&                              wf_nc_k_new,wf_ncx_new,wf_ncx_save,wf_ng_new,wf_ng_save
 !
#include<y_memory.h>
 !
 type(levels),intent(in)  :: E
 type(bz_samp),intent(inout) :: k
 !
 ! Work space
 !
 type(levels)          :: E_new
 !
 character(lchlen)     :: core_io_path_save
 integer               :: n_steps,nb1,nb2,ng_closed_save
 integer               :: ACTION_,ID
 integer               :: io_err
 !
 integer               :: wf_nb_io_save
 !
 complex(SP), allocatable :: wf_disk(:,:,:)
 complex(SP), allocatable :: wf_tmp(:,:)
 !
 ! Dummies
 !
 integer               :: is,i_sp_pol,ik,ik_save,ifrag
 integer               :: ib,ib_grp,ib_disk,i_wf,i_spinor
 integer               :: ic,ic_rot,ig,ig_rot
 logical               :: l_found
 !
 ! External functions
 !
 integer, external :: io_DB1
 !
 ! First check and fix the number of bands
 !
 call load_SC_components('check_WF_DB',n_bands=bands_to_load(2))
 !
 if (found_SC_DB.and.compatible_SC_DB) then
   !
   !if(allocated(states_to_load).and..not.force_WFo_) then
   !  YAMBO_FREE(states_to_load)
   !  call warning(' [WF'//trim(wf_title)//'] WF-distribution turned off, not compatible with SC wave functions')
   !endif
   !
   if (any ( WF%b/=(/1,SC_bands(2)/) )) then
     call warning(' [WF'//trim(wf_title)//'] Forced '//trim(intc(SC_bands(2)))//' bands to be loaded')
     if(allocated(states_to_load)) states_to_load(WF%b(2)+1:SC_bands(2),:,:)=.TRUE.
   endif
   WF%b=(/1,SC_bands(2)/)
   !
 else if (found_SC_DB.and..not.compatible_SC_DB) then
   !
   call error(' [WF'//trim(wf_title)//'] Too many bands required ('//&
&             trim(intc(bands_to_load(2)))//'). Only '//trim(intc(SC_bands(2)))//' available.')
   !
 endif
 !
 !
 call section('=',"Energies and WFs shells")
 !===========================================
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID)
 io_err=io_WF(ID)
 !
 core_io_path_save=core_io_path
 core_io_path=more_io_path
 !
 ! For some reason ioDB1 changes the value of ng_closed and of wf_nb_io
 ! This is a work-around to this problem
 ng_closed_save=ng_closed
 wf_nb_io_save=wf_nb_io
 !
 call msg('s',' I/O...')
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 io_err=io_DB1(E,k,ID)
 !
 ng_closed=ng_closed_save
 wf_nb_io=wf_nb_io_save
 !
 call section('=',"Wavefunctions")
 !===============================
 !
 ! Rotate in the SC basis
 !========================
 !
 call load_SC_components('WF',COM_=NONE,MODE_=DUMP,kind=WF_kind,xc_functional=WF_xc_functional,&
 &                          perturbation=WF_perturbation)
 !
 call msg('rns','[WF'//trim(wf_title)//' loader] '//trim(global_XC_string(WF_kind,WF_xc_functional,WF_perturbation))//&
 &                             ' wavefunctions loaded')
   !



   !
   YAMBO_ALLOC(wf_disk,(wf_ncx_new,n_spinor,wf_nb_io))
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
   io_err=io_WF(ID)
   !
   ACTION_=OP_APP_CL
   if(frag_WF) ACTION_=OP_WR_CL
   !
   do i_sp_pol=1,n_sp_pol
     !
     call live_timing('Reduction',k_save%nibz)
     !
     do ik=1,k%nibz
       !
       ifrag=ik+(i_sp_pol-1)*k%nibz
       !
       !do ib_grp=1,wf_nb_io_groups
        !nb1=wf_nb_io*(ib_grp-1)+1
        !nb2=wf_nb_io*ib_grp
        nb1=SC_bands(1)
        nb2=SC_bands(2)
        !
         wf_disk=cZERO
         !
         nkibz=k_%nibz
         core_io_path=core_io_path_save
         call WF_load(WF,wf_ng,1,SC_bands,(/ik,ik/),(/i_sp_pol,i_sp_pol/),&
&                     space='C',title='',impose_free_and_alloc=.true.)
         !
         call WF_rotate(ik,i_sp_pol,WF,wf_ng)
         !
         do ib=nb1,nb2
           ib_disk=ib-nb1+1
           i_wf=WF%index(ib,ik_save,i_sp_pol)
           wf_disk(:wf_nc_k(ik),:,ib_disk)= WF%c(:wf_nc_k(ik),:,i_wf)
         enddo
         !
         call WF_free(WF)
         !
         nkibz=k%nibz
         wf_ncx=wf_ncx_new
         core_io_path=more_io_path
         call io_control(ACTION=ACTION_,COM=NONE,SEC=(/ifrag+1,ib_grp/),ID=ID)
         io_err=io_WF(ID,wf_disk)
         !
       enddo
       !
       call live_timing(steps=1)
       !
     enddo
     !
     call live_timing()
   !
     call load_SC_components('WF_clean')
     ! 
 if(frag_WF) then
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID)
   io_err=io_WF(ID)
 endif
 !
 core_io_path=core_io_path_save
 !
 YAMBO_FREE(wf_disk)
 !
end subroutine