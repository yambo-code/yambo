!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine WF_load(iG_in,iGo_max_in,bands_to_load,kpts_to_load,&
&                   spins_to_load,space,title,impose_free_and_alloc)
 !
 ! Load and (eventually) FFTs the wavefunctions
 !
 use pars,          ONLY:SP,DP,schlen
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use electrons,     ONLY:n_bands,nel,n_spin,n_spinor,n_sp_pol
 use R_lattice,     ONLY:nkibz
 use FFT_m,         ONLY:fft_dim_loaded,fft_dim,fft_norm,&
&                        fftw_plan,fft_g_table,fft_size
 use wave_func,     ONLY:wf_nc_k,wf_igk,WF_alloc,wf_b,wf_k,wf_space,wf,wf_s,&
&                        wf_state,wf_ng,wf_norm_test,wf_ncx,ioWF,WF_free,&
&                        wf_n_states,wf_nb_io,wf_nb_io_groups,QUIET_alloc,&
&                        QUIET_free
#if defined _DISTRIBUTED
 use wave_func,     ONLY:states_to_load,states_loaded
 use WF_distribute, ONLY:Distributed_Memory
#endif
 use timing,        ONLY:live_timing
 use IO_m,          ONLY:io_control,OP_RD,NONE,VERIFY,RD_CL_IF_END,&
&                        io_fragmented, io_netcdf_support, RD_CL,DUMP
#if defined _SC
 use com,           ONLY:warning,error
 use SC,            ONLY:compatible_SC_DB,load_SC_components,SC_bands,found_SC_DB
 use global_XC,     ONLY:WF_kind,WF_xc_functional,&
&                        WF_perturbation,global_xc_string,SE_NONE
 use xc_functionals,ONLY:GS_xc_KIND,GS_xc_FUNCTIONAL
#endif
 implicit none
 !
 integer               :: iG_in,iGo_max_in,bands_to_load(2),kpts_to_load(2)
 integer     ,optional :: spins_to_load(2)
 character(*),optional :: space,title
 logical     ,optional :: impose_free_and_alloc
 !
 ! Work Space
 !
 character(2)     ::wf_space_here
 character(schlen)::wf_title
 integer          ::i1,ikibz,ib,i2,ic,is,ig,igfft,wf_size,ib_grp,&
&                   s_2_load(2),iG_max,iG_bounds_loaded(2),iGo_max,ACTION_
 real(SP)         ::mndp,mxdp
 complex(SP)      ::c
 logical          ::loaded_bounds_ok,use_direct_access,use_live_timing,free_and_alloc
 real(SP),    allocatable :: wf_disk(:,:,:,:)
 complex(DP), allocatable :: wf_DP(:)
 !
 ! I/O
 !
 integer ::io_err,ID
 !
 ! Close iG/iGo_max to the nearest shell
 !
 iG_max=iG_in
 if (iG_max==0) iG_max=wf_ng
 !
 iGo_max=iGo_max_in
 !
 call Gclose(iG_max,'tRL')
 call Gclose(iGo_max,'tRL')
 !
 wf_space_here='R'
 if (present(space)) wf_space_here=space
 !
 wf_title=""
 s_2_load=(/1,n_spin/)
 if (present(title)) wf_title=title
 if (present(spins_to_load)) s_2_load=spins_to_load
 !
 ! [1]: check that loaded bounds are larger(equal) then bounds 
 !      asked now
 !
 loaded_bounds_ok=all((/bands_to_load(1)>=wf_b(1),bands_to_load(2)<=wf_b(2),&
&                       kpts_to_load(1)>=wf_k(1),kpts_to_load(2)<=wf_k(2),&
&                       s_2_load(1)>=wf_s(1),s_2_load(2)<=wf_s(2),&
&                       wf_space==wf_space_here/))
 !
#if defined _DISTRIBUTED
 !
 if(loaded_bounds_ok.and.allocated(states_to_load)) then
   !
   if(allocated(states_loaded)) then
     !
     ikibz=wf_k(1)
     do while(ikibz<=wf_k(2).and.loaded_bounds_ok) 
       is   =wf_s(1)
       do while(is<=wf_s(2).and.loaded_bounds_ok) 
          ib   =wf_b(1)
          do while(ib<=wf_b(2).and.loaded_bounds_ok) 
            loaded_bounds_ok=states_to_load(ib,ikibz,is).and..not.states_loaded(ib,ikibz,is)
            ib=ib+1
          enddo
          is=is+1
       enddo
       ikibz=ikibz+1
     enddo
     !
   endif
   !
 endif
 !
#endif
 !
 ! [2]: Check FFT size
 !
 if (loaded_bounds_ok) then
   !
   if (wf_space=="G".or.wf_space=="C") return
   !
   call fft_setup(iG_max,iGo_max,.true.)
   !
   if (all(fft_dim<=fft_dim_loaded)) then
     iG_bounds_loaded=shape(fft_g_table)
     if (iG_bounds_loaded(1)>=iG_max.and.&
&        iG_bounds_loaded(2)>=iGo_max) then
       !
       ! Reset dimensions to fft dim loaded 
       !
       fft_dim=fft_dim_loaded
       fft_size=product(fft_dim)
       return
     endif
   endif
   !
 endif
 !
 ! Manage direct access I/O
 ! 
 use_direct_access=.FALSE.
#if defined _MAGNETIC || _BOLTZMANN
 use_direct_access=(kpts_to_load(1)==kpts_to_load(2))
#endif
#if defined _MANYK
 use_direct_access=.TRUE.
#endif
 use_live_timing =.not.use_direct_access
 !
 ! In case of k by k I/O prevent multiple free/alloc
 !
 free_and_alloc=.TRUE.
 QUIET_alloc   =.FALSE.
 QUIET_free    =.FALSE.
 if (kpts_to_load(1)==kpts_to_load(2)) then
   free_and_alloc =kpts_to_load(1)==1.or.nkibz==1
   QUIET_alloc    =kpts_to_load(1)>1
   QUIET_free     =kpts_to_load(2)<nkibz
   if (present(impose_free_and_alloc)) then
     free_and_alloc=impose_free_and_alloc
     if (free_and_alloc) QUIET_alloc=.FALSE.
   endif
   if (QUIET_alloc) call IO_and_Messaging_switch("-report -log")
 endif
 !
 ! First WF_free then fft_setup.
 !
 if (free_and_alloc) call WF_free()
 !
 wf_b=bands_to_load
 !
#if defined _SC
 !
 ! First check and fix the number of bands 
 !
 call load_SC_components('check_WF_DB',n_bands=bands_to_load(2))
 !
 if (found_SC_DB.and.compatible_SC_DB) then
   !
   if (any ( wf_b/=(/1,SC_bands/) )) call warning(' [WF] Forced '//trim(intc(SC_bands))//' bands to be loaded')
   wf_b=(/1,SC_bands/)
   !
 else if (found_SC_DB.and..not.compatible_SC_DB) then
   !
   call error(' [WF] Too many bands required ('//&
&             trim(intc(bands_to_load(2)))//'). Only '//trim(intc(SC_bands))//' available.')
   !
 endif
 !
#endif
 !
 wf_k=kpts_to_load
 wf_s=s_2_load
 wf_space=wf_space_here
 !
 if (wf_space=='R') call fft_setup(iG_max,iGo_max,.false.)
 if (wf_space=="R") fft_dim_loaded=fft_dim
 !
 if (free_and_alloc) call WF_alloc()
 !
 if (wf_space=='R') allocate(wf_DP(fft_size))
 if (wf_space=='G') allocate(wf_DP(wf_ng))
 if (wf_space=='C') allocate(wf_DP(wf_ncx))
 !
 if (wf_space=='R') call msg('rns','[FFT'//trim(wf_title)//'] Mesh size:',fft_dim)
 !
 if (use_live_timing) call live_timing('[WF'//trim(wf_title)//' loader] Wfs (re)loading',wf_n_states,SERIAL=.true.)
 !
 allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
 !
 call io_control(ACTION=OP_RD,COM=NONE,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=ioWF(ID,wf_disk)
 !
 ! GPL_INCLUDE_START SPIN_IGNORE
 !is =1
 ! GPL_INCLUDE_END SPIN_IGNORE
 !
 wf_n_states=0
 do ikibz=1,nkibz
   !
   do ib_grp=1,wf_nb_io_groups
     !
     ! Use fragmentation ot NETCDF support to reduce the I/O
     ! to the k-pts and bands needed only
     !
     if (io_fragmented(ID).or.io_netcdf_support(ID)) then
       if (any((/ikibz<wf_k(1),ikibz>wf_k(2)/))) cycle
       if (wf_nb_io*(ib_grp-1)+1>wf_b(2)) cycle
     endif
     !
     ACTION_=RD_CL_IF_END
     if (use_direct_access) ACTION_=RD_CL
     call io_control(ACTION=ACTION_,COM=NONE,SEC=(/ikibz+1,ib_grp/),ID=ID)
     !
     io_err=ioWF(ID,wf_disk)
     if (any((/ikibz<wf_k(1),ikibz>wf_k(2)/))) cycle
     if (wf_nb_io*(ib_grp-1)+1>wf_b(2)) cycle
     !
     ! GPL_EXCLUDE_START SPIN_IGNORE
     !
     do is=1,n_spin
       !
       ! GPL_EXCLUDE_END SPIN_IGNORE
       !
       do ib=wf_nb_io*(ib_grp-1)+1,wf_nb_io*ib_grp
         !
         i2=ib-wf_nb_io*(ib_grp-1)
         !
         if (any((/ib<wf_b(1),ib>wf_b(2),is<wf_s(1),is>wf_s(2)/))) cycle 
         !
#if defined _DISTRIBUTED
         if(allocated(states_to_load).and.Distributed_Memory) then
           if(.not.states_to_load(ib,ikibz,is)) cycle
         endif
#endif
         !
         wf_DP=(0._DP,0._DP)
         wf_n_states=wf_n_states+1
         wf_state(ib,ikibz,is)=wf_n_states
         do ic=1,wf_nc_k(ikibz)
           ig=wf_igk(ic,ikibz)
           if (ig>wf_ng) cycle
           igfft=ig
           if (wf_space=='R') igfft=fft_g_table(ig,1)
           if (wf_space=='C') igfft=ic
           wf_DP(igfft)=cmplx(wf_disk(1,i2,ic,is),wf_disk(2,i2,ic,is),DP)
         enddo
         if (wf_space=='G'.or.wf_space=='C') then
           wf(:,wf_n_states)=wf_DP(:)
           !
           if (use_live_timing) call live_timing(steps=1)
           !
           cycle
         endif
#if defined _FFTW
         call fft_3d(wf_DP,fft_dim,+1,fftw_plan)
#else
         call fft_3d(wf_DP,fft_dim,+1)
#endif
         wf(:,wf_n_states)=wf_DP(:)*fft_norm
         !
         if (use_live_timing) call live_timing(steps=1)
         !
       enddo
       !
       ! GPL_EXCLUDE_START SPIN_IGNORE
       !
     enddo
     !
     ! GPL_EXCLUDE_END SPIN_IGNORE
     !
   enddo
 enddo
 !
 if (use_live_timing) call live_timing()
 !
 ! CLEAN
 !
 deallocate(wf_disk)
 call mem_est("wf_disk")
 !
 deallocate(wf_DP)
 !
#if defined _DISTRIBUTED
 if(allocated(states_to_load)) then
   if(allocated(states_loaded)) deallocate(states_loaded)
   allocate(states_loaded(wf_b(1):wf_b(2),wf_k(1):wf_k(2),wf_s(1):wf_s(2)))
   states_loaded(wf_b(1):wf_b(2),wf_k(1):wf_k(2),wf_s(1):wf_s(2))= &
&         states_to_load(wf_b(1):wf_b(2),wf_k(1):wf_k(2),wf_s(1):wf_s(2))
   deallocate(states_to_load)
 endif
#endif
 !
 ! Rotate in the SC basis
 !========================
 !
#if defined _SC
 !
 if (found_SC_DB) then
   !
   call load_SC_components('WF',COM_=NONE,MODE_=DUMP,kind=WF_kind,xc_functional=WF_xc_functional,&
&                          perturbation=WF_perturbation)
   !
   call msg('rns','[WF loader] '//trim(global_XC_string(WF_kind,SE_NONE,WF_xc_functional,WF_perturbation))//&
&                             ' wavefunctions loaded')
   !
   if (wf_space=='R') wf_size=fft_size
   if (wf_space=='G') wf_size=wf_ng
   do ikibz=wf_k(1),wf_k(2)
     call WF_rotate(ikibz,wf,wf_size)
   enddo
   call load_SC_components('WF_clean')
   !
 endif
 !
#endif
 !
 if (.not.wf_norm_test) then
   if (QUIET_alloc) call IO_and_Messaging_switch("+report +log ")
   return
 endif
 !
 ! Check normalization @ 1st k-point only.
 !
 mndp=10.
 mxdp=-1.
 do i1=1,min(int(nel)+5,wf_b(2))
   do i2=1,min(int(nel)+5,wf_b(2))
     !
     ! GPL_EXCLUDE_START SPIN_IGNORE
     !
     do is=1,n_sp_pol
       !
       ! GPL_EXCLUDE_END SPIN_IGNORE
       !
       if (wf_state(i1,1,is)==0.or.wf_state(i2,1,is)==0) cycle
       !
       c=dot_product(wf(:, wf_state(i1,1,is) ),wf(:, wf_state(i2,1,is) ))
       !
       if (n_spinor==2) then
         c=c+dot_product(wf(:, wf_state(i1,1,2) ),wf(:, wf_state(i2,1,2) ))
       endif
       !
       if (abs(c)>mxdp) mxdp=abs(c)
       if (abs(c)<mndp) mndp=abs(c)
       !
       ! GPL_EXCLUDE_START SPIN_IGNORE
       !
     enddo
     !
     ! GPL_EXCLUDE_END SPIN_IGNORE
     !
   enddo
 enddo
 !
 wf_norm_test=.false.
 call msg('rn','[WF loader] Normalization (few states)  min/max  :',(/mndp,mxdp/))
 !
 if (QUIET_alloc) call IO_and_Messaging_switch("+report +log ")
 !
end subroutine
