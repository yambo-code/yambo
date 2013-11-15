!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine WF_load(WF,iG_in,iGo_max_in,bands_to_load,kpts_to_load,&
&                  spins_to_load,space,title,impose_free_and_alloc,force_WFo)
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
 use wave_func,     ONLY:wf_nc_k,wf_igk,WF_buffer,&
&                        wf_ng,wf_norm_test,wf_ncx,ioWF,&
&                        wf_nb_io,wf_nb_io_groups,QUIET_alloc,&
&                        QUIET_free,WAVEs
 use wave_func,     ONLY:states_to_load
 use IO_m,          ONLY:io_control,OP_RD,NONE,VERIFY,RD_CL_IF_END,&
&                        io_fragmented, io_netcdf_support, RD_CL,DUMP,IO_and_Messaging_switch
#if defined _SC
 use com,           ONLY:warning,error
 use SC,            ONLY:compatible_SC_DB,load_SC_components,SC_bands,found_SC_DB
 use global_XC,     ONLY:WF_kind,WF_xc_functional,&
&                        WF_perturbation,global_XC_string,SE_NONE
 use xc_functionals,ONLY:GS_xc_KIND,GS_xc_FUNCTIONAL
#endif
 implicit none
 !
 integer               :: iG_in,iGo_max_in,bands_to_load(2),kpts_to_load(2)
 type(WAVEs)           :: WF
 integer     ,optional :: spins_to_load(2)
 character(*),optional :: space
 character(*),optional :: title
 logical     ,optional :: impose_free_and_alloc
 logical     ,optional :: force_WFo
 !
 ! Work Space
 !
 character(2)     ::wf_space_here
 character(schlen)::wf_title
 integer          ::i1,ikibz,ib,ib1,ib2,i2,ic,is,ig,igfft,wf_size,ib_grp,&
&                   s_to_load(2),iG_max,iG_bounds_loaded(2),iGo_max,ACTION_
 real(SP)         ::mndp,mxdp
 complex(SP)      ::c
 logical          ::loaded_bounds_ok,use_direct_access,free_and_alloc,buffer_is_ok
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
 s_to_load=(/1,n_spin/)
 if (present(title)) wf_title=title
 if (present(spins_to_load)) s_to_load=spins_to_load
 !
 ! [1]: check that loaded bounds are larger(equal) then bounds 
 !      asked now
 !
 call check_bounds(WF,loaded_bounds_ok)
 loaded_bounds_ok= WF%space==wf_space_here
 !
 call check_bounds(WF_buffer,buffer_is_ok)
 !
 buffer_is_ok=.FALSE.
 !
 if (allocated(states_to_load)) then
   !
   if (loaded_bounds_ok) then
     !
     ! See if the %c in WF is already ok
     !
     if (associated(WF%state)) call check_states(WF,loaded_bounds_ok)
     !
   endif
   !
   if (associated(WF_buffer%state).and.wf_space_here/='B') then
     !
     ! See if the %c Buffered are ok
     !
     buffer_is_ok=.TRUE.
     !
     call check_states(WF_buffer,buffer_is_ok)
     !
     ! if not clean the buffer
     !
     if (.not.buffer_is_ok) call WF_free(WF_buffer)
     !
   endif
 endif
 !
 !
 ! [2]: Check FFT size
 !
 if (loaded_bounds_ok) then
   !
   if (WF%space=="G".or.WF%space=="C".or.WF%space=="B") return
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
 use_direct_access=(kpts_to_load(1)==kpts_to_load(2))
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
 if (free_and_alloc) call WF_free(WF)
 !
 WF%b=bands_to_load
 WF%k=kpts_to_load
 WF%spin=s_to_load
 WF%space=wf_space_here
 !
 ! Is WF to load ? If not and the space is "R" I need to know the fft_size, at least. 
 !
 if (allocated(states_to_load)) then
   WF%to_load=count(states_to_load .eqv. .true.)>0
   if (.not.WF%to_load) then
     if(associated(WF%state)) nullify(WF%state)
     allocate(WF%state(WF%b(1):WF%b(2),WF%k(1):WF%k(2),WF%spin(1):WF%spin(2)))
     WF%state(WF%b(1):WF%b(2),WF%k(1):WF%k(2),WF%spin(1):WF%spin(2))= &
&             states_to_load(WF%b(1):WF%b(2),WF%k(1):WF%k(2),WF%spin(1):WF%spin(2))
     deallocate(states_to_load)
     return
   endif
 endif
 !
#if defined _SC
 !
 ! First check and fix the number of bands 
 !
 call load_SC_components('check_WF_DB',n_bands=bands_to_load(2))
 !
 if (found_SC_DB.and.compatible_SC_DB) then
   !
   if(allocated(states_to_load).and..not.present(force_WFo)) then
     deallocate(states_to_load)
     call warning(' [WF] WF-distribution turned off, not compatible with SC wave functions')
   endif
   !
   if (any ( WF%b/=(/1,SC_bands(2)/) )) then 
     call warning(' [WF] Forced '//trim(intc(SC_bands(2)))//' bands to be loaded')
     if(allocated(states_to_load)) states_to_load(WF%b(2)+1:SC_bands(2),:,:)=.TRUE.
   endif
   WF%b=(/1,SC_bands(2)/)
   !
 else if (found_SC_DB.and..not.compatible_SC_DB) then
   !
   call error(' [WF] Too many bands required ('//&
&             trim(intc(bands_to_load(2)))//'). Only '//trim(intc(SC_bands(2)))//' available.')
   !
 endif
 !
#endif
 !
 if (WF%space=='R') call fft_setup(iG_max,iGo_max,.false.)
 if (WF%space=="R") fft_dim_loaded=fft_dim
 !
 if (free_and_alloc) call WF_alloc(WF)
 !
 if (WF%space=='R') allocate(wf_DP(fft_size))
 if (WF%space=='G') allocate(wf_DP(wf_ng))
 if (WF%space=='C'.or.WF%space=='B') allocate(wf_DP(wf_ncx))
 call mem_est("wf_DP",(/size(wf_DP)/),(/2*DP/))
 !
 ! LOG the states loaded
 !
 if (buffer_is_ok.and.WF%space/="B") then
   call msg('rns','[WF] Loading Wave-Functions from buffer')
 else
   call msg('rns','[WF] Performing Wave-Functions I/O')
 endif
 !
 if (WF%space=='R') call msg('rns','[FFT'//trim(wf_title)//'] Mesh size:',fft_dim)
 !
 ! Bug-fix (noticed by AF) fixed by AM 26/8/2013
 !
 ! The wf_disk is written on disk in blocks dimensioned by wf_nb_io. 
 ! A smaller I/O is possible ONLY with NETCDF while in plain Fortran 
 ! it produces and erroneous loading of WFs
 !
 ! Update of 21/10/2013. This reduced I/O leads to problems and the Al111 test is not 
 ! passed when using few bands. I switch it off.
 !
 if (.not.buffer_is_ok) then
   !
   allocate(wf_disk(2,wf_nb_io,wf_ncx,n_spin))
   call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
   !
   call io_control(ACTION=OP_RD,COM=NONE,MODE=VERIFY,SEC=(/1/),ID=ID)
   io_err=ioWF(ID,wf_disk)
   !
 endif
 !
 WF%N=0
 do ikibz=1,nkibz
   !
   do ib_grp=1,wf_nb_io_groups
     !
     if (.not.buffer_is_ok) then
       !
       ! Use fragmentation ot NETCDF support to reduce the I/O
       ! to the k-pts and bands needed only
       !
       if (io_fragmented(ID).or.io_netcdf_support(ID)) then
         if (any((/ikibz<WF%k(1),ikibz>WF%k(2)/))) cycle
         if (wf_nb_io*(ib_grp-1)+1>WF%b(2)) cycle
         !
         ! In case the states to load are distributed in a complex way
         ! by PARALLEL_WF_distribute I skip fragments that are not needed
         ! 
         if ( allocated(states_to_load) ) then
           ib1=wf_nb_io*(ib_grp-1)+1
           ib2=wf_nb_io*ib_grp
           if(ib2>WF%b(2)) ib2=WF%b(2)
           if (.not.any(states_to_load(ib1:ib2,ikibz,:))) cycle
         endif
       endif
       !
       ACTION_=RD_CL_IF_END
       if (use_direct_access) ACTION_=RD_CL
       call io_control(ACTION=ACTION_,COM=NONE,SEC=(/ikibz+1,ib_grp/),ID=ID)
       !
       io_err=ioWF(ID,wf_disk)
       if (any((/ikibz<WF%k(1),ikibz>WF%k(2)/))) cycle
       if (wf_nb_io*(ib_grp-1)+1>WF%b(2)) cycle
       !
     endif
     !
     do is=1,n_spin
       do ib=wf_nb_io*(ib_grp-1)+1,wf_nb_io*ib_grp
         !
         i2=ib-wf_nb_io*(ib_grp-1)
         !
         if (any((/ib<WF%b(1),ib>WF%b(2),is<WF%spin(1),is>WF%spin(2)/))) cycle 
         !
         if(allocated(states_to_load)) then
           if(.not.states_to_load(ib,ikibz,is)) cycle
         endif
         !
         wf_DP=(0._DP,0._DP)
         WF%N=WF%N+1
         WF%index(ib,ikibz,is)=WF%N
         do ic=1,wf_nc_k(ikibz)
           ig=wf_igk(ic,ikibz)
           if (ig>wf_ng) cycle
           igfft=ig
           if (WF%space=='R') igfft=fft_g_table(ig,1)
           if (WF%space=='C'.or.WF%space=='B') igfft=ic
           if (buffer_is_ok) then
             wf_DP(igfft)=WF_buffer%c(ic,WF_buffer%index(ib,ikibz,is))
           else
             wf_DP(igfft)=cmplx(wf_disk(1,i2,ic,is),wf_disk(2,i2,ic,is),DP)
           endif
         enddo
         if (WF%space=='G'.or.WF%space=='C'.or.WF%space=='B') then
           WF%c(:,WF%N)=wf_DP(:)
           cycle
         endif
#if defined _FFTW
         call fft_3d(wf_DP,fft_dim,+1,fftw_plan)
#else
         call fft_3d(wf_DP,fft_dim,+1)
#endif
         WF%c(:,WF%N)=wf_DP(:)*fft_norm
         !
       enddo
     enddo
   enddo
 enddo
 !
 ! CLEAN
 !
 if (.not.buffer_is_ok) then
   deallocate(wf_disk)
   call mem_est("wf_disk")
 endif
 !
 deallocate(wf_DP)
 call mem_est("wf_DP")
 !
 if(allocated(states_to_load)) then
   !
   if(associated(WF%state)) nullify(WF%state)
   !
   allocate(WF%state(WF%b(1):WF%b(2),WF%k(1):WF%k(2),WF%spin(1):WF%spin(2)))
   !
   WF%state(WF%b(1):WF%b(2),WF%k(1):WF%k(2),WF%spin(1):WF%spin(2))= &
&           states_to_load(WF%b(1):WF%b(2),WF%k(1):WF%k(2),WF%spin(1):WF%spin(2))
   !
   ! When using the buffer the states_to_load must not be cleaned
   !
   if (.not.associated(WF_buffer%c)) deallocate(states_to_load)
   !
 endif
 !
#if defined _SC
 !
 ! Rotate in the SC basis
 !========================
 !
 if (found_SC_DB.and..not.present(force_WFo)) then
   !
   call load_SC_components('WF',COM_=NONE,MODE_=DUMP,kind=WF_kind,xc_functional=WF_xc_functional,&
&                          perturbation=WF_perturbation)
   !
   call msg('rns','[WF loader] '//trim(global_XC_string(WF_kind,SE_NONE,WF_xc_functional,WF_perturbation))//&
&                             ' wavefunctions loaded')
   !
   if (WF%space=='R') wf_size=fft_size
   if (WF%space=='G') wf_size=wf_ng
   do ikibz=WF%k(1),WF%k(2)
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
 do i1=1,min(int(nel)+5,WF%b(2))
   do i2=1,min(int(nel)+5,WF%b(2))
     do is=1,n_sp_pol
       !
       if (WF%index(i1,1,is)==0.or.WF%index(i2,1,is)==0) cycle
       !
       c=dot_product(WF%c(:, WF%index(i1,1,is) ),WF%c(:, WF%index(i2,1,is) ))
       if (n_spinor==2) c=c+dot_product(WF%c(:, WF%index(i1,1,2) ),WF%c(:, WF%index(i2,1,2) ))
       !
       if (abs(c)>mxdp) mxdp=abs(c)
       if (abs(c)<mndp) mndp=abs(c)
       !
     enddo
     !
   enddo
 enddo
 !
 wf_norm_test=.false.
 call msg('rn','[WF loader] Normalization (few states)  min/max  :',(/mndp,mxdp/))
 !
 if (QUIET_alloc) call IO_and_Messaging_switch("+report +log ")
 !
 contains
   !
   subroutine check_bounds(WF_,flag_)
     type(WAVEs)  :: WF_
     logical      :: flag_
     flag_=all((/bands_to_load(1)>=WF_%b(1),bands_to_load(2)<=WF_%b(2),&
&                kpts_to_load(1)>=WF_%k(1),kpts_to_load(2)<=WF_%k(2),&
&                s_to_load(1)>=WF_%spin(1),s_to_load(2)<=WF_%spin(2)/))
   end subroutine
   !
   subroutine check_states(WF_,flag_)
     type(WAVEs)  :: WF_
     logical      :: flag_
     ikibz=WF_%k(1)
     do while(ikibz<=WF_%k(2).and.flag_) 
       is   =WF_%spin(1)
       do while(is<=WF_%spin(2).and.flag_) 
          ib   =WF_%b(1)
          do while(ib<=WF_%b(2).and.flag_) 
            flag_=states_to_load(ib,ikibz,is).eqv.WF_%state(ib,ikibz,is)
            ib=ib+1
          enddo
          is=is+1
       enddo
       ikibz=ikibz+1
     enddo
   end subroutine
   !
end subroutine
