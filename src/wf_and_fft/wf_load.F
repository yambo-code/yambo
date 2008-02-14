!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine wf_load(iG_in,iGo_max_in,bands_to_load,kpts_to_load,&
&                   spins_to_load,space,title)
 !
 ! Load and (eventually) FFTs the wavefunctions
 !
 use pars,          ONLY:SP,DP,schlen
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use stderr,        ONLY:string_pack
 use electrons,     ONLY:n_bands,nel,n_spin,n_spinor,n_sp_pol
 use R_lattice,     ONLY:nkibz
 use FFT_m,         ONLY:fft_dim_loaded,fft_dim,fft_norm,&
&                        fftw_plan,fft_g_table,fft_size
 use wave_func,     ONLY:wf_nc_k,wf_igk,WF_alloc,wf_b,wf_k,wf_space,wf,wf_s,&
&                        wf_state,wf_ng,wf_norm_test,wf_ncx,ioWF,WF_free,&
&                        wf_n_states
 use timing,        ONLY:live_timing
 use IO_m,          ONLY:io_control,OP_RD,NONE,VERIFY,RD_CL_IF_END,&
&                        io_fragmented, io_netcdf_support, RD_CL
 implicit none
 !
 integer               :: iG_in,iGo_max_in,bands_to_load(2),kpts_to_load(2)
 integer     ,optional :: spins_to_load(2)
 character(*),optional :: space,title
 !
 ! Work Space
 !
 character(1)     ::wf_space_here
 character(schlen)::wf_title
 integer          ::i1,ikibz,i2,ic,is,ig,igfft,&
&                   s_2_load(2),iG_max,iG_bounds_loaded(2),iGo_max
 real(SP)         ::mndp,mxdp
 complex(SP)      ::c
 logical          ::loaded_bounds_ok
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
 ! [2]: Check FFT size
 !
 if (loaded_bounds_ok) then
   !
   if (wf_space=="G") return
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
 ! First WF_free then fft_setup.
 !
 call WF_free()
 wf_b=bands_to_load
 wf_k=kpts_to_load
 wf_s=s_2_load
 wf_space=wf_space_here
 !
 if (wf_space=='R') call fft_setup(iG_max,iGo_max,.false.)
 if (wf_space=="R") fft_dim_loaded=fft_dim
 !
 call WF_alloc()
 !
 if (wf_space=='R') allocate(wf_DP(fft_size))
 if (wf_space=='G') allocate(wf_DP(wf_ng))
 if (wf_space=='C') allocate(wf_DP(wf_ncx))
 !
 wf_title=string_pack('[FFT',trim(wf_title),'] Mesh size:')
 if (wf_space=='R') call msg('rns',trim(wf_title),fft_dim)
 !
#if !defined _BIGSYS
 call live_timing('[WF loader] Wfs (re)loading',wf_n_states,SERIAL=.true.)
#endif
 !
 allocate(wf_disk(2,n_bands,wf_ncx,n_spin))
#if defined _BIGSYS
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/),quiet=.true.)
#else
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))
#endif
 !
 call io_control(ACTION=OP_RD,COM=NONE,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=ioWF(ID,wf_disk)
 !
 wf_n_states=0
 do ikibz=1,nkibz
   !
   if (io_fragmented(ID).or.io_netcdf_support(ID)) then
     if (any((/ikibz<wf_k(1),ikibz>wf_k(2)/))) cycle
   endif
   !
#if defined _BIGSYS
   call io_control(ACTION=RD_CL,COM=NONE,SEC=(/ikibz+1/),ID=ID)
#else
   call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ikibz+1/),ID=ID)
#endif
   io_err=ioWF(ID,wf_disk)
   if (any((/ikibz<wf_k(1),ikibz>wf_k(2)/))) cycle
   !
   do i2=1,n_bands
     do is=1,n_spin
       if (any((/i2<wf_b(1),i2>wf_b(2),&
&                is<wf_s(1),is>wf_s(2)/))) cycle
       wf_DP=(0._DP,0._DP)
       wf_n_states=wf_n_states+1
       wf_state(i2,ikibz,is)=wf_n_states
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
#if !defined _BIGSYS
         call live_timing(steps=1)
#endif
         cycle
       endif
#if defined _FFTW
       call fft_3d(wf_DP,fft_dim,+1,fftw_plan)
#else
       call fft_3d(wf_DP,fft_dim,+1)
#endif
       wf(:,wf_n_states)=wf_DP(:)*fft_norm
       call live_timing(steps=1)
     enddo
   enddo
 enddo
#if !defined _BIGSYS
 call live_timing()
#endif
 !
 ! CLEAN
 !
 deallocate(wf_disk)
#if defined _BIGSYS
 call mem_est("wf_disk",quiet=.true.)
#else
 call mem_est("wf_disk")
#endif
 deallocate(wf_DP)
 !
 if (.not.wf_norm_test) return
 !
 ! Check normalization @ 1st k-point only.
 !
 mndp=10.
 mxdp=-1.
 do i1=1,min(int(nel)+5,wf_b(2))
   do i2=1,min(int(nel)+5,wf_b(2))
     !
     do is=1,n_sp_pol
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
     enddo
   enddo
 enddo
 wf_norm_test=.false.
 call msg('rn','[WF loader] Normalization (few states)  min/max  :',(/mndp,mxdp/))
 !
end subroutine
