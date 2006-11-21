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
 subroutine wf_load(iG_in,iGo_max,bands_to_load,kpts_to_load,&
&                   spins_to_load,space,title)
!=============================================================
 use pars,          ONLY:SP,DP,schlen
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use stderr,        ONLY:string_pack
 use electrons,     ONLY:n_bands,nel,n_spin,n_spinor,n_sp_pol
 use R_lattice,     ONLY:nkibz
 use par_indexes_m, ONLY:par_indexes
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use FFT_m,         ONLY:fft_dim_loaded,fft_dim,fft_norm,&
&                        fftw_plan,fft_g_table,fft_size,fft_sign
 use wave_func,     ONLY:ng_wf,wf_igk,WF_alloc,wf_b,wf_k,wf_space,wf,wf_s,&
&                        wf_state,wf_ng,wf_norm_test,wf_free,wf_ng_disk,ioWF
 use timing,        ONLY:live_timing
 use IO_m,          ONLY:io_control,OP_RD,NONE,VERIFY,RD_CL_IF_END
 implicit none
 integer               :: iG_in,iGo_max,bands_to_load(2),kpts_to_load(2)
 integer     ,optional :: spins_to_load(2)
 character(*),optional :: space,title
!
!ws
 type(pp_indexes) ::px
 character(1)     ::wf_space_here
 character(schlen)::wf_title
 integer          ::i1,ikibz,i2,i3,is,wf_total_states,ig,igfft,s_2_load(2),iG_max
 real(SP)         ::mndp,mxdp
 complex(SP)      ::c
 real(SP),    allocatable :: wf_disk(:,:,:,:)
 complex(DP), allocatable :: wf_DP(:)
!
!I/O
 integer ::io_err,ID

 iG_max=iG_in
 if (iG_max==0) iG_max=wf_ng

 wf_space_here='R'
 if (present(space)) wf_space_here=space
 
 wf_title=""
 s_2_load=(/1,n_spin/)
 if (present(title)) wf_title=title
 if (present(spins_to_load)) s_2_load=spins_to_load
!
! To call with .false. as I need in ANY CASE the table updated with the
! given iG_max/iGo_max.
! 
! So even when conditions below are respected and the routine returns
! the fft_g_table must be reloaded
!
 if (wf_space_here=='R') call fft_setup(iG_max,iGo_max,.false.)

 if (all((/bands_to_load(1)>=wf_b(1),bands_to_load(2)<=wf_b(2),&
&          kpts_to_load(1)>=wf_k(1),kpts_to_load(2)<=wf_k(2),&
&          s_2_load(1)>=wf_s(1),s_2_load(2)<=wf_s(2),&
&          wf_space==wf_space_here/))) then
   if (wf_space=="R".and.all(fft_dim==fft_dim_loaded)) return
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
 wf_total_states=WF_alloc()
 if (wf_space=='R') allocate(wf_DP(fft_size))
 if (wf_space=='G') allocate(wf_DP(wf_ng))

 wf_title=string_pack('[FFT',trim(wf_title),'] Mesh size:')
 if (wf_space=='R') call msg('rns',trim(wf_title),fft_dim)

 call live_timing('[WF loader] Wfs (re)loading',wf_total_states,SERIAL=.true.)

 allocate(wf_disk(2,n_bands,wf_ng_disk,n_spin))
 call mem_est("wf_disk",(/size(wf_disk)/),(/SP/))

 call io_control(ACTION=OP_RD,COM=NONE,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=ioWF(ID,wf_disk)

 wf_total_states=0
 call pp_indexes_reset(px)
 do ikibz=1,nkibz
   call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ikibz+1/),ID=ID)
   io_err=ioWF(ID,wf_disk)
   do i2=1,n_bands
     do is=1,n_spin
       wf_DP=(0._DP,0._DP)
       if (any((/i2<wf_b(1),i2>wf_b(2),&
&                ikibz<wf_k(1),ikibz>wf_k(2),&
&                is<wf_s(1),is>wf_s(2)/))) cycle
       wf_total_states=wf_total_states+1
       wf_state(i2,ikibz,is)=wf_total_states
       do i3=1,ng_wf(ikibz)
         ig=wf_igk(i3,ikibz)
         if (ig>wf_ng) cycle
         igfft=ig
         if (wf_space=='R') igfft=fft_g_table(ig,1)
         wf_DP(igfft)=cmplx(wf_disk(1,i2,i3,is),wf_disk(2,i2,i3,is),DP)
       enddo
       if (wf_space=='G') then
         wf(:,wf_total_states)=wf_DP(:)
         call live_timing(steps=1)
         cycle
       endif
#if defined FFTW
       call fft_3d(wf_DP,fft_dim,fft_sign,fftw_plan)
#else
       call fft_3d(wf_DP,fft_dim,fft_sign)
#endif
       wf(:,wf_total_states)=wf_DP(:)*fft_norm
       call live_timing(steps=1)
     enddo
   enddo
 enddo
 call live_timing()
!
! CLEAN
!
 deallocate(wf_disk)
 call mem_est("wf_disk")
 deallocate(wf_DP)
 call pp_indexes_reset(px)

 if (.not.wf_norm_test) return
!
! Check normalization @ 1st k-point only.
! 
!
 mndp=10.
 mxdp=-1.
 do i1=1,min(int(nel)+5,wf_b(2))
   do i2=1,min(int(nel)+5,wf_b(2))

     do is=1,n_sp_pol

       c=dot_product(wf(:, wf_state(i1,1,is) ),wf(:, wf_state(i2,1,is) ))

       if (n_spinor==2) then
         c=c+dot_product(wf(:, wf_state(i1,1,2) ),wf(:, wf_state(i2,1,2) ))
       endif

       if (abs(c)>mxdp) mxdp=abs(c)
       if (abs(c)<mndp) mndp=abs(c)
     enddo
   enddo
 enddo
 wf_norm_test=.false.
 call msg('rn','[WF loader] Normalization (few states)  min/max  :',(/mndp,mxdp/))

 end subroutine
