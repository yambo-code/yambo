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
 subroutine elph_dV_bare_2_gkkp(k,iq)
!====================================
 use pars,          ONLY:SP,DP
 use D_lattice,     ONLY:nsym,i_time_rev
 use R_lattice,     ONLY:ng_vec,nkibz,qindx_X,bz_samp,g_vec
 use ELPH,          ONLY:ph_modes,elph_bands,elph_dV,dV_bare
 use wave_func,     ONLY:wf,wf_state,wf_load,WF_free,wf_igk,wf_nc_k,wf_ncx,wf_ng
 use functions,     ONLY:BZ_index
 use FFT_m,         ONLY:fft_dim,fft_g_table,fft_size,fft_rot_r,fft_norm
 implicit none
 type(bz_samp) :: k
 integer       :: iq
!
!Work Space
 integer     :: ik,iko,iko_bz,il,ig,ib1,ib2,ifft1,ifft2,ir,iso,ic
 integer(8)  :: fw_plan,bw_plan
#if defined _DOUBLE
 complex(SP) :: zdotc
#else
 complex(SP) :: cdotc
#endif
 complex(DP), allocatable :: dV_bare_r(:),aux1(:)
 complex(SP), allocatable :: aux2(:),aux3(:)

 if (iq==1) call wf_load(ng_vec,1,elph_bands,(/1,nkibz/),title='-ELPH')

 allocate(dV_bare_r(fft_size),aux1(fft_size),aux2(wf_ncx),aux3(wf_ncx))

 fw_plan=0
 bw_plan=0

 do il=1,ph_modes
   dV_bare_r=cmplx(0.,0.,DP)
   do ig=1,ng_vec
     dV_bare_r( fft_g_table(ig,1) ) = dV_bare(iq,ig,il)
   enddo
   !
   ! G->R (FW) +1
   !
#if defined FFTW
   call fft_3d(dV_bare_r,fft_dim,+1,fw_plan)
#else
   call fft_3d(dV_bare_r,fft_dim,+1)
#endif
!
! DEBUG
! if (iq==1.and.il==3) then
! write (100,'(f20.10)') real(dV_bare_r)
! write (200,'(f20.10)') aimag(dV_bare_r)
! endif
   !
   do ik=1,nkibz
     iko_bz=qindx_X(iq,BZ_index(ik,k),1)
     iko=k%sstar(iko_bz,1)
     iso=k%sstar(iko_bz,2)
   
     if (iso>nsym/(i_time_rev+1)) iso=iso-nsym/(i_time_rev+1)

     do ib2=elph_bands(1),elph_bands(2)

       ifft2=wf_state(ib2,iko,1)

       if (iso==k%sstar(iko_bz,2)) then
         aux1(:)=dV_bare_r(:)* wf(fft_rot_r(iso,:),ifft2)/fft_norm
       else
         aux1(:)=dV_bare_r(:)*conjg( wf(fft_rot_r(iso,:),ifft2) )/fft_norm
       endif

!DEBUG>
! if (iq==1.and.il==3.and.iko==1.and.ib2==1) then
! write (300,'(f20.10)') real(aux1)
! write (400,'(f20.10)') aimag(aux1)
! endif
!DEBUG<
       !
       ! R->G (BW) -1 of the dV_bare x wf(ik-iq)
       !
#if defined FFTW
       call fft_3d(aux1,fft_dim,-1,bw_plan)
#else
       call fft_3d(aux1,fft_dim,-1)
#endif
       do ic=1,wf_ncx
         aux2(ic)=aux1(fft_g_table(wf_igk(ic,iko),1))/real(fft_size,SP)
       enddo

!DEBUG
 if (iq==1.and.il==3.and.iko==2.and.ib2==1) then
 write (500,'(f20.10)') real(aux2(:wf_nc_k(iko)))
 write (600,'(f20.10)') aimag(aux2(:wf_nc_k(iko)))
 endif

       do ib1=elph_bands(1),elph_bands(2)
         ifft1=wf_state(ib1,ik,1)
         !
         ! R->G (BW) -1 of the left WF, wf(ik)
         !
         aux1=wf(:,ifft1)
#if defined FFTW
         call fft_3d(aux1,fft_dim,-1,bw_plan)
#else
         call fft_3d(aux1,fft_dim,-1)
#endif
         ! Note the fft_norm factor due to the fft_norm factor
         ! already embodied in wf definition (wf = wf_DP * fft_norm) in wf_load
         !
         do ic=1,wf_ncx
           aux3(ic)=aux1(fft_g_table(wf_igk(ic,ik),1))/real(fft_size,SP)/fft_norm
         enddo

!elph_dV(BZ_index(ik,k),il,ib1-elph_bands(1)+1,ib2-elph_bands(1)+1)=&
!&       cdotc(wf_ncx,aux3,1,aux2,1)

!DEBUG
 if (iq==1.and.il==3.and.iko==3.and.ib2==1.and.ib1==3) then
 write (100,'(f20.10)') real(aux3(:wf_nc_k(ik)))
 write (200,'(f20.10)') aimag(aux3(:wf_nc_k(ik)))
 write (300,'(f20.10)') real(aux2(:wf_nc_k(iko)))
 write (400,'(f20.10)') aimag(aux2(:wf_nc_k(iko)))
         write (*,'(/5i3)') iq,ik,ib1,ib2,il
         write (*,'(2f20.15)') cdotc(wf_ncx,aux3,1,aux2,1)
 stop
 endif
!!         write (*,'(2f20.15)') &
!&           elph_dV(BZ_index(ik,k),il,ib1-elph_bands(1)+1,ib2-elph_bands(1)+1)

       enddo
     enddo


   enddo

 enddo

 if (iq==3) stop

 deallocate(dV_bare_r,aux1,aux2,aux3)

 end subroutine

