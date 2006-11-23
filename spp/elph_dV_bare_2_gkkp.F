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
 use R_lattice,     ONLY:ng_vec,nkibz,qindx_X,bz_samp
 use ELPH,          ONLY:ph_modes,elph_bands,elph_dV,dV_bare
 use wave_func,     ONLY:wf,wf_state,wf_load,WF_free,wf_ng
 use functions,     ONLY:BZ_index
 use FFT_m,         ONLY:fft_dim,fft_g_table,fft_size,fft_rot_r
 implicit none
 type(bz_samp) :: k
 integer       :: iq
!
!Work Space
 integer     :: ik,iko,iko_bz,il,ig,ib1,ib2,ifft1,ifft2,ir,iso
 integer(8)  :: fw_plan,bw_plan
 complex(SP) :: cdotc
 complex(DP), allocatable :: aux1(:)
 complex(SP), allocatable :: aux2(:),aux3(:)

 if (iq==1) call wf_load(ng_vec,1,elph_bands,(/1,nkibz/),title='-ELPH')

 allocate(aux1(fft_size),aux2(wf_ng),aux3(wf_ng))

 fw_plan=0
 bw_plan=0

 do il=1,ph_modes
   aux1=cmplx(0.,0.,DP)
   do ig=1,ng_vec
     aux1( fft_g_table(ig,1) ) = dV_bare(iq,ig,il)
   enddo
   !
   ! G->R (FW) +1
#if defined FFTW
   call fft_3d(aux1,fft_dim,+1,fw_plan)
#else
   call fft_3d(aux1,fft_dim,+1)
#endif
   !
   do ik=1,nkibz
     iko_bz=qindx_X(iq,BZ_index(ik,k),1)
     iko=k%sstar(iko_bz,1)
     iso=k%sstar(iko_bz,2)
   
     if (iso>nsym/(i_time_rev+1)) iso=iso-nsym/(i_time_rev+1)

     do ib1=elph_bands(1),elph_bands(2)
       do ib2=elph_bands(1),elph_bands(2)

         ifft1=wf_state(ib1,ik,1)
         ifft2=wf_state(ib2,iko,1)

         if (iso==k%sstar(iko_bz,2)) then
           aux1(:)=aux1(:)* wf(fft_rot_r(iso,:),ifft2)
         else
           aux1(:)=aux1(:)*conjg( wf(fft_rot_r(iso,:),ifft2) )
         endif
         !
         ! R->G (BW) -1
         !
#if defined FFTW
         call fft_3d(aux1,fft_dim,-1,bw_plan)
#else
         call fft_3d(aux1,fft_dim,-1)
#endif
         aux2(1:wf_ng)=aux1(fft_g_table(1:wf_ng,1))

         aux1=wf(:,ifft1)
#if defined FFTW
         call fft_3d(aux1,fft_dim,-1,bw_plan)
#else
         call fft_3d(aux1,fft_dim,-1)
#endif
         aux3(1:wf_ng)=aux1(fft_g_table(1:wf_ng,1))

         write (*,'(/5i3)') iq,ik,ib1,ib2,il
         write (*,'(2f20.15)') cdotc(wf_ng,aux2,1,aux3,1)
         write (*,'(2f20.15)') elph_dV(BZ_index(ik,k),il,ib1-elph_bands(1)+1,ib2-elph_bands(1)+1)

       enddo
     enddo


   enddo

 enddo

 if (iq==3) stop

 deallocate(aux1,aux2,aux3)

 end subroutine

