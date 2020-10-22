!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
subroutine NL_build_valence_bands(en,V_input,VAL_BANDS,lowest_band) 
 !
 ! Construct time-dependent valence bands
 !
 use pars,        ONLY:SP,cZERO
 use electrons,   ONLY:levels,n_sp_pol,n_spinor
 use nl_optics,   ONLY:NL_bands
 use FFT_m,       ONLY:fft_size
 use QP_m,        ONLY:QP_nk
 use wave_func,   ONLY:WF
 use parallel_m,  ONLY:PAR_IND_Xk_ibz,PAR_Xk_ibz_index,PAR_Xk_nibz
 !
 implicit none
 !
 type(levels), intent(in)  :: en
 integer,      intent(in)  :: lowest_band
 complex(SP),  intent(in)  :: V_input(NL_bands(2),en%nbf,QP_nk,n_sp_pol)
 complex(SP),  intent(out) :: VAL_BANDS(fft_size,en%nbf,PAR_Xk_nibz,n_sp_pol)
 !
 ! Work Space
 !
 integer :: ib,is,ik,i_wf,i_v,ik_mem,i_spinor
 !
 Val_bands=cZERO
 !
 do ik=1,QP_nk
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_Xk_ibz_index(ik)
   !
   do is=1,n_sp_pol
     do ib=lowest_band,NL_bands(2)
       !
       i_wf=WF%index(ib,ik,is)
!$omp parallel do default(shared), private(i_v,i_spinor)
       do i_v=1,en%nbf
         do i_spinor=1,n_spinor
           Val_bands(:,i_v,ik_mem,is)=Val_bands(:,i_v,ik_mem,is)+V_input(ib,i_v,ik,is)*WF%c(:,i_spinor,i_wf)
         enddo
       enddo
!$omp end parallel do
       !
     enddo
   enddo
   !
 enddo
 !
end subroutine NL_build_valence_bands
