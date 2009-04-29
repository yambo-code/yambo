!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Energy(en,k,E_kin,E_ion,E_h,Exc,Trace_Gf)
 !
 use pars,           ONLY:SP,DP,schlen
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc,Ko,H_ion
 use real_time,      ONLY:G_lesser,RT_step,rho,l_rt_exc
 use electrons,      ONLY:levels,n_sp_pol
 use wave_func,      ONLY:wf
 use FFT_m,          ONLY:fft_size
 use real_time,      ONLY:V_xc_0,V_hartree_0
 use xc_functionals, ONLY:V_xc,E_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(out):: E_kin,E_ion,E_h,Trace_Gf
 real(SP),      intent(out):: Exc
 !
 ! Work Space
 !
 integer       :: ik,i1
 complex(DP)   :: zdotc
 complex(SP)   :: cdotc
 complex(SP)   :: TmpMatrix(SC_bands,SC_bands,n_sp_pol)
 !
#if defined _DOUBLE
 real(DP):: ddot
#else
 real(SP):: sdot
#endif
 !
 TmpMatrix=(0._SP,0._SP)
 !
 ! Ko is the non-interaction kinetc
 !
 ! E_Kin = - i Tr {Ko G^<}  
 !
 E_kin=(0._SP,0._SP)
 !
 do ik=1,en%nk
   !      
#if defined _DOUBLE
   E_kin=E_kin-(0._SP,1._SP)*zdotc(SC_bands*SC_bands,Ko(:,:,ik,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#else
   E_kin=E_kin-(0._SP,1._SP)*cdotc(SC_bands*SC_bands,Ko(:,:,ik,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#endif
   !
 enddo
 !  
 ! E_ion = - i Tr {H_ion G^<}
 !
 E_ion=(0._SP,0._SP)
 !
 do ik=1,en%nk
   !      
#if defined _DOUBLE
   E_ion=E_ion-(0._SP,1._SP)*zdotc(SC_bands*SC_bands,H_ion(:,:,ik,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#else
   E_ion=E_ion-(0._SP,1._SP)*cdotc(SC_bands*SC_bands,H_ion(:,:,ik,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#endif
   !
 enddo
 !
 ! Hartree potential Energy
 !
 E_h=(0._SP,0._SP)
 !
 do ik=1,en%nk
   !
   TmpMatrix=(0._SP,0._SP)
   !
   if(l_rt_exc) then
     call V_to_H(ik,V_hartree_sc,TmpMatrix,wf)
   else
     call V_to_H(ik,V_hartree_0,TmpMatrix,wf)
   endif
   !
#if defined _DOUBLE
   E_h=E_h-(0._SP,0.5_SP)*zdotc(SC_bands*SC_bands,TmpMatrix(:,:,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#else
   E_h=E_h-(0._SP,0.5_SP)*cdotc(SC_bands*SC_bands,TmpMatrix(:,:,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#endif
   !
 enddo
 !
 ! E_xc Energy 
 !
 call IO_mute("report log io_in io_out")
 call xc_lda_driver(en,k,WF_KIND,WF_xc_functional,0)
 call IO_mute("  ")
 !
#if defined _DOUBLE
 Exc=ddot(fft_size,rho,1,E_xc,1)
#else 
 Exc=sdot(fft_size,rho,1,E_xc,1)
#endif
 !
 ! Total number of electrons
 !
 Trace_Gf=(0._SP,0._SP)
 !
 do i1=1,SC_bands
   Trace_Gf=Trace_Gf-(0._SP,1._SP)*sum(G_lesser(i1,i1,:)*k%weights(:))
 enddo
 !
end subroutine RT_Energy

