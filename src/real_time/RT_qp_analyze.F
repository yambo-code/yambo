!
! Copyright (C) 2000-2010  C. Attaccalite and the YAMBO team 
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
!
subroutine RT_qp_analyze(it,en,k,Xk,G_input)
 !
 ! Evaluate quasi-particle energies and occupations
 ! and write them in en%E, en%f
 !
 use pars,           ONLY:SP,DP
 use units,          ONLY:HARTREE
 use electrons,      ONLY:levels
 use real_time,      ONLY:H_t,RT_diagonalization_steps,Occ_natural_orb,Thermal_steps
 use QP_m,           ONLY:QP_nk  
 use stderr,         ONLY:set_real_printed_length
 use SC,             ONLY:H_nl_sc,SC_bands,E_bare
 use R_lattice,      ONLY:bz_samp
 use com,            ONLY:msg
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK
 !
 implicit none
 !
 type(levels),  intent(out)  :: en
 integer,       intent(in)   :: it
 complex(SP) ,  intent(in)   :: G_input(SC_bands,SC_bands,QP_nk)
 type(bz_samp), intent(in)   :: k,Xk
 !
 ! Work Space
 !
 integer     :: ik,ib
 complex(SP) :: tmp_v(SC_bands)
 !
#if defined _DOUBLE
 complex(DP) :: zdotc
#else
 complex(SP) :: cdotc
#endif
 !
 if(.not.mod(it,RT_diagonalization_steps)==0.and..not.it==Thermal_steps) return
 !
 call set_real_printed_length()
 !
 do ik=1,QP_nk
   !
   ! Find the natural orbitals
   !
   H_nl_sc(:,:,1)=H_t(:,:,ik,1) 
   call mat_dia_inv(DIAGO,USE_LK,H_nl_sc(:,:,1),E_real=en%E(1:SC_bands,ik,1))
   !
   do ib=1,SC_bands
#if defined _DOUBLE
     call zgemv('N',SC_bands,SC_bands,(0._SP,-1._SP),G_input(:,:,ik),SC_bands,H_nl_sc(:,ib,1),1,(0._SP,0._SP),tmp_v,1)
     Occ_natural_orb(ib,ik)=real(zdotc(SC_bands,H_nl_sc(:,ib,1),1,tmp_v,1))
#else 
     call cgemv('N',SC_bands,SC_bands,(0._SP,-1._SP),G_input(:,:,ik),SC_bands,H_nl_sc(:,ib,1),1,(0._SP,0._SP),tmp_v,1)
     Occ_natural_orb(ib,ik)=real(cdotc(SC_bands,H_nl_sc(:,ib,1),1,tmp_v,1))
#endif   
   enddo
   !
 enddo
 !
 call OCCUPATIONS_Fermi(en,k,0)
 !
 en%E(:SC_bands,:,:)=en%E(:SC_bands,:,:)-en%Efermi(1)
 !
 !
 if(it==Thermal_steps) then
   call msg('r',' ')
   call msg('r','Natural (O)ccupations, (E)nergies and (d_E) E-Eo.')
   do ik=1,QP_nk
     call REPORT_Energies(en%E(:SC_bands,:,:),k,Xk,SC_bands,(/ik,ik/),'E  ',.TRUE.)
     call REPORT_Energies(E_bare,k,Xk,SC_bands,(/ik,ik/),'E_o  ',.FALSE.)
     call REPORT_Energies(en%E(:SC_bands,:,:)-E_bare,k,Xk,SC_bands,(/ik,ik/),'d_E',.FALSE.)
     call REPORT_Energies(Occ_natural_orb(:SC_bands,:)/HARTREE,k,Xk,SC_bands,(/ik,ik/),'O  ',.FALSE.)
   enddo
 endif
 !
 call set_real_printed_length(f_length=15,g_length=15)
 !
 ! History
 !
 call  SC_History(it,en)
 !
end subroutine RT_qp_analyze
