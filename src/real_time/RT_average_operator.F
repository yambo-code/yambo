!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_average_operator(VALUE,O_REAL,O_QP,O_MATRIX,G,k,TRANS)
 !
 use pars,       ONLY:SP,cI
 use SC,         ONLY:SC_bands
 use R_lattice,  ONLY:bz_samp 
 use QP_m,       ONLY:QP_nk,QP_n_states
 use electrons,  ONLY:n_sp_pol
 use FFT_m,      ONLY:fft_size
 use wave_func,  ONLY:wf
 use wrapper,    ONLY:Vstar_dot_V
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: G(SC_bands,SC_bands,QP_nk)
 complex(SP),  intent(out) :: VALUE
 complex(SP),     optional, intent(in) :: O_MATRIX(SC_bands,SC_bands,QP_nk,n_sp_pol)
 complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
 real(SP),        optional, intent(in) :: O_REAL(fft_size,n_sp_pol)
 logical,         optional, intent(in) :: TRANS
 !
 ! Work Space
 !
 integer     :: i1,ik
 complex(SP) :: Hk(SC_bands,SC_bands,n_sp_pol)
 !
 VALUE=(0._SP,0._SP)
 !
 ! If not operator are present the subroutine returns the trace of the Green Function
 ! 
 if(.not.present(O_MATRIX).and..not.present(O_REAL).and..not.present(O_QP)) then
   !
   ! Total number of electrons
   !
   do i1=1,SC_bands
     VALUE=VALUE-cI*sum(G(i1,i1,:)*k%weights(:))
   enddo
   !
   return
   !
 endif 
 !
 do ik=1,QP_nk
   !
   Hk=(0._SP,0._SP)
   !
   if(present(O_MATRIX)) then
     if(present(TRANS)) then 
        forall(i1=1:n_sp_pol) Hk(1:SC_bands,1:SC_bands,i1)=transpose(O_MATRIX(1:SC_bands,1:SC_bands,ik,i1))
     else
        forall(i1=1:n_sp_pol) Hk(1:SC_bands,1:SC_bands,i1)=O_MATRIX(1:SC_bands,1:SC_bands,ik,i1)
     endif
   endif
   if(present(O_QP))     call V_qp_basis_to_H(ik,O_QP,Hk)
   if(present(O_REAL))   call V_real_space_to_H(ik,O_REAL,Hk,wf)
   !
   VALUE=VALUE-cI*Vstar_dot_V(SC_bands**2,Hk(1:SC_bands,1:SC_bands,1),G(1:SC_bands,1:SC_bands,ik))*k%weights(ik)
   !
 enddo
 !
end subroutine RT_average_operator

