!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_average_operator(VALUE,O_REAL,O_QP,O_MATRIX,G,k,en,TRANS,nK_mem)
 !
 use pars,           ONLY:SP,cI
 use SC,             ONLY:SC_bands,SC_nbands,RT_all2ibz,RT_Nk_ibz,RT_nk,RT_ibz_coo
 use R_lattice,      ONLY:bz_samp 
 use QP_m,           ONLY:QP_n_states
 use electrons,      ONLY:n_sp_pol,levels
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:wf
 use wrapper,        ONLY:Vstar_dot_V
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_Xk_ibz_index,PAR_COM_Xk_ibz_INDEX
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 integer,       intent(in) :: nK_mem
 complex(SP),  intent(out) :: VALUE
 complex(SP),     optional, intent(in) :: O_MATRIX(SC_bands(2),SC_bands(2),nK_mem,n_sp_pol)
 complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
 real(SP),        optional, intent(in) :: O_REAL(fft_size,n_sp_pol)
 logical,         optional, intent(in) :: TRANS
 type(levels),    optional, intent(in) :: en
 !
 ! Work Space
 !
 integer     :: i1,ik,SC_bands2,ik_ibz
 complex(SP) :: Hk(SC_bands(2),SC_bands(2),n_sp_pol)
 !
 SC_bands2=SC_nbands**2
 VALUE=(0._SP,0._SP)
 !
 ! If not operator are present the subroutine returns the trace of the Green Function
 ! 
 if(.not.present(O_MATRIX).and..not.present(O_REAL).and..not.present(O_QP)) then
   !
   ! Total number of electrons
   !
   do i1=1,SC_bands(1)-1
     VALUE=VALUE+sum(en%f(i1,:,1)*k%weights(:))
   enddo
   do i1=SC_bands(1),SC_bands(2)
     do ik=1,RT_nk
       VALUE=VALUE-cI*G(i1,i1,ik)*k%weights(RT_all2ibz(ik))/real(RT_Nk_ibz(RT_all2ibz(ik)))
     enddo
   enddo
   !
   return
   !
 endif 
 !
 do ik_ibz=1,k%nibz
   ! 
   if (k%nibz/=nK_mem) then
     if (.not.PAR_IND_Xk_ibz%element_1D(ik_ibz)) cycle   
   endif
   !
   call make_Hk_matrix(ik_ibz)
   !
   do i1=1,SC_bands(1)-1
     VALUE=VALUE+conjg(Hk(i1,i1,1))*en%f(i1,ik_ibz,1)*k%weights(ik_ibz)
   enddo
   ! 
   do ik=RT_ibz_coo(ik_ibz,1),RT_ibz_coo(ik_ibz,2)
     !
     VALUE=VALUE-cI*Vstar_dot_V(SC_bands2,Hk(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1), & 
&                                          G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik))*&  
&                                          k%weights(ik_ibz)/real(RT_Nk_ibz(ik_ibz))
     !
   enddo
 enddo
 !
 if (k%nibz/=nK_mem) call PP_redux_wait(VALUE,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 contains 
   !
   subroutine make_Hk_matrix(ik)
     implicit none
     integer, intent(in) :: ik
     integer :: i1,ik_mem
     !
     ik_mem=ik
     if (k%nibz/=nK_mem) ik_mem=PAR_Xk_ibz_index(ik)
     !
     Hk=(0._SP,0._SP)
     !
     if(present(O_MATRIX)) then
       if(present(TRANS)) then 
          forall(i1=1:n_sp_pol) Hk(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),i1)= &
&                                 transpose(O_MATRIX(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik_mem,i1))
       else
          forall(i1=1:n_sp_pol) Hk(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),i1)= &
&                                 O_MATRIX(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik_mem,i1)
       endif
       !
       do i1=1,SC_bands(1)-1
         Hk(i1,i1,1:n_sp_pol)=O_MATRIX(i1,i1,ik_mem,1:n_sp_pol)       
       enddo
       !
     endif
     !
     if(present(O_QP))     call V_qp_basis_to_H(ik,O_QP,Hk)
     if(present(O_REAL))   call V_real_space_to_H(ik,O_REAL,Hk,wf)
     !
   end subroutine make_Hk_matrix
   !
end subroutine RT_average_operator
