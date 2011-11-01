!
! Copyright (C) 2000-2011 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_average_operator(VALUE,O_REAL,O_QP,O_MATRIX,G,k,en,TRANS,USE_ERIM)
 !
 use pars,           ONLY:SP,cI
 use SC,             ONLY:SC_bands
 use R_lattice,      ONLY:bz_samp 
 use QP_m,           ONLY:QP_nk,QP_n_states
 use electrons,      ONLY:n_sp_pol,levels,BZ_RIM_table,BZ_RIM_nkpt,BZ_RIM_nbands
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:wf
 use wrapper,        ONLY:Vstar_dot_V
 use X_m,            ONLY:use_X_RIM
 use real_time,      ONLY:NE_time
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(in) :: G(SC_bands(2),SC_bands(2),QP_nk)
 complex(SP),  intent(out) :: VALUE
 complex(SP),     optional, intent(in) :: O_MATRIX(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol)
 complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
 real(SP),        optional, intent(in) :: O_REAL(fft_size,n_sp_pol)
 logical,         optional, intent(in) :: TRANS,USE_ERIM
 type(levels),    optional, intent(in) :: en
 !
 ! Work Space
 !
 integer     :: i1,i2,i3,ikbz,ik,ik_rand,SC_bands2
 complex(SP) :: Hk(SC_bands(2),SC_bands(2),n_sp_pol)
 complex(SP) :: G_k_rand
 complex(SP) :: AVE_k_VALUE
 type(pp_indexes) :: px
 !
 SC_bands2=SC_bands(2)**2
 VALUE=(0._SP,0._SP)
 !
 ! If not operator are present the subroutine returns the trace of the Green Function
 ! 
 if(.not.present(O_MATRIX).and..not.present(O_REAL).and..not.present(O_QP)) then
   !
   ! Total number of electrons
   !
   do i1=1,SC_bands(2)
     VALUE=VALUE-cI*sum(G(i1,i1,:)*k%weights(:))
   enddo
   !
   return
   !
 endif 
 !
 call pp_indexes_reset(px)
 !
 if(present(USE_ERIM).and.use_X_RIM) then
   !
   call par_indexes(px,(/k%nbz/))
   !
   do ikbz=1,k%nbz
     !
     if (.not.px%element_1D(ikbz)) cycle   
     !
     ik=k%sstar(ikbz,1)
     call make_Hk_matrix(ik)
     !
     AVE_k_VALUE=(0.,0.)
     !
     do i1=1,min(SC_bands(2),BZ_RIM_nbands)
       do i2=i1+1,min(SC_bands(2),BZ_RIM_nbands)
         G_k_rand=(0.,0.)
         do i3=1,BZ_RIM_nkpt(ikbz)
            ik_rand=BZ_RIM_table(ikbz,i3)
            G_k_rand= G_k_rand+exp((0._SP,1._SP)* &
&                   ((en%Eo(i1,ik,1)-en%Eo(i2,ik,1))-(en%E_RIM(i1,ik_rand,1)-en%E_RIM(i2,ik_rand,1)))*NE_time)
         enddo
         !
         AVE_k_VALUE=AVE_k_VALUE+aimag(conjg(Hk(i1,i2,1))*G_k_rand*G(i1,i2,ik))
         !
       enddo
     enddo
     !
     AVE_k_VALUE=AVE_k_VALUE/real(BZ_RIM_nkpt(ikbz))
     !
     ! Terms between BZ_RIM_nbands and SC_bands(2)
     !
     do i1=BZ_RIM_nbands+1,SC_bands(2)
       do i2=i1+1,SC_bands(2)
         AVE_k_VALUE=AVE_k_VALUE+aimag(conjg(Hk(i1,i2,1))*G(i1,i2,ik))
       enddo
     enddo
     !
     do i1=1,SC_bands(2)
       do i2=max(BZ_RIM_nbands,i1)+1,SC_bands(2)
         AVE_k_VALUE=AVE_k_VALUE+aimag(conjg(Hk(i1,i2,1))*G(i1,i2,ik))
       enddo
     enddo
     !
     ! Off-diagonal elements counted 2 times (i,j) and (j,i)
     AVE_k_VALUE=(0.,2._SP)*AVE_k_VALUE
     !
     ! Diagonal
     !
     do i1=1,SC_bands(2)
         AVE_k_VALUE=AVE_k_VALUE+(0._SP,1._SP)*aimag(conjg(Hk(i1,i1,1))*G(i1,i1,ik))
     enddo     
     !
     VALUE=VALUE-cI*AVE_k_VALUE
     !
   enddo
   !
   VALUE=VALUE/real(k%nbz)
   !
 else
   !
   call par_indexes(px,(/QP_nk/))
   !
   do ik=1,QP_nk
     ! 
     if (.not.px%element_1D(ik)) cycle   
     !
     call make_Hk_matrix(ik)
     !
     VALUE=VALUE-cI*Vstar_dot_V(SC_bands2,Hk(1:SC_bands(2),1:SC_bands(2),1),G(1:SC_bands(2),1:SC_bands(2),ik))*k%weights(ik)
     !
   enddo
   !
 endif
 !
 call pp_redux_wait(VALUE)
 call pp_indexes_reset(px)
 !
 contains 
   !
   subroutine make_Hk_matrix(ik)
     implicit none
     integer, intent(in) :: ik
     !
     Hk=(0._SP,0._SP)
     !
     if(present(O_MATRIX)) then
       if(present(TRANS)) then 
          forall(i1=1:n_sp_pol) Hk(1:SC_bands(2),1:SC_bands(2),i1)=transpose(O_MATRIX(1:SC_bands(2),1:SC_bands(2),ik,i1))
       else
          forall(i1=1:n_sp_pol) Hk(1:SC_bands(2),1:SC_bands(2),i1)=O_MATRIX(1:SC_bands(2),1:SC_bands(2),ik,i1)
       endif
     endif
     !
     if(present(O_QP))     call V_qp_basis_to_H(ik,O_QP,Hk)
     if(present(O_REAL))   call V_real_space_to_H(ik,O_REAL,Hk,wf)
     !
   end subroutine make_Hk_matrix
   !
end subroutine RT_average_operator
