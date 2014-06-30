!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine RT_G_retarded_EP_initialize(E,k)
 !
 use pars,           ONLY:rZERO,SP,IP4,pi
 use R_lattice,      ONLY:nkibz,nqbz,bz_samp,qindx_S
 use electrons,      ONLY:levels,BZ_RIM_tot_nkpts,BZ_RIM_nkpt,BZ_RIM_table
 use SC,             ONLY:SC_bands
 use LIVE_t,         ONLY:live_timing
 use real_time,      ONLY:B_mat_index,Gr_ep_matrix,&
&                         l_RT_uses_E_RIM,Gr_kind
 use memory_m,       ONLY:mem_est
 use parallel_m,     ONLY:PAR_IND_B_mat,PAR_Xk_nibz,PAR_nQ,PAR_n_B_mat_elements,PAR_IND_Xk_ibz,&
&                         PAR_Xk_ibz_index,PAR_IND_Q,PAR_B_mat_index,PAR_Q_index
 use ELPH,           ONLY:ph_modes,ph_freqs_sq
 !
 implicit none
 !
 type(levels)  :: E
 type(bz_samp) :: k
 !
 ! Work Space
 !
 real(SP):: GAMMA_,E_center
 integer :: ik,iqbz,i_n,i_np,ik_mem,i_b_mat_mem,iq_mem,ik_m_q,il,is,i_trans(2),kin_scatt(3),&
&           ik_rim_1,ik_rim_2,i1,i2
 real(SP), allocatable :: Gr_integrand(:)
 real(SP),    external :: Laplace_transform,HyperbolicSecant_FT
 real(SP):: TRANSITIONS_E((SC_bands(2)-SC_bands(1)+1)*nkibz)
 logical :: eval_it
 integer(IP4)          :: TRANSITIONS_table((SC_bands(2)-SC_bands(1)+1)*nkibz,&
&                                           (SC_bands(2)-SC_bands(1)+1)*nkibz,3)
 !
 ! Allocation
 !
 call live_timing('[Gret] Electron-Phonon:',PAR_Xk_nibz*PAR_n_B_mat_elements*PAR_nQ)
 !
 allocate(Gr_ep_matrix(PAR_Xk_nibz,PAR_n_B_mat_elements,PAR_nQ,2*ph_modes))
 Gr_ep_matrix=rZERO
 call mem_est("Gr_ep_matrix",(/size(Gr_ep_matrix)/))
 !
 do ik=1,nkibz
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_Xk_ibz_index(ik)
   !
   do iqbz=1,nqbz
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
     iq_mem=PAR_Q_index(iqbz)
     !
     ik_m_q =k%sstar(qindx_S(ik,iqbz,1),1)
     !
     do i_n=SC_bands(1),SC_bands(2)
       !
       do i_np=SC_bands(1),SC_bands(2)
         !
         if ( .not. PAR_IND_B_mat%element_1D( B_mat_index(i_n,i_np) ) ) cycle
         i_b_mat_mem=PAR_B_mat_index(i_n,i_np)
         !
         do il=1,ph_modes
           !
           do is=-1,1,2
             !
             E_center=(E%E(i_n,ik,1)-E%E(i_np,ik_m_q,1))+is*abs(sqrt(ph_freqs_sq(iqbz,il)))
             !
             if (.not.Energy_Condition(E%E(i_n,ik,1)-E%E(i_np,ik_m_q,1),E_center)) cycle
             !
             GAMMA_ = abs(E%W(i_n,ik,1))+abs(E%W(i_np,ik_m_q,1))
             !
             if(trim(Gr_kind)=='QP') Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)=GAMMA_/(E_center**2+GAMMA_**2)
             !
             if(trim(Gr_kind)=='HS')&
               Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)=HyperbolicSecant_FT(E_center,GAMMA_,1)
             !
             !DEBUG>
             !write (100,*) E_center*27.21, Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes),GAMMA_*27.21
             !DEBUG<
             !
           enddo
           !
         enddo
         !
         call live_timing(steps=1)
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 call live_timing()
 !
 contains
   !
   logical function Energy_Condition(E1,E2,Etot,Egap)
     real(SP) :: E1,E2,Etot,Egap
     Energy_Condition=.true.
     if ( abs(E1)  <tol2     ) Energy_Condition=.false.      ! C1a
     if ( abs(E2)  <tol2     ) Energy_Condition=.false.      ! C1b
     if ( abs(Etot)<tol2     ) Energy_Condition=.false.      ! C2
     if ( abs(Etot)>2.*Egap  ) Energy_Condition=.false.      ! C3
     if (    E1/E2 >0._SP    ) Energy_Condition=.false.      ! C4a+C4b
   end function
   !
end subroutine RT_G_retarded_EP_initialize

