!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP
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
subroutine RT_PHEL_TSQP_Kernel(ik,ik_m_q,i_q,i_l,E,ph_freq,Gkkp_sq)
 !
 ! This subroutine is the same as RT_ELPH_CCA_Kernel, except it also
 ! computes the phonon-electron lifetimes needed for the collision
 ! integral in the phonon dynamics case.
 !
 use pars,           ONLY:SP,zero_dfl,rZero,pi
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,n_full_bands,spin_occ
 use hamiltonian,    ONLY:B_mat_index
 use real_time,      ONLY:l_RT_uses_E_DbGd,DbGd_ibz_coo,RT_occupations_locked,&
&                         RT_H_occupations_reference,RT_E_occupations_reference,&
&                         RT_life_delta_occupations,l_initial_equilibrium,&
&                         RT_eh_en,RT_ibz_coo,RT_Nk_ibz,RT_bands
use PH_real_time,    ONLY:RT_PH_occupations,RT_PH_occupations_reference,&
&                         P_E_sp_res_lifetime,P_E_sp_ares_lifetime,P_E_eh_lifetime
 use parallel_m,     ONLY:PAR_IND_B_mat
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 integer,       intent(in) :: ik,ik_m_q,i_q,i_l
 real(SP),      intent(in) :: ph_freq
 real(SP),      intent(in) :: Gkkp_sq(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 real(SP), external        :: RT_G_retarded_convolution
 !
 ! Work Space ...
 !
 integer  :: i_n,i_np,ik_rim,ip_rim,i1,i2
 real(SP) :: D_res,D_ares,DELTA_E,DELTA_W,pre_factor,f_nkq,Hole_f_nkq,f_mk,Hole_f_mk,&
&            TMP_Res_lifetime,TMP_Ares_lifetime,TMP_eh_lifetime,N_q(2)
 !
 if (l_initial_equilibrium) then
   N_q    = (RT_PH_occupations_reference(i_l,i_q)+(/0._SP,spin_occ/))/spin_occ
 else
   N_q    = (RT_PH_occupations(i_l,i_q)+(/0._SP,spin_occ/))/spin_occ
 endif
 !
 ! Kernel kind
 !=============
 !
 do i_n=RT_bands(1),RT_bands(2)
   !
   do i_np=RT_bands(1),RT_bands(2)
     !
     ! Extra condition: the EP scattering cannot cross the gap
     !
     if ((i_n<=n_full_bands.and.i_np>  n_full_bands).and.(E%E_ind_gap(1)>0.1/HA2EV)) cycle
     if ((i_n> n_full_bands.and.i_np<= n_full_bands).and.(E%E_ind_gap(1)>0.1/HA2EV)) cycle
     !
     if ( .not. PAR_IND_B_mat%element_1D( B_mat_index(i_n,i_np,RT_bands) ) ) cycle
     !
     if (.not. l_RT_uses_E_DbGd) then
       !
       if(E%E(i_np,ik_m_q,1)<RT_eh_en(1) .or. E%E(i_np,ik_m_q,1)>RT_eh_en(2)) cycle
       if(E%E(i_n ,ik    ,1)<RT_eh_en(1) .or. E%E(i_n ,ik    ,1)>RT_eh_en(2)) cycle
       !
       DELTA_E=    E%E(i_n,ik,1) -    E%E(i_np,ik_m_q,1)
       DELTA_W=abs(E%W(i_n,ik,1))+abs(E%W(i_np,ik_m_q,1))
       !
     endif
     ! [NOTE] Full prefactor here is 4._SP*pi. Only for now I put 2._SP *assuming* the rest comes from the delta function
     pre_factor=2._SP*Gkkp_sq(i_np,i_n)/real(RT_Nk_ibz(ik)) !Full prefactor should be 4*pi
     !
     if( pre_factor==0._SP ) cycle
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       if (RT_occupations_locked(i_n,ik_rim)) cycle
       !
       ! Zeroing
       !=========
       TMP_Res_lifetime=rZero
       TMP_Ares_lifetime=rZero
       TMP_eh_lifetime=rZero
       !
       do ip_rim=RT_ibz_coo(ik_m_q,1),RT_ibz_coo(ik_m_q,2)
         !
         if (RT_occupations_locked(i_np,ip_rim)) cycle
         !
         if (l_RT_uses_E_DbGd) then
           !
           i1=ik_rim-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
           i2=ip_rim-RT_ibz_coo(ik_m_q,1)+DbGd_ibz_coo(ik_m_q,1)
           !
           if(E%E_DbGd(i_np,i2,1)<RT_eh_en(1) .or. E%E_DbGd(i_np,i2,1)>RT_eh_en(2)) cycle
           if(E%E_DbGd(i_n ,i1,1)<RT_eh_en(1) .or. E%E_DbGd(i_n ,i1,1)>RT_eh_en(2)) cycle
           !
           DELTA_E=    E%E_DbGd(i_n,i1,1) -    E%E_DbGd(i_np,i2,1)
           DELTA_W=abs(E%W_DbGd(i_n,i1,1))+abs(E%W_DbGd(i_np,i2,1))
           !
         endif
         !
         ! Delta functions [NOTE] I need to check that this function does what I want
         D_ares  =RT_G_retarded_convolution(DELTA_E,+ph_freq,DELTA_W,0._SP,'EP')
         D_res =RT_G_retarded_convolution(DELTA_E,-ph_freq,DELTA_W,0._SP,'EP')
         !
         if(l_initial_equilibrium) then
           f_nkq      = RT_E_occupations_reference(i_np,ip_rim)/spin_occ
           Hole_f_nkq = RT_H_occupations_reference(i_np,ip_rim)/spin_occ
           f_mk       = RT_E_occupations_reference(i_n,ik_rim)/spin_occ
           Hole_f_mk  = RT_H_occupations_reference(i_n,ik_rim)/spin_occ
         else
           f_nkq      =  RT_life_delta_occupations(i_np,ip_rim)/spin_occ
           Hole_f_nkq = -RT_life_delta_occupations(i_np,ip_rim)/spin_occ
           f_mk       =  RT_life_delta_occupations(i_n,ik_rim)/spin_occ
           Hole_f_mk  = -RT_life_delta_occupations(i_n,ik_rim)/spin_occ
         endif
         !
         TMP_Res_lifetime=TMP_Res_lifetime+Hole_f_nkq*f_mk*D_res
         TMP_Ares_lifetime=TMP_Ares_lifetime+f_nkq*Hole_f_mk*D_ares
         !
         if( abs(N_q(1))>zero_dfl ) then
           TMP_eh_lifetime=TMP_eh_lifetime+(f_nkq-f_mk)*(D_ares-D_res)
         endif
         !
       enddo
       !
     enddo
     !
     ! Spontaneous phonon emission (resonant and antiresonant)
     P_E_sp_res_lifetime(i_l,i_q)=P_E_sp_res_lifetime(i_l,i_q)+&
&                                    TMP_Res_lifetime*pre_factor
     P_E_sp_ares_lifetime(i_l,i_q)=P_E_sp_ares_lifetime(i_l,i_q)+&
&                                    TMP_Ares_lifetime*pre_factor
     !
     if(.not.abs(N_q(1))>zero_dfl) cycle
     !
     ! Stimulated Emission / absorption
     P_E_eh_lifetime(i_l,i_q)=P_E_eh_lifetime(i_l,i_q)+&
&                                    TMP_eh_lifetime*pre_factor
     !
   enddo
   !
 enddo
 !
end subroutine RT_PHEL_TSQP_Kernel
