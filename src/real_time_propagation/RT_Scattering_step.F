!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_Scattering_step(dG_old,dG_inp,dG_out,ik,deltaT)
 !
 ! The propagation of the scattering part is done in terms of electrons and holes occupations.
 ! The lifetimes used (computed by RT_relaxation) are all constructed in terms of f_e and f_h
 !
 ! Thus I propagate the following equation (with f_h=1-f_e):
 !    \partial_t f_e = -GAMMA_e*f_e + GAMMA_h*f_h
 !    \partial_t f_h = +GAMMA_e*f_e - GAMMA_h*f_h
 !
 ! The two equations are identical but have, numerically, different sensitivity.
 ! They are rewritten in a form which is similar to the RTA
 !    \partial_t f_e = -(GAMMA_e+GAMMA_h)*(f_e - f_inf_e)
 !    \partial_t f_h = -(GAMMA_e+GAMMA_h)*(f_h - f_inf_h)
 ! Moreover splitting the terms to keep sensitivity (only electron channel here)
 !    \partial_t df_e = -(GAMMA0_e+GAMMA0_h)*df_e -(GAMMA0_e+GAMMA0_h)*(f0_e-f_inf_e)
 !                      -(dGAMMA_e+dGAMMA_h)*df_e -(dGAMMA_e+dGAMMA_h)*(f0_e-f_inf_e)
 ! with f_i=f0_i+df_i and GAMMA_i=GAMMA0_i+dGAMMA_i for i=e/h
 !
 ! NOTICE:
 ! - The equation is always solved with the EULER step
 ! - The reference occupations are updated during the dynamics and so the delta occupations,
 !   unless the relaxation time approximation is used
 !
 ! Only after the G lesser has been propagated, the occupations are updated
 ! Here remember that the occupations stored in the module are always equal to dG_inp
 ! 
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:spin_occ
 use RT_lifetimes,   ONLY:RT_EP_life,RT_EE_life,RT_EH_life,REF_lifetime
 use real_time,      ONLY:RT_bands,RTibz,l_elel_scatt,l_elph_scatt,l_elphoton_scatt,  &
&                         l_RT_relax_time_approx,RT_RTA_E_occ_infty,RT_RTA_H_occ_infty,&
&                         RT_E_occupations,RT_H_occupations,l_RT_CCA_Kernel, &
&                         RT_delta_occupations,l_RT_include_eq_scatt,&
&                         RT_is_statically_dephased,RT_is_dynamically_dephased,RT_Deph_Matrix,G_lesser_reference
 !
 implicit none
 !
 complex(SP), intent(in)    :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)    :: dG_inp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(inout) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 integer,     intent(in)    :: ik
 real(SP),    intent(in)    :: deltaT
 !
 integer     :: ib
 real(SP)    :: f_(2),df(2),f0(2),df_out(2),f_inf(2),&
&               GAMMA_(2),dGAMMA(2),GAMMA0(2),GAMMA__TOT,dGAMMA_TOT,GAMMA0_TOT
 !
 if( .not. RT_is_dynamically_dephased) return
 !
 do ib=RT_bands(1),RT_bands(2)
   !
   ! 1=electrons
   ! 2=holes
   !
   f_(1)=RT_E_occupations(ib,ik)
   f_(2)=RT_H_occupations(ib,ik)
   !
   f0(1)=         aimag(G_lesser_reference(ib,ib,ik))
   f0(2)=spin_occ-aimag(G_lesser_reference(ib,ib,ik))
   df(1)= RT_delta_occupations(ib,ik)
   df(2)=-RT_delta_occupations(ib,ik)
   !
   ! The GAMMA0 term could be moved inside RT_apply_RWA.F
   ! For the moment I keep it here, together with the rest of the relaxation term
   !
   GAMMA0(1)=REF_lifetime(ib,ik,2)+REF_lifetime(ib,ik,4)+REF_lifetime(ib,ik,6)+REF_lifetime(ib,ik,8)
   GAMMA0(2)=REF_lifetime(ib,ik,1)+REF_lifetime(ib,ik,3)+REF_lifetime(ib,ik,5)+REF_lifetime(ib,ik,7)
   !
   dGAMMA(1)=0._SP
   dGAMMA(2)=0._SP
   if(.not.l_RT_relax_time_approx) then
     if (RT_EE_life%active) then
       dGAMMA(1)=RT_EE_life%SPONT_emit(ib,ik)
       dGAMMA(2)=RT_EE_life%SPONT_abs(ib,ik)
     endif
     if (RT_EH_life%active) then
       dGAMMA(1)=dGAMMA(1)+RT_EH_life%SPONT_emit(ib,ik)
       dGAMMA(2)=dGAMMA(2)+RT_EH_life%SPONT_abs(ib,ik)
     endif
     if (RT_EP_life%active) then
       dGAMMA(1)=dGAMMA(1)+ RT_EP_life%SPONT_emit(ib,ik)+RT_EP_life%STIM_abs(ib,ik)
       dGAMMA(2)=dGAMMA(2)+ RT_EP_life%SPONT_abs(ib,ik) +RT_EP_life%STIM_emit(ib,ik)
     endif
   endif
   !
   if (RT_is_statically_dephased) GAMMA0(1)=GAMMA0(1)+RT_Deph_Matrix(ib,ib,RTibz%k_map(ik))
   !
   GAMMA_(1)=GAMMA0(1)+dGAMMA(1)
   GAMMA_(2)=GAMMA0(2)+dGAMMA(2)
   !
   dGAMMA_TOT=dGAMMA(1)+dGAMMA(2)
   GAMMA0_TOT=GAMMA0(1)+GAMMA0(2)
   GAMMA__TOT=GAMMA_(1)+GAMMA_(2)
   !
   if(.not.l_RT_relax_time_approx) then
     !
     ! Option A: Standard definition, however not compatible with RTA
     !
     if(l_RT_include_eq_scatt) then
       !        A1 with    gamma0*f0 terms
       df_out(1) = -deltaT*(GAMMA_(1)*f_(1)-GAMMA_(2)*f_(2))
     else
       !        A2 without gamma0*f0 terms
       df_out(1) = -deltaT*( (GAMMA0(1)*df(1)-GAMMA0(2)*df(2))+ &
&                            (dGAMMA(1)*f0(1)-dGAMMA(2)*f0(2))+ &
&                            (dGAMMA(1)*df(1)-dGAMMA(2)*df(2))  )
     endif
     !
     dG_out(ib,ib) = dG_out(ib,ib) + cI*df_out(1)
     !
     ! DEBUG<
     !! Option B: Written in the same form of RTA
     !if( GAMMA__TOT> 0._SP  ) then
     !  f_inf(1)= GAMMA_(2)/GAMMA__TOT*spin_occ
     !  f_inf(2)= GAMMA_(1)/GAMMA__TOT*spin_occ
     !endif
     !if( GAMMA__TOT<=0._SP  ) then
     !  f_inf(1)=         aimag(G_lesser_reference(ib,ib,ik))
     !  f_inf(2)=spin_occ-aimag(G_lesser_reference(ib,ib,ik))
     !endif
     !df_out(:) = -deltaT*(GAMMA0_TOT*df(:) + GAMMA0_TOT*(f0(:)-f_inf(:)) + &
     !&                    dGAMMA_TOT*df(:) + dGAMMA_TOT*(f0(:)-f_inf(:)) )
     !!
     !if (f0(1)<=0.5_SP*spin_occ) dG_out(ib,ib) = dG_out(ib,ib) + cI*df_out(1)   ! electron channel
     !if (f0(1)> 0.5_SP*spin_occ) dG_out(ib,ib) = dG_out(ib,ib) - cI*df_out(2)   ! hole     channel
     ! DEBUG>
     !
   else
     !
     if (RT_is_statically_dephased) GAMMA0_TOT=GAMMA0_TOT+RT_Deph_Matrix(ib,ib,RTibz%k_map(ik))
     !
     f_inf(1)= RT_RTA_E_occ_infty(ib,ik)
     f_inf(2)= RT_RTA_H_occ_infty(ib,ik)
     !
     df_out(:) = -deltaT*(GAMMA0_TOT*df(:) + GAMMA0_TOT*(f0(:)-f_inf(:)) + &
&                         dGAMMA_TOT*df(:) + dGAMMA_TOT*(f0(:)-f_inf(:)) )
     !
     if (f0(1)<=0.5_SP*spin_occ) dG_out(ib,ib) = dG_out(ib,ib) + cI*df_out(1)   ! electron channel
     if (f0(1)> 0.5_SP*spin_occ) dG_out(ib,ib) = dG_out(ib,ib) - cI*df_out(2)   ! hole     channel
     !
   endif
   !
 enddo
 !
end subroutine RT_Scattering_step
