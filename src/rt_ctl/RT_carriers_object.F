! 
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_carriers_object(RT,En,kgrid,WHAT)
 !
 ! This subroutine contains the RT objects, included the RT table
 ! In the future the RT code can be based on those, at least partially
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:n_sp_pol,spin_occ,levels
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:RT_t,REF_lifetime,E_E_Hole_lifetime,E_E_Electron_lifetime,   &
&                         E_P_Hole_lifetime,E_P_Electron_lifetime,E_P_Ho_abs_lifetime, &
&                         E_P_El_abs_lifetime,l_RT_uses_E_rim,RT_ibz_coo,RIM_ibz_coo,  &
&                         RT_nk,RT_nstates,RT_all2ibz,RT_delta0_occupations,RT_Nk_ibz
 !
 implicit none
 !
 ! WHAT=1 allocate and define, 2 deallocate
 !
 type(RT_t),    intent(inout):: RT
 type(levels),  intent(in)   :: En
 type(bz_samp), intent(in)   :: kgrid
 character(*),  intent(in)   :: WHAT
 !
 integer  :: i_RT,i_b,i_k,i_k_RIM,i_k_RT,i_spin
 real(SP) :: Hole_lifetime,Elec_lifetime,f_occ
 !
 select case(WHAT)
   !
   case('allocate')
     !
     ! Allocate and define
     !
     if(.not.associated(RT%kpt))      allocate(RT%kpt(RT_nk,3))
     if(.not.associated(RT%k_weight)) allocate(RT%k_weight(RT_nk))
     if(.not.associated(RT%table))    allocate(RT%table(RT_nstates,4))
     !
     RT%nk=RT_nk
     RT%nb=SC_bands
     RT%nstates=RT_nstates
     !
     if(.not.associated(RT%E_bare))      allocate(RT%E_bare(RT_nstates)) 
     if(.not.associated(RT%f_bare))      allocate(RT%f_bare(RT_nstates))
     if(.not.associated(RT%GAMMA_bare))  allocate(RT%GAMMA_bare(RT_nstates))
     !
     RT%E_bare=0.
     RT%f_bare=0.
     RT%GAMMA_bare=0.
     !
     do i_k=1,kgrid%nibz
       if(     l_RT_uses_E_RIM) then
         RT%kpt(RT_ibz_coo(i_k,1):RT_ibz_coo(i_k,2),:)= kgrid%ptbz_RIM(RIM_ibz_coo(i_k,1):RIM_ibz_coo(i_k,2),:)
         RT%k_weight(RT_ibz_coo(i_k,1):RT_ibz_coo(i_k,2)) = kgrid%weights(i_k)/real(RT_Nk_ibz(i_k),SP)        
       endif
       if(.not.l_RT_uses_E_RIM) then
         RT%kpt(i_k,:)= kgrid%pt(i_k,:)
         RT%k_weight(i_k) = kgrid%weights(i_k)
       endif
     enddo
     !
     call build_RT_table()
     !
     do i_RT=1,RT%nstates
       !
       i_b    =RT%table(i_RT,1)
       i_k    =RT%table(i_RT,2)
       i_k_RT =RT%table(i_RT,3)
       i_spin =RT%table(i_RT,4)
       !
       i_k_RIM=i_k_RT+RIM_ibz_coo(i_k,1)-RT_ibz_coo(i_k,1)
       !
       if(l_RT_uses_E_RIM) then
         RT%E_bare(i_RT)=En%E_RIM(i_b,i_k_RIM,i_spin)
         RT%f_bare(i_RT)=En%f_RIM(i_b,i_k_RIM,i_spin)
       else
         RT%E_bare(i_RT)=En%E(i_b,i_k,i_spin)
         RT%f_bare(i_RT)=En%f(i_b,i_k,i_spin)
       endif
       if(allocated(REF_lifetime)) RT%GAMMA_bare(i_RT)=sum(REF_lifetime(i_b,i_k_RT,:))
       !
     enddo
     !
   case('fill')
     !
     if(.not.associated(RT%delta_E))      allocate(RT%delta_E(RT_nstates))   
     if(.not.associated(RT%delta_f))      allocate(RT%delta_f(RT_nstates))  
     if(.not.associated(RT%delta_GAMMA))  allocate(RT%delta_GAMMA(RT_nstates)) 
     if(.not.associated(RT%NEQ_Lifetime)) allocate(RT%NEQ_Lifetime(RT_nstates)) 
     !
     RT%delta_E=0.
     RT%delta_f=0.
     RT%delta_GAMMA=0.
     RT%NEQ_Lifetime =0.
     !
     Hole_lifetime=0._SP
     Elec_lifetime=0._SP
     !
     do i_RT=1,RT%nstates
       !
       i_b    =RT%table(i_RT,1)
       i_k    =RT%table(i_RT,2)
       i_k_RT =RT%table(i_RT,3)
       i_spin =RT%table(i_RT,4)
       !
       i_k_RIM=i_k_RT+RIM_ibz_coo(i_k,1)-RT_ibz_coo(i_k,1)
       !
       RT%delta_f(i_RT)=RT_delta0_occupations(i_b,i_k_RT)
       !
       if(l_elel_scatt) then
         Hole_lifetime=E_E_Hole_lifetime(i_b,i_k_RT)
         Elec_lifetime=E_E_Electron_lifetime(i_b,i_k_RT)
       endif
       !
       if(l_elph_scatt) then
         Hole_lifetime=Hole_lifetime+E_P_Hole_lifetime(i_b,i_k_RT)    +E_P_Ho_abs_lifetime(i_b,i_k_RT)
         Elec_lifetime=Elec_lifetime+E_P_Electron_lifetime(i_b,i_k_RT)+E_P_El_abs_lifetime(i_b,i_k_RT)
       endif
       !
       RT%delta_GAMMA(i_RT)=  Hole_lifetime+Elec_lifetime
       !
       if(l_elel_scatt.or.l_elph_scatt) then
         Hole_lifetime=Hole_lifetime+REF_lifetime(i_b,i_k_RT,1)+REF_lifetime(i_b,i_k_RT,3)+REF_lifetime(i_b,i_k_RT,5)
         Elec_lifetime=Elec_lifetime+REF_lifetime(i_b,i_k_RT,2)+REF_lifetime(i_b,i_k_RT,4)+REF_lifetime(i_b,i_k_RT,6)
       endif
       f_occ=(RT%f_bare(i_RT)+RT%delta_f(i_RT))/spin_occ
       !
       if(f_occ>0.) then
         RT%NEQ_Lifetime(i_RT)=  (Hole_lifetime+Elec_lifetime) - Hole_lifetime / f_occ
       else
         RT%NEQ_Lifetime(i_RT)=  (Hole_lifetime+Elec_lifetime)
       endif
       !
     enddo
     !
   case('free')
     !
     ! Free
     !
     RT%nk=0
     RT%nb=0
     RT%nstates=0
     if(associated(RT%E_bare))      deallocate(RT%E_bare)
     if(associated(RT%f_bare))      deallocate(RT%f_bare)
     if(associated(RT%GAMMA_bare))      deallocate(RT%GAMMA_bare)
     if(associated(RT%delta_E))      deallocate(RT%delta_E)
     if(associated(RT%delta_f))      deallocate(RT%delta_f)
     if(associated(RT%delta_GAMMA))      deallocate(RT%delta_GAMMA)
     if(associated(RT%NEQ_Lifetime))       deallocate(RT%NEQ_Lifetime)
     if(associated(RT%kpt))       deallocate(RT%kpt)
     if(associated(RT%k_weight))      deallocate(RT%k_weight)
     if(associated(RT%table))   deallocate(RT%table)
     !
 end select
 !
 contains
   !
   subroutine build_RT_table()
     !
     i_RT=0
     !
     do i_k_RT=1,RT%nk
       do i_b=RT%nb(1),RT%nb(2)
         do i_spin=1,n_sp_pol
           i_RT=i_RT+1
           RT%table(i_RT,1)=i_b
           RT%table(i_RT,2)=RT_all2ibz(i_k_RT)
           RT%table(i_RT,3)=i_k_RT
           RT%table(i_RT,4)=i_spin
           !
         enddo
       enddo
     enddo
     !
   end subroutine
   !
end subroutine RT_carriers_object

