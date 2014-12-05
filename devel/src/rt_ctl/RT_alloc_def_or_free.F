! 
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine RT_alloc_def_or_free(RT_obj,En,kgrid,action_kind)
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
 ! action_kind=1 allocate and define, 2 deallocate
 !
 type(RT_t),    intent(inout):: RT_obj
 type(levels),  intent(in)   :: En
 type(bz_samp), intent(in)   :: kgrid
 integer,       intent(in)   :: action_kind
 !
 integer  :: i_RT,i_b,i_k,i_k_RIM,i_k_RT,i_spin
 real(SP) :: Hole_lifetime,Elec_lifetime,f_occ
 !
 select case(action_kind)
   case(1)
     !
     ! Allocate and define
     !
     if(.not.associated(RT_obj%k))      allocate(RT_obj%k(RT_nk,3))
     if(.not.associated(RT_obj%wk))     allocate(RT_obj%wk(RT_nk))
     if(.not.associated(RT_obj%table))  allocate(RT_obj%table(RT_nstates,4))   ! ib,ik_RT,ik,i_spin
     !
     RT_obj%nk=RT_nk
     RT_obj%nb=SC_bands
     RT_obj%nstates=RT_nstates
     !
     if(.not.associated(RT_obj%E0))  allocate(RT_obj%E0(RT_nstates))    ! equilibrium energy, i.e. the QP energy
     if(.not.associated(RT_obj%f0))  allocate(RT_obj%f0(RT_nstates))    ! equilibrium occupations
     if(.not.associated(RT_obj%W0))  allocate(RT_obj%W0(RT_nstates))    ! "initial equilibrium" lifetimes. In the code the "initial equilibrium"
                                                                        ! lifetimes differ from the equilibrium lifetimes. This should be overcome ...
     !
     RT_obj%E0=0.
     RT_obj%f0=0.
     RT_obj%W0=0.
     !
     do i_k=1,kgrid%nibz
       if(     l_RT_uses_E_RIM) then
         RT_obj%k(RT_ibz_coo(i_k,1):RT_ibz_coo(i_k,2),:)= kgrid%ptbz_RIM(RIM_ibz_coo(i_k,1):RIM_ibz_coo(i_k,2),:)
         RT_obj%wk(RT_ibz_coo(i_k,1):RT_ibz_coo(i_k,2)) = kgrid%weights(i_k)/real(RT_Nk_ibz(i_k),SP)        
       endif
       if(.not.l_RT_uses_E_RIM) then
         RT_obj%k(i_k,:)= kgrid%pt(i_k,:)
         RT_obj%wk(i_k) = kgrid%weights(i_k)
       endif
     enddo
     !
     call build_RT_table()
     !
     do i_RT=1,RT_obj%nstates
       !
       i_b    =RT_obj%table(i_RT,1)
       i_k    =RT_obj%table(i_RT,2)
       i_k_RT =RT_obj%table(i_RT,3)
       i_spin =RT_obj%table(i_RT,4)
       !
       i_k_RIM=i_k_RT+RIM_ibz_coo(i_k,1)-RT_ibz_coo(i_k,1)
       !
       if(l_RT_uses_E_RIM) then
         RT_obj%E0(i_RT)=En%E_RIM(i_b,i_k_RIM,i_spin)
         RT_obj%f0(i_RT)=En%f_RIM(i_b,i_k_RIM,i_spin)
         !RT_obj%W0(i_RT)=En%W_RIM(i_b,i_k_RIM,i_spin)  this is not the case because of the difference equilibrium / initial equilibrium
       else
         RT_obj%E0(i_RT)=En%E(i_b,i_k,i_spin)
         RT_obj%f0(i_RT)=En%f(i_b,i_k,i_spin)
         !RT_obj%W0(i_RT)=En%W(i_b,i_k,i_spin)  this is not the case because of the difference equilibrium / initial equilibrium
       endif
       if(allocated(REF_lifetime)) RT_obj%W0(i_RT)=sum(REF_lifetime(i_b,i_k_RT,:))
       !
     enddo
     !
   case(2)
     !
     if(.not.associated(RT_obj%dE))  allocate(RT_obj%dE(RT_nstates))    ! Here I could insert the BGR
     if(.not.associated(RT_obj%df))  allocate(RT_obj%df(RT_nstates))    ! Here the change in the occupations
     if(.not.associated(RT_obj%dW))  allocate(RT_obj%dW(RT_nstates))    ! Here the change with respect to the "initial equilibrium" lifetimes
     if(.not.associated(RT_obj%W))   allocate(RT_obj%W(RT_nstates))     ! Here the time dependent lifetime defined as for Silicon
     !
     RT_obj%dE=0.
     RT_obj%df=0.
     RT_obj%dW=0.
     RT_obj%W =0.
     !
     Hole_lifetime=0._SP
     Elec_lifetime=0._SP
     !
     do i_RT=1,RT_obj%nstates
       !
       i_b    =RT_obj%table(i_RT,1)
       i_k    =RT_obj%table(i_RT,2)
       i_k_RT =RT_obj%table(i_RT,3)
       i_spin =RT_obj%table(i_RT,4)
       !
       i_k_RIM=i_k_RT+RIM_ibz_coo(i_k,1)-RT_ibz_coo(i_k,1)
       !
       !RT_obj%dE(i_RT)=sum of all BGR
       RT_obj%df(i_RT)=RT_delta0_occupations(i_b,i_k_RT)
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
       RT_obj%dW(i_RT)=  Hole_lifetime+Elec_lifetime
       !
       if(l_elel_scatt.or.l_elph_scatt) then
         Hole_lifetime=Hole_lifetime+REF_lifetime(i_b,i_k_RT,1)+REF_lifetime(i_b,i_k_RT,3)+REF_lifetime(i_b,i_k_RT,5)
         Elec_lifetime=Elec_lifetime+REF_lifetime(i_b,i_k_RT,2)+REF_lifetime(i_b,i_k_RT,4)+REF_lifetime(i_b,i_k_RT,6)
       endif
       f_occ=(RT_obj%f0(i_RT)+RT_obj%df(i_RT))/spin_occ
       !
       if(f_occ>0.) then
         RT_obj%W(i_RT)=  (Hole_lifetime+Elec_lifetime) - Hole_lifetime / f_occ
       else
         RT_obj%W(i_RT)=  (Hole_lifetime+Elec_lifetime)
       endif
       !
     enddo
     !
   case(3)
     !
     ! Free
     !
     RT_obj%nk=0
     RT_obj%nb=0
     RT_obj%nstates=0
     if(associated(RT_obj%E0))      deallocate(RT_obj%E0)
     if(associated(RT_obj%f0))      deallocate(RT_obj%f0)
     if(associated(RT_obj%W0))      deallocate(RT_obj%W0)
     if(associated(RT_obj%dE))      deallocate(RT_obj%dE)
     if(associated(RT_obj%df))      deallocate(RT_obj%df)
     if(associated(RT_obj%dW))      deallocate(RT_obj%dW)
     if(associated(RT_obj%W))       deallocate(RT_obj%W)
     if(associated(RT_obj%k))       deallocate(RT_obj%k)
     if(associated(RT_obj%wk))      deallocate(RT_obj%wk)
     if(associated(RT_obj%table))   deallocate(RT_obj%table)
     !
 end select
 !
 contains
   !
   subroutine build_RT_table()
     !
     i_RT=0
     !
     do i_k_RT=1,RT_obj%nk
       do i_b=RT_obj%nb(1),RT_obj%nb(2)
         do i_spin=1,n_sp_pol
           i_RT=i_RT+1
           RT_obj%table(i_RT,1)=i_b
           RT_obj%table(i_RT,2)=RT_all2ibz(i_k_RT)
           RT_obj%table(i_RT,3)=i_k_RT
           RT_obj%table(i_RT,4)=i_spin
           !
         enddo
       enddo
     enddo
     !
   end subroutine
   !
end subroutine RT_alloc_def_or_free

