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
subroutine RT_Lifetimes_use_delta_Occupations( )
 !
 use electrons,      ONLY:spin_occ
 use pars,           ONLY:SP,rZERO
 use parallel_m,     ONLY:PAR_COM_PLASMA_INDEX,PAR_COM_Q_INDEX,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_A2A
 use RT_lifetimes,   ONLY:RT_EP_emit_life,RT_EE_life,RT_EH_life,Life_MEM_steps,&
&                         l_RT_relaxation_update_reference,RT_EP_abs_life,RT_lifetime
 use real_time,      ONLY:RT_occupations_locked,RT_E_occupations_reference,RT_H_occupations_reference,&
&                         RT_life_delta_occupations,RT_H_occupations,RT_E_occupations,RT_delta_occupations,&
&                         RT_bands,G_lesser_reference,RT_nk,RTibz
 !
 implicit none
 !
 ! Work Space
 !
 integer  :: ik,ib
 real(SP) :: f_E_occ,f_H_occ,f_E_occ_reference,f_H_occ_reference,NC
 !
 if (l_RT_relaxation_update_reference) then
   !
   do ik=1,RT_nk
     !
     do ib=RT_bands(1),RT_bands(2)
       !
       f_E_occ=RT_E_occupations(ib,ik)
       f_H_occ=RT_H_occupations(ib,ik)
       f_E_occ_reference=RT_E_occupations_reference(ib,ik)
       f_H_occ_reference=RT_H_occupations_reference(ib,ik)
       !
       if(f_E_occ<=0.5*spin_occ) RT_life_delta_occupations(ib,ik) =   f_E_occ-f_E_occ_reference
       if(f_E_occ> 0.5*spin_occ) RT_life_delta_occupations(ib,ik) = -(f_H_occ-f_H_occ_reference)
       !
     enddo
   enddo
   !
 endif
 !
 do ik=1,RT_nk
   !
   do ib=RT_bands(1),RT_bands(2)
     !
     RT_occupations_locked(ib,ik)=.FALSE. 
     !
     if (l_RT_relaxation_update_reference) then
       !
       if (.not.PAR_IND_Xk_ibz%element_1D(RTibz%k_map(ik)) ) cycle
       !
       ! _lifetime(ib,ik) are never used at different k-points. While occupations are
       !
       ! April 2020. AndreaM. The additional NC prefactor here is because the Lifetimes go trough
       ! an ALL2ALL in the PAR_COM_Xk_ibz_A2A WORLD in RT_Lifetimes_merge. 
       ! BUT the e-p lifetimes, for example, do not use the PLASMA communicator
       !
       NC=real(PAR_COM_Xk_ibz_A2A%n_CPU)/real(PAR_COM_PLASMA_INDEX%n_CPU*PAR_COM_Q_INDEX%n_CPU)
       call LOCAL_use_save(RT_EH_life)
       NC=real(PAR_COM_Xk_ibz_A2A%n_CPU)
       call LOCAL_use_save(RT_EE_life)
       NC=real(PAR_COM_Xk_ibz_A2A%n_CPU)/real(PAR_COM_PLASMA_INDEX%n_CPU)
       call LOCAL_use_save(RT_EP_abs_life)
       call LOCAL_use_save(RT_EP_emit_life)
       !
     else
       !
       RT_life_delta_occupations(ib,ik) =RT_delta_occupations(ib,ik)
       RT_E_occupations_reference(ib,ik)=         aimag(G_lesser_reference(ib,ib,ik))
       RT_H_occupations_reference(ib,ik)=spin_occ-aimag(G_lesser_reference(ib,ib,ik))
       !
       call LOCAL_zero(RT_EH_life)
       call LOCAL_zero(RT_EE_life)
       call LOCAL_zero(RT_EP_emit_life)
       call LOCAL_zero(RT_EP_abs_life)
       !
     endif
     !
   enddo
 enddo
 !
 contains
   !
   subroutine LOCAL_zero(LIFE)
     type(RT_lifetime) :: LIFE
     if (.not.LIFE%active) return
     LIFE%abs(ib,ik) =rZERO
     LIFE%emit(ib,ik)=rZERO
   end subroutine
   !
   subroutine LOCAL_use_save(LIFE)
     type(RT_lifetime) :: LIFE
     if (.not.LIFE%active) return
     LIFE%abs(ib,ik)  =LIFE%abs_save(ib,ik,Life_MEM_steps)/NC
     LIFE%emit(ib,ik) =LIFE%emit_save(ib,ik,Life_MEM_steps)/NC
   end subroutine
   !
end subroutine RT_Lifetimes_use_delta_Occupations
