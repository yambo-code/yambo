!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_occupations_and_levels_init(bands,E,k,q,k_FG,E_FG)
 !
 use pars,           ONLY:SP
 use functions,      ONLY:Fermi_fnc,bose_f
 use RT_occupations, ONLY:l_RT_relax_time_approx,RT_RTA_chem,RT_RTA_temp,RT_occupation_clean, &
&                         RT_RTA_E_occ_infty,RT_RTA_H_occ_infty,RT_el_occ,RT_ho_occ,RT_life_occ,RT_ph_occ
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,PAR_IND_Xk_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use real_time,      ONLY:RT_bands,RT_levels,RT_is_dynamically_dephased,RT_nk
 use ELPH,           ONLY:PH_freqs_sq,PH_Q_modulus,PH_acoustic_branch
 use R_lattice,      ONLY:bz_samp,bz_fine_grid
 use electrons,      ONLY:levels,spin_occ,E_fine_grid,n_sp_pol
 !
 implicit none
 !
 integer           ,intent(inout):: bands(2)
 type(levels)      ,intent(inout):: E
 type(bz_samp)     ,intent(in)   :: k,q
 type(bz_fine_grid),intent(inout):: k_FG
 type(E_fine_grid) ,intent(inout):: E_FG
 !
 ! Work Space
 !
 real(SP):: Energy(n_sp_pol),kpt(3),f_occ(n_sp_pol),f_occ_ref(n_sp_pol),ph_freq,Width(n_sp_pol)
 integer :: ik,ik_FG,iE_FG,ib,ik_RT,il,iq,iqbz
 !
 call RT_occupation_clean(RT_el_occ)
 call RT_occupation_clean(RT_ph_occ)
 call RT_occupation_clean(RT_ho_occ)
 call RT_occupation_clean(RT_life_occ)
 !
 ! El/Ho Occupations 
 !===================
 do ib=bands(1),bands(2)
   do ik=1,k%nibz
     !
     do ik_RT=k_FG%k_range(ik,1),k_FG%k_range(ik,2)
       !
       if (ik_RT/=k_FG%k_range(ik,1)) then
         !
         ik_FG=k_FG%k_map(ik_RT)
         iE_FG=k_FG%E_map(ik_RT)
         !
         if (allocated(E%W))  E%FG%W(ib,iE_FG,:)     = abs(E%W(ib,ik,:))
         !
         kpt       = k%FGibz%pt(ik_FG,:)
         Energy    = E%FG%E(ib,iE_FG,:)
         f_occ     = E%FG%f(ib,iE_FG,:)
         f_occ_ref = f_occ
         Width     = 0._SP
         if (allocated(E%fo)) f_occ_ref = E%FG%f(ib,iE_FG,:)-E%FG%df(ib,iE_FG,:)
         if (allocated(E%W))  Width     = abs(E%FG%W(ib,iE_FG,:))
       else
         kpt       = k%pt(ik,:)
         Energy    = E%E(ib,ik,:)
         f_occ     = E%f(ib,ik,:)
         f_occ_ref = f_occ
         Width     = 0._SP
         if (allocated(E%fo)) f_occ_ref = E%fo(ib,ik,:)
         if (allocated(E%W))  Width     = abs(E%W(ib,ik,:))
         !
       endif
       !
       ! .. levels
       E_FG%E(ib,ik_RT,:) = Energy
       E_FG%W(ib,ik_RT,:) = Width 
       !
       ! .. kpt
       k_FG%pt(ik_RT,:)       = kpt
       !
       ! .. occ
       RT_el_occ%N_ref(ib,ik_RT) = f_occ_ref(1)
       RT_ho_occ%N_ref(ib,ik_RT) = spin_occ-f_occ_ref(1)
       !
       if (l_RT_relax_time_approx) then
         RT_RTA_E_occ_infty(ib,ik_FG)= Fermi_fnc(  Energy(1)-E%E_CBm(1)-RT_RTA_chem(1) ,RT_RTA_temp(1))*spin_occ
         RT_RTA_H_occ_infty(ib,ik_FG)= Fermi_fnc(-(Energy(1)-E%E_VBM(1)-RT_RTA_chem(2)),RT_RTA_temp(2))*spin_occ
       endif
       !
#if defined _RT
       if (.not.PAR_IND_Xk_ibz%element_1D(ik) ) cycle
#endif
       !
       RT_el_occ%dN(ib,ik_RT)   = f_occ(1)-f_occ_ref(1)
       RT_el_occ%N(ib,ik_RT)    = f_occ(1)
       RT_ho_occ%N(ib,ik_RT)    = spin_occ-f_occ(1)
       !
       if (.not.RT_is_dynamically_dephased) cycle
       !
       RT_life_occ%dN(ib,ik_RT) = f_occ(1)-f_occ_ref(1)
       !
     enddo
   enddo
 enddo
 !
 ! Ph Occupations 
 !================
 if (RT_ph_occ%active) then
   do il=RT_ph_occ%D1(1),RT_ph_occ%D1(2)
     do iq=1,RT_ph_occ%D2
       iqbz = 1
       if (iq>1) iqbz = sum(q%nstar(:iq-1))+1
       ph_freq=abs(sqrt(PH_freqs_sq(iqbz,il)))
       if (PH_acoustic_branch(il)) then
         RT_ph_occ%N_ref(il,iq) = PH_Q_modulus(iqbz)*bose_f(ph_freq)/spin_occ
       else
         RT_ph_occ%N_ref(il,iq) = bose_f(ph_freq)/spin_occ
       endif
       RT_ph_occ%N(il,iq)     = RT_ph_occ%N_ref(il,iq)
     enddo
   enddo
 endif
 !
#if defined _RT
 call PP_redux_wait(RT_el_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_ho_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_el_occ%dN,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 if (.not.RT_is_dynamically_dephased) return
 !
 call PP_redux_wait(RT_life_occ%dN,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
#endif
 !
end subroutine RT_occupations_and_levels_init
