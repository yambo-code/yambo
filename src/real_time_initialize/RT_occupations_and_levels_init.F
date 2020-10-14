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
subroutine RT_occupations_and_levels_init(E,k,q)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_phel_scatt
 use functions,      ONLY:Fermi_fnc,bose_f
 use RT_occupations, ONLY:l_RT_relax_time_approx,RT_RTA_chem,RT_RTA_temp,RT_occupation_clean, &
&                         RT_RTA_E_occ_infty,RT_RTA_H_occ_infty,RT_el_occ,RT_ho_occ,RT_life_occ,RT_ph_occ
 use parallel_m,     ONLY:PAR_COM_Xk_ibz_INDEX,PAR_IND_Xk_ibz,PP_redux_wait 
 use real_time,      ONLY:RTibz,RT_bands,RT_levels,RT_is_dynamically_dephased,RT_nk
 use ELPH,           ONLY:ph_freqs_sq
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin_occ
 !
 implicit none
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 !
 ! Work Space
 !
 real(SP):: Energy,kpt(3),f_occ,f_occ_ref,ph_freq,Width
 integer :: ik,ik_FG,iE_FG,ib,ik_RT,il,iq,iqbz
 !
 call RT_occupation_clean(RT_el_occ)
 call RT_occupation_clean(RT_ph_occ)
 call RT_occupation_clean(RT_ho_occ)
 call RT_occupation_clean(RT_life_occ)
 !
 ! El/Ho Occupations 
 !===================
 do ib=RT_bands(1),RT_bands(2)
   do ik=1,k%nibz
     !
     do ik_RT=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       !
       if (ik_RT/=RTibz%k_range(ik,1)) then
         !
         ik_FG=RTibz%k_map(ik_RT)
         iE_FG=RTibz%E_map(ik_RT)
         !
         if (allocated(E%W))  E%FG%W(ib,iE_FG,1)     = abs(E%W(ib,ik,1))
         !
         kpt       = k%FGibz%pt(ik_FG,:)
         Energy    = E%FG%E(ib,iE_FG,1)
         f_occ     = E%FG%f(ib,iE_FG,1)
         f_occ_ref = f_occ
         Width     = 0._SP
         if (allocated(E%fo)) f_occ_ref = E%FG%f(ib,iE_FG,1)-E%FG%df(ib,iE_FG,1)
         if (allocated(E%W))  Width     = abs(E%FG%W(ib,iE_FG,1))
       else
         kpt       = k%pt(ik,:)
         Energy    = E%E(ib,ik,1)
         f_occ     = E%f(ib,ik,1)
         f_occ_ref = f_occ
         Width     = 0._SP
         if (allocated(E%fo)) f_occ_ref = E%fo(ib,ik,1)
         if (allocated(E%W))  Width     = abs(E%W(ib,ik,1))
         !
       endif
       !
       ! .. levels
       RT_levels%E(ib,ik_RT,1) = Energy
       RT_levels%W(ib,ik_RT,1) = Width 
       !
       ! .. kpt
       RTibz%pt(ik_RT,:)       = kpt
       !
       ! .. occ
       RT_el_occ%N_ref(ib,ik_RT) = f_occ_ref
       RT_ho_occ%N_ref(ib,ik_RT) = spin_occ-f_occ_ref
       !
       if (l_RT_relax_time_approx) then
         RT_RTA_E_occ_infty(ib,ik_FG)= Fermi_fnc(  Energy-E%E_CBm(1)-RT_RTA_chem(1) ,RT_RTA_temp(1))*spin_occ
         RT_RTA_H_occ_infty(ib,ik_FG)= Fermi_fnc(-(Energy-E%E_VBM(1)-RT_RTA_chem(2)),RT_RTA_temp(2))*spin_occ
       endif
       !
       if (.not.PAR_IND_Xk_ibz%element_1D(ik) ) cycle
       !
       RT_el_occ%dN(ib,ik_RT)   = f_occ-f_occ_ref
       RT_el_occ%N(ib,ik_RT)    = f_occ
       RT_ho_occ%N(ib,ik_RT)    = spin_occ-f_occ
       !
       if (.not.RT_is_dynamically_dephased) cycle
       !
       RT_life_occ%dN(ib,ik_RT) = f_occ-f_occ_ref
       !
     enddo
   enddo
 enddo
 !
 ! Ph Occupations 
 !================
 if (l_phel_scatt) then
   do il=RT_ph_occ%D1(1),RT_ph_occ%D1(2)
     do iq=1,RT_ph_occ%D2
       iqbz = q%k_table(iq,1)
       ph_freq=abs(sqrt(ph_freqs_sq(iqbz,il)))
       RT_ph_occ%N(il,iq)     = bose_f(ph_freq)
       RT_ph_occ%N_ref(il,iq) = RT_ph_occ%N(il,iq)+RT_ph_occ%dN(il,iq)
     enddo
   enddo
 endif
 !
 call PP_redux_wait(RT_el_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_ho_occ%N,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(RT_el_occ%dN,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 if (.not.RT_is_dynamically_dephased) return
 !
 call PP_redux_wait(RT_life_occ%dN,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
end subroutine RT_occupations_and_levels_init
