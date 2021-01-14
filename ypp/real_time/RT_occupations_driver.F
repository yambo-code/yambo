! 
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_occupations_driver(k,q,en)
 !
 use pars,           ONLY:rZERO
 use drivers,        ONLY:l_rt_carriers_in_use
 use DIPOLES,        ONLY:DIP_iR
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol
 use IO_m,           ONLY:LOG
 use QP_CTL_m,       ONLY:QP_apply
 use parallel_m,     ONLY:PAR_DIPk_ibz_index,PAR_IND_DIPk_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use YPP_interfaces, ONLY:PROJECTION_setup
 use RT_control,     ONLY:RT_apply,CARR_RT_IO_t
 use real_time,      ONLY:RT_bands
 use YPPm,           ONLY:DIPs_mask,DIPs_bands,DIPs_direction,l_dipoles,DIPs
 use YPP_real_time,  ONLY:l_RT_time,l_RT_energy,l_RT_bands,l_RT_dos,RT_conf
 !
#include<memory.h>
 !
 type(bz_samp) :: k,q
 type(levels)  :: en
 !
 ! Workspace
 !
 integer :: ik,ib,i_spin,ik_mem
 !
 call section('+','Real-Time components (Occupations, Lifetimes,...)')
 !=====================================================================
 !
 ! Pre-setup of carrier components and Time evolution parameters
 !===============================================================
 call RT_apply(RT_bands,en,k,IO_COM=LOG,TIMES_ONLY=.TRUE.)
 !
 if (.not.l_rt_carriers_in_use) return
 !
 if (l_dipoles) then
   call section('=',"The Dipoles `Mask`")
   !=====================================
   call DIPOLES_driver("Database",k,en,q)
   YAMBO_ALLOC(DIPs_mask,(RT_bands(1):RT_bands(2),k%nibz,n_sp_pol))
   DIPs_mask=rZERO
   do ik=1,k%nibz
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
     ik_mem=PAR_DIPk_ibz_index(ik)
     do ib=RT_bands(1),RT_bands(2)
       do i_spin=1,n_sp_pol
         DIPs_mask(ib,ik,i_spin)=abs(dot_product(DIPs_direction,DIP_iR(:,DIPs_bands(2)-DIPs%ib_lim(2)+1,ib,ik_mem,i_spin)))**2.
       enddo
     enddo
   enddo
   call PP_redux_wait(DIPs_mask)
   call DIPOLES_driver("Clean",k,en,q)
 endif
 !
 ! Setups
 !========
 call PROJECTION_setup("setup")
 !
 call RT_time_configuration_setup("CARRIERS",CARR_RT_IO_t)
 !
 ! Additional QP corrections
 !===========================
 call QP_state_table_setup(en)
 !
 call QP_apply(RT_bands,en,k,'G',msg_fmt='rs')
 !
 ! Occupations energy plot 
 !=========================
 if (l_RT_energy) call RT_components_energy_plot(en,k,RT_conf%T_n_steps)
 !
 ! Occupations time plot 
 !=========================
 if (l_RT_time)   call RT_occ_time_plot(en,k,RT_conf%T_n_steps)
 !
 ! Occupations 2D dos plot 
 !=========================
 if (l_RT_dos)    call RT_dos_time_plot(en,k,RT_conf%T_n_steps)
 !
 ! Occupations bands fit 
 !=======================
 if (l_RT_bands) call RT_occ_bands_interpolation(en,k,q,RT_conf%T_n_steps)
 !
 ! Clean..
 YAMBO_FREE(DIPs_mask)
 !
end subroutine RT_occupations_driver
