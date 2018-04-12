! 
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_occ_2D_dos_plot(En,kpt,n_T_steps)
 !
 use units,          ONLY:HA2EV,AUT2FS
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:levels,n_spin,n_spinor
 use YPP,            ONLY:DOS_broadening,DOS_E_range,DOS_E_steps
 use YPP_real_time,  ONLY:RT_time
 use com,            ONLY:msg,error
 use functions,      ONLY:Fermi_fnc_derivative
 use LIVE_t,         ONLY:live_timing
 use RT_control,     ONLY:RT_output,RT_apply
 use real_time,      ONLY:RT_carriers,RT_bands
 use interfaces,     ONLY:PARALLEL_index
 use parallel_m,     ONLY:PP_indexes,PP_redux_wait
 use parser_m,       ONLY:parser
 !
#include<memory.h>
 !
 type(bz_samp) ::kpt
 type(levels)  ::En
 integer       ::n_T_steps
 !
 ! Work Space
 ! 
 real(SP)              :: el_dos(DOS_E_steps,n_spin),dos_E,delta_E,f_occ,dos_norm,&
&                         el_dos_max,el_dos_spin(2)
 real(SP),allocatable  :: spin_factors(:,:,:)
 integer               :: i_RT,i_E,i_spin,i_T,i_k,i_b
 logical               :: l_sum_equilibrium,l_separate_eh
 type(PP_indexes)      :: px
 !
 call section('*','Electr. Time-dependent DOS')
 !
 if (n_T_steps==1) call error(' Only one time step. Use standard dos instead')
 !
 call msg('s','Energy steps   :',DOS_E_steps)
 call msg('s','Bands          :',RT_carriers%nb)
 !
 call parser('SeparateEH',l_separate_eh)
 call parser('IncludeEQocc',l_sum_equilibrium)
 !
 el_dos_max=0._SP
 el_dos_spin(2)=0._SP
 !
 ! Output files headers
 !
 call RT_output("open 2D_occ_dos")
 !
 if (DOS_E_range(1)>DOS_E_range(2)) then
   DOS_E_range(1)=minval(RT_carriers%E_bare)-5._SP*DOS_broadening
   DOS_E_range(2)=maxval(RT_carriers%E_bare)+5._SP*DOS_broadening
 endif
 delta_E=(DOS_E_range(2)-DOS_E_range(1))/DOS_E_steps
 !
 dos_norm=1._SP/DL_vol
 !
 if (n_spinor==2) then
   YAMBO_ALLOC(spin_factors,(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_spinor))
   call electrons_spin_factors((/1,kpt%nibz/),RT_carriers%nb,spin_factors)
 endif 
 !
 ! Parallel setup and live_timing
 !================================
 !
 call PARALLEL_index(px,(/DOS_E_steps/))
 call live_timing('DOS',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   call RT_apply(RT_bands,En,kpt,Time=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   el_dos(:,:)=0._SP
   !
   do i_E=1,DOS_E_steps
     !
     if (.not.px%element_1D(i_E)) cycle
     !
     dos_E=DOS_E_range(1)+i_E*delta_E
     !
     do i_RT=1,RT_carriers%nstates
       !
       i_b   =RT_carriers%table(i_RT,1)
       i_k   =RT_carriers%table(i_RT,2)
       i_spin=RT_carriers%table(i_RT,4)
       !
       if(.not.l_sum_equilibrium) f_occ=RT_carriers%delta_f(i_RT)
       if(     l_sum_equilibrium) f_occ=RT_carriers%delta_f(i_RT)+RT_carriers%f_bare(i_RT)
       if(n_spinor==2) then
         el_dos(i_E,:)=el_dos(i_E,:)+f_occ*spin_factors(i_b,i_k,:)*&
&           Fermi_fnc_derivative(RT_carriers%E_bare(i_RT)-dos_E,DOS_broadening)*dos_norm     
       else
         el_dos(i_E,i_spin)=el_dos(i_E,i_spin)+f_occ*&
&           Fermi_fnc_derivative(RT_carriers%E_bare(i_RT)-dos_E,DOS_broadening)*dos_norm
       endif
       !
     enddo
     !
   enddo
   !
   call PP_redux_wait(el_dos)
   !
   el_dos_max=max(el_dos_max,maxval(abs(el_dos)))
   !
   call msg('o dos','')
   !
   ! Output file
   !
   do i_E=1,DOS_E_steps
     dos_E=DOS_E_range(1)+real(i_E,SP)*delta_E
     !
     if(dos_E>En%E_VBM(1)+DOS_broadening .and. dos_E<En%E_CBm(1)-DOS_broadening) then
       if(l_separate_eh) then
         call RT_output("space 2D_occ_dos")
       endif
       l_separate_eh=.false.
       cycle
     endif
     if(n_spin==1) then
       call RT_output("2D_occ_dos",VALUEs=(/el_dos(i_E,1)/),E=dos_E*HA2EV,TIME=RT_time(i_T)*AUT2FS)
     else
       call RT_output("2D_occ_dos",VALUEs=(/el_dos(i_E,1)+el_dos(i_E,2),el_dos(i_E,1),el_dos(i_E,2)/),&
&                                  E=dos_E*HA2EV,TIME=RT_time(i_T)*AUT2FS)
     endif
     !
   enddo
   !
   call RT_output("space 2D_occ_dos")
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call msg('s','DOS max value  :',el_dos_max)
 !
 call live_timing()
 !
 YAMBO_FREE(spin_factors)
 !
end subroutine RT_occ_2D_dos_plot
