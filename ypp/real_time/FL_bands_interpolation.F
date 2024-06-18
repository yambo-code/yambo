!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine FL_bands_interpolation(en,k,q)
 !
 use pars,           ONLY:SP,schlen,rZERO
 use parser_m,       ONLY:parser
 use units,          ONLY:AUT2FS
 use electrons,      ONLY:levels,n_sp_pol,n_spinor,E_reset,n_spin
 use R_lattice,      ONLY:bz_samp
 use YPPm,           ONLY:BANDS_steps,INTERP_grid,CIRCUIT_k,BANDS_path,CIRCUIT_E_DB_path,&
&                         CIRCUIT_made_of_SPECIAL_k,BANDS_bands,l_dipoles,&
&                         l_PROJECT_line,l_PROJECT_plane,PROJECT_N_par,DIPs_mask
 use YPP_real_time,  ONLY:FL_freq_indx
 use YPP_interfaces, ONLY:PROJECTION_setup,PROJECTION_plot,k_special
 use stderr,         ONLY:real2ch,intc,STRING_remove,STRING_same
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,DUMP,NONE,VERIFY
 use nl_optics,      ONLY:NL_bands
 use fl_optics,      ONLY:FL_output_to_FL_E,n_tot_fl_modes
 use interpolate,    ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do,electrons_bands_interpolate,&
&                         INTERP_obj,INTERP_mode,GRID_k
 use LIVE_t,         ONLY:live_timing
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: k,q
 type(levels),  intent(inout) :: en
 !
 ! Work space
 !
 type(bz_samp)         :: RT_k
 type(levels)          :: CIRCUIT_E,CIRCUIT_dE,FL_E
 character(schlen)     :: operation,basename,extension,sec_name
 logical               :: QUIET_interp,plot_STD_style,plot_RT_style
 integer               :: ID_circuit,ID_spin,ID_magn,ID_bands(3),ID_grid,IDs(6)
 integer               :: io_Floquet,ID_FL
 integer               :: ib,ik,ik_path,i_spin,i_mode,nfiles(5),nqnt
 !
 logical, external     :: file_exists
 integer, external     :: io_FL
 !
 call section('*','Interpolation tool')
 !=====================================
 !
 if (FL_freq_indx==0) FL_freq_indx=1
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID_FL)
 io_Floquet=io_FL(ID_FL)
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1+FL_freq_indx/),ID=ID_FL)
 io_Floquet=io_FL(ID_FL)
 !
 if (io_Floquet/=0) call error(" Floque database not found")
 !
 !call parser("STD_style",plot_STD_style)
 plot_STD_style=.true.
 plot_RT_style = .not.plot_STD_style
 !
 IDs       =-1
 !
 ! Logicals & Sectioning...
 sec_name="Levels"
 !
 if (STRING_same(trim(INTERP_mode),"none")) call error(" FL bands works only with interpolation")
 !
 call section('*','Interpolation and plotting:'//trim(sec_name))
 !
 ! Checks...
 !
 if (all(INTERP_grid>0)) call error("FL Interpolation only on a band circuit")
 if (n_sp_pol>1) call error('FL interoplation for spin polarized systems not implemented')
 !
 if (BANDS_bands(1)<NL_bands(1).or.BANDS_bands(2)>NL_bands(2)) then
   BANDS_bands(1)=max(BANDS_bands(1),NL_bands(1))
   BANDS_bands(2)=min(BANDS_bands(2),NL_bands(2))
   call warning("User bands restricted to "//trim(intc(BANDS_bands(1)))//" - "//trim(intc(BANDS_bands(2))))
 endif
 !
 if (file_exists(trim(CIRCUIT_E_DB_path)//"/SAVE/ns.db1")) BANDS_path= " "
 !
 ! Define the circuit
 !====================
 call k_special( )
 call k_circuit_driver( BANDS_path, BANDS_steps )
 !
 CIRCUIT_made_of_SPECIAL_k=CIRCUIT_k%nbz>0
 !
 call section('=',"Floquet components maps")
 !============================================
 !
 QUIET_interp=trim(INTERP_mode)=="NN".and.n_tot_fl_modes==1
 if (QUIET_interp) call live_timing('Time Interpolation(s)',n_tot_fl_modes)
 !
 do i_mode=1,n_tot_fl_modes
   !
   call FL_output_to_FL_E(FL_E,en,i_mode)
   !
   call section('=',"Levels interpolation")
   !
   ! Perform interpolation of spinorial factors and magnetization
   !==============================================================
   if (n_spinor>1) call electrons_spin_and_magn_interpolate("S",k,CIRCUIT_k,NL_bands,ID_spin,ID_magn)
   !
   call FL_output_to_FL_E(en,FL_E,i_mode)
   !
   ! Perform the energies interpolation
   !====================================
   call electrons_bands_interpolate(FL_E,k,BANDS_bands,ID_bands,CIRCUIT_E,CIRCUIT_dE=CIRCUIT_dE)
   !
   !extension="circuit"
   !operation=trim(operation)//" Mode "//trim(intc(i_mode))//"/"//trim(intc(n_tot_fl_modes))
   !!
   !call INTERPOLATION_driver_seed(trim(operation),ID_circuit,RT_bands,RT_k,SEED_RT=RT_carriers,QUIET=QUIET_interp)
   !call INTERPOLATION_driver_do(trim(operation),ID_circuit,CIRCUIT_k)
   !
   ! OUTPUT
   !=======
   !
   !if (plot_RT_style ) then
   !  call RT_plot_interpolated_values(CIRCUIT_E,FL_E,CIRCUIT_dE,i_mode,ID_circuit,ID_spin,1._SP,.false.,.false.)
   !endif
   !
   if (plot_STD_style) then
     IDs=0
     nqnt=1
     IDs(1)=ID_bands(2)
     nfiles(1)=n_sp_pol
     !
     if(n_spinor==2.and.i_mode==1) then
       nqnt=nqnt+2
       nfiles(nqnt-1:nqnt)=(/2,3/)
       IDs(nqnt-1:nqnt)=(/ID_spin,ID_magn/)
     endif
     !
     basename="interpolated_mode"//trim(intc(i_mode))//"_freq"//trim(intc(FL_freq_indx))
     call plot_interpolated_values(k,en,0._SP,CIRCUIT_k,IDs(1:nqnt),BANDS_bands,nqnt,nfiles(1:nqnt),basename,"bands",i_mode)
     !
   endif
   !
   if(ID_bands(1)/=-1) call INTERPOLATION_driver_end(ID_bands(1))
   if(ID_bands(2)/=-1) call INTERPOLATION_driver_end(ID_bands(2))
   if(ID_bands(3)/=-1) call INTERPOLATION_driver_end(ID_bands(3))
   !
   if (QUIET_interp) call live_timing(steps=1)
   !
 enddo
 !
 if (QUIET_interp) call live_timing()
 !
 ! Clean
 !
 call INTERPOLATION_driver_end(0)
 call E_reset(FL_E)
 !
end subroutine FL_bands_interpolation
