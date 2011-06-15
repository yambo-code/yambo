!
<<<<<<< .mine
! Copyright (C) 2000-2010 M. Gruning and the YAMBO team 
=======
! Copyright (C) 2000-2011 M. Gruning, A. Marini and the YAMBO team 
>>>>>>> .r885
!              http://www.yambo-code.org
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
subroutine RT_X_inversion(en)
 !
 use pars,           ONLY:SP,pi,schlen
 use YPP,            ONLY:RT_P_t,N_probes,Probe_path,RT_databases,RT_initialize,&
&                         X_rt,N_probe_frequencies,X_order
 use units,          ONLY:HARTREE,AU2Vmm,sVcm2Vmm
 use electrons,      ONLY:levels
 use IO_m,           ONLY:LOG
 use fields,         ONLY:Efield,EtoT,Divide_by_Field
 use real_time,      ONLY:NE_steps,RT_output,RT_step
 use stderr,         ONLY:intc
 implicit none
 !
 type(levels)  :: en
 !
 character(schlen) :: X_file_name
 integer           :: i_order,min_X_order
 real(SP)          :: Period,Unit_of_Measure
 complex(SP)       :: X_tot
 real(SP),    allocatable    :: RT_P_probe(:)
 complex(SP), allocatable    :: X_effective(:,:)
 !
 N_probe_frequencies=max(N_probes,N_probe_frequencies) 
 if (N_probe_frequencies.ne.1) return
 !
 call section("=",'Response function(s) by Inversion')
 !====================================================
 !
 allocate(RT_P_probe(NE_steps),X_effective(X_order+1,3))
 RT_P_probe=(0.,0.)
 call RT_databases(en,COM_=LOG, mesg=trim(Probe_path(1)))
 !
 ! Time Range
 !========================
 !
 Period = EtoT(E=Efield(1)%frequency(1))
 X_rt%time_range(2)= X_rt%time_range(1)+ Period
 !
 if (X_rt%time_range(2)> NE_steps*RT_step) then
   X_rt%time_range(2)= NE_steps*RT_step
   X_rt%time_range(1)= X_rt%time_range(2)-Period
 endif
 !
 call RT_initialize(dump_and_write=.FALSE.)
 !
 ! X effective (1 Probe, 1 frequency)
 ! ===========
 !
 do i_c = 1,3
   RT_P_probe(:)=real(RT_P_t(:,i_c))
   !
   call RT_Coefficients_inv(NE_steps,RT_P_probe(:),X_effective(:,i_c),X_order,Efield(1)%frequency(1))
   !
 end do
 !
 do i_order=min_X_order,X_order,1
   X_file_name="X_probe_order_"//trim(intc(i_order))
   if (i_order==1) X_file_name="eps"
   !
   call RT_output("open "//trim(X_file_name) )
   !
   X_tot=4.*pi*sum(X_effective(i_order+1,:)*Efield(i_probe)%versor(:))*Divide_by_Field(Efield(1),i_order)
   ! 
   Unit_of_Measure = 1._SP
   if (i_order.eq.0) Unit_of_Measure = sVcm2Vmm/AU2Vmm
   if (i_order.lt.1) Unit_of_Measure = (sVcm2Vmm/AU2Vmm)**(i_order-1)  
   X_tot = X_tot*Unit_of_Measure
   call RT_output(trim(X_file_name),VALUEs=(/aimag(X_tot,real(X_tot)/), E=Efield(1)%frequency(1)*HARTREE)
   !
   call RT_output("close "//trim(X_file_name))
 enddo
 !
 deallocate(RT_P_probe,X_effective)
 !
end subroutine RT_X_inversion
