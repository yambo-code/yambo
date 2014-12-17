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
subroutine RT_IO_carriers(Time,imposed_COM)
 !
 use pars,           ONLY:SP
 use rt_ctl,         ONLY:ID,RT_IO_JPO_steps
 use real_time,      ONLY:G_lesser,NE_i_time,RT_step,&
&                         RT_E_occupations,RT_H_occupations,&
&                         G_lesser_reference,RT_delta0_occupations,RT_nk
 use SC,             ONLY:SC_bands
 use IO_m,           ONLY:io_control,NONE,OP_RD,RD_CL,OP_RD_CL,DUMP
 use electrons,      ONLY:spin_occ
 use com,            ONLY:jobstr,error
 !
 implicit none
 !
 real(SP), optional, intent(inout) :: Time
 integer , optional, intent(in)    :: imposed_COM
 !
 ! Work Space
 !
 integer           :: ik,ib
 !
 ! I/O
 !
 integer           :: COM_,io_err_occ
 integer, external :: io_RT_components
 !
 ! Real-Time occupations load
 !============================
 !
 COM_=NONE 
 if (present(imposed_COM)) then
   COM_=imposed_COM
 endif
 ! 
 ! OCCUPATIONS I/O
 !====================
 ! 
 ! NE_time=(NE_i_time-1)*RT_step but I/O only when NE_i_time= N * RT_IO_steps
 !
 NE_i_time= nint(Time/RT_step)+1
 NE_i_time= NE_i_time-mod(NE_i_time-1,RT_IO_JPO_steps)
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/NE_i_time+1/),MODE=DUMP,ID=ID(6))
 io_err_occ=io_RT_components('carriers',ID(6))
 !
 if (io_err_occ<0) call error('Carriers database not found in folder: '//trim(jobstr))
 ! 
 ! REFERENCE I/O
 !===============
 !  
 if (NE_i_time==1) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID(6))
   io_err_occ=io_RT_components('REF',ID(6))
 endif
 !
 ! Add to the read occupations/lifetimes the reference part
 !
 do ik=1,RT_nk
   do ib=SC_bands(1),SC_bands(2)
     if (allocated(RT_E_occupations)) then
       RT_E_occupations(ib,ik)= RT_delta0_occupations(ib,ik)+          aimag(G_lesser_reference(ib,ib,ik))
       RT_H_occupations(ib,ik)=-RT_delta0_occupations(ib,ik)+(spin_occ-aimag(G_lesser_reference(ib,ib,ik)))
     endif
   enddo
 enddo
 !
end subroutine RT_IO_carriers
