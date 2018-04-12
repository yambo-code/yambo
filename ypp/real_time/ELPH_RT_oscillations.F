!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine ELPH_RT_oscillations(E)
 !
 use pars,                ONLY:SP,pi
 use units,               ONLY:HA2EV
 use com,                 ONLY:error,msg
 use stderr,              ONLY:intc
 use electrons,           ONLY:levels
 use YPP_real_time,       ONLY:PhDisp,RT_OBSERVABLES_IO,l_skip_OBS_IO,RT_conf,PhDisp
 use ELPH,                ONLY:ph_modes,ph_freqs_sq
 use IO_m,                ONLY:io_control,OP_RD,DUMP
 use RT_control,          ONLY:RT_IO_t,OBS_RT_IO_t,RT_output
 use real_time,           ONLY:N_RT_PH_displaced,RT_PH_displaced,l_RT_PH_oscillations
 !
#include<memory.h>
 !
 type(levels)   :: E
 !
 ! Work Space 
 !
 integer               ::il,it,i_w,il_active
 real(SP), allocatable ::out_data(:),PhDisp_offset(:),ph_freqs(:)
 !
 !I/O
 !
 integer           ::io_err,ID
 real(SP)          ::R_dummy(2)
 integer, external ::io_ELPH
 !
 call section("+","Phonon displacement")
 !
 l_RT_PH_oscillations=.TRUE.
 !
 ! Allocations
 !=============
 !
 allocate(out_data(2* N_RT_PH_displaced ))
 allocate(PhDisp_offset(ph_modes ))
 allocate(ph_freqs(ph_modes ))
 !
 ! I/O
 !=====
 call io_control(ACTION=OP_RD,SEC=(/1,2/),MODE=DUMP,ID=ID)
 io_err=io_ELPH(ID,'gkkp_expanded no_matrix_elements')
 if (io_err/=0) call error("GKKP database not found")
 !
 call RT_OBSERVABLES_IO_paths("D",E)
 !
 call RT_time_configuration_setup('PH_DISPLACEMENT',OBS_RT_IO_t)
 !
 call RT_OBSERVABLES_IO(what='D')
 !
 do il=1,ph_modes
   ph_freqs(il)=abs(sqrt(ph_freqs_sq(1,il)))
   PhDisp_offset(il)=PhDisp%F_t(1,il)
   PhDisp%F_t(:,il)=PhDisp%F_t(:,il)-PhDisp_offset(il)
 enddo
 !
 ! Output (Time)
 !===============
 !
 call RT_OBSERVABLES_damp_and_write(PhDisp,RT_conf,l_skip_OBS_IO)
 !
 ! FT ....
 !=========
 !
 ! @ Full freq range
 !-------------------
 !
 if (any(RT_conf%W_range<0.)) RT_conf%W_range=(/0.,maxval(ph_freqs)/)
 !
 call RT_1D_Fourier_setup(PhDisp,RT_conf,ph_modes) 
 !
 call RT_1D_Fourier_Transform('T2W',PhDisp,RT_conf,ph_modes)
 !
 ! Output
 !--------
 !
 call RT_output(what="open FT_displace")
 !
 do i_w = 1, RT_conf%W_n_steps
   !
   il_active=1
   !
   do il=1,ph_modes
     !
     if (RT_PH_displaced(il)==0) cycle
     !
     out_data(il_active  )=real (PhDisp%F_w(i_w,il))
     out_data(il_active+1)=aimag(PhDisp%F_w(i_w,il))
     !
     il_active=il_active+2
     !
   enddo
   !
   call RT_output('FT_displace',VALUEs=out_data,E=  real(PhDisp%W(i_w))*HA2EV )
   !
 enddo
 !
 ! @ PH freqs
 !------------
 !
 do il=1,ph_modes
   PhDisp%F_w(il,:)=ph_freqs
 enddo
 RT_conf%W_n_steps=ph_modes
 !
 call RT_1D_Fourier_Transform('T2W',PhDisp,RT_conf,ph_modes)
 !
 ! Output
 !--------
 !
 do il=1,ph_modes
   !
   if (RT_PH_displaced(il)==0) cycle
   !
   call msg('s','[Mode #'//trim(intc(il))//'] Energy [meV]:',ph_freqs(il)*HA2EV*1000.)
   R_dummy=pi/ph_freqs(il)*(/-real(PhDisp%F_w(il,il)),aimag(PhDisp%F_w(il,il))/)
   call msg('s','          R factor    :',R_dummy(1))
   call msg('s','          D factor    :',R_dummy(2))
   !
 enddo
 !
 deallocate(out_data,PhDisp_offset,ph_freqs)
 !
 call RT_output(what="close FT_displace")
 !
end subroutine
