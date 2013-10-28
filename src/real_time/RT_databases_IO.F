!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_databases_IO(E,it)
 !
 use drivers,        ONLY:l_ph_corr
 use real_time,      ONLY:ID,NE_dynamics,l_NE_with_fields,NE_i_time,it_start,&
&                         OCCUPATIONS_IO_and_interpolate,RT_IO_steps
 use SC,             ONLY:RT_nk
 use IO_m,           ONLY:io_control,OP_WR,WR_CL_IF_END,NONE,WR,REP,OP_WR_CL,io_GF,io_OCCs,io_J
 use electrons,      ONLY:levels
#if defined _ELPH
 use parallel_m,     ONLY:PP_redux_wait
 use real_time,      ONLY:THETA_matrix,l_RT_CCA_Kernel
#endif
 !
 implicit none
 !
 integer, intent(in)      :: it
 integer, external        :: io_RT_components
 type(levels),intent(in)  :: E
 !
 ! Work Space
 !
 integer :: ik,io_G,io_J_and_P,COM_,io_THETA
 !
 ! When the simulation has been restarted the first step (it==it_start)
 ! corresponds to the latest of the previous simulation. Therefore
 ! databases must not be written
 !
 if (it==it_start.and.it_start>1) return
 !
 ! Polarization and current are automaticaly written every RT_IO_stesp
 ! the cache save value at steps different from mod(NE_i_time,RT_IO_steps)/=0)
 !
 if(l_NE_with_fields.and.NE_dynamics.and.io_J) then
   !
   ! Remember that negative time = adiabatic switching
   ! no need to store current
   !
   io_J_and_P=0 
   !
   ! Write current and polarization on disk
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(4)) 
   io_J_and_P=io_RT_components('J_and_P',ID(4))
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/NE_i_time+1/),ID=ID(4)) 
   io_J_and_P=io_RT_components('J_and_P',ID(4))
   !
 endif
 !
 ! Write every RT_IO_steps
 !
 if (.not. (NE_i_time==1.or.mod(NE_i_time,RT_IO_steps)==0) ) return
 !
 if (it==it_start.and.it_start>1) return
 !
 ! Write G_lesser
 !================
 !
 if (io_GF) then
   !
   io_G=0
   io_THETA=0
   COM_=NONE
   if (it==it_start) COM_=REP
   call io_control(ACTION=OP_WR,COM=COM_, SEC=(/1/),ID=ID(5))
   io_G=io_RT_components('G',ID(5))
   !
#if defined _ELPH
   if (l_ph_corr.and..not.l_RT_CCA_Kernel) then
     call io_control(ACTION=OP_WR,COM=COM_, SEC=(/1/),ID=ID(7))
     io_THETA=io_RT_components('THETA',ID(7))
   endif
#endif
   !
   do ik = 1, RT_nk 
     call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(5))
     io_G=io_RT_components('G',ID(5))
#if defined _ELPH
     if (l_ph_corr.and..not.l_RT_CCA_Kernel) then
       !
       ! The I/O of the THETA matrix is tricky when the run is parallel.
       ! The point is that every CPU evolves a certain group of q-points.
       ! If I do here a all-to-all each CPU will have non-zero values for 
       ! q-points that it will not evolve. At the next call of this routine
       ! therefore, values relative to different times will be wrongly merged.
       !
       call PP_redux_wait(THETA_matrix(:,:,:,ik))
       !
       call io_control(ACTION=WR_CL_IF_END,COM=NONE,SEC=(/ik+1/),ID=ID(7))
       io_THETA=io_RT_components('THETA',ID(7))
       !
       call RT_ELPH_zeroing()
       !
     endif
#endif
   enddo
   !
 endif
 !
 ! Occupations
 !
 if (io_OCCs) call OCCUPATIONS_IO_and_interpolate(E,'G')
 !
end subroutine RT_databases_IO
