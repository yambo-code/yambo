!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_databases_IO( )
 !
 use drivers,        ONLY:l_elph_scatt
 use electrons,      ONLY:n_spin
 use RT_control,     ONLY:RT_do_it,ID,RT_carriers_object,J_cache,P_cache,M_cache,JPC_RT_IO_t,&
&                         GF_RT_IO_t,IO_JPSM_cache_steps,SAVE_G_history
 use real_time,      ONLY:NE_i_time,l_NE_dynamics,RT_nk,NE_i_time,RT_ind_J,RT_P,RT_magn
 use IO_m,           ONLY:io_control,OP_WR,WR_CL,WR_CL_IF_END,NONE,WR,OP_WR_CL, &
&                         io_GF,io_CARRIERs,io_J,OP_IF_START_WR_CL_IF_END
 use parallel_m,     ONLY:PP_redux_wait
 use real_time,      ONLY:THETA_matrix,l_RT_CCA_Kernel,RT_carriers,NE_time,NE_steps
 !
 implicit none
 !
 ! Work Space
 !
 integer           :: ik,io_G,io_JPSM,io_carr,io_THETA,i_c,ACTION_,i_time
 integer, external :: io_RT_components
 !
 ! Store the current and the polarization in the cache for future writing
 !
 if(l_NE_dynamics) then
   i_time=mod(NE_i_time-1,IO_JPSM_cache_steps)+1
   do i_c=1,3
     J_cache(2*i_c-1,i_time)= real(RT_ind_J(i_c))
     J_cache(2*i_c  ,i_time)=aimag(RT_ind_J(i_c))
     P_cache(2*i_c-1,i_time)= real(  RT_P(i_c)  )
     P_cache(2*i_c  ,i_time)=aimag(  RT_P(i_c)  )
     if(n_spin>1) then
       M_cache(2*i_c-1,i_time)= real(RT_magn(i_c) )
       M_cache(2*i_c  ,i_time)=aimag(RT_magn(i_c) )
     endif
   enddo
 endif
 !
 ! Polarization, current and curriers
 !===================================
 if(RT_do_it('JPC')) then
   !
   JPC_RT_IO_t%N=JPC_RT_IO_t%N+1
   JPC_RT_IO_t%Time(JPC_RT_IO_t%N)=NE_time
   JPC_RT_IO_t%last_point=NE_i_time
   !
   if (io_J) then
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(4)) 
     io_JPSM=io_RT_components('JPSM',ID(4))
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/NE_i_time+1/),ID=ID(4)) 
     io_JPSM=io_RT_components('JPSM',ID(4))
   endif
   !
   ! GPL_EXCLUDE_START
   !
   if (io_CARRIERs) then
     call RT_carriers_object(RT_carriers,WHAT='fill')
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(6)) 
     io_carr=io_RT_components('carriers',ID(6))
     call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/NE_i_time+1/),ID=ID(6))
     io_carr=io_RT_components('carriers',ID(6))
   endif
   !
   ! GPL_EXCLUDE_END
   !
 endif
 !
 ! GPL_EXCLUDE_START
 !
 ! G_lesser and THETA
 !====================
 if (io_GF.and.RT_do_it('G')) then
   !
   GF_RT_IO_t%N=GF_RT_IO_t%N+1
   GF_RT_IO_t%Time(GF_RT_IO_t%N)=NE_time
   GF_RT_IO_t%last_point=NE_i_time
   !
   io_G=0
   io_THETA=0
   !
   ACTION_=OP_WR
   if(SAVE_G_history) ACTION_=OP_WR_CL
   !
   call io_control(ACTION=ACTION_,COM=NONE, SEC=(/1/),ID=ID(5))
   io_G=io_RT_components('G',ID(5))
   !
   if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
     call io_control(ACTION=OP_WR,COM=NONE, SEC=(/1/),ID=ID(7))
     io_THETA=io_RT_components('THETA',ID(7))
   endif
   !
   do ik = 1, RT_nk
     !
     ACTION_=WR_CL_IF_END
     if (SAVE_G_history) then
       call PP_redux_wait()
       ACTION_=OP_IF_START_WR_CL_IF_END
     endif
     !
     call io_control(ACTION=ACTION_,COM=NONE,SEC=(/ik+1/),ID=ID(5))
     io_G=io_RT_components('G',ID(5))
     !
#if defined _ELPH
     !
     if (l_elph_scatt.and..not.l_RT_CCA_Kernel) then
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
     !
#endif
     !
   enddo
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
end subroutine RT_databases_IO
