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
subroutine RT_IO_J_and_P(IO_error,first_sec_only,G_lesser_only,COM_,mesg)
 !
 use YPP_real_time,  ONLY:RT_J_t,RT_P_t,RT_conf,RT_time
 use pars,           ONLY:DP,SP,schlen
 use electrons,      ONLY:n_spin
 use R_lattice,      ONLY:nkibz
 use LIVE_t,         ONLY:live_timing
 use IO_m,           ONLY:OP_RD,OP_RD_CL,io_control,NONE,DUMP,LOG,RD_CL_IF_END
 use RT_control,     ONLY:J_cache,P_cache,Ms_cache,Ml_cache,IO_JPSM_cache_steps
 use real_time,      ONLY:NE_steps,NE_i_time,&
&                         G_lesser,G_lesser_reference,RT_bands,RT_dyn_step,RT_step
 use fields,         ONLY:Efield,n_ext_fields,Efield_strength
 !
#include<memory.h>
 !
 logical, optional :: IO_error,first_sec_only,G_lesser_only
 integer, optional :: COM_
 character(*), optional :: mesg
 !
 ! Work Space
 !
 real(SP)           :: TIME
 integer            :: io_JPSM,ID,i_f,io_G,COM_here,ik,NE_steps_save,i_c,i_time
 integer, external  :: io_RT_components
 character(schlen)  :: mesg_
 !
 NE_steps_save=NE_steps
 !
 COM_here=LOG
 if (present(COM_)) COM_here=COM_
 mesg_=" "
 if (present(mesg)) mesg_="["//trim(mesg)//"]"
 !
 if (COM_here==LOG) call section("+",'Current and Polarization Database')
 !=======================================================================
 !
 if (present(G_lesser_only)) then
   if (G_lesser_only) then
     !
     YAMBO_ALLOC(G_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nkibz,1))
     !
     YAMBO_ALLOC(G_lesser_reference,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nkibz))
     !
     call io_control(ACTION=OP_RD,COM=COM_here,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
     io_G=io_RT_components('G',ID)
     !
     if (present(IO_error)) IO_error=io_G/=0
     if (io_G/=0) return
     !
     do ik=1,nkibz
       call io_control(ACTION=RD_CL_IF_END,COM=COM_here,SEC=(/ik+1,1/),MODE=DUMP,ID=ID)
       io_G=io_RT_components('G',ID)
     enddo
     !
   endif
   !
   return
   !
 else
   call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
   io_G=io_RT_components('G',ID)
 endif
 !
 COM_here=NONE
 !
 ! Use Core value for dynamical step 
 !-----------------------------------
 RT_dyn_step=RT_step
 !
 ! Check for saved current and polarization database
 !---------------------------------------------------
 !
 if (present(IO_error)) then
   IO_error=.FALSE.
 endif
 !
 call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/1/),ID=ID)
 io_JPSM=io_RT_components('JPSM',ID)
 if( io_JPSM/=0 ) then
   if (present(IO_error)) then
     IO_error=.TRUE.
   endif
   return
 endif
 !
 ! Only first section I/O
 !------------------------
 !
 if (present(first_sec_only)) then
   if (first_sec_only) return
 endif
 !
 ! I/O of Current and Vector Potential
 !=====================================
 !
 if (.not.allocated(RT_J_t)) then
   YAMBO_ALLOC(RT_J_t,(RT_conf%T_n_steps,3))
   YAMBO_ALLOC(RT_P_t,(RT_conf%T_n_steps,6))
 endif
 !
 YAMBO_ALLOC(J_cache,(6,IO_JPSM_cache_steps))
 !
 YAMBO_ALLOC(P_cache,(6,IO_JPSM_cache_steps))
 !
 if(n_spin>1) then
   YAMBO_ALLOC(Ms_cache,(6,IO_JPSM_cache_steps))
   YAMBO_ALLOC(Ml_cache,(6,IO_JPSM_cache_steps))
 endif
 !
 RT_J_t        =(0._DP,0._DP)
 RT_P_t        =(0._DP,0._DP)
 !
 NE_steps=NE_steps_save
 !
 call live_timing('I/O'//trim(mesg_)//':',RT_conf%T_n_steps)
 !
 do i_time=1,RT_conf%T_n_steps
   !
   TIME=RT_time(i_time)
   !
   NE_i_time=nint( TIME/RT_step ) + 1
   !
   call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/NE_i_time+1/),ID=ID) 
   io_JPSM=io_RT_components('JPSM',ID)
   !
   do i_c=1,3
     RT_J_t(i_time,i_c) =cmplx(J_cache(2*i_c-1,1),J_cache(2*i_c,1),SP)
     RT_P_t(i_time,i_c) =cmplx(P_cache(2*i_c-1,1),P_cache(2*i_c,1),SP)
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 ! Define Efield amplitude
 !
 do i_f=1,n_ext_fields
   Efield(i_f)%amplitude=Efield_strength(Efield(i_f)%intensity,'AU')
 enddo
 !
 YAMBO_FREE(J_cache)
 YAMBO_FREE(P_cache)
 if(n_spin>1) then
   YAMBO_FREE(Ms_cache)
   YAMBO_FREE(Ml_cache)
 endif
 !
end subroutine
