!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine RT_OBSERVABLES_IO(what,IO_error,first_sec_only,G_lesser_only,COM_,mesg)
 !
 use YPP_real_time,  ONLY:RT_conf,Pol,Curr
 use pars,           ONLY:DP,schlen,cZERO_DP
 use R_lattice,      ONLY:nkibz
 use IO_m,           ONLY:OP_RD,OP_RD_CL,io_control,manage_action,NONE,DUMP,LOG,RD_CL_IF_END
 use RT_control,     ONLY:RT_control_alloc,RT_control_free,J_cache,P_cache,IO_OBS_cache_steps,TMP_OBS_cache_steps
 use real_time,      ONLY:NE_steps,NE_i_time,&
&                         G_lesser,G_lesser_reference,RT_bands,RT_dyn_step,RT_step
 use fields,         ONLY:Efield,n_ext_fields,Efield_strength
 !
#include<memory.h>
 !
 character(*), optional :: what
 logical,      optional :: IO_error,first_sec_only,G_lesser_only
 integer,      optional :: COM_
 character(*), optional :: mesg
 !
 ! Work Space
 !
 integer            :: io_OBS,ID,IO_ACT,i_f,io_G,COM_here,ik,NE_steps_save,i_dir
 integer, external  :: io_RT_components
 character(schlen)  :: mesg_
 !
 NE_steps_save=NE_steps
 !
 COM_here=LOG
 if (present(COM_)) COM_here=COM_
 !
 if (COM_here==LOG) then
   if (what=="JP") mesg_='Current and Polarization'
   if (what=="D" ) mesg_='Phonon Displacement Field'
   call section("+",trim(mesg_)//' Database')
 endif
 !
 mesg_=" "
 if (present(mesg)) mesg_="["//trim(mesg)//"]"
 !=======================================================================
 !
 if (present(G_lesser_only)) then
   !
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
       IO_ACT=manage_action(RD_CL_IF_END,ik,1,nkibz)
       call io_control(ACTION=IO_ACT,COM=COM_here,SEC=(/2,ik+2/),MODE=DUMP,ID=ID)
       io_G=io_RT_components('G',ID)
     enddo
     !
   endif
   !
   return
   !
 else
   !
   call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
   io_G=io_RT_components('G',ID)
   !
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
 if (present(IO_error)) IO_error=.FALSE.
 !
 NE_i_time=1
 call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/1/),ID=ID)
 io_OBS=io_RT_components(what,ID)
 if( io_OBS/=0 ) then
   if (present(IO_error)) IO_error=.TRUE.
   return
 endif
 !
 ! Only first section I/O
 !------------------------
 !
 if (present(first_sec_only)) then
   if (first_sec_only) then
     call RT_control_alloc("times")
     call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/2/),ID=ID)
     io_OBS=io_RT_components(what,ID)
     return
   endif
 endif
 !
 ! I/O of OBSERVABLES
 !====================
 !
 Pol%title    ="Polarization"
 Curr%title   ="Current"
 !
 Pol%N    =3
 Curr%N   =3
 !
 if (what=="JP".and..not.allocated(Curr%F_t)) then
   IO_OBS_cache_steps=RT_conf%T_n_steps
   TMP_OBS_cache_steps=RT_conf%T_n_steps
   call RT_control_alloc("cache")
   NE_i_time=NE_steps
   call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/3/),ID=ID)
   io_OBS=io_RT_components(what,ID)
   YAMBO_ALLOC(Curr%F_t,(RT_conf%T_n_steps,3))
   YAMBO_ALLOC(Pol%F_t,(RT_conf%T_n_steps,3))
   Curr%F_t =cZERO_DP
   Pol%F_t =cZERO_DP
   do i_dir=1,3
     Curr%F_t(:,i_dir)=J_cache(i_dir,:)
     Pol%F_t(:,i_dir) =P_cache(i_dir,:)
   enddo
   !
   call RT_control_free("cache")
 endif
 !
 ! Define Efield amplitude
 !
 do i_f=1,n_ext_fields
   Efield(i_f)%amplitude=Efield_strength(Efield(i_f)%intensity,'AU')
 enddo
 !
 call RT_control_free("times")
 !
end subroutine
