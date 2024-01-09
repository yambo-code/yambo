!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine RT_OBSERVABLES_IO(what,Curr,Pol,IO_error,first_sec_only,G_lesser_only,COM_)
 !
 use YPP_real_time,  ONLY:RT_conf,OBS_el
 use pars,           ONLY:DP,cZERO_DP,cZERO
 use R_lattice,      ONLY:nkibz
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD,RD_CL,OP_RD_CL,manage_action,NONE,DUMP,LOG,OP_IF_START_RD_CL_IF_END
 use RT_control,     ONLY:RT_control_alloc,RT_control_free,J_cache,P_cache,CACHE_OBS_steps,CACHE_OBS_steps_now
 use real_time,      ONLY:NE_steps,NE_i_time,G_lesser,G_lesser_reference,RT_bands,RT_dyn_step,RT_step
 use fields,         ONLY:Efield,n_ext_fields,Efield_strength
 !
#include<memory.h>
 !
 character(*)           :: what
 type(OBS_el), optional :: Curr,Pol
 logical,      optional :: IO_error,first_sec_only,G_lesser_only
 integer,      optional :: COM_
 !
 ! Work Space
 !
 integer            :: io_OBS,ID,IO_ACT,i_f,io_G,COM_here,ik,NE_steps_save,i_dir,it,it_now
 integer, external  :: io_RT_components
 !
 NE_steps_save=NE_steps
 !
 COM_here=LOG
 if (present(COM_)) COM_here=COM_
 !
 if (present(G_lesser_only)) then
   !
   if (G_lesser_only) then
     !
     YAMBO_ALLOC(G_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nkibz,1))
     !
     YAMBO_ALLOC(G_lesser_reference,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nkibz))
     !
     call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/1,2/),ID=ID) 
     io_G=io_RT_components('G_lesser_RESTART',ID)
     !
     if (present(IO_error)) IO_error=io_G/=0
     if (io_G/=0) return
     !
     do ik=1,nkibz
       IO_ACT=manage_action(OP_IF_START_RD_CL_IF_END,ik,1,nkibz)
       call io_control(ACTION=IO_ACT,COM=COM_here,SEC=(/ik+2/),MODE=DUMP,ID=ID)
       io_G=io_RT_components('G_lesser_RESTART_K_section',ID)
     enddo
     !
   endif
   !
   return
   !
 else
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) 
   io_G=io_RT_components('G_lesser_RESTART',ID)
   !
 endif
 !
 ! Use Core value for dynamical step 
 !-----------------------------------
 RT_dyn_step=RT_step
 !
 ! Check for saved current and polarization database
 !---------------------------------------------------
 if (present(IO_error)) IO_error=.FALSE.
 !
 NE_i_time=1
 call io_control(ACTION=OP_RD,COM=COM_here,MODE=DUMP,SEC=(/1/),ID=ID)
 io_OBS=io_RT_components(what,ID)
 if( io_OBS/=0 ) then
   if (present(IO_error)) IO_error=.TRUE.
   return
 endif
 !
 ! Only first section I/O
 !------------------------
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
 Pol%N    =3
 Curr%N   =3
 !
 if (what=="JP".and..not.allocated(Curr%F_t)) then
   !
   CACHE_OBS_steps=RT_conf%T_n_steps+RT_conf%T_n_range(1)-1
   CACHE_OBS_steps_now=RT_conf%T_n_steps+RT_conf%T_n_range(1)-1
   call RT_control_alloc("cache")
   NE_i_time=NE_steps
   !
   call io_control(ACTION=RD_CL,COM=COM_here,MODE=DUMP,SEC=(/3/),ID=ID)
   io_OBS=io_RT_components("JP",ID)
   !
   YAMBO_ALLOC(Curr%F_t,(RT_conf%T_n_steps,3))
   YAMBO_ALLOC(Pol%F_t,(RT_conf%T_n_steps,3))
   Curr%F_t =cZERO_DP
   Pol%F_t =cZERO_DP
   do i_dir=1,3
     do it=1,RT_conf%T_n_steps
       it_now=it+RT_conf%T_n_range(1)-1
       Curr%F_t(it,i_dir)=cmplx(J_cache(i_dir,it_now),kind=DP)
       Pol%F_t(it,i_dir) =cmplx(P_cache(i_dir,it_now),kind=DP)
     enddo
   enddo
   !
   call RT_control_free("cache")
   !
 endif
 !
 ! Define Efield amplitude
 !-------------------------
 do i_f=1,n_ext_fields
   Efield(i_f)%amplitude=Efield_strength(Efield(i_f)%intensity,'AU')
 enddo
 !
 call RT_control_free("times")
 !
end subroutine
