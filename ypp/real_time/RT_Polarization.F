!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine RT_Polarization(en,k,q)
 !
 use pars,           ONLY:cZERO,SP
 use D_lattice,      ONLY:dl_sop,sop_inv,DL_vol
 use units,          ONLY:FS2AUT
 use YPPm,           ONLY:DIPs_kind
 use YPP_real_time,  ONLY:RT_time,Pol,RT_conf,gimme_a_G_Time_Point
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol
 use LIVE_t,         ONLY:live_timing
 use RT_control,     ONLY:Gless_RESTART_RT_IO_t
 use real_time,      ONLY:RT_nk,RT_bands,dG_lesser,RT_P
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:DUMP,OP_RD_CL,REP,NONE,manage_action,OP_IF_START_RD_CL_IF_END
 use parallel_int,   ONLY:PARALLEL_global_indexes
 use DIPOLES,        ONLY:DIP_iR
 !
#include<memory.h>
 !
 type(levels)  :: en
 type(bz_samp) :: k,q
 !
 ! Work Space
 !
 integer               :: ID,io_err,i_T,ik,IO_ACT,i_n,i_m,i_s,i_star,i_sp_pol
 integer, external     :: io_RT_components
 complex(SP)           :: DIP_symm(3)
 !
 call section('+','Real-Time Polarization') 
 !=========================================
 !
 ! I/O...
 !--------
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1,2/),ID=ID) 
 io_err=io_RT_components('G_lesser',ID)
 if(io_err/=0) then
   call warning(" History of G_lesser not found. Impossible to re-build the Polarization")
   return
 endif
 !
 ! Dipoles
 !---------
 DIPs_kind="LOAD"
 call DIPOLES_driver(k,en,q)
 !
 ! Alloc
 !-------
 YAMBO_ALLOC(dG_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,1))
 dG_lesser=cZERO
 !
 ! Time conf
 !-----------
 YAMBO_FREE(RT_time)
 call RT_time_configuration_setup('Glesser reset',Gless_RESTART_RT_IO_t)
 stop
 !
 ! Par setup
 !-----------
 call PARALLEL_global_indexes(en,k,q," ",RESET=.TRUE.)
 !
 ! Loop
 !------
 RT_P=cZERO
 call live_timing("Building and slicing the P(t)",RT_conf%T_n_steps)
 do i_T=1,RT_conf%T_n_steps
   !
   call gimme_a_G_Time_Point(i_T)
   !
#if defined _PAR_IO
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/3/),ID=ID)
   io_err=io_RT_components('G_lesser',ID)
#else
   do ik = 1, RT_nk 
     IO_ACT=manage_action(OP_IF_START_RD_CL_IF_END,ik,1,RT_nk)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+2/),ID=ID)
     io_err=io_RT_components('G_lesser',ID)
   enddo
#endif
   !
   RT_P=cZERO
   do i_sp_pol=1,n_sp_pol
     do i_n=RT_bands(1),RT_bands(2)
       do i_m=RT_bands(1),RT_bands(2)
         do ik=1,k%nibz
           DIP_symm=cZERO
           do i_star=1,k%nstar(ik)
             i_s = sop_inv(k%star(ik,i_star))
             DIP_symm(:)=DIP_symm(:)+matmul(dl_sop(:,:,i_s), DIP_iR(:,i_n,i_m,ik,i_sp_pol))
           enddo
           RT_P=RT_P+DIP_symm*dG_lesser(i_m,i_n,ik,1)/DL_vol*k%weights(ik)
         enddo
       enddo
     enddo
   enddo
   write (100,'(4g20.5)') RT_time(i_T)/FS2AUT,real(RT_P)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing(steps=1)
 !
 ! CLEAN
 !
end subroutine
