!
!        Copyright (C) 2000-2014 the YAMBO team
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
 use YPP,            ONLY:RT_J_t,RT_P_t,J_steps
 use pars,           ONLY:DP,SP,schlen
 use R_lattice,      ONLY:nkibz
 use memory_m,       ONLY:mem_est
 use SC,             ONLY:SC_bands
 use LIVE_t,         ONLY:live_timing
 use IO_m,           ONLY:OP_RD,OP_RD_CL,io_control,NONE,DUMP,LOG,RD_CL_IF_END
 use rt_ctl,         ONLY:RT_IO_JPO_steps,J_and_P_cache,J_and_P_cache_size,RT_IO_JPO_steps
 use real_time,      ONLY:NE_steps,NE_i_time,RT_ind_J,RT_P,&
&                         G_lesser,G_lesser_reference
 use fields,         ONLY:Efield,n_ext_fields,Efield_strength
 !
 implicit none
 !
 logical, optional :: IO_error,first_sec_only,G_lesser_only
 integer, optional :: COM_
 character(*), optional :: mesg
 !
 ! Work Space
 !
 integer            :: io_J_and_P,ID,i_f,io_G,COM_here,ik,file_number,NE_steps_save
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
     allocate(G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),nkibz,1))
     call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
     !
     allocate(G_lesser_reference(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),nkibz))
     call mem_est("Gf_l_ref",(/size(G_lesser_reference)/),(/2*SP/))
     !
     call io_control(ACTION=OP_RD,COM=COM_here,MODE=DUMP,SEC=(/1/),ID=ID) ! Current response
     io_G=io_RT_components('G',ID)
     !
     if (present(IO_error)) then
       IO_error=io_G/=0
     endif
     if (io_G/=0) return
     !
     do ik=1,nkibz
       call io_control(ACTION=RD_CL_IF_END,COM=COM_here,SEC=(/ik+1/),MODE=DUMP,ID=ID)
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
 ! Check for saved current and polarization database
 !---------------------------------------------------
 !
 if (present(IO_error)) then
   IO_error=.FALSE.
 endif
 !
 call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/1/),ID=ID)
 io_J_and_P=io_RT_components('J_and_P',ID)
 if( io_J_and_P/=0 ) then
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
   allocate(RT_J_t(J_steps,3),RT_P_t(J_steps,6))
   call mem_est("RT_P_t",(/size(RT_J_t)/),(/2*SP/))
   call mem_est("RT_J_t",(/size(RT_J_t)/),(/2*SP/))
 endif
 allocate(J_and_P_cache(J_and_P_cache_size,RT_IO_JPO_steps))
 call mem_est("Current_Cache",(/size(J_and_P_cache)/),(/SP/))
 !
 RT_J_t        =(0._DP,0._DP)
 RT_P_t        =(0._DP,0._DP)
 !
 NE_steps=NE_steps_save
 !
 call live_timing('I/O'//trim(mesg_)//':',J_steps)
 !
 do NE_i_time=1,NE_steps
   !
   if (mod(NE_i_Time-1,RT_IO_JPO_steps)/=0) cycle
   !
   file_number=(NE_i_Time-1)/RT_IO_JPO_steps+1
   !
   call io_control(ACTION=OP_RD_CL,COM=COM_here,MODE=DUMP,SEC=(/file_number/),ID=ID) 
   io_J_and_P=io_RT_components('J_and_P',ID)
   !
   RT_J_t(file_number,1:3) = RT_ind_J(:)
   RT_P_t(file_number,1:3) = RT_P(:)
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 ! Define Efield amplitude
 !
 do i_f=1,n_ext_fields
   Efield(i_f)%amplitude=Efield_strength(Efield(i_f)%intensity,'AU')
 enddo
 !
 deallocate(J_and_P_cache)
 call mem_est("Current_Cache")
 !
end subroutine
