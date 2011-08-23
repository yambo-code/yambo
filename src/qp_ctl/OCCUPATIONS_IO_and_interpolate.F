!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine OCCUPATIONS_IO_and_interpolate(E,what,k,Time,imposed_COM)
 !
 use pars,           ONLY:SP,lchlen
 use real_time,      ONLY:ID,G_lesser,NE_i_time,max_rt_steps_disk,RT_step
 use SC,             ONLY:SC_bands,RT_occupations,RT_nk,RT_k 
 use IO_m,           ONLY:io_control,NONE,OP_APP_WR_CL,OP_RD,RD_CL,DUMP
 use electrons,      ONLY:levels
 use com,            ONLY:jobstr,msg
 use QP_m,           ONLY:OCC_ctl
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp
 use timing,         ONLY:what_is_running
 !
 implicit none
 !
 type(levels),       intent(in)    :: E
 character(1)                      :: what
 type(bz_samp),optional,intent(in) :: k
 real(SP), optional, intent(inout) :: Time
 integer , optional, intent(in)    :: imposed_COM
 !
 ! Work Space
 !
 integer           :: ib,io_err,COM_,ik,i_err,ik_near
 integer, external :: io_SC_components
 character(lchlen) :: jobstr_save
 real(SP)          :: k_diff(3)
 !
 integer, external :: k_the_nearest
 !
 ! Real-Time occupations
 !=======================
 !
 ! (a) Load
 !
 if (.not.present(Time)) then
   do ib=1,SC_bands(2)
     RT_occupations(ib,:)=-real((0._SP,1._SP)*G_lesser(ib,ib,:))
   enddo
   ! 
   ! Note that here as well as in ndb.GreenF the SEC is used only to
   ! store the header + the descriptors. Therefore in order to write
   ! those informations I pass directly NE_i_time that starts from 1.
   !
   call io_control(ACTION=OP_APP_WR_CL,COM=NONE,SEC=(/NE_i_time/),ID=ID(6))
   io_err=io_SC_components('occupations',E,ID(6))
   ! 
 else
   ! 
   if (index(what_is_running,'YPP')==0) call section('p','NEGF Occupations I/O and interpolation')
   !
   jobstr_save=jobstr
   if (what=="X") jobstr=trim(OCC_ctl(1))
   if (what=="K") jobstr=trim(OCC_ctl(2))
   if (what=="G") jobstr=trim(OCC_ctl(3))
   !  
   COM_=NONE 
   if (present(imposed_COM)) then
     COM_=imposed_COM
   endif
   !  
   call io_control(ACTION=OP_RD,COM=COM_,SEC=(/1/),MODE=DUMP,ID=ID(6))
   io_err=io_SC_components('occupations',E,ID(6))
   !
   if (io_err<0) then
     jobstr=jobstr_save
     return
   endif
   ! 
   ! NE_time=(NE_i_time-1)*RT_step
   !
   NE_i_time= int( real(Time/RT_step+1)/real(max_rt_steps_disk) ) * max_rt_steps_disk
   Time=(NE_i_time-1)*RT_step
   !
   if (.not.allocated(RT_k)) then
     allocate(RT_occupations(SC_bands(2),RT_nk),RT_k(RT_nk,3))
     call mem_est("RT_occupations",(/size(RT_occupations)/),(/SP/))
   endif
   !
   call io_control(ACTION=RD_CL,COM=COM_,SEC=(/NE_i_time/),MODE=DUMP,ID=ID(6))
   io_err=io_SC_components('occupations',E,ID(6))
   !
   jobstr=jobstr_save
   !
   if (index(what_is_running,'YPP')>0) return
   !
   ! Interpolation
   !===============
   !
   do ik=1,k%nibz
     ik_near=k_the_nearest(k%pt(ik,:),RT_k,RT_nk,.FALSE.,i_err)
     do ib=SC_bands(1),SC_bands(2)
       E%f(ib,ik,:)=RT_occupations(ib,ik_near)
     enddo
   enddo
   !
   deallocate(RT_occupations,RT_k)
   call mem_est("RT_occupations")
   !
   ! I need to redefine here several quantities related to the occupations
   ! using REPORT_Occupations
   !
   call msg('r','')
   call REPORT_Occupations(E,2)
   !
 endif
 !
end subroutine OCCUPATIONS_IO_and_interpolate
