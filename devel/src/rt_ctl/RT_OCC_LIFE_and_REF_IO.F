!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_OCC_LIFE_and_REF_IO(E,what,k,Time,imposed_COM)
 !
 use units,          ONLY:BO2ANG,FS2AUT,HA2EV
 use pars,           ONLY:SP,lchlen
 use parser_m,       ONLY:parser
 use rt_ctl,         ONLY:ID,RT_IO_JPO_steps,RT_IO_steps_DB
 use real_time,      ONLY:G_lesser,NE_i_time,RT_step,MEM_index,&
&                         RT_E_occupations,RT_H_occupations,G_MEM_steps,REF_lifetime,&
&                         G_lesser_reference,RT_delta0_occupations,E_P_Hole_lifetime,&
&                         E_P_Electron_lifetime,E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,RT_nk,l_RT_occupations_in_use,&
&                         RT_E_occupations_reference,RT_H_occupations_reference
 use SC,             ONLY:SC_bands
 use IO_m,           ONLY:io_control,NONE,OP_WR_CL,OP_RD,RD_CL,OP_RD_CL,DUMP,variable_is_found
 use electrons,      ONLY:levels,n_sp_pol,n_met_bands,&
&                         n_full_bands,nel,E_reset,spin_occ
 use com,            ONLY:jobstr,msg,file_exists,warning,error
 use QP_m,           ONLY:OCC_ctl
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp,nXkibz,bz_samp_reset
 use D_lattice,      ONLY:DL_vol,input_Tel_is_negative
 use LIVE_t,         ONLY:what_is_running
 use stderr,         ONLY:intc
 use vec_operate,    ONLY:sort
 use interfaces,     ONLY:OCCUPATIONS_Gaps
 !
 implicit none
 !
 type(levels),       intent(inout) :: E
 character(1)                      :: what
 type(bz_samp),optional,intent(in) :: k
 real(SP), optional, intent(inout) :: Time
 integer , optional, intent(in)    :: imposed_COM
 !
 ! Work Space
 !
 integer           :: ik,ib,i1
 real(SP)          :: PE_nel
 logical           :: is_def
 !
 ! I/O
 !
 integer           :: io_err_occ,io_err_E,COM_,file_number
 character(lchlen) :: jobstr_save
 integer, external :: io_RT_components
 !
 ! Real-Time occupations
 !=======================
 !
 !
 ! (a) Write
 !
 if (.not.present(Time)) then
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1/),ID=ID(6)) 
   io_err_occ=io_RT_components('occupations',ID(6))
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/NE_i_time+1/),ID=ID(6))
   io_err_occ=io_RT_components('occupations',ID(6))
   !
   return
   !
 endif
 !
 ! (b) Load
 !
 jobstr_save=jobstr
 if (what=="X") jobstr=trim(OCC_ctl(1))
 if (what=="K") jobstr=trim(OCC_ctl(2))
 if (what=="G") jobstr=trim(OCC_ctl(3))
 call parser(what//'fnFdb',is_def)
 if (.not.is_def) jobstr=jobstr_save
 !  
 COM_=NONE 
 if (present(imposed_COM)) then
   COM_=imposed_COM
 endif
 ! 
 ! OCCUPATIONS I/O (I)
 !====================
 !
 call io_control(ACTION=OP_RD,COM=COM_,SEC=(/1/),MODE=DUMP,ID=ID(6))
 io_err_occ=io_RT_components('occupations',ID(6))
 !
 if (io_err_occ<0) then
#if !defined _YPP_RT 
   call warning ('Occupations database not found in folder: '//trim(jobstr))
#endif
   jobstr=jobstr_save
   return
 else
#if !defined _YPP_RT 
   call msg('s','Occupations database read from folder: '//trim(jobstr))
#endif
 endif
 !
 ! OCCUPATIONS I/O (II)
 !======================
 ! 
 ! NE_time=(NE_i_time-1)*RT_step but I/O only when NE_i_time= N * RT_IO_steps
 !
 NE_i_time= (Time+RT_step)/RT_step
 NE_i_time= NE_i_time-mod(NE_i_time-1,RT_IO_JPO_steps)
 !
 if (.not.allocated(RT_delta0_occupations)) then
   allocate(RT_delta0_occupations(SC_bands(1):SC_bands(2),RT_nk))
   call mem_est("RT_delta0_occupations",(/size(RT_delta0_occupations)/),(/SP/))
 endif
 !
 call io_control(ACTION=RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=ID(6))
 io_err_occ=io_RT_components('occupations',ID(6))
 if(NE_i_time>0) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/NE_i_time+1/),MODE=DUMP,ID=ID(6))
   io_err_occ=io_RT_components('occupations',ID(6))
 endif
 ! 
 ! REFERENCE I/O
 !===============
 !  
 if (.not.allocated(REF_lifetime)) then
   allocate(REF_lifetime(SC_bands(1):SC_bands(2),RT_nk,6))
   call mem_est("REF_lifetime",(/size(REF_lifetime)/),(/SP/))
   allocate(G_lesser_reference(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk))
   call mem_est("Gf_l_ref",(/size(G_lesser_reference)/),(/2*SP/))
   allocate(RT_E_occupations_reference(SC_bands(1):SC_bands(2),RT_nk))
   allocate(RT_H_occupations_reference(SC_bands(1):SC_bands(2),RT_nk))
   call mem_est("RT_E_occ_ref RT_H_occ_ref",&
   &  (/size(RT_E_occupations_reference),size(RT_H_occupations_reference)/),(/SP,SP/))
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
 jobstr=jobstr_save
 !
end subroutine RT_OCC_LIFE_and_REF_IO
