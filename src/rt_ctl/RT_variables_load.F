!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_variables_load(Xen,Xk,RT_db1_E,RT_db1_k,n_T_steps)
 !
 use pars,            ONLY:lchlen
 use real_time,       ONLY:RT_carriers,NE_i_time
 use R_lattice,       ONLY:bz_samp,nqbz,nqibz,nkbz,nkibz,&
&                          bz_samp_duplicate,bz_samp_reset
 use com,             ONLY:core_io_path
 use electrons,       ONLY:levels,BZ_RIM_table,BZ_RIM_nkpt,BZ_RIM_ipol_weight,BZ_RIM_nbands,&
&                          E_duplicate,E_reset
 use com,             ONLY:jobstr,error
 use IO_m,            ONLY:io_control,OP_RD_CL,NONE,DUMP
 use memory_m,        ONLY:mem_est
 use wave_func,       ONLY:wf_ng
 use interfaces,      ONLY:RT_carriers_object
 !
 implicit none
 !
 type(levels)      :: Xen,RT_db1_E
 type(bz_samp)     :: Xk,RT_db1_k
 integer           :: n_T_steps
 !
 ! Work space
 !
 type(levels)      :: Xen_tmp
 type(bz_samp)     :: RT_db1_q,Xk_tmp,Xq_tmp
 !
 integer           :: io_err,ID,nqbz_save,nqibz_save,nkbz_save,nkibz_save,wf_ng_save
 character(lchlen) :: core_io_path_save
 !
 integer, external :: io_DB1_selective_scan,io_RIM_E_and_k,io_QINDX
 integer, external :: RT_occ_check_and_load_ref
 !
 if(allocated(BZ_RIM_table)) call error("RIM DBs already present. This is not compatible with Transient abs")
 !
 nqbz_save =nqbz
 nqibz_save=nqibz
 !
 nkbz_save =nkbz
 nkibz_save=nkibz
 !
 wf_ng_save=wf_ng
 !
 io_err=io_DB1_selective_scan(RT_db1_E,RT_db1_k,trim(jobstr),'all')
 if (io_err<0) call error ('DB1 database not found in '//trim(jobstr))
 call k_expand(RT_db1_k)
 nkbz=RT_db1_k%nbz
 !
 core_io_path_save=core_io_path
 core_io_path=jobstr
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,ID=ID)
 io_err=io_RIM_E_and_k(RT_db1_E,RT_db1_k,ID)
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=ID)
 io_err=io_QINDX(RT_db1_k,RT_db1_q,ID)
 call k_expand(RT_db1_q)
 !
 nqibz=RT_db1_q%nibz
 nqbz=RT_db1_q%nbz
 !
 NE_i_time=0
 n_T_steps=RT_occ_check_and_load_ref(RT_db1_E,RT_db1_k)
 call RT_check_RIM(RT_db1_k,RT_db1_E) 
 call RT_carriers_object(RT_carriers,RT_db1_E,RT_db1_k,WHAT='allocate')
 !
 BZ_RIM_nbands=0
 if(allocated(BZ_RIM_table)) then
   deallocate(BZ_RIM_nkpt,BZ_RIM_table,BZ_RIM_ipol_weight)
   call mem_est("BZ_RIM_nkpt BZ_RIM_table")
 endif
 !
 ! Reset symmetries and nqbz, nkbz
 !
 core_io_path=core_io_path_save
 call E_reset(Xen_tmp)
 call bz_samp_reset(Xk_tmp)
 call E_duplicate(Xen,Xen_tmp)
 call bz_samp_duplicate(Xk,Xk_tmp)
 !
 io_err=io_DB1_selective_scan(Xen_tmp,Xk_tmp,trim(core_io_path),'all')
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=ID)
 io_err=io_QINDX(Xk_tmp,Xq_tmp,ID)
 call k_expand(Xk_tmp)
 !
 nqbz =nqbz_save
 nqibz=nqibz_save
 !
 nkbz =nkbz_save
 nkibz=nkibz_save
 !
 wf_ng=wf_ng_save
 !
end subroutine RT_variables_load
