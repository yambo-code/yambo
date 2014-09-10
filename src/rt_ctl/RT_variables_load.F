!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine RT_variables_load(Xen,Xk,n_T_steps)
 !
 use pars,            ONLY:lchlen
 use real_time,       ONLY:rt,NE_i_time
 use R_lattice,       ONLY:bz_samp,nqbz,nkbz
 use com,             ONLY:core_io_path
 use electrons,       ONLY:levels,BZ_RIM_table
 use com,             ONLY:jobstr,error
 use IO_m,            ONLY:io_control,OP_RD_CL,NONE,DUMP
 !
 implicit none
 !
 type(levels)      :: Xen
 type(bz_samp)     :: Xk
 integer           :: n_T_steps
 !
 ! Work space
 !
 type(levels)      :: RT_db1_E,Xen_tmp
 type(bz_samp)     :: RT_db1_k,RT_db1_q,Xk_tmp,Xq_tmp
 !
 integer           :: io_err,ID
 character(lchlen) :: core_io_path_save
 !
 integer, external :: io_DB1_selective_scan,io_RIM_E_and_k,io_QINDX
 integer, external :: RT_occ_check_and_load_ref,RT_k_grid
 !
 if(allocated(BZ_RIM_table)) call error("RIM DBs already present. This is not compatible with Transient abs")
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
 nqbz=RT_db1_q%nbz
 !
 NE_i_time=0
 n_T_steps=RT_occ_check_and_load_ref(RT_db1_E,RT_db1_k)
 call RT_check_RIM(RT_db1_k,RT_db1_E) 
 call RT_alloc_def_or_free(rt,RT_db1_E,RT_db1_k,1)
 !
 ! Reset symmetries and nqbz
 core_io_path=core_io_path_save
 io_err=io_DB1_selective_scan(Xen_tmp,Xk_tmp,trim(core_io_path),'all')
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=ID)
 io_err=io_QINDX(Xk_tmp,Xq_tmp,ID)
 nqbz=Xq_tmp%nbz
 nkbz=Xk%nbz
 !
end subroutine RT_variables_load
