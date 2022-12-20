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
subroutine ELPH_sngl_db(k,E,q)
 !
 use units,               ONLY:HA2EV
 use pars,                ONLY:schlen,SP
 use stderr,              ONLY:intc
 use interfaces,          ONLY:ELPH_alloc 
 use ELPH,                ONLY:GKKP,elph_nb,ph_modes,elph_nQ,elph_nk_bz
 use YPP_ELPH,            ONLY:ELPH_DB_alloc,ELPH_DB_free,DB_PH_kpt,DB_nq,DB_q_map,DB_PH_qpt,&
&                              DB_ph_modes,DB_nb,DB_nq         
 use com,                 ONLY:msg
 use electrons,           ONLY:levels
 use YPP_ELPH,            ONLY:l_gkkp_expand_only_k,elph_dbs_path,DB_nk
 use R_lattice,           ONLY:bz_samp
 use parser_m,            ONLY:parser
 use zeros,               ONLY:k_iku_zero
 use vec_operate,         ONLY:v_is_zero
 use LIVE_t,              ONLY:live_timing
 use IO_m,                ONLY:REP,OP_WR_CL,VERIFY,OP_APP_CL,OP_RD_CL
 use IO_int,              ONLY:io_control
 !
#include<memory.h>
 !
 type(bz_samp)      :: k,q
 type(levels)       :: E
 integer            :: qindx_ID,qindx_ID_frag,io_err,iqbz,idb
 integer, external  :: ELPH_databases_IO_elemental,io_ELPH
 character(schlen)  :: db_name 
 integer            :: n_dbs_found,ID
 real(SP)           :: v(3)
 !
 call parser('GkkpExpOnlyK',l_gkkp_expand_only_k)
 !
 if(l_gkkp_expand_only_k) call ELPH_load_indexes(k,E,q,qindx_ID,qindx_ID_frag)
 !
 call msg('s','Inspecting databases ...')
 n_dbs_found=0
 !
 ! Withtout symmetries at maximum there will be q%nbz databases
 do idb=1,q%nbz
   io_err=ELPH_databases_IO_elemental("dV EXIST",idb)
   if (io_err==0) n_dbs_found=n_dbs_found+1 
 enddo
 DB_nq=n_dbs_found
 !
 call msg('sr','Found '//intc(n_dbs_found)//' databases in: '//trim(elph_dbs_path))
 if(n_dbs_found==0) return
 !
 call ELPH_DB_alloc( )
 !
 call k_ibz2bz(k,'i',.TRUE.)
 call k_ibz2bz(q,'i',.TRUE.)
 !
 db_name='gkkp_expanded'
 !
 ph_modes   =DB_ph_modes
 elph_nb    =DB_nb
 elph_nQ    =q%nbz
 elph_nk_bz =k%nbz
 ! 
 call ELPH_alloc("PHONONS")
 call ELPH_alloc("LEVELS dV DW",GKKP=GKKP,Nq_mem=1)
 !
 call live_timing('ELPH databases: ',DB_nq) 
 do idb=1,q%nbz
   io_err=ELPH_databases_IO_elemental("dV EXIST",idb)
   if(io_err==0) then
     if(DB_nk/=k%nbz.and..not.l_gkkp_expand_only_k) call error('Wrong number of k-points ')
     !
     ! the q-point is read with KPT
     !
     io_err=ELPH_databases_IO_elemental("dV FREQS POL_VEC KPT",idb)
     call ELPH_databases_IO_grids_check(k)
     io_err=ELPH_databases_IO_elemental("dV KPT GKKP",idb)
     call ELPH_databases_IO_transfer_and_write(idb,k,E,GKKP)
     !
     ! Search for the q-point
     !
     do iqbz=1,q%nbz
       v=DB_PH_qpt(idb,:)
       if (v_is_zero(v+q%ptbz(iqbz,:),zero_=k_iku_zero)) DB_Q_map(idb)=iqbz
     enddo
     !
     if(DB_Q_map(idb)==0) call error('Q-point '//intc(idb)//' not found ! ')
     !
     if (DB_Q_map(idb)==1) call io_control(ACTION=OP_APP_CL,SEC=(/1,2/),ID=ID)
     if (DB_Q_map(idb)> 1) call io_control(ACTION=OP_APP_CL,SEC=(/DB_Q_map(idb)+1/),ID=ID)
     io_err=io_ELPH(ID,db_name)
     call live_timing(steps=1)
     !
   endif
 enddo
 call live_timing( )
 !
end subroutine
