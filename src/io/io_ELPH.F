!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function io_ELPH(ID,what)
 !
 use pars,           ONLY:SP,schlen,LP
 use IO_m,           ONLY:io_connect,io_sec,ver_is_gt_or_eq,&
&                         io_variable_elemental,io_status,io_bulk,io_header,&
&                         read_is_on,write_is_on,def_variable_elemental,&
&                         io_mode,DUMP,io_fragment,frag_ELPH,IO_INCOMPATIBLE_VAR,IO_return
 use ELPH,           ONLY:ph_modes,elph_nb,PH_freqs_sq,elph_nk_bz,ELPH_desc,&
&                         QP_PH_n_G_bands,PH_W_debye,elph_nQ,PH_kpt_bz,l_GKKP_hosts_bare_dV,&
&                         elph_use_q_grid,PH_qpt,elph_nQ_used,GKKP,GKKP_bare,&
&                         gsqF_fan,gsqF_dw ,gsqF_ca_corr ,gsqF_life_bose ,gsqF_life_f,&
&                         PH_pol_vector,elph_Ham_bands,elph_branches,GKKP,elph_grids_are_expanded
 use stderr,         ONLY:intc
 use interfaces,     ONLY:ELPH_alloc
 use R_lattice,      ONLY:nkbz,nqibz,q_pt
 use D_lattice,      ONLY:n_atoms
 use electrons,      ONLY:n_sp_pol
 use QP_m,           ONLY:QP_n_states,QP_nb,QP_nk,QP_table
 use parallel_m,     ONLY:PAR_Q_bz_index
 use vec_operate,    ONLY:rlu_v_is_zero,c2a
 use zeros,          ONLY:k_rlu_zero
 use timing_m,       ONLY:timing
 use descriptors,    ONLY:IO_desc_reset
 !
#include<memory.h>
 !
 integer      ::ID
 character(*) ::what
 !
 !Work Space
 !
 integer                ::iq,ierr,iq_mem
 real(SP)               ::v(3)
 character(schlen)      ::db_name
 logical                ::l_no_matrix_elements,l_gFsq,l_gkkp
 !
 call timing('io_ELPH',OPR='start')
 !
 l_no_matrix_elements=.FALSE.
 l_gFsq              =.FALSE.
 l_gkkp              =.FALSE.
 !
 ! DB name
 !
 if (index(what,'gkkp_expanded')>0) then
   db_name='elph_gkkp_expanded'
 else if (index(what,'gFsq')>0) then
   db_name='elph_gFsq'
 else 
   db_name='elph_gkkp'
   l_gkkp=.TRUE.
 endif
 !
 elph_grids_are_expanded=index(what,'gkkp_expanded')>0
 l_gFsq                 =index(what,'gFsq')>0
 l_no_matrix_elements   =index(what,'no_matrix_elements')>0
 !
 io_ELPH=io_connect(desc=trim(db_name),type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))).and.frag_ELPH)
 if (io_ELPH/=0) then
   call IO_return(ID,"io_ELPH")
   if (elph_grids_are_expanded) elph_grids_are_expanded=.FALSE.
   return
 endif
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   if (.not.elph_grids_are_expanded) io_ELPH=io_header(ID,R_LATT=.true.,KPTS=.true.,FRAG=frag_ELPH)
   if (     elph_grids_are_expanded) io_ELPH=io_header(ID,R_LATT=.true.,KPTS=.false.,IMPOSE_SN=.FALSE.,FRAG=frag_ELPH)
   if (io_ELPH/=0) then
     call IO_return(ID,"io_ELPH")
     return
   endif
   !
   call IO_desc_reset(ELPH_desc)
   !
   if (.not.ver_is_gt_or_eq(ID,revision=18920)) call def_variable_elemental(ID,"PARS",6,SP,0)
   if (     ver_is_gt_or_eq(ID,revision=18920)) call def_variable_elemental(ID,"PARS",7,SP,0)
   call io_variable_elemental(ID,VAR="[ELPH] Phonon modes",I0=ph_modes,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=ELPH_desc)
   call io_variable_elemental(ID,VAR="[ELPH] Q points",I0=elph_nQ,CHECK=.true.,OP=(/"=="/),&
&                             TERMINATOR="[avail]",DESCRIPTOR=ELPH_desc)
   if (l_gFsq) call io_variable_elemental(ID,VAR="[ELPH]",I0=elph_nQ_used,CHECK=.true.,OP=(/"=="/),&
&                                         TERMINATOR="[used]",DESCRIPTOR=ELPH_desc)
   call io_variable_elemental(ID,VAR="[ELPH] K points",I0=elph_nk_bz,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=ELPH_desc)
   call io_variable_elemental(ID,VAR="[ELPH] El-PH bands",I0=elph_nb,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=ELPH_desc)
   call io_variable_elemental(ID,VAR="[ELPH] Using the Q-grid",L0=elph_use_q_grid,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=ELPH_desc)
   if (ver_is_gt_or_eq(ID,revision=18920)) then
     call io_variable_elemental(ID,VAR="[ELPH] Bare interaction",L0=l_GKKP_hosts_bare_dV,&
&                               CHECK=.true.,OP=(/"=="/),DESCRIPTOR=ELPH_desc)
   endif
   if (l_gFsq) then
     call def_variable_elemental(ID,"QP_nb_nk_n_states",3,SP,0)
     call io_variable_elemental(ID,I0=QP_nb)
     call io_variable_elemental(ID,I0=QP_nk)
     call io_variable_elemental(ID,I0=QP_n_states,VAR='[ELPH] QP tot states')
   endif
   call def_variable_elemental(ID,"",0,0,1)
   !
   if (io_mode(ID)==DUMP.or.write_is_on(ID)) then
     QP_PH_n_G_bands=elph_nb
     elph_Ham_bands =(/1,elph_nb/)
     elph_branches  =(/1,ph_modes/)
   endif
   !
   io_ELPH=io_status(ID)
   if (io_ELPH/=0) then
     call IO_return(ID,"io_ELPH")
     return
   endif
   !
   call io_bulk(ID,VAR="MAX_PH_FREQ",VAR_SZ=(/1/))
   call io_bulk(ID,R0=PH_W_debye)
   !
   if (read_is_on(ID)) call ELPH_alloc("PHONONS")
   !
   call io_bulk(ID,VAR="PH_Q",VAR_SZ=(/elph_nQ,3/) )
   call io_bulk(ID,R2=PH_qpt(:,:))
   !
   if (l_gkkp.and.allocated(q_pt).and.elph_use_q_grid) then
     do iq=1,nqibz
       call c2a(v_in=q_pt(iq,:)+PH_qpt(iq,:),v_out=v,mode="ki2a")
       if (.not.rlu_v_is_zero(v,zero_=k_rlu_zero)) then
         io_ELPH=IO_INCOMPATIBLE_VAR
         call IO_return(ID,"io_ELPH")
         return
       endif
     enddo
   endif
   !
   if (elph_grids_are_expanded) then
     !
     call io_bulk(ID,VAR="PH_K",VAR_SZ=(/nkbz,3/) )
     call io_bulk(ID,R2=PH_kpt_bz(:,:))
     !
   endif
   !
 endif
 !
 iq=maxval(io_sec(ID,:))-1
 if (iq<=0) then
   call IO_return(ID,"io_ELPH")
   return
 endif
 !
 ! Fragmentation
 !
 call io_fragment(ID,i_fragment=iq,ierr=ierr)
 !
 ! When the DB is fragmented I allow a partial reading checking
 ! if the fragment exists or not.
 ! If the fragment does not exist (ierr<0) I return an error code
 !
 if (ierr<0.and.read_is_on(ID)) then
   io_ELPH=-1
   call IO_return(ID,"io_ELPH")
   return
 endif
 !
 if (l_gFsq) then
   !
   if (iq==1) then
     call io_bulk(ID,VAR="QP_table",VAR_SZ=(/QP_n_states,3+n_sp_pol-1/))
     call io_bulk(ID,I2=QP_table)
   endif
   !
   iq_mem=iq
   if (allocated(PAR_Q_bz_index)) then
     iq_mem=PAR_Q_bz_index(iq)
   endif
   !
   call io_bulk(ID,VAR="ELPH_GFSQ_fan_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
   call io_bulk(ID,R2=gsqF_fan(:,iq_mem,:,1))
   call io_bulk(ID,VAR="ELPH_GFSQ_dw_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
   call io_bulk(ID,R2=gsqF_dw(:,iq_mem,:))
   call io_bulk(ID,VAR="ELPH_GFSQ_ca_corr_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
   call io_bulk(ID,R2=gsqF_ca_corr(:,iq_mem,:,1))
   call io_bulk(ID,VAR="ELPH_GFSQ_life_bose_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
   call io_bulk(ID,R2=gsqF_life_bose(:,iq_mem,:,1))
   call io_bulk(ID,VAR="ELPH_GFSQ_life_f_Q"//trim(intc(iq)),VAR_SZ=(/QP_n_states,ph_modes/))
   call io_bulk(ID,R2=gsqF_life_f(:,iq_mem,:,1))
   !
   ! PH frequencies (again)
   !
   if (iq==1) then
     call io_bulk(ID,VAR="PH_FREQS",VAR_SZ=(/elph_nQ,ph_modes/))
     call io_bulk(ID,R2=PH_freqs_sq)
   endif
   !
   call IO_return(ID,"io_ELPH")
   return
   !
 endif
 !
 ! Allocation
 !
 if (read_is_on(ID)) then
   if (.not.l_no_matrix_elements) then
     call ELPH_alloc("LEVELS dV DW",GKKP=GKKP)
     if (l_GKKP_hosts_bare_dV) call ELPH_alloc("dV_bare",GKKP=GKKP_bare)
   else
     call ELPH_alloc("LEVELS",GKKP=GKKP)
   endif
 endif
 !
 ! PH frequencies
 !
 call io_bulk(ID,VAR="PH_FREQS"//trim(intc(iq)),VAR_SZ=(/ph_modes/))
 call io_bulk(ID,R1=PH_freqs_sq(iq,:))
 !
 ! Polarization vectors
 ! 
 call io_bulk(ID,VAR="POLARIZATION_VECTORS",VAR_SZ=(/2,ph_modes,n_atoms,3/))
 call io_bulk(ID,C3=PH_pol_vector(:,:,:,iq))
 ! 
 call io_bulk(ID,VAR="E_K_PLUS_Q"//trim(intc(iq)),VAR_SZ=(/elph_nb,nkbz,1/) )
 call io_bulk(ID,R3=GKKP%E_kpq(iq)%E)
 ! 
 if (l_no_matrix_elements) then
   call IO_return(ID,"io_ELPH")
   return
 endif
 ! 
 ! ELPH_gkkp 
 !
 call io_bulk(ID,VAR="ELPH_GKKP_Q"//trim(intc(iq)),VAR_SZ=(/2,ph_modes,elph_nb,elph_nb,nkbz/) )
 call io_bulk(ID,C4=GKKP%dVc(:,:,:,:,1))
 !
 if (l_GKKP_hosts_bare_dV) then
   ! 
   ! ELPH_gkkp_bare 
   !
   call io_bulk(ID,VAR="ELPH_GKKP_BARE_Q"//trim(intc(iq)),VAR_SZ=(/2,ph_modes,elph_nb,elph_nb,nkbz/) )
   call io_bulk(ID,C4=GKKP_bare%dVc(:,:,:,:,1))
   !
 endif
 !
 ! ELPH_DW 
 !
 call io_bulk(ID,VAR="ELPH_DW_Q"//trim(intc(iq)),VAR_SZ=(/ph_modes,elph_nb,elph_nb,nkbz/))
 call io_bulk(ID,R4=GKKP%dVr(:,:,:,:,1))
 !
 call IO_return(ID,"io_ELPH")
 !
end function
