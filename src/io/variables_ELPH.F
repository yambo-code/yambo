!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM AC
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
integer function variables_ELPH(ID,what)
 !
 use pars,           ONLY:SP
 use IO_m,           ONLY:io_status,read_is_on,write_is_on,io_mode,DUMP,IO_INCOMPATIBLE_VAR
 use IO_int,         ONLY:ver_is_gt_or_eq,io_variable_elemental,io_bulk,def_variable_elemental
 use ELPH,           ONLY:ph_modes,elph_nb,PH_freqs_sq,elph_nk_bz,ELPH_desc,elph_grids_are_expanded, &
&                         QP_PH_n_G_bands,PH_W_debye,elph_nQ,PH_kpt_bz,l_GKKP_hosts_bare_dV,&
&                         elph_use_q_grid,PH_qpt,elph_nQ_used,PH_pol_vector,elph_Ham_bands,elph_branches
 use interfaces,     ONLY:ELPH_alloc
 use R_lattice,      ONLY:nkbz,nqibz,q_pt
 use QP_m,           ONLY:QP_n_states,QP_nb,QP_nk,QP_table
 use vec_operate,    ONLY:rlu_v_is_zero,c2a
 use zeros,          ONLY:k_rlu_zero
 use descriptors,    ONLY:IO_desc_reset
 !
#include<memory.h>
 !
 integer, intent(in) ::ID
 character(*) ::what
 !
 !Work Space
 !
 integer                ::iq
 logical                ::l_gFsq,l_gkkp
 real(SP)               ::v(3)
 !
 ! DB type
 !
 elph_grids_are_expanded=index(what,'gkkp_expanded')>0
 l_gFsq                 =index(what,'gFsq')>0
 l_gkkp=.FALSE.
 if(.not.l_gFsq.and..not.elph_grids_are_expanded)  l_gkkp=.TRUE.
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
   QP_PH_n_G_bands=(/1,elph_nb/)
   elph_Ham_bands =(/1,elph_nb/)
   elph_branches  =(/1,ph_modes/)
 endif
 !
 variables_ELPH=io_status(ID)
 if (variables_ELPH/=0) return
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
       variables_ELPH=IO_INCOMPATIBLE_VAR
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
 variables_ELPH=io_status(ID)
 !
end function
