!
!        Copyright (C) 2000-2018 the YAMBO team
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
integer function io_ELPH_old(ID,what)
 !
 use pars,           ONLY:SP,schlen
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,&
&                         io_elemental,io_status,io_bulk,io_header,&
&                         read_is_on,write_is_on,RD_CL,RD_CL_IF_END,io_action,&
&                         io_mode,DUMP,io_fragment,frag_ELPH,IO_INCOMPATIBLE_VAR
 use ELPH,           ONLY:ph_modes,elph_nb,elph_global_alloc,ph_freqs_sq,&
&                         QP_PH_n_G_bands,W_debye,elph_nDBs,&
&                         elph_use_q_grid,E_k_plus_q,ph_qpt,ph_kpt_bz,elph_nDBs_used,&
&                         gsqF_fan,gsqF_dw ,gsqF_ca_corr ,gsqF_life_bose ,gsqF_life_f,&
&                         pol_vector,elph_Ham_bands,elph_branches,elph_nk_bz,E_k_p_q,elph_grids_are_expanded
 use ELPH_old,       ONLY:elph_gkkp_old,elph_DW_old
 use stderr,         ONLY:intc
 use electrons,      ONLY:E_reset
 use R_lattice,      ONLY:nkbz,nqibz,q_pt
 use D_lattice,      ONLY:n_atoms_species
 use electrons,      ONLY:n_sp_pol
 use QP_m,           ONLY:QP_n_states,QP_nb,QP_nk,QP_table
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use parallel_m,     ONLY:PAR_Q_index
 use vec_operate,    ONLY:rlu_v_is_zero,c2a
 use zeros,          ONLY:k_rlu_zero
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
#include<memory.h>
 !
 integer      ::ID
 character(*) ::what
 !
 !Work Space
 !
 integer                ::iq,im,ik,i1,i2,ierr,iq_mem
 real(SP)               ::v(3)
 real(SP), allocatable  ::data_disk(:,:,:,:),pol_vec_disk(:,:,:,:)
 character(schlen)      ::db_name
 !
#if defined _TIMING
 call timing('io_ELPH_old',OPR='start')
#endif
 !
 if (what=='gkkp'.or.what=='no_gkkp') db_name='elph_gkkp'
 if (what=='gFsq')                    db_name='elph_gFsq'
 if (what=='gkkp_expanded') then
   db_name='elph_gkkp_expanded'
   elph_grids_are_expanded=.TRUE.
 endif
 !
 io_ELPH_old=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (io_ELPH_old/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   if (what/='gkkp_expanded') io_ELPH_old=io_header(ID,R_LATT=.true.,KPTS=.true.,FRAG=frag_ELPH)
   if (what=='gkkp_expanded') io_ELPH_old=io_header(ID,R_LATT=.true.,KPTS=.false.,IMPOSE_SN=.FALSE.,FRAG=frag_ELPH)
   if (io_ELPH_old/=0) goto 1
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=6,MENU=0)
   !
   call io_elemental(ID,&
&       VAR=" Phonon modes           :",I0=ph_modes,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" Q points        [avail]:",I0=elph_nDBs,CHECK=.true.,OP=(/"=="/))
   !
   if (what=='gFsq') then
     call io_elemental(ID,&
&       VAR="                  [used]:",I0=elph_nDBs_used,CHECK=.true.,OP=(/"=="/))
   endif
   call io_elemental(ID,&
&       VAR=" K points (BZ)          :",I0=elph_nk_bz,CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(ID,&
&       VAR=" El-PH bands            :",I0=elph_nb,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" Using the Q-grid       :",L0=elph_use_q_grid,CHECK=.true.,OP=(/"=="/))
   !
   if (what=='gFsq') then
     call io_elemental(ID,VAR="QP_nb_nk_n_states",VAR_SZ=3,MENU=0)
     call io_elemental(ID,I0=QP_nb)
     call io_elemental(ID,I0=QP_nk)
     call io_elemental(ID,I0=QP_n_states,VAR=' QP tot states          :')
   endif
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   if (io_mode(ID)==DUMP.or.write_is_on(ID)) then
     QP_PH_n_G_bands=elph_nb
     elph_Ham_bands =(/1,elph_nb/)
     elph_branches  =(/1,ph_modes/)
   endif
   !
   io_ELPH_old=io_status(ID)
   if (io_ELPH_old/=0) goto 1
   !
   call io_bulk(ID,VAR="MAX_PH_FREQ",VAR_SZ=(/1/))
   call io_bulk(ID,R0=W_debye)
   !
   ! Q point coordinates [iku]...
   !
   ! ... allocation
   !
   if (read_is_on(ID).and..not.allocated(ph_qpt)) then
     YAMBO_ALLOC(ph_qpt,(elph_nDBs,3))
     YAMBO_ALLOC(ph_kpt_bz,(elph_nk_bz,3))
     ph_kpt_bz=0._SP
   endif
   !
   call io_bulk(ID,VAR="PH_Q",VAR_SZ=(/elph_nDBs,3/) )
   call io_bulk(ID,R2=ph_qpt(:,:))
   !
   if (what=='gkkp'.and.allocated(q_pt).and.elph_use_q_grid) then
     do iq=1,nqibz
       call c2a(v_in=q_pt(iq,:)+ph_qpt(iq,:),v_out=v,mode="ki2a")
       if (.not.rlu_v_is_zero(v,zero_=k_rlu_zero)) then
         io_ELPH_old=IO_INCOMPATIBLE_VAR
         goto 1
       endif
     enddo
   endif
   !
   if (what=='gkkp_expanded') then
     !
     call io_bulk(ID,VAR="PH_K",VAR_SZ=(/nkbz,3/) )
     call io_bulk(ID,R2=ph_kpt_bz(:,:))
     !
   endif
   !
 endif
 !
 iq=maxval(io_sec(ID,:))-1
 if (iq<=0) goto 1
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
   io_ELPH_old=-1
   goto 1
 endif
 !
 if (what=='gFsq') then
   !
   if (iq==1) then
     call io_bulk(ID,VAR="QP_table",VAR_SZ=(/QP_n_states,3+n_sp_pol-1/))
     call io_bulk(ID,I2=QP_table)
   endif
   !
   iq_mem=iq
   if (allocated(PAR_Q_index)) then
     iq_mem=PAR_Q_index(iq)
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
   ! PH frequencies
   !
   if (iq==1) then
     call io_bulk(ID,VAR="PH_FREQS",VAR_SZ=(/elph_nDBs,ph_modes/))
     call io_bulk(ID,R2=ph_freqs_sq)
   endif
   !
   goto 1
   !
 endif
 !
 ! Allocation
 !
 if (read_is_on(ID)) then
   if (what/='no_gkkp') call elph_global_alloc('gkkp')
   if (what=='no_gkkp') call elph_global_alloc('no_gkkp')
 endif
 !
 ! Manage RD_CL_IF_END
 !
 if (io_action(ID)==RD_CL_IF_END.and.iq==nqibz) io_action(ID)=RD_CL
 !
 ! PH frequencies
 !
 call io_bulk(ID,VAR="PH_FREQS"//trim(intc(iq)),VAR_SZ=(/ph_modes/))
 call io_bulk(ID,R1=ph_freqs_sq(iq,:))
 !
 YAMBO_ALLOC(pol_vec_disk,(ph_modes,sum(n_atoms_species),2,3))
 !
 ! Polarization vectors
 ! 
 if (write_is_on(ID)) then
   call mat_c2r(pol_vector(:,:,1),pol_vec_disk(:,:,:,1))
   call mat_c2r(pol_vector(:,:,2),pol_vec_disk(:,:,:,2))
   call mat_c2r(pol_vector(:,:,3),pol_vec_disk(:,:,:,3))
 endif
 !
 call io_bulk(ID,VAR="POLARIZATION_VECTORS_REAL",VAR_SZ=(/ph_modes,sum(n_atoms_species),3/))
 call io_bulk(ID,R3=pol_vec_disk(:,:,1,:))
 call io_bulk(ID,VAR="POLARIZATION_VECTORS_IMAG",VAR_SZ=(/ph_modes,sum(n_atoms_species),3/))
 call io_bulk(ID,R3=pol_vec_disk(:,:,2,:))
 ! 
 if (read_is_on(ID)) then
   call mat_r2c(pol_vec_disk(:,:,:,1),pol_vector(:,:,1))
   call mat_r2c(pol_vec_disk(:,:,:,2),pol_vector(:,:,2))
   call mat_r2c(pol_vec_disk(:,:,:,3),pol_vector(:,:,3))
 endif
 !
 YAMBO_FREE(pol_vec_disk)
 !
 if (what=='no_gkkp') goto 1
 ! 
 ! Ek+q (to be used when  elph_use_q_grid=.FALSE.)
 !
 call io_bulk(ID,VAR="E_K_PLUS_Q"//trim(intc(iq)),VAR_SZ=(/elph_nb,nkbz,1/) )
 call io_bulk(ID,R3=E_k_plus_q)
 !
 if (read_is_on(ID)) then
   call E_reset(E_k_p_q)
   E_k_p_q%nb=elph_nb
   E_k_p_q%nk=nkbz
   YAMBO_ALLOC(E_k_p_q%E,(elph_nb,nkbz,1))
   E_k_p_q%E=E_k_plus_q
 endif
 ! 
 ! ELPH_gkkp 
 !
 YAMBO_ALLOC(data_disk,(elph_nb*elph_nb,ph_modes,2,1))
 !
 call io_bulk(ID,VAR="ELPH_GKKP_Q"//trim(intc(iq)),VAR_SZ=(/elph_nb*elph_nb,ph_modes,2,nkbz/) )
 !
 do ik=1,nkbz
   ! 
   ! WRITE
   ! 
   if (write_is_on(ID)) then
     do im=1,ph_modes
       do i1=1,elph_nb
         do i2=1,elph_nb
           data_disk((i1-1)*elph_nb+i2,im,1,1)=real(elph_gkkp_old(ik,im,i1,i2))
           data_disk((i1-1)*elph_nb+i2,im,2,1)=aimag(elph_gkkp_old(ik,im,i1,i2))
         enddo
       enddo
     enddo
   endif
   ! 
   call io_bulk(ID,R4=data_disk,IPOS=(/1,1,1,ik/))
   ! 
   ! READ 
   ! 
   if (read_is_on(ID)) then
     do im=1,ph_modes
       do i1=1,elph_nb
         do i2=1,elph_nb
           elph_gkkp_old(ik,im,i1,i2)=data_disk((i1-1)*elph_nb+i2,im,1,1)+&
&                   (0._SP,1._SP)*data_disk((i1-1)*elph_nb+i2,im,2,1)
         enddo
       enddo
     enddo
   endif
   !
 enddo
 !
 YAMBO_FREE(data_disk)
 ! 
 ! ELPH_DW 
 !
 YAMBO_ALLOC(data_disk,(ph_modes,elph_nb,elph_nb,1))
 !
 call io_bulk(ID,VAR="ELPH_DW_Q"//trim(intc(iq)),VAR_SZ=(/ph_modes,elph_nb,elph_nb,nkbz/))
 !
 do ik=1,nkbz
   ! 
   if (write_is_on(ID)) data_disk(:,:,:,1)=elph_DW_old(ik,:,:,:)
   ! 
   call io_bulk(ID,R4=data_disk,IPOS=(/1,1,1,ik/))
   ! 
   if (read_is_on(ID)) elph_DW_old(ik,:,:,:)=data_disk(:,:,:,1)
   ! 
 enddo
 !
 YAMBO_FREE(data_disk)
 !
1 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_ELPH_old',OPR='stop')
#endif
 !
end function
