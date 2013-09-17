!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
integer function ioELPH(ID,what)
 !
 use pars,           ONLY:SP,schlen
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,&
&                         io_elemental,io_status,io_bulk,io_header,&
&                         read_is_on,write_is_on,RD_CL,RD_CL_IF_END,io_action,&
&                         io_mode,DUMP,ver_is_gt_or_eq
 use ELPH,           ONLY:ph_modes,elph_nb,elph_global_alloc,ph_freqs_sq,elph_gkkp,&
&                         QP_PH_n_G_bands,W_debye,elph_DW,elph_nDBs,&
&                         elph_use_q_grid,E_k_plus_q,ph_qpt,ph_kpt_bz,elph_nDBs_used,&
&                         gsqF_fan,gsqF_dw ,gsqF_ca_corr ,gsqF_life_bose ,gsqF_life_f,&
&                         pol_vector,elph_Ham_bands,elph_branches,elph_nk_bz,E_k_p_q
 use stderr,         ONLY:intc
 use electrons,      ONLY:E_reset
 use R_lattice,      ONLY:nkbz,nqibz
 use D_lattice,      ONLY:n_atoms_species
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:n_sp_pol
 use QP_m,           ONLY:QP_n_states,QP_nb,QP_nk,QP_table
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use fragments,      ONLY:io_fragment
 implicit none
 integer      ::ID
 character(*) ::what
 !
 !Work Space
 !
 integer                ::iq,im,ik,i1,i2,ierr,VAR_SZ_
 real(SP), allocatable  ::elph_gkkp_disk(:,:,:),pol_vec_disk(:,:,:,:)
 character(schlen)      ::db_name
 !
 if (what=='gkkp'.or.what=='no_gkkp') db_name='elph_gkkp'
 if (what=='gkkp_expanded')           db_name='elph_gkkp_expanded'
 if (what=='gFsq')                    db_name='elph_gFsq'
 !
 ioELPH=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (ioELPH/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   if (what/='gkkp_expanded') ioELPH=io_header(ID,R_LATT=.true.,KPTS=.true.)
   if (what=='gkkp_expanded') ioELPH=io_header(ID,R_LATT=.true.,KPTS=.false.,IMPOSE_SN=.FALSE.)
   if (ioELPH/=0) goto 1
   !
   VAR_SZ_=4
   if (ver_is_gt_or_eq(ID,(/3,2,1/)    )) VAR_SZ_=5
   if (ver_is_gt_or_eq(ID,revision=1074)) VAR_SZ_=6
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=VAR_SZ_,MENU=0)
   !
   call io_elemental(ID,&
&       VAR=" Phonon modes           :",I0=ph_modes,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" Q points        [avail]:",I0=elph_nDBs,CHECK=.true.,OP=(/"=="/))
   !
   if ( (ver_is_gt_or_eq(ID,revision=356).and.what=='gFsq')) then
     call io_elemental(ID,&
&       VAR="                  [used]:",I0=elph_nDBs_used,CHECK=.true.,OP=(/"=="/))
   endif
   if (ver_is_gt_or_eq(ID,revision=1074)) then
     call io_elemental(ID,&
&       VAR=" K points (BZ)          :",I0=elph_nk_bz,CHECK=.true.,OP=(/"=="/))
   endif
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
   ioELPH=io_status(ID)
   if (ioELPH/=0) goto 1
   !
   call io_bulk(ID,VAR="MAX_PH_FREQ",VAR_SZ=(/1/))
   call io_bulk(ID,R0=W_debye)
   !
   ! Q point coordinates [iku]...
   !
   ! ... allocation
   !
   if (read_is_on(ID).and..not.allocated(ph_qpt)) then
     allocate(ph_qpt(elph_nDBs,3))
     call mem_est("ph_qpt",(/size(ph_qpt)/),(/SP/))
     allocate(ph_kpt_bz(elph_nk_bz,3))
     call mem_est("ph_kpt_bz",(/size(ph_kpt_bz)/),(/SP/))
     ph_kpt_bz=0._SP
   endif
   !
   call io_bulk(ID,VAR="PH_Q",VAR_SZ=(/elph_nDBs,3/) )
   call io_bulk(ID,R2=ph_qpt(:,:))
   !
   if (ver_is_gt_or_eq(ID,revision=1074).and.what=='gkkp_expanded') then
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
 if (what=='gFsq') then
   !
   call io_bulk(ID,VAR="QP_table",VAR_SZ=(/QP_n_states,3+n_sp_pol-1/))
   call io_bulk(ID,I2=QP_table)
   call io_bulk(ID,VAR="ELPH_GFSQ_fan",VAR_SZ=(/QP_n_states,elph_nDBs_used,ph_modes/))
   call io_bulk(ID,R3=gsqF_fan(:,:,:,1))
   call io_bulk(ID,VAR="ELPH_GFSQ_dw",VAR_SZ=(/QP_n_states,elph_nDBs_used,ph_modes/))
   call io_bulk(ID,R3=gsqF_dw(:,:,:))
   call io_bulk(ID,VAR="ELPH_GFSQ_ca_corr",VAR_SZ=(/QP_n_states,elph_nDBs_used,ph_modes/))
   call io_bulk(ID,R3=gsqF_ca_corr(:,:,:,1))
   call io_bulk(ID,VAR="ELPH_GFSQ_life_bose",VAR_SZ=(/QP_n_states,elph_nDBs_used,ph_modes/))
   call io_bulk(ID,R3=gsqF_life_bose(:,:,:,1))
   call io_bulk(ID,VAR="ELPH_GFSQ_life_f",VAR_SZ=(/QP_n_states,elph_nDBs_used,ph_modes/))
   call io_bulk(ID,R3=gsqF_life_f(:,:,:,1))
   !
   ! PH frequencies
   !
   call io_bulk(ID,VAR="PH_FREQS",VAR_SZ=(/elph_nDBs,ph_modes/))
   call io_bulk(ID,R2=ph_freqs_sq)
   !
   goto 1
   !
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
   ioELPH=-1
   goto 1
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
 if (ver_is_gt_or_eq(ID,revision=464)) then
   !
   allocate(pol_vec_disk(ph_modes,sum(n_atoms_species),2,3))
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
   deallocate(pol_vec_disk)
 else
   !
   if (allocated(pol_vector)) deallocate(pol_vector)
   !
 endif
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
   allocate(E_k_p_q%E(elph_nb,nkbz,1))
   E_k_p_q%E=E_k_plus_q
 endif
 ! 
 ! ELPH_gkkp 
 !
 allocate(elph_gkkp_disk(elph_nb*elph_nb,ph_modes,2))
 !
 call io_bulk(ID,VAR="ELPH_GKKP_Q"//trim(intc(iq)),&
&                VAR_SZ=(/elph_nb*elph_nb,ph_modes,2,nkbz/) )
 !
 do ik=1,nkbz
   ! 
   ! WRITE
   ! 
   if (write_is_on(ID)) then
     forall(im=1:ph_modes,i1=1:elph_nb,i2=1:elph_nb) &
&          elph_gkkp_disk((i1-1)*elph_nb+i2,im,1)=real(elph_gkkp(ik,im,i1,i2))
     forall(im=1:ph_modes,i1=1:elph_nb,i2=1:elph_nb) &
&          elph_gkkp_disk((i1-1)*elph_nb+i2,im,2)=aimag(elph_gkkp(ik,im,i1,i2))
   endif
   ! 
   call io_bulk(ID,R3=elph_gkkp_disk,IPOS=(/1,1,1,ik/))
   ! 
   ! READ 
   ! 
   if (read_is_on(ID)) then
     forall(im=1:ph_modes,i1=1:elph_nb,i2=1:elph_nb) &
&          elph_gkkp(ik,im,i1,i2)=elph_gkkp_disk((i1-1)*elph_nb+i2,im,1)+&
&                   (0._SP,1._SP)*elph_gkkp_disk((i1-1)*elph_nb+i2,im,2)
   endif
   !
 enddo
 !
 deallocate(elph_gkkp_disk)
 ! 
 ! ELPH_DW 
 !
 call io_bulk(ID,VAR="ELPH_DW_Q"//trim(intc(iq)),VAR_SZ=(/ph_modes,elph_nb,elph_nb,nkbz/))
 !
 do ik=1,nkbz
   ! 
   call io_bulk(ID,R3=elph_DW(ik,:,:,:),IPOS=(/1,1,1,ik/))
   ! 
 enddo
 !
1 call io_disconnect(ID=ID)
 !
end function
