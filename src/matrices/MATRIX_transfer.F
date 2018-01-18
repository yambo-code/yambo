!
!        Copyright (C) 2000-2017 the YAMBO team
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
SUBROUTINE MATRIX_transfer( M_in, M_bse, M, M_out, INDEX_in, INDEX_out, SND, RCV, COMM, COMM_name)
 !
 ! General, multi-purpose M <-> M transfer
 !
 use parallel_m,     ONLY:MPI_comm,PAR_COM_WORLD,COMM_copy,COMM_reset,l_par_X_G_q0,l_par_X_G_all_q,l_par_X_G_finite_q
 use interfaces,     ONLY:MATRIX_duplicate
 use pars,           ONLY:SP
 use BS,             ONLY:BS_block
 use X_m,            ONLY:X_FILL_UP_matrix_only
 use D_lattice,      ONLY:i_space_inv
 use matrix,         ONLY:PAR_matrix,I_do_SND,I_do_RCV,TRANSFER_world,PAR_to_PAR_comm_NAME
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 type(PAR_matrix), optional :: M_in
 type(PAR_matrix), optional :: M_out
 type(BS_block),   optional :: M_bse(:)
 integer,          optional :: INDEX_in
 integer,          optional :: INDEX_out
 complex(SP),      optional :: M(:,:)
 logical,          optional :: SND
 logical,          optional :: RCV
 type(MPI_comm),   optional :: COMM
 character(*),     optional :: COMM_name
 !
 ! Work Space
 !
 integer              :: N
 character(3)         :: M_in_kind, M_out_kind
 logical              :: l_MAT_ARRAY
 type(PAR_matrix)     :: M_Xdn
 !
 ! KIND of the I/O matrices
 !
 l_MAT_ARRAY=.FALSE.
 if (present(M)) then
   l_MAT_ARRAY=.TRUE.
   N=size(M,1)
 endif
 !
 M_in_kind  ="NAN"
 if (present(M_in)) then
   M_in_kind=M_in%kind
 else if (present(M_bse)) then
   M_in_kind="BSE"
 endif
 if (l_MAT_ARRAY.and.present(M_out)) then
   M_in_kind="SER"
 endif
 !
 M_out_kind="NAN"
 if (present(M_out)) then
   M_out_kind=M_out%kind
 endif
 if (l_MAT_ARRAY.and.present(M_in)) then
   M_out_kind="SER"
 endif
 !
 if (M_in_kind=="NAN".or.M_out_kind=="NAN") return
 !
 ! INDEXes
 !
 if (present(M_in).and.present(INDEX_in)) then
   M_in%I=INDEX_in
 endif
 if (present(M_out).and.present(INDEX_out)) then
   M_out%I=INDEX_out
 endif
 !
 ! Local SND/RCV
 !
 I_do_SND=.TRUE.
 if (present(SND)) then
   I_do_SND=SND
 endif
 I_do_RCV=.TRUE.
 if (present(RCV)) then
   I_do_RCV=RCV
 endif
 !
 ! PAR 2 PAR COMM
 !
 if (present(COMM_name)) then 
   if (l_par_X_G_q0)       PAR_to_PAR_comm_NAME="X_G_q0_"//trim(COMM_name)
   if (l_par_X_G_finite_q) PAR_to_PAR_comm_NAME="X_G_finite_q_"//trim(COMM_name)
   if (l_par_X_G_all_q)    PAR_to_PAR_comm_NAME="X_G_all_q_"//trim(COMM_name)
 else
   PAR_to_PAR_comm_NAME="NONE"
 endif
 !
#if defined _TIMING
 call timing('MATRIX transfer ('//COMM_name//')',OPR='start')
#endif
 !
 ! Transfer World
 !
 if (present(COMM)) then
   call COMM_copy(COMM,TRANSFER_world)
 else
   call COMM_copy(PAR_COM_WORLD,TRANSFER_world)
 endif
 !
 ! Actual Interface
 !        =========
 !
 if (M_in_kind=="BSE") then
   !
   if (M_out_kind=="SLK") call MATRIX_BSE_to_PAR(M_bse,M_out)
   !
 else if (l_MAT_array) then
   !
   if (present(M_in)) then
     call MATRIX_PAR_to_from_PAR(M_in%blc(M_in%rows(1):M_in%rows(2),M_in%cols(1):M_in%cols(2),M_in%I)  ,&
&                                M_in%rows, M_in%cols,&
&                                M        , (/1,N/),   (/1,N/))
   else if (present(M_out)) then
     call MATRIX_PAR_to_from_PAR(M                     ,(/1,N/),   (/1,N/),&
&                                M_out%blc(M_out%rows(1):M_out%rows(2),M_out%cols(1):M_out%cols(2),M_out%I),&
&                                M_out%rows,M_out%cols) 
   endif
   !
 else
   !
   call MATRIX_PAR_to_from_PAR(M_in%blc(M_in%rows(1):M_in%rows(2),M_in%cols(1):M_in%cols(2),M_in%I)  ,&
&                              M_in%rows, M_in%cols,&
&                              M_out%blc(M_out%rows(1):M_out%rows(2),M_out%cols(1):M_out%cols(2),M_out%I),&
&                              M_out%rows,M_out%cols) 
   !
 endif
 !
 if (M_in_kind=="XUP".and.X_FILL_UP_matrix_only) then
   if (i_space_inv==0) call MATRIX_duplicate("+TC",M_in=M_in,INDEX_in=M_in%I,M_out=M_Xdn)
   if (i_space_inv==1) call MATRIX_duplicate("+T" ,M_in=M_in,INDEX_in=M_in%I,M_out=M_Xdn)
   call MATRIX_PAR_to_from_PAR(M_Xdn%blc(M_Xdn%rows(1):M_Xdn%rows(2),M_Xdn%cols(1):M_Xdn%cols(2),M_Xdn%I)  ,&
&                              M_Xdn%rows, M_Xdn%cols,&
&                              M_out%blc(M_out%rows(1):M_out%rows(2),M_out%cols(1):M_out%cols(2),M_out%I),&
&                              M_out%rows,M_out%cols) 
   call MATRIX_duplicate("free",M_out=M_Xdn)
 endif
 !
 call COMM_reset(TRANSFER_world)
 !
#if defined _TIMING
 call timing('MATRIX transfer ('//COMM_name//')',OPR='stop')
#endif
 !
end SUBROUTINE
