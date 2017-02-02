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
SUBROUTINE MATRIX_transfer( M_in, M_bse, M, M_out, INDEX_in, INDEX_out, SND, RCV, COMM)
 !
 ! For the table of actions see below...
 !
 use parallel_m,     ONLY:MPI_comm,PAR_COM_WORLD,COMM_copy,COMM_reset
 use interfaces,     ONLY:MATRIX_duplicate
 use pars,           ONLY:SP
 use BS,             ONLY:BS_block
 use X_m,            ONLY:X_FILL_UP_matrix_only
 use D_lattice,      ONLY:i_space_inv
 use linear_algebra, ONLY:PAR_matrix,I_do_SND,I_do_RCV,TRANSFER_world
#include<memory.h>
 !
 type(PAR_matrix), optional :: M_in
 type(PAR_matrix), optional :: M_out
 type(BS_block),   optional :: M_bse(:)
 integer,          optional :: INDEX_in
 integer,          optional :: INDEX_out
 complex(SP),      optional :: M(:,:)
 logical,          optional :: SND,RCV
 type(MPI_comm),   optional :: COMM
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
 M_in%I =1
 M_out%I=1
 if (present(INDEX_in)) then
   M_in%I=INDEX_in
 endif
 if (present(INDEX_out)) then
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
 ! Transfer World
 !
 if (present(COMM)) then
   call COMM_copy(COMM,TRANSFER_world)
 else
   call COMM_copy(PAR_COM_WORLD,TRANSFER_world)
 endif
 !
 ! Table of actions
 !
 ! in\out NAN SER                     BSE PAR                     SLK                    XUP XDN 
 ! NAN     x   x                       x   x                       x                      x   x
 ! SER     x  MATRIX_SER_to_from_PAR   x  MATRIX_SER_to_from_PAR  MATRIX_SER_to_from_PAR  x   x
 ! BSE     x   x                       x   x                      MATRIX_BSE_to_PAR       x   x
 ! PAR     x  MATRIX_PAR_to_from_PAR   x   x                      MATRIX_PAR_to_from_PAR  x   x  
 ! SLK     x  MATRIX_PAR_to_from_PAR   x  MATRIX_PAR_to_from_PAR   x                      x   x
 ! XUP     x  MATRIX_PAR_to_from_PAR   x   x                      MATRIX_PAR_to_from_PAR  x   x
 ! XDN     x  MATRIX_PAR_to_from_PAR   x   x                      MATRIX_PAR_to_from_PAR  x   x
 !
 if (M_in_kind=="SER".and.l_MAT_array) then
   !
   call MATRIX_SER_to_from_PAR(N,M,">",M_out) 
   !
 else if (M_in_kind=="SER") then
   !
   call MATRIX_PAR_to_from_PAR(M_in,M_out) 
   !
 else if (M_in_kind=="BSE") then
   !
   if (M_out_kind=="SLK") call MATRIX_BSE_to_PAR(M_bse,M_out)
   !
 else if (M_in_kind=="PAR".or.M_in_kind=="SLK") then
   !
   if (M_out_kind=="SER") then
     if (     l_MAT_ARRAY) call MATRIX_SER_to_from_PAR(N,M,"<",M_in) 
     if (.not.l_MAT_ARRAY) call MATRIX_PAR_to_from_PAR(M_in,M_out) 
   endif
   if (M_out_kind=="SLK".or.M_out_kind=="PAR") call MATRIX_PAR_to_from_PAR(M_in,M_out)
   !
 else if (M_in_kind=="XUP".or.M_in_kind=="XDN") then
   !
   call MATRIX_PAR_to_from_PAR(M_in,M_out) 
   !
   if (M_in_kind=="XUP".and.X_FILL_UP_matrix_only) then
     if (i_space_inv==0) call MATRIX_duplicate("+TC",M_in=M_in,INDEX_in=M_in%I,M_out=M_Xdn)
     if (i_space_inv==1) call MATRIX_duplicate("+T" ,M_in=M_in,INDEX_in=M_in%I,M_out=M_Xdn)
     call MATRIX_PAR_to_from_PAR(M_Xdn,M_out) 
     call MATRIX_duplicate("free",M_out=M_Xdn)
   endif
   !
 endif
 !
 call COMM_reset(TRANSFER_world)
 !
end SUBROUTINE
