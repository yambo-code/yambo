!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine io_RT_components_Reference(ID)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elph_scatt
 use R_lattice,      ONLY:nqbz
 use IO_m,           ONLY:def_variable_bulk,io_variable_bulk,read_is_on
 use RT_lifetimes,   ONLY:REF_lifetime
 use real_time,      ONLY:RT_init_occupation_reference,q_weight_RT,&
&                         G_lesser_reference,RT_nk,RT_nbands
#if defined _ELPH
 use ELPH,           ONLY:elph_branches
#endif
 !
#include<memory.h>
 !
 integer, intent(in)  :: ID
 !
#if defined _ELPH
 !
 if (l_elph_scatt) then
   !
   if (read_is_on(ID)) then
     YAMBO_FREE(q_weight_RT)
     YAMBO_ALLOC(q_weight_RT,(nqbz,elph_branches(1):elph_branches(2),2))
   endif
   !
   call def_variable_bulk(ID,"Q_WEIGHT",3,(/nqbz,elph_branches(2)-elph_branches(1)+1,2/),SP)
   call  io_variable_bulk(ID,3,R3=q_weight_RT(:,:,:))
   !
 endif
 !
#endif
 !
 if (allocated(REF_lifetime)) then
   !
   call def_variable_bulk(ID,"RT_E_E_REF_Hole_lifetime",10,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,10,R2=REF_lifetime(:,:,1))
   !
   call def_variable_bulk(ID,"RT_E_E_REF_Electron_lifetime",11,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,11,R2=REF_lifetime(:,:,2))
   !
   call def_variable_bulk(ID,"RT_E_P_REF_Hole_lifetime",12,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,12,R2=REF_lifetime(:,:,3))
   !
   call def_variable_bulk(ID,"RT_E_P_REF_Electron_lifetime",13,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,13,R2=REF_lifetime(:,:,4))
   !
   call def_variable_bulk(ID,"RT_E_P_REF_Ho_abs_lifetime",14,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,14,R2=REF_lifetime(:,:,5))
   !
   call def_variable_bulk(ID,"RT_E_P_REF_El_abs_lifetime",15,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,15,R2=REF_lifetime(:,:,6))
   !
   call def_variable_bulk(ID,"RT_E_PHOT_REF_Hole_abs_lifetime",16,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,16,R2=REF_lifetime(:,:,7))
   !
   call def_variable_bulk(ID,"RT_E_PHOT_REF_El_abs_lifetime",17,(/RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,17,R2=REF_lifetime(:,:,8))
   !
 endif
 !
 if (allocated(G_lesser_reference)) then
   !
   call def_variable_bulk(ID,"G_lesser_reference",4,(/2,RT_nbands,RT_nbands,RT_nk/),SP)
   call  io_variable_bulk(ID,4,C3=G_lesser_reference)
   !
#if !defined _YPP_RT
   if (read_is_on(ID)) call RT_init_occupation_reference( )
#endif
   !
 endif
 !
end subroutine io_RT_components_Reference
