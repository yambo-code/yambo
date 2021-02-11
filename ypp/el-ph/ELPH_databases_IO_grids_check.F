!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, FP
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
subroutine ELPH_databases_IO_grids_check(k)
 !
 ! ELPH <-> YAMBO k table correspondance
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
 use zeros,          ONLY:k_rlu_zero
 use YPP_ELPH,       ONLY:DB_K_map,DB_ph_kpt,YAMBO_K_found,DB_nk
 !
 implicit none
 !
 type(bz_samp) :: k
 !
 ! Work Space
 !
 integer  :: ik,ik_elph
 real(SP) :: v(3)
 !
 DB_K_map=0
 YAMBO_K_found=.FALSE.
 do ik_elph=1,DB_nk
   !
   ! For non zero phonon q GS grid is composed of (k_1,k_1+q,k_2,k_2+q ...).
   ! I table the  k1,k2 ...
   !
   do ik=1,k%nbz
     if (YAMBO_K_found(ik)) cycle
     v=k%ptbz(ik,:)-DB_ph_kpt(:,ik_elph)
     call c2a(v_in=v,mode="ki2a")
     if (rlu_v_is_zero(v,zero_=k_rlu_zero)) then
       DB_K_map(ik_elph)=ik
       YAMBO_K_found(ik)=.TRUE.
       exit
     endif
   enddo
   !
   if (DB_K_map(ik_elph)==0) call error("Wrong K-grid correspondance")
   !
 enddo
 !
end subroutine
