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
subroutine ELPH_databases_grids_map(k,q)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp
 use ELPH,           ONLY:PH_qpt,PH_kpt_bz,elph_nQ,elph_nk_bz,K_io_map,Q_io_map,&
&                         l_GKKP_expanded_DB_exists,l_GKKP_DB_exists
 use vec_operate,    ONLY:v_is_zero,rlu_v_is_zero,c2a
 use zeros,          ONLY:k_rlu_zero
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 !
 ! Work Space
 !
 integer       ::i1,i2
 real(SP)      ::v(3)
 !
 if (l_GKKP_expanded_DB_exists) then
   if (elph_nQ   /=q%nbz) call error('Number of q-points does not match')
   if (elph_nk_bz/=k%nbz) call error('Number of k-points does not match')
 endif
 !
 if (.not.allocated(Q_io_map)) then
   YAMBO_ALLOC(Q_io_map,(elph_nQ))
   YAMBO_ALLOC(K_io_map,(k%nbz))
 endif
 !
 if (l_GKKP_DB_exists) then
   do i1=1,elph_nQ
     Q_io_map(i1)=i1
   enddo
   do i1=1,k%nbz
     K_io_map(i1)=i1
   enddo
   return
 endif
 !
 ! Q-points map
 !
 Q_io_map=0
 call k_ibz2bz(q,'i',.false.)
 do i1=1,elph_nQ
   do i2=1,elph_nQ
     if (l_GKKP_expanded_DB_exists) call c2a(v_in=q%ptbz(i1,:)+PH_qpt(i2,:),v_out=v,mode="ki2a")
     if (l_GKKP_DB_exists)          call c2a(v_in=q%pt(i1,:)+PH_qpt(i2,:),v_out=v,mode="ki2a")
     if (rlu_v_is_zero(v,zero_=k_rlu_zero)) Q_io_map(i1)=i2
   enddo
 enddo
 call k_ibz2bz(q,'d',.false.)
 if (any(Q_io_map==0)) call error('Q-points do not match')
 !
 ! K-points map
 !
 K_io_map=0
 call k_ibz2bz(k,'i',.false.)
 do i1=1,k%nbz
   do i2=1,k%nbz
     call c2a(v_in=k%ptbz(i1,:)-PH_kpt_bz(i2,:),v_out=v,mode="ki2a")
     if (rlu_v_is_zero(v,zero_=k_rlu_zero)) K_io_map(i1)=i2
   enddo
 enddo
 call k_ibz2bz(k,'i',.false.)
 if (any(K_io_map==0)) call error('K-points do not match')
 !
end subroutine
