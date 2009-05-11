!
! Copyright (C) 2000-2008 D. Sangalli and the YAMBO team 
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
subroutine K_doubles_degeneracies(BS_E_in,pos_max_in,W_new,n_check_out)
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_K_dim
 use BS_doubles,     ONLY:change,inv_change,tab_check2icv_zero,degeneracy_zero   
 use memory_m,       ONLY:mem_est
 use frequency,      ONLY:w_samp
 use units,          ONLY:HARTREE
 !
 implicit none
 !
 ! iput variables
 type(w_samp)  :: W_new
 integer       :: pos_max_in(BS_K_dim,W_new%n(1))
 real(SP)      :: BS_E_in(BS_K_dim,W_new%n(1))
 !
 ! output variables
 integer       :: n_check_out
 !  
 ! Dummys
 integer       :: icv1,icv2,icv3,icv4,iw,i1,i2,i3
 !
 ! Work space
 integer       :: degeneracy(BS_K_dim,2),loc_change(BS_K_dim)
 integer       :: tab_check2icv(BS_K_dim,2),tab_cross2icv(BS_K_dim,2)
 integer       :: n_cross
 real(SP)      :: tollerance,ref_der
 real(SP)      :: derivative(BS_K_dim,3),second_derivative(BS_K_dim,2),&
&                 tmp_der_diff,check,dist_tmp
 !
 !
 ! Inizialisation
 tollerance=1.e-5              !5.e-4
 n_check_out=BS_K_dim
 !
 !
 !    ***** DEGENERACY ALLOCATION *****
 allocate(change(BS_K_dim,W_new%n(1)),inv_change(BS_K_dim,W_new%n(1)))
 allocate(tab_check2icv_zero(BS_K_dim),degeneracy_zero(BS_K_dim))
 forall(icv1=1:BS_K_dim) change(icv1,:)=icv1 
 forall(icv1=1:BS_K_dim) inv_change(icv1,:)=icv1
 degeneracy_zero(:)=0
 forall(icv1=1:BS_K_dim) tab_check2icv_zero(icv1)=icv1
 !
 ! ***** DEGENERACY DEALLOCATION *****
 deallocate(inv_change)
 !
 return
 !
end subroutine



 
