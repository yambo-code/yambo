!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine DIPOLE_build_parallelization_mask(X,eval_DIP,n_LT_steps)
 !
 use X_m,                 ONLY:X_t,l_X_terminator
 use electrons,           ONLY:n_sp_pol
 use parallel_m,          ONLY:PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,&
&                              PAR_IND_DIPk_ibz,PAR_IND_DIPk_ID,l_par_SE,&
&                              l_par_X_T,l_par_X_G_q0,l_par_X_G_all_q,l_par_X_G_finite_q,l_par_NL
#if defined _RT
 use parallel_m,          ONLY:PAR_IND_B_mat,l_par_X_T,l_par_X_G_q0,l_par_X_G_all_q,l_par_X_G_finite_q,l_par_RT
 use hamiltonian,         ONLY:B_mat_index
#endif
 !
 implicit none
 !
 type(X_t),    intent(in)  :: X
 logical,      intent(out) :: eval_DIP(X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
 integer,      intent(out) :: n_LT_steps
 !
 ! Work space
 !
 integer      :: iv,ic,ic_min
 !
 n_LT_steps=0
 !
 eval_DIP=.false.
 !
 do iv=X%ib(1),X%ib_lim(1) 
   !
   ic_min=max(iv,X%ib_lim(2))
   if (l_X_terminator) ic_min=iv
   !
   do ic=ic_min,X%ib(2)
     !
#if defined _RT
     if (l_par_RT) then
         eval_DIP(ic,iv)=PAR_IND_B_mat%element_1D( B_mat_index(ic,iv,X%ib))
     elseif(l_par_NL) then 
         eval_DIP(ic,iv)=.true.
     endif
#endif
     !
     if (any((/l_par_X_T,l_par_X_G_q0,l_par_X_G_all_q,l_par_X_G_finite_q,l_par_SE/))) then
       eval_DIP(ic,iv)= PAR_IND_CON_BANDS_X(X%whoami)%element_1D(ic).and. &
&                       PAR_IND_VAL_BANDS_X(X%whoami)%element_1D(iv)
     endif
     !
     if (eval_DIP(ic,iv)) n_LT_steps=n_LT_steps+1
     !
   enddo
 enddo
 !
 n_LT_steps=n_LT_steps*PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_DIPk_ID+1)*n_sp_pol
 !
end subroutine
