!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine SET_dip_parallelization(X,eval_DIP,n_LT_steps)
 !
 use X_m,                 ONLY:X_t
 use electrons,           ONLY:n_sp_pol
 use com,                 ONLY:secnm,error
 use drivers,             ONLY:l_real_time,l_sc_run
 use parallel_m,          ONLY:PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,&
&                              PAR_IND_DIPk_ibz,PAR_IND_Xk_ibz_ID
#if defined _SC
 use parallel_m,          ONLY:PAR_IND_B_mat
 use SC,                  ONLY:B_mat_index
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
 logical      :: l_evaluating_em1s,l_evaluating_eps_G,l_evaluating_eps_eh,&
&                use_X_parallel_scheme,use_sc_parallel_scheme
 integer      :: iv,ic
 !
 l_evaluating_em1s  = index(secnm,"Dielectric")/=0
 l_evaluating_eps_G = index(secnm,"Optics")/=0
 l_evaluating_eps_eh= index(secnm,"absorption")/=0
 !
 use_X_parallel_scheme =l_evaluating_em1s.or.l_evaluating_eps_G.or.l_evaluating_eps_eh
 use_sc_parallel_scheme=(l_sc_run.or.l_real_time).and..not.l_evaluating_em1s
 !
 if(.not.(use_X_parallel_scheme.or.use_sc_parallel_scheme)) &
&  call error(' DIP parallelization scheme unkonw')
 !
 n_LT_steps=0
 !
 do iv=X%ib(1),X%ib_lim(1) 
   do ic=max(iv,X%ib_lim(2)),X%ib(2)
     !
#if defined _SC
     if(use_sc_parallel_scheme) eval_DIP(ic,iv)=PAR_IND_B_mat%element_1D( B_mat_index(ic,iv))
#endif
     !
     if( use_X_parallel_scheme) &
&      eval_DIP(ic,iv)= PAR_IND_CON_BANDS_X(X%whoami)%element_1D(ic).and. &
       &                PAR_IND_VAL_BANDS_X(X%whoami)%element_1D(iv)
     !
     if(eval_DIP(ic,iv)) n_LT_steps=n_LT_steps+1
     !
   enddo
 enddo
 !
 n_LT_steps=n_LT_steps*PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)*n_sp_pol
 !
end subroutine
