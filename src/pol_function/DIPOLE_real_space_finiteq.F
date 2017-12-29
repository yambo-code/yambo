!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP&AMS
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
subroutine DIPOLE_real_space_finiteq(Xen,Xk,X,iq)
 !
 use drivers,            ONLY:l_sc_run
 use pars,               ONLY:SP,cI,cZERO
 use D_lattice,          ONLY:x_cc
 use wave_func,          ONLY:WF
 use interfaces,         ONLY:WF_load
 use wrapper_omp,        ONLY:Vstar_dot_V_omp
 use FFT_m,              ONLY:fft_size
 use R_lattice,          ONLY:qindx_X,bz_samp
 use electrons,          ONLY:levels,n_sp_pol,n_spinor
 use X_m,                ONLY:X_t,DIP_projected,X_alloc
 use com,                ONLY:error
 use LIVE_t,             ONLY:live_timing
 !use parallel_m,         ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(in)    :: X
 integer,       intent(in)    :: iq
 !
 ! Work space
 !
 integer                      :: ik,ik_bz,isymm,i_sp_pol,ic,iv,ic_min,&
&                                ic_indexes(4),ov_indexes(4),ok_bz,ok,osymm
 complex(SP), allocatable    :: WF_symm_i(:,:),WF_symm_o(:,:)
 !
 call X_alloc('DIP_projected',(/X%ib_lim(2),X%ib(2),X%ib(1),X%ib_lim(1),Xk%nbz/))
 DIP_projected = cZERO
 !
 ! WF load
 ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
 ! there will be other WF_load calls using the present WF distribution. 
 !
 call WF_load(WF,0,1,(/1,X%ib(2)/),(/1,Xk%nibz/),space='R',title='-Oscillators/R space',&
&             force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
 !
 YAMBO_ALLOC(WF_symm_i,(fft_size,n_spinor))
 YAMBO_ALLOC(WF_symm_o,(fft_size,n_spinor))
 !
 !call DIPOLE_build_parallelization_mask(X,l_mat_eval_DIP,n_LT_steps) 
 !
 !if (n_LT_steps>0) call live_timing('Dipoles in real space',n_LT_steps)
 call live_timing('Dipoles finite momentum',n_sp_pol*Xk%nibz)
 !
 do i_sp_pol=1,n_sp_pol
   do ik_bz=1,Xk%nbz 
     !
     ik   =Xk%sstar(ik_bz,1) 
     isymm=Xk%sstar(ik_bz,2) 
     !
     ok_bz=qindx_X(iq,ik_bz,1)
     !
     ok   =Xk%sstar(ok_bz,1)
     osymm=Xk%sstar(ok_bz,2)
     !
     !if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
     !
     !ik_mem=PAR_DIPk_ibz_index(ik)
     !
     do iv=X%ib(1),X%ib_lim(1) 
       !
       ic_min=max(iv,X%ib_lim(2))
       !if (l_X_terminator) ic_min=iv
       !
       do ic=ic_min,X%ib(2)
         !
         !if (.not.l_mat_eval_DIP(ic,iv)) cycle
         !
         ! |ib ik_is i_sp>
         ic_indexes=(/ic,ik,isymm,i_sp_pol/)
         call WF_apply_symm(ic_indexes,WF_symm_i)
         !
         ! | ob ok_os o_sp>   
         ov_indexes=(/iv,ok,osymm,i_sp_pol/)
         call WF_apply_symm(ov_indexes,WF_symm_o)
         !
         DIP_projected(ic,iv,ik_bz,i_sp_pol) = Vstar_dot_V_omp(fft_size*n_spinor,WF_symm_o,WF_symm_i )
         !
       enddo 
       !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
   !
 enddo
 !
 call live_timing()
 !
 YAMBO_FREE(WF_symm_i)
 YAMBO_FREE(WF_symm_o)
 !
 call WF_free(WF)
 !
end subroutine Dipole_real_space_finiteq
