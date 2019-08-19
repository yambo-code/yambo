!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP AMS DS
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
subroutine DIPOLE_gspace_finiteq(Xen,Xk,Dip,iq)
 !
 use drivers,            ONLY:l_sc_run
 use pars,               ONLY:SP,cI,cZERO
 use X_m,                ONLY:global_gauge
 use D_lattice,          ONLY:x_cc
 use wave_func,          ONLY:WF,wf_ng
 use interfaces,         ONLY:WF_load,WF_free,PARALLEL_WF_distribute,PARALLEL_WF_index
 use wrapper_omp,        ONLY:Vstar_dot_V_omp
 use FFT_m,              ONLY:fft_size
 use R_lattice,          ONLY:qindx_X,bz_samp
 use electrons,          ONLY:levels,n_sp_pol,n_spinor
 use DIPOLES,            ONLY:DIPOLE_t,DIP_projected,DIP_alloc
 use com,                ONLY:error
 use LIVE_t,             ONLY:live_timing
 use collision_el,       ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use parallel_m,         ONLY:PAR_IND_Xk_bz,PAR_Xk_bz_index,PAR_Xk_nbz
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(in)    :: Dip
 integer,       intent(in)    :: iq
 !
 ! Work space
 !
 integer                      :: ik,ik_bz,ik_bz_mem,isymm,i_sp_pol,ic,iv,ic_min,iGo,&
&                                ic_indexes(4),ov_indexes(4),ok_bz,ok,osymm
 type(elemental_collision):: Xo_scatt
 !
 call DIP_alloc('DIP_projected',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_Xk_nbz/))
 DIP_projected = cZERO
 !
 call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index()
 !
 call WF_load(WF,1,maxval(qindx_X(:,:,2)),(/Dip%ib(1),Dip%ib(2)/),(/1,Xk%nibz/),title='-Oscillators/R space')
 !
 !call elemental_collision_free(Xo_scatt,INIT_ONLY=.true.)
 call elemental_collision_free(Xo_scatt)
 call elemental_collision_alloc(Xo_scatt,NG=1,TITLE="DIPOLES") 
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
     iGo  =qindx_X(iq,ik_bz,2)
     !
     ok   =Xk%sstar(ok_bz,1)
     osymm=Xk%sstar(ok_bz,2)
     !
     if (.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle 
     !
     ik_bz_mem= PAR_Xk_bz_index(ik_bz)
     !
     do iv=Dip%ib(1),Dip%ib_lim(1) 
       !
       ic_min=max(iv,Dip%ib_lim(2))
       !if (l_X_terminator) ic_min=iv
       !
       do ic=ic_min,Dip%ib(2)
         !
         ! |ib ik_is i_sp>
         ic_indexes=(/ic,ik,isymm,i_sp_pol/)
         !
         ! | ob ok_os o_sp>   
         ov_indexes=(/iv,ok,osymm,i_sp_pol/)
         !
         Xo_scatt%is=ic_indexes
         Xo_scatt%os=ov_indexes
         Xo_scatt%qs=(/iGo,iq,1/)
         if(trim(global_gauge)=="length")   call scatter_Bamp(Xo_scatt)
         if(trim(global_gauge)=="velocity") call scatter_Bamp_using_the_gradient(Xo_scatt,"o")
         !
         DIP_projected(ic,iv,ik_bz_mem,i_sp_pol) = -conjg(Xo_scatt%rhotw(1))
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
 call elemental_collision_free(Xo_scatt)
 !
 call WF_free(WF)
 !
end subroutine Dipole_gspace_finiteq
