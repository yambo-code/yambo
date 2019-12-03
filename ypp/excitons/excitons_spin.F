!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine excitons_spin(Xk,BS_R_right,BS_E,BS_E_degs)
 !
 use pars,          ONLY:SP,cZERO,rZERO
 use BS,            ONLY:BS_bands
 use BS_solvers,    ONLY:BS_mat,BSS_n_eig
 use com,           ONLY:msg,error,warning
 use FFT_m,         ONLY:fft_size
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:n_spinor,n_sp_pol,n_spin
 use LIVE_t,        ONLY:live_timing
 use com,           ONLY:msg,error,warning
 use interfaces,    ONLY:WF_load,WF_free,WF_apply_symm,PARALLEL_index
 use wave_func,     ONLY:WF,wf_ng
 use wrapper,       ONLY:Vstar_dot_V
 use YPP_interfaces,ONLY:excitons_sort_and_report
 use YPPm,          ONLY:EXCITONS_user_indexes,EXCITONS_n_user_states,&
&                        EXCITON_kin,excitons_degeneracy_average,EXCITON_spin,BSiq
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 !
#include<memory.h>
 !
 type(bz_samp) :: Xk
 integer       :: BS_E_degs(BSS_n_eig)
 complex(SP)   :: BS_R_right(BSS_n_eig),BS_E(BSS_n_eig)
 ! 
 ! Work Space
 !
 type(PP_indexes)        ::px
 integer                 ::i_eh,i_eh_p,i_c,i_l,i_S,i_M,s_e,s_h,s_e_p,s_h_p
 type(EXCITON_kin)       ::eh,eh_p
 type(EXCITON_spin)      ::EXC_spin
 complex(SP)             ::cc(n_spin,n_spin),vv(n_spin,n_spin),cc_vv(0:1,-1:1),TMP_SM(BSS_n_eig,0:1,-1:1)
 real(SP)                ::P(0:1,-1:1,2,2)
 complex(SP),allocatable ::WF_eh(:,:),WF_eh_p(:,:)
 !
 if (n_sp_pol==1.and.n_spinor==1) return
 !
 call section('+','Spin properties')
 !                 ===============
 !
 EXC_spin%N=maxval( EXCITONS_user_indexes )
 YAMBO_ALLOC(EXC_spin%N_SM,(EXC_spin%N,0:1,-1:1))
 YAMBO_ALLOC(EXC_spin%S_sq,(EXC_spin%N))
 YAMBO_ALLOC(EXC_spin%S_z,(EXC_spin%N))
 EXC_spin%S_sq   = rZERO
 EXC_spin%S_z    = rZERO
 EXC_spin%N_SM   = rZERO
 TMP_SM = cZERO
 P      = rZERO
 !
 P(0, 0,:,:)=1./sqrt(2.)*reshape((/ 1.,0.,0.,1./),(/2,2/))
 P(1,-1,:,:)=            reshape((/ 0.,0.,1.,0./),(/2,2/))
 P(1, 0,:,:)=1./sqrt(2.)*reshape((/-1.,0.,0.,1./),(/2,2/))
 P(1, 1,:,:)=            reshape((/ 0.,1.,0.,0./),(/2,2/))
 !
 call WF_load(WF,wf_ng,1,BS_bands,(/1,Xk%nibz/),space='R',title='-EXCWF')
 !
 YAMBO_ALLOC(WF_eh,(fft_size,n_spinor))
 YAMBO_ALLOC(WF_eh_p,(fft_size,n_spinor))
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 call PP_redux_wait
 !
 call live_timing('Excitonic Spin',px%n_of_elements(myid+1)) 
 !
 do i_eh = 1,BSS_n_eig
   !
   if (.not.px%element_1D(i_eh)) cycle
   !
   call excitons_kinematics(Xk,eh,i_eh)
   !
   do i_eh_p = 1,BSS_n_eig
     !
     call excitons_kinematics(Xk,eh_p,i_eh_p)
     !
     if (eh%ik_bz /= eh_p%ik_bz) cycle
     !
     call elemental_compose(eh%ic,eh_p%ic,cc)
     !
     call elemental_compose(eh%iv,eh_p%iv,vv)
     !
     cc_vv=rZERO
     !
     if (n_sp_pol==2) then
       do i_S=0,1
         do i_M=-i_S,i_S
            cc_vv(i_S,i_M)=cc_vv(i_S,i_M)+P(i_S,i_M,eh%spin,eh%spin)*P(i_S,i_M,eh_p%spin,eh_p%spin)*&
&                                              cc(eh%spin,eh_p%spin)*conjg(vv(eh%spin,eh_p%spin))
         enddo
       enddo
     else
       do i_S=0,1
         do i_M=-i_S,i_S
           do s_e=1,2
             do s_h=1,2
               do s_e_p=1,2
                 do s_h_p=1,2
                   cc_vv(i_S,i_M)=cc_vv(i_S,i_M)+P(i_S,i_M,s_e,s_h)*P(i_S,i_M,s_e_p,s_h_p)*&
&                                                 conjg(cc(s_e,s_e_p))*vv(s_h,s_h_p)
                 enddo
               enddo
             enddo
           enddo
         enddo
       enddo
     endif
     !
     do i_c=1,EXCITONS_n_user_states
       !
       i_l=EXCITONS_user_indexes(i_c)
       !
       do i_S=0,1
         do i_M=-i_S,i_S
           TMP_SM(i_l,i_S,i_M)=TMP_SM(i_l,i_S,i_M)+conjg(BS_mat(i_eh,i_l))*BS_mat(i_eh_p,i_l)*cc_vv(i_S,i_M)
         enddo
       enddo
       !
     enddo
       !
   enddo  ! Matrix elements
   !
   call live_timing(steps=1)
   !
 enddo !  Matrix elements
 !
 call live_timing() 
 !
 call section('=','Degeneracy average')
 !                 ==================
 EXC_spin%N_SM=real(TMP_SM(:EXC_spin%N,:,:))
 !
 call PP_redux_wait(EXC_spin%N_SM)
 !
 do i_S=0,1
   do i_M=-i_S,i_S
     call excitons_degeneracy_average(EXC_spin%N,BS_E_degs,R=EXC_spin%N_SM(:,i_S,i_M),VERB=i_S==0 )
   enddo
 enddo
 !
 ! <S> and <S_z> section
 !======================
 do i_l=1,EXC_spin%N
   do i_S=0,1
     do i_M=-i_S,i_S
       EXC_spin%S_sq(i_l)=EXC_spin%S_sq(i_l)+real(i_S)*real(i_S+1)*EXC_spin%N_SM(i_l,i_S,i_M)
       EXC_spin%S_z(i_l) =EXC_spin%S_z(i_l)+      i_M             *EXC_spin%N_SM(i_l,i_S,i_M)
     enddo
   enddo
 enddo 
 !
 ! Sorting & reporting'
 !=====================
 !
 call excitons_sort_and_report(BSiq,BS_R_right,BS_E,EXC_spin=EXC_spin)
 !
 do i_c=1,EXCITONS_n_user_states
   !
   i_l=EXCITONS_user_indexes(i_c)
   !
   if (BS_E_degs(i_l)/=i_l.or.myid>0) cycle
   !
 enddo
 !
 !Clean 
 !=====
 !
 YAMBO_FREE(WF_eh)
 YAMBO_FREE(WF_eh_p)
 call WF_free(WF)
 YAMBO_FREE(EXC_spin%N_SM)
 YAMBO_FREE(EXC_spin%S_sq)
 YAMBO_FREE(EXC_spin%S_z)
 call PP_indexes_reset(px)
 !
 contains
   !
   subroutine elemental_compose(n,n_p,inner_product)
     !
     integer     :: n,n_p
     complex(SP) :: inner_product(n_spin,n_spin)
     integer     :: i_spin,i_spin_p
     !
     inner_product=cZERO
     !
     call WF_apply_symm((/n,  eh%ik_ibz,  eh%ik_sym,  eh%spin  /),WF_eh)
     call WF_apply_symm((/n_p,eh_p%ik_ibz,eh_p%ik_sym,eh_p%spin/),WF_eh_p)
     !
     if (n_sp_pol==2) then
       inner_product(eh%spin,eh_p%spin)=Vstar_dot_V(fft_size,WF_eh(:,1),WF_eh_p(:,1))
     else
       do i_spin=1,n_spinor
         do i_spin_p=1,n_spinor
           inner_product(i_spin,i_spin_p)=Vstar_dot_V(fft_size,WF_eh(:,i_spin),WF_eh_p(:,i_spin_p))
         enddo
       enddo
     endif
     !
   end subroutine
   !
end subroutine
