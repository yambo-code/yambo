!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine RT_ELPH_kernel(ik,E,k,q,TIME)
 !  
 ! In this routine I do not perform a time step. I calculate
 ! the el-ph Kernel at time TIME using an approximate iterative scheme (light but approximated).
 !  
 ! EULER: 
 !  
 ! In the EULER scheme everything is extremely easy. G_lesser is G(TIME) and  
 ! I use the saved THETA_matrix (that in EULER is saved at TIME-RT_step) and the newly calculated
 ! DELTA matrix to get the new THETA_matrix.
 !  
 ! RK2:  
 !  
 ! In RK2 everything is much more complicated because RT_elph_kernel is called at NE_time and
 ! at NE_time+RT_step/2 and in the two cases a different strategy ***should*** be applied.
 !  
 ! Iteration: in this case I ***think*** (even if I am not completely sure) that everything is ok.
 !  
 use pars,           ONLY:SP,cI,pi
 use units,          ONLY:FS2AUT
 use electrons,      ONLY:levels,spin_occ
 use R_lattice,      ONLY:bz_samp,qindx_S
 use SC,             ONLY:SC_bands,it_now
 use parser_m,       ONLY:parser
 use real_time,      ONLY:RT_step,THETA_matrix,I1_lesser,Integrator_name,q_weight,&
&                         G_lesser,MEM_index
 use ELPH,           ONLY:ph_freqs_sq,elph_branches
 use functions,      ONLY:bose_f
 use wrapper,        ONLY:M_by_M
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 integer,       intent(in)    :: ik
 real(SP),      intent(in)    :: TIME
 !
 ! Work Space ...
 !
 integer               :: i_s,i_map(-1:1),il,i_counter
 real(SP)              :: ph_freq,N_q(2),DELTA_T
 !
 complex(SP)           :: DELTA_matrix
 complex(SP)           :: M1(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: M2(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: M3(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat(2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: Xi_eta,Xi_np,S_summed
 complex(SP), external :: RT_G_retarded
 !
 ! ... band index ...
 integer               :: i_n,i_np,i_e,i_m,B_dim,i_np_m,i_n_m
 ! ... BZ index ...
 integer               :: iqbz,iq,iqs,ik_m_q,ik_m_qs
 !
 real(SP)              :: g_q_diagonal_sq,g_q_off_diagonal_sq
 real(SP)              :: g_q_diagonal,g_q_off_diagonal
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 !
 g_q_off_diagonal=0.
 !
 ! Time step
 !===========
 DELTA_T=RT_step
 if (trim(Integrator_name)=="RK2") DELTA_T=RT_step/2.
 !
 i_map(-1)=1
 i_map( 0)=1
 i_map( 1)=2
 !
 i_counter=0
 !
 S_summed=(0.,0.)
 !
 do iqbz=1,q%nbz
   !
   iq =q%sstar(iqbz,1)
   iqs=q%sstar(iqbz,2)
   ik_m_q =k%sstar(qindx_S(ik,iqbz,1),1)
   ik_m_qs=k%sstar(qindx_S(ik,iqbz,1),2)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_freq=sqrt(ph_freqs_sq(iq,il))
     !
     ! N-\+ (note the different order of the +/-). 1/2 here ?
     !
     N_q    = bose_f(ph_freq)+(/0.,2./)
     !
     call get_g_diagonal(g_q_diagonal_sq)
     !
     g_q_diagonal_sq=g_q_diagonal_sq*q_weight(iqbz)
     g_q_diagonal=sqrt(g_q_diagonal_sq)
     g_q_off_diagonal=0.
     !
     !
     ! Note the m.e. in Gkkp are stored with indexes order exchanged
     !
     do i_n=SC_bands(1),SC_bands(2)
       i_n_m=i_n-SC_bands(1)+1
       Gkkp(i_n_m,i_n_m)=g_q_diagonal 
       do i_np=SC_bands(1),SC_bands(2)
         i_np_m=i_np-SC_bands(1)+1
         if (i_n/=i_np) Gkkp(i_n_m,i_np_m)=g_q_off_diagonal
       enddo 
     enddo
     !
     M1=(0.,0.)
     M2=(0.,0.)
     M3=(0.,0.)
     !
     S_mat=(0.,0.)
     !
     call M_by_M('n','n',B_dim,G_lesser(:,:,ik_m_q,MEM_index(it_now)),Gkkp,M1)
     !
     call M_by_M('n','n',B_dim,Gkkp,G_lesser(:,:,ik,MEM_index(it_now)),M2)
     !
     call M_by_M('n','n',B_dim,M1,G_lesser(:,:,ik,MEM_index(it_now)), M3)
     !
     do i_s=-1,1,2
       !
       i_counter=i_counter+1
       !
       do i_e=1,B_dim
         !
         Xi_eta=RT_G_retarded(E,i_e+SC_bands(1)-1,ik_m_q,0.,DELTA_T)
         !
         do i_np=1,B_dim
           !
           Xi_np=RT_G_retarded(E,i_np+SC_bands(1)-1,ik    ,0.,DELTA_T)
           !
           DELTA_matrix=-cI*exp(-cI*i_s*ph_freq*TIME)*&
&                       ( ( N_q(i_map(-i_s))-N_q(i_map(i_s)) )*cI*M3(i_e,i_np) +&
&              spin_occ*( N_q(i_map(-i_s))*M2(i_e,i_np)-N_q(i_map(i_s))*M1(i_e,i_np) ) ) 
           !
           THETA_matrix(i_counter,i_e,i_np,ik)=Xi_eta*conjg(Xi_np)*( THETA_matrix(i_counter,i_e,i_np,ik)+DELTA_T*DELTA_matrix )
           !
         enddo ! i_np
       enddo ! i_e
       !
       call M_by_M('c','n',B_dim,Gkkp,THETA_matrix(i_counter,:,:,ik),S_mat(i_map(i_s),:,:))
       !
     enddo
     !
     do i_s=-1,1,2
       !
       if (ik==44) S_summed=S_summed+exp(cI*i_s*ph_freq*TIME)*(S_mat(i_map(i_s),2,2)+conjg(S_mat(i_map(-i_s),2,2)))
       !
       do i_n=SC_bands(1),SC_bands(2)
         do i_np=SC_bands(1),SC_bands(2)
           i_n_m=i_n-SC_bands(1)+1
           i_np_m=i_np-SC_bands(1)+1
           I1_lesser(i_n,i_np,ik)=I1_lesser(i_n,i_np,ik)+exp(cI*i_s*ph_freq*TIME)*(S_mat(i_map(i_s),i_n_m,i_np_m)+&
&                                                                            conjg(S_mat(i_map(-i_s),i_np_m,i_n_m)))
         enddo 
       enddo
       !
     enddo ! i_s
     !
   enddo ! ph branch
   !
 enddo ! iq
 !
 if (ik==44) write (100,'(3f20.10)') TIME/FS2AUT,S_summed
 if (ik==44) write (200,'(3f20.10)') TIME/FS2AUT,G_lesser(5,5,ik,MEM_index(it_now))
 !
end subroutine RT_ELPH_kernel
!
subroutine get_g_diagonal(g_q_diagonal_sq)
  !
  use pars,           ONLY:SP,pi
  use vec_operate,    ONLY:iku_v_norm
  use ELPH,           ONLY:ph_freqs_sq
  use D_lattice,      ONLY:DL_vol
  use real_time,      ONLY:RT_alpha_frohlich
  implicit none
  !
  real(SP)::g_q_diagonal_sq,alpha,reduced_mass,alpha_tilde
  !
  ! GaAs (adim)
  !
  reduced_mass=.0591
  !
  g_q_diagonal_sq=RT_alpha_frohlich*4.*pi*sqrt( ph_freqs_sq(1,1) )**3./2. / ( sqrt(2.*reduced_mass)*DL_vol )
  !
end subroutine
