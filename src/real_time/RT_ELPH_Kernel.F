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
 use electrons,      ONLY:levels,spin_occ,n_full_bands
 use R_lattice,      ONLY:bz_samp,qindx_S
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:RT_step,THETA_matrix,I1_lesser,Integrator_name,q_weight,&
&                         G_lesser,MEM_index,elph_gkkp_expanded,GKKP_matrix_index,&
&                         THETA_matrix_index,RT_q_px
 use ELPH,           ONLY:ph_freqs_sq,elph_branches
 use functions,      ONLY:bose_f
 use wrapper,        ONLY:M_by_M
 use par_proc_m,     ONLY:pp_redux_wait
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
 integer               :: i_s,i_map(-1:1),il,i_theta,i_gkkp,ik_bz
 real(SP)              :: ph_freq,N_q(2),DELTA_T
 !
 complex(SP)           :: DELTA_matrix
 complex(SP)           :: M1(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: M2(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: M3(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: G_k(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: G_kmq(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat(2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: S_mat_sum(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: Xi_eta,Xi_np
 complex(SP), external :: RT_G_retarded
 !
 ! ... band index ...
 integer               :: i_n,i_np,i_e,i_m,B_dim,i_np_m,i_n_m
 ! ... BZ index ...
 integer               :: iqbz,ik_m_q,ik_m_qs
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 ik_bz=sum(k%nstar(:ik-1))+1
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
 S_mat_sum=(0.,0.)
 !
 do iqbz=1,q%nbz
   !
   if (.not.RT_q_px%element_1D(iqbz)) cycle
   !
   ik_m_q =k%sstar(qindx_S(ik,iqbz,1),1)
   ik_m_qs=k%sstar(qindx_S(ik,iqbz,1),2)
   !
   i_gkkp=GKKP_matrix_index(iqbz)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_freq=abs(sqrt(ph_freqs_sq(iqbz,il)))
     !
     if (abs(ph_freq)<epsilon(1._SP)) cycle
     !
     ! N-\+ (note the different order of the +/-). 1/2 here ?
     !
     N_q    = (bose_f(ph_freq)+(/0.,2./))/2.
     !
     ! Note the m.e. in Gkkp are stored with indexes order exchanged
     !
     M1=(0._SP,0._SP)
     M2=(0._SP,0._SP)
     M3=(0._SP,0._SP)
     S_mat=(0._SP,0._SP)
     !
     G_k  =(0._SP,0._SP)
     G_kmq=(0._SP,0._SP)
     !
     G_k  =G_lesser(:,:,ik,MEM_index(it_now))
     G_kmq=G_lesser(:,:,ik_m_q,MEM_index(it_now))
     !
     Gkkp=(0._SP,0._SP)
     !
     do i_n=SC_bands(1),SC_bands(2)
       i_n_m=i_n-SC_bands(1)+1
       Gkkp(i_n_m,i_n_m)=elph_gkkp_expanded(i_gkkp+ik_bz,il,i_n,i_n)*sqrt(q_weight(iqbz))/sqrt(ph_freq)
       do i_np=SC_bands(1),SC_bands(2)
         i_np_m=i_np-SC_bands(1)+1
         !
         if (i_n<=n_full_bands.and.i_np> n_full_bands) cycle
         if (i_n> n_full_bands.and.i_np<=n_full_bands) cycle
         !
         Gkkp(i_n_m,i_np_m)=elph_gkkp_expanded(i_gkkp+ik_bz,il,i_np,i_n)*sqrt(q_weight(iqbz))/sqrt(ph_freq)
         !
       enddo 
     enddo
     !
     call M_by_M('n','n',B_dim,G_kmq,Gkkp,M1)
     !
     call M_by_M('n','n',B_dim,Gkkp,G_k,M2)
     !
     call M_by_M('n','n',B_dim,M1,G_k,M3)
     !
     do i_s=-1,1,2
       !
       i_theta=THETA_matrix_index(iqbz,il,i_map(i_s))
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
           THETA_matrix(i_theta,i_e,i_np,ik)=Xi_eta*conjg(Xi_np)*( THETA_matrix(i_theta,i_e,i_np,ik)+DELTA_T*DELTA_matrix )
           !
         enddo ! i_np
       enddo ! i_e
       !
       call M_by_M('c','n',B_dim,Gkkp,THETA_matrix(i_theta,:,:,ik),S_mat(i_map(i_s),:,:))
       !
     enddo
     !
     do i_s=-1,1,2
       !
       do i_n=1,B_dim
         do i_np=1,B_dim
           S_mat_sum(i_n,i_np)=S_mat_sum(i_n,i_np)+exp(cI*i_s*ph_freq*TIME)*(S_mat(i_map(i_s),i_n,i_np)+&
&                                                                      conjg(S_mat(i_map(-i_s),i_np,i_n)))
         enddo
       enddo
       !
     enddo ! i_s
     !
   enddo ! ph branch
   !
 enddo ! iqbz
 !
 !call pp_redux_wait(THETA_matrix(:,:,:,ik))
 call pp_redux_wait(S_mat_sum(:,:))
 !
 do i_n=SC_bands(1),SC_bands(2)
   do i_np=SC_bands(1),SC_bands(2)
     !
     i_n_m=i_n-SC_bands(1)+1
     i_np_m=i_np-SC_bands(1)+1
     !
     I1_lesser(i_n,i_np,ik)=I1_lesser(i_n,i_np,ik)+S_mat_sum(i_n_m,i_np_m)
     !
   enddo 
 enddo
 !
end subroutine RT_ELPH_kernel
