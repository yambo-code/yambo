!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
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
 ! In this routine I do not perform a time step. I simply calculate
 ! the el-ph Kernel at time TIME using two different approaches: a time integration 
 ! (heavy) and an approximate iterative scheme (light but approximated).
 !  
 ! EULER: 
 !  
 ! In the EULER scheme everything is extremely easy. G_lesser is G(t<=TIME) and  
 ! I simply use it to integrate the S_matrix. In the iterative scheme
 ! I use the saved S_matrix (that in EULER is saved at TIME-RT_step) and the newly calculated
 ! DELTA_matrix to get the new S_matrix.
 !  
 ! RK2:  
 !  
 ! In RK2 everything is much more complicated because RT_elph_kernel is called at NE_time and
 ! at NE_time+RT_step/2 and in the two cases a different strategy ***should*** be applied.
 !  
 !  Integration: in this case in the half-step the integral shoul be performed up to NE_time+RT_step/2
 !  Iteration: in this case I ***think*** (even if I am not completely sure) that everything is ok.
 !  
 use pars,           ONLY:SP,cI
 use units,          ONLY:HA2EV,FS2AUT
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,qindx_S
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:NE_MEM_steps,RT_step,THETA_matrix,I1_lesser,NE_MEM_Treshold,Integrator_name,G_lesser,MEM_index
 use ELPH,           ONLY:ph_freqs_sq,ph_modes
 use functions,      ONLY:bose_f
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
 integer               :: MEM_steps,i_t,i_sign(2),i_s,i_map(-1:1),it_mem,il,i_t_range(2)
 real(SP)              :: Time_vector(NE_MEM_steps),ph_freq,N_q(2),DELTA_T
 logical               :: Iterative
 complex(SP)           :: L_gl(SC_bands(2),SC_bands(2),NE_MEM_steps)
 complex(SP)           :: L_lg(SC_bands(2),SC_bands(2),NE_MEM_steps)
 complex(SP)           :: DELTA(SC_bands(2),SC_bands(2),NE_MEM_steps)
 complex(SP)           :: Gkkp(SC_bands(2),Sc_bands(2))
 complex(SP)           :: V_dummy(NE_MEM_steps),Xi_eta,Xi_np,XI_elph,DELTA_matrix
 complex(SP), external :: CIntegrate,RT_G_retarded
 !
 ! ... band index ...
 integer               :: i_n,i_np,i_e,i_ep,i_m
 ! ... BZ index ...
 integer               :: iqbz,iq,iqs,ik_m_q
 !
 real(SP),  parameter  :: g_q_diagonal=sqrt(0.01/HA2EV)
 real(SP),  parameter  :: g_q_off_diagonal=-sqrt(0.001/HA2EV)
 !
 ! Time step
 !===========
 DELTA_T=RT_step
 if (trim(Integrator_name)=="RK2") DELTA_T=RT_step/2.
 !
 ! Time steps
 !============
 MEM_steps=NE_MEM_steps
 if (it_now-NE_MEM_steps+1<=0) MEM_steps=it_now
 i_t_range=(/it_now-MEM_steps+1,it_now/)
 !
 ! Iterative approach ?
 !======================
 Iterative=NE_MEM_Treshold<0.
 if (Iterative) then
   i_t_range=it_now
   MEM_steps=1
 else
   do i_t=it_now-MEM_steps+1,it_now
     it_mem=i_t-it_now+MEM_steps
     Time_vector(it_mem)=i_t*RT_step
   enddo
 endif
 !
 i_sign=(/-1,1/)
 i_map(-1)=1
 i_map( 0)=1
 i_map( 1)=2
 !
 ! Gkkp
 !======
 do i_n=1,SC_bands(2)
   do i_np=1,SC_bands(2)
     if (i_n==i_np) Gkkp(i_n,i_np)=g_q_diagonal
     if (i_n/=i_np) Gkkp(i_n,i_np)=g_q_off_diagonal
   enddo 
 enddo
 !
 !write (100+10**ik+7,'(3f20.10)') TIME/FS2AUT,aimag(G_lesser(7,7,ik,MEM_index(MEM_steps)))
 !write (100+10**ik+8,'(3f20.10)') TIME/FS2AUT,aimag(G_lesser(8,8,ik,MEM_index(MEM_steps)))
 !write (100+10**ik+9,'(3f20.10)') TIME/FS2AUT,aimag(G_lesser(9,9,ik,MEM_index(MEM_steps)))
 write (1000,'(3f20.10)') TIME/FS2AUT,sum(aimag(G_lesser(9,9,:,MEM_index(MEM_steps))))/4.
 !if (ik/=2) return
 !
 do iqbz=1,q%nbz
   !
   iq =q%sstar(iqbz,1)
   iqs=q%sstar(iqbz,2)
   ik_m_q=qindx_S(ik,iqbz,1)
   !
   call RT_generalized_KBA(E,Gkkp,L_gl,ik_m_q,ik,'><',i_t_range,MEM_steps)
   call RT_generalized_KBA(E,Gkkp,L_lg,ik_m_q,ik,'<>',i_t_range,MEM_steps)
   !
   do il=1,ph_modes
     !
     ph_freq=sqrt(ph_freqs_sq(iq,il))
     !
     ! N-\+ (note the different order of the +/-). 1/2 here ?
     !
     N_q    = bose_f(ph_freq)+(/0.,2./)
     !
     do i_s=-1,1,2
       !
       DELTA(:,:,:)= (N_q(i_map(-i_s))*L_gl(:,:,:)-N_q(i_map(i_s))*L_lg(:,:,:))*exp(-cI*i_s*ph_freq*TIME)
       !
       do i_np=SC_bands(1),SC_bands(2)
         do i_n=SC_bands(1),SC_bands(2)
           !
           Xi_np =RT_G_retarded(E,i_np,ik    ,0.,DELTA_T)
           !
           do i_e=SC_bands(1),SC_bands(2)
             !
             Xi_eta=RT_G_retarded(E,i_e ,ik_m_q,0.,DELTA_T)
             !
             THETA_matrix(i_map(i_s),i_n,i_np,i_e,ik,iqbz,il)=Xi_eta*conjg(Xi_np)*&
&                        ( THETA_matrix(i_map(i_s),i_n,i_np,i_e,ik,iqbz,il)-&
&                          cI*DELTA_T*conjg(Gkkp(i_e,i_n))*DELTA(i_e,i_np,1)  )
             !
             I1_lesser(i_n,i_np,ik)=I1_lesser(i_n,i_np,ik)+exp(cI*i_s*ph_freq*TIME)*&
&                                   (       THETA_matrix(i_map(i_s),i_n,i_np,i_e,ik,iqbz,il) + &
&                                     conjg(THETA_matrix(i_map(-i_s),i_np,i_n,i_e,ik,iqbz,il)) )
             !
           enddo ! i_e
         enddo ! i_n
       enddo ! i_np
     enddo ! i_s
     !
   enddo ! ph branch
   !
 enddo ! iq
 !               !
 !               !
 !               ! S^{I}_{nmk}(t)=\int_0^t dt' e^{-i I w_q t'}[ N_q^{-I} L^{><}_{k-qk,nm}(t,t') - N_q^{I} L^{<>}_{k-qk,nm}(t,t')]
 !               !
 !               !                             ---------------------- V_dummy ---------------------------------------------------
 !              do i_t=1,MEM_steps
 !                !
 !                V_dummy(i_t)=exp(-cI*i_s*ph_freq*Time_vector(i_t))*( N_q(i_map(-i_s))*L_gl(i_n,i_np,i_t) &
 !&                                                                  - N_q(i_map( i_s))*L_lg(i_n,i_np,i_t) )
 !                !
 !              enddo
 !              !
 !              S_matrix(i_map(i_s),i_n,i_np,1,ik,iqbz,il)=g_q_diagonal**2*CIntegrate(V_dummy(:MEM_steps),Time_vector(:MEM_steps),MEM_steps)
 !               ! 
end subroutine RT_ELPH_kernel

