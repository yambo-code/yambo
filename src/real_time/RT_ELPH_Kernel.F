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
 ! In this routine I do not perform a time step. I calculate
 ! the el-ph Kernel at time TIME using an approximate iterative scheme (light but approximated).
 !  
 ! EULER: 
 !  
 ! In the EULER scheme everything is extremely easy. G_lesser is G(t<=TIME) and  
 ! I simply use it to integrate the THETA_matrix. In the iterative scheme
 ! I use the saved THETA_matrix (that in EULER is saved at TIME-RT_step) and the newly calculated
 ! DELTA matrix to get the new THETA_matrix.
 !  
 ! RK2:  
 !  
 ! In RK2 everything is much more complicated because RT_elph_kernel is called at NE_time and
 ! at NE_time+RT_step/2 and in the two cases a different strategy ***should*** be applied.
 !  
 !  Iteration: in this case I ***think*** (even if I am not completely sure) that everything is ok.
 !  
 use pars,           ONLY:SP,cI
 use units,          ONLY:HA2EV,FS2AUT
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,qindx_S
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:RT_step,THETA_matrix,I1_lesser,Integrator_name,G_lesser,MEM_index,index_3b
 use ELPH,           ONLY:ph_freqs_sq,elph_branches
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
 integer               :: i_s,i_map(-1:1),il
 real(SP)              :: ph_freq,N_q(2),DELTA_T
 complex(SP)           :: L_gl(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP)           :: L_lg(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP)           :: DELTA(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP)           :: Gkkp(SC_bands(1):SC_bands(2),SC_bands(1):Sc_bands(2))
 complex(SP)           :: Xi_n,Xi_np,Xi_eta
 complex(SP), external :: RT_G_retarded
 !
 ! ... band index ...
 integer               :: i_n,i_np,i_e,i_m,i_nnpe,i_npne
 ! ... BZ index ...
 integer               :: iqbz,iq,iqs,ik_m_q
 !
 real(SP),  parameter  :: g_q_diagonal=sqrt(.01/HA2EV)
 real(SP),  parameter  :: g_q_off_diagonal=sqrt(0.00/HA2EV)
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
 ! Gkkp
 !======
 !
 ! Note the m.e. in Gkkp are stored with indexes order exchanged
 !
 do i_n=SC_bands(1),SC_bands(2)
   do i_np=SC_bands(1),SC_bands(2)
     if (i_n==i_np) Gkkp(i_n,i_np)=g_q_diagonal
     if (i_n/=i_np) Gkkp(i_n,i_np)=g_q_off_diagonal
   enddo 
 enddo
 !
 do i_n=SC_bands(1),SC_bands(2)
   write (100*ik+i_n,'(3f20.10)') TIME/FS2AUT,aimag(G_lesser(i_n,i_n,ik,MEM_index(it_now)))
 enddo
 !
 do iqbz=1,q%nbz
   !
   iq =q%sstar(iqbz,1)
   iqs=q%sstar(iqbz,2)
   ik_m_q=qindx_S(ik,iqbz,1)
   !
   call RT_generalized_KBA(E,Gkkp,L_gl,ik_m_q,ik,'><')
   call RT_generalized_KBA(E,Gkkp,L_lg,ik_m_q,ik,'<>')
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_freq=sqrt(ph_freqs_sq(iq,il))
     !
     ! N-\+ (note the different order of the +/-). 1/2 here ?
     !
     N_q    = bose_f(ph_freq)+(/0.,2./)
     !
     do i_s=-1,1,2
       !
       DELTA(:,:)= (N_q(i_map(-i_s))*L_gl(:,:)-N_q(i_map(i_s))*L_lg(:,:))*exp(-cI*i_s*ph_freq*TIME)
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
             i_nnpe=index_3b(i_n,i_np,i_e)
             !
             THETA_matrix(i_map(i_s),i_nnpe,ik,iqbz,il)=Xi_eta*conjg(Xi_np)*&
&                        ( THETA_matrix(i_map(i_s),i_nnpe,ik,iqbz,il)-cI*DELTA_T*conjg(Gkkp(i_e,i_n))*DELTA(i_e,i_np))
             !
           enddo ! i_e
         enddo ! i_n
       enddo ! i_np
       !
     enddo ! i_s
     !
     do i_s=-1,1,2
       do i_np=SC_bands(1),SC_bands(2)
         do i_n=SC_bands(1),SC_bands(2)
           do i_e=SC_bands(1),SC_bands(2)
             !
             i_nnpe=index_3b(i_n,i_np,i_e)
             !
             i_npne=index_3b(i_np,i_n,i_e)
             !
             I1_lesser(i_n,i_np,ik)=I1_lesser(i_n,i_np,ik)+exp(cI*i_s*ph_freq*TIME)*&
&                                   (       THETA_matrix(i_map(i_s),i_nnpe,ik,iqbz,il) + &
&                                     conjg(THETA_matrix(i_map(-i_s),i_npne,ik,iqbz,il)) )
           enddo ! i_e
         enddo ! i_n
       enddo ! i_np
       !
     enddo ! i_s
     !
   enddo ! ph branch
   !
 enddo ! iq
 !
end subroutine RT_ELPH_kernel
!
