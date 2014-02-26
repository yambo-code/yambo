!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine RT_ELPH_out_of_eq_kernel(ik,iqbz,il,i_map,ik_m_q,N_q,E,ph_freq,Gkkp,S_mat)
 !  
 use pars,           ONLY:SP,cI,pi,cZERO
 use electrons,      ONLY:levels,spin_occ
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:RT_step,THETA_matrix,Integrator_name,G_lesser,MEM_index,&
&                         THETA_matrix_index,G_MEM_steps
 use wrapper,        ONLY:M_by_M
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 integer,       intent(in)  :: ik,iqbz,il,i_map(-1:1),ik_m_q
 real(SP),      intent(in)  :: ph_freq,N_q(2)
 complex(SP),   intent(in)  :: Gkkp(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP),   intent(out) :: S_mat(2,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 !
 ! Work Space ...
 !
 integer               :: i_s,i_theta,i_n,i_np,i_e,i_m,B_dim
 real(SP)              :: DELTA_T
 !
 complex(SP)           :: DELTA_matrix
 complex(SP)           :: M1(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: M2(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: M3(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: G_k(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: G_kmq(SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1)
 complex(SP)           :: Xi_eta,Xi_np
 complex(SP), external :: RT_G_retarded_time
 !
 S_mat=cZERO
 !
 ! Time step
 !===========
 DELTA_T=RT_step
 if (trim(Integrator_name)=="RK2") DELTA_T=RT_step/2.
 !
 ! Dimensions
 !============
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 !
 G_k(:,:)  =G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,&
&                    MEM_index(it_now,G_MEM_steps))/spin_occ
 G_kmq(:,:)=G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),&
&                    ik_m_q,MEM_index(it_now,G_MEM_steps))/spin_occ
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
     Xi_eta=RT_G_retarded_time("Lorentzian",E,i_e+SC_bands(1)-1,ik_m_q,0.,DELTA_T)
     !
     do i_np=1,B_dim
       !
       Xi_np=RT_G_retarded_time("Lorentzian",E,i_np+SC_bands(1)-1,ik    ,0.,DELTA_T)
       !
       DELTA_matrix=-cI*exp(cI*i_s*ph_freq*DELTA_T)*&
&                   ( ( N_q(i_map(-i_s))-N_q(i_map(i_s)) )*cI*M3(i_e,i_np) +&
&                   ( N_q(i_map(-i_s))*M2(i_e,i_np)-N_q(i_map(i_s))*M1(i_e,i_np) ) ) 
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
end subroutine RT_ELPH_out_of_eq_kernel
