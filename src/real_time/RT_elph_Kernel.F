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
subroutine RT_elph_kernel(ik,E,k,q,Time)
 !  
 use pars,           ONLY:SP,cI
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,qindx_S
 use SC,             ONLY:SC_bands,it_now
 use real_time,      ONLY:NE_MEM_steps,RT_step,S_matrix,I1_lesser
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 integer,       intent(in)    :: ik
 real(SP),      intent(in)    :: Time
 !
 ! Work Space
 !
 integer               :: iqbz,iq,iqs,ik_m_q,MEM_steps,i_t,ib,ibp,i_sign(2),i_s,i_map(-1:1)
 real(SP)              :: Time_vector(NE_MEM_steps),w_ph,g_q,N_q(2)
 complex(SP)           :: L_gl(SC_bands(2),SC_bands(2),NE_MEM_steps)
 complex(SP)           :: L_lg(SC_bands(2),SC_bands(2),NE_MEM_steps)
 complex(SP)           :: V_dummy(NE_MEM_steps)
 complex(SP), external :: CIntegrate
 !
 MEM_steps=NE_MEM_steps
 if (it_now-NE_MEM_steps+1<=0) MEM_steps=it_now
 !
 forall (i_t=it_now-MEM_steps+1:it_now) Time_vector(i_t)=i_t*RT_step
 !
 i_sign=(/-1,1/)
 N_q   =(/ 0.,2./)
 i_map(-1)=1
 i_map( 0)=1
 i_map( 1)=2
 !
 do iqbz=1,q%nbz
   !
   g_q=1./100.
   w_ph=3/HA2EV
   !
   iq =q%sstar(iqbz,1)
   iqs=q%sstar(iqbz,2)
   ik_m_q=qindx_S(ik,iqbz,1)
   !
   call RT_generalized_KBA(E,L_gl,ik_m_q,ik,Time,'><')
   call RT_generalized_KBA(E,L_lg,ik_m_q,ik,Time,'<>')
   !
   do ib=SC_bands(1),SC_bands(2)
     do ibp=SC_bands(1),SC_bands(2)
       !
       do i_s=-1,1,2
         !
         ! S^{I}_{nmk}(t)=\int_0^t dt' e^{-i I w_q t'}[ N_q^{-I} L^{><}_{k-qk,nm}(t,t') - N_q^{I} L^{<>}_{k-qk,nm}(t,t')]
         !
         !                             ---------------------- V_dummy ---------------------------------------------------
         do i_t=1,MEM_steps
           !
           V_dummy(i_t)=exp(-cI*i_s*w_ph*Time_vector(i_t))*( N_q(i_map(-i_s))*L_gl(ib,ibp,i_t) +&
&                                                          - N_q(i_map( i_s))*L_lg(ib,ibp,i_t) )
           !
         enddo
         !
         S_matrix(i_map(i_s),ib,ibp,ik,iqbz)=CIntegrate(V_dummy(:MEM_steps),Time_vector(:MEM_steps),MEM_steps)
         !
         I1_lesser(ib,ibp,ik)=I1_lesser(ib,ibp,ik)+exp(cI*i_s*w_ph*TIME)*&
&                             (S_matrix(i_map(i_s),ib,ibp,ik,iqbz) - S_matrix(i_map(-i_s),ib,ibp,ik,iqbz))
         !
       enddo ! i_s
       !
     enddo ! ibp
   enddo ! ib
   !
 enddo
 !
end subroutine RT_elph_kernel

