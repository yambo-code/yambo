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
subroutine RT_Coulomb_relaxation(i_k,E,k,q)
 !  
 use pars,            ONLY:SP,cI,rZero
 use electrons,       ONLY:levels,spin_occ
 use R_lattice,       ONLY:qindx_S,bz_samp,qindx_X,nkibz
 use SC,              ONLY:SC_bands,it_now 
 use real_time,       ONLY:RT_qp_px,G_lesser,MEM_index,E_E_Electron_lifetime,E_E_Hole_lifetime,Sigma_relax
 use QP_m,            ONLY:QP_n_states,QP_table
 use D_lattice,       ONLY:DL_vol
 use collisions_CACHE,ONLY:CACHE
 use par_proc_m,      ONLY:pp_redux_wait
 !DEBUG>
 use units
 !DEBUG<
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k,q
 integer,       intent(in) :: i_k
 !
 ! Work Space ...
 !
 integer               :: i_qp1,i_qp2,B_dim,i_p,i_kmq,i_pmq,i_n,i_m,i_mp,i_np,i_p_star,&
&                         iqbz,i_p_s,iqibz,i_p_bz
 real(SP)              :: f_k(SC_bands(2)),f_p,f_kmq,f_pmq,GAMMA_,DELTA_,pre_factor,lorentzian,&
&                         f_E_factor,f_H_factor
 !
 ! Dimensioning and Zeroing
 !==========================
 !
 B_dim=SC_bands(2)-SC_bands(1)+1
 !
 E_E_Hole_lifetime(:,i_k)    =rZero
 E_E_Electron_lifetime(:,i_k)=rZero
 !
 ! Select only the m.e. of QP_table relative to kpt i_k
 !======================================================
 !
 do i_qp1=1,QP_n_states
   !
   if (.not.RT_qp_px%element_1D(i_qp1)) cycle
   !
   if (QP_table(i_qp1,3)/=i_k) cycle
   !
   i_n =QP_table(i_qp1,1)
   i_np=QP_table(i_qp1,2)
   !
   f_k(i_n)=-cI*G_lesser(i_n,i_n,i_k,MEM_index(it_now))/spin_occ
   !
   do iqbz=1,q%nbz
     !
     iqibz = q%sstar(iqbz,1)
     i_kmq = k%sstar(qindx_S(i_k,iqbz,1),1)
     !
     f_kmq=-cI*G_lesser(i_np,i_np,i_kmq,MEM_index(it_now))/spin_occ
     !
     do i_qp2=1,QP_n_states
       !
       i_m  = QP_table(i_qp2,1)
       i_mp = QP_table(i_qp2,2)
       i_p  = QP_table(i_qp2,3)
       !
       f_p=-cI*G_lesser(i_m,i_m,i_p,MEM_index(it_now))/spin_occ
       !
       do i_p_star=1,k%nstar(i_p)
         !
         i_p_s =k%star(i_p,i_p_star)
         i_p_bz=i_p_star
         if (i_p>1) i_p_bz=sum(k%nstar(:i_p-1))+i_p_star
         i_pmq   = k%sstar(qindx_X(iqibz,i_p_bz,1),1)
         !
         f_pmq=-cI*G_lesser(i_mp,i_mp,i_pmq,MEM_index(it_now))
         !
         pre_factor=abs(CACHE(i_qp1)%COLLISIONS_v3(i_qp2,i_p_s,iqbz))**2
         !
         DELTA_ = E%E(i_np,i_kmq,1)+E%E(i_m,i_p,1)-E%E(i_mp,i_pmq,1)-E%E(i_n,i_k,1)
         GAMMA_ = abs(E%W(i_np,i_kmq,1))+abs(E%W(i_m,i_p,1))+abs(E%W(i_mp,i_pmq,1))+abs(E%W(i_n,i_k,1))
         !
         lorentzian=GAMMA_/(DELTA_**2+GAMMA_**2)
         !
         f_E_factor=(1._SP-f_kmq)*(1._SP-f_p)*f_pmq
         f_H_factor=f_kmq*f_p*(2._SP-f_pmq)
         !
         E_E_Electron_lifetime(i_n,i_k)= E_E_Electron_lifetime(i_n,i_k)+pre_factor*lorentzian*f_E_factor
         E_E_Hole_lifetime(i_n,i_k)    = E_E_Hole_lifetime(i_n,i_k)+pre_factor*lorentzian*F_H_factor
  ! DEBUG>
  !if (i_k==7.and.i_n==5.and.abs(f_E_factor)>1.E-5.and.i_p_s==1) then
  !  write (*,*) 
  !  write (*,*) 'k-mq',i_np,(1.-f_kmq)
  !  write (*,*) 'p',i_m,(1.-f_p)
  !  write (*,*) 'p-q',i_mp,f_pmq
  !  write (*,*) 'E_k-E_k-q',(E%E(i_n,i_k,1)-E%E(i_np,i_kmq,1))*HA2EV
  !  write (*,*) 'E_p-E_p-q',(E%E(i_m,i_p,1)-E%E(i_mp,i_pmq,1))*HA2EV
  !endif
  ! DEBUG<
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 ! All-to-All
 !============
 !
 call pp_redux_wait(E_E_Electron_lifetime(:,:))
 call pp_redux_wait(E_E_Hole_lifetime(:,:))
 !
 ! Kernel composition
 !====================
 !
 do i_n=SC_bands(1),SC_bands(2)
   Sigma_relax(i_n,i_n,i_k)=Sigma_relax(i_n,i_n,i_k)+E_E_Hole_lifetime(i_n,i_k)*(f_k(i_n)-1._SP)+&
&                                                    E_E_Electron_lifetime(i_n,i_k)*f_k(i_n)
 enddo
 !
end subroutine RT_Coulomb_relaxation
