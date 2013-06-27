!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine RT_G_retarded_EP_initialize(E,k)
 !
 use pars,           ONLY:rZERO,SP
 use R_lattice,      ONLY:nkibz,nqbz,bz_samp,qindx_S
 use electrons,      ONLY:levels
 use SC,             ONLY:SC_bands
 use LIVE_t,         ONLY:live_timing
 use real_time,      ONLY:B_mat_index,Gr_ep_matrix,l_G_retarded_DB,Gr,Gr_peak_pos,Gr_T_steps,Gr_T_step
 use memory_m,       ONLY:mem_est
 use parallel_m,     ONLY:PAR_IND_B_mat,PAR_Xk_nibz,PAR_nQ,PAR_n_B_mat_elements,PAR_IND_Xk_ibz,&
&                         PAR_Xk_ibz_index,PAR_IND_Q,PAR_B_mat_index,PAR_Q_index
 use ELPH,           ONLY:ph_modes,ph_freqs_sq
 !
 implicit none
 !
 type(levels)  :: E
 type(bz_samp) :: k
 !
 ! Work Space
 !
 real(SP):: GAMMA_(2),WIDTH(4,2),E_center,pre_factor
 integer :: ik,iqbz,i_n,i_np,ik_mem,i_b_mat_mem,iq_mem,ik_m_q,il,is,i_s
 real(SP), allocatable :: Gr_integrand(:)
 real(SP),    external :: Laplace_transform
 !
 ! Allocation
 !
 call live_timing('Electron-Phonon:',PAR_Xk_nibz*PAR_n_B_mat_elements*PAR_nQ)
 !
 allocate(Gr_ep_matrix(PAR_Xk_nibz,PAR_n_B_mat_elements,PAR_nQ,2*ph_modes))
 Gr_ep_matrix=rZERO
 call mem_est("Gr_ep_matrix",(/size(Gr_ep_matrix)/))
 !
 if (l_G_retarded_DB) allocate(Gr_integrand(Gr_T_steps))
 !
 do ik=1,nkibz
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
   ik_mem=PAR_Xk_ibz_index(ik)
   !
   do iqbz=1,nqbz
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
     iq_mem=PAR_Q_index(iqbz)
     !
     ik_m_q =k%sstar(qindx_S(ik,iqbz,1),1)
     !
     do i_n=SC_bands(1),SC_bands(2)
       !
       do i_np=SC_bands(1),SC_bands(2)
         !
         if ( .not. PAR_IND_B_mat%element_1D( B_mat_index(i_n,i_np) ) ) cycle
         i_b_mat_mem=PAR_B_mat_index(i_n,i_np)
         !
         do il=1,ph_modes
           !
           do is=-1,1,2
             !
             if (l_G_retarded_DB) then
               !
               !pre_factor=4.8
               !
               !WIDTH(1,:)=set_WIDTH(i_n,i_k,maxval(  Gr_E_ranges(i_k,i_n,:) ))
               !WIDTH(2,:)=set_WIDTH(i_m,i_p,maxval(  Gr_E_ranges(i_p,i_m,:) ))
               !
               !GAMMA_(1) = WIDTH(1,1)+WIDTH(2,2)
               !GAMMA_(2) = WIDTH(1,2)+WIDTH(2,1)
               !
               E_center=Gr_peak_pos(ik,i_n)-Gr_peak_pos(ik_m_q,i_np)+i_s*abs(sqrt(ph_freqs_sq(iqbz,il)))
               !
               !if (E_center> GAMMA_(2)*pre_factor) goto 1
               !if (E_center<-GAMMA_(1)*pre_factor) goto 1
               !
               Gr_integrand(:)=real( conjg(Gr(ik,i_n,:))*Gr(ik_m_q,i_np,:) )
               !
               Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)=Laplace_transform(Gr_integrand,Gr_T_steps,Gr_T_step,E_center)
               !
             else
               !
               pre_factor=8.7
               !
               E_center=E%E(i_n,ik,1)-E%E(i_np,ik_m_q,1)+is*abs(sqrt(ph_freqs_sq(iqbz,il)))
               !
               WIDTH(1,:)=set_WIDTH(i_n ,ik    ,abs(E%W(i_n ,ik    ,1)))
               WIDTH(2,:)=set_WIDTH(i_np,ik_m_q,abs(E%W(i_np,ik_m_q,1)))
               !
               GAMMA_(1) = WIDTH(1,1)+WIDTH(2,2)
               GAMMA_(2) = WIDTH(1,2)+WIDTH(2,1)
               !
               !if (E_center> GAMMA_(2)*pre_factor) cycle
               !if (E_center<-GAMMA_(1)*pre_factor) cycle
               !
               Gr_ep_matrix(ik_mem,i_b_mat_mem,iq_mem,il+max(is,0)*ph_modes)=GAMMA_(1)/(E_center**2+GAMMA_(1)**2)
               !
             endif
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
   enddo
   !
 enddo
 !
 call live_timing()
 !
 contains
   !
   function set_WIDTH(i_n,i_k,W)
     !
     integer  :: i_k,i_n
     real(SP) :: W
     real(SP) :: set_WIDTH(2)
     !
     set_WIDTH=W
     !
     !DEBUG>
     !if (i_n<=E%nbf  ) set_WIDTH(2)=min( W ,abs( E%E(i_n,i_k,1) - E_vb_max )) 
     !if (i_n>=E%nbm+1) set_WIDTH(1)=min( W ,abs( E%E(i_n,i_k,1) - E_cb_min )) 
     !DEBUG<
     !
   end function
   !
end subroutine RT_G_retarded_EP_initialize

