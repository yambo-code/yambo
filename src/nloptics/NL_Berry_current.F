!
!        Copyright (C) 2000-2019 the Yambo team
!              http://www.yambo-code.org/ 
!
! Authors (see AUTHORS file for details): CA
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
subroutine NL_Berry_current(en,Xk,k_map,V_bands,NL_J)
 !
 ! Evalute current using Eq. D1 of Souza et al. 
 ! PRB 69, 085106 (2004) 
 !
 use pars,           ONLY:SP,cZERO,pi
 use electrons,      ONLY:n_sp_pol,levels,spin_occ
 use R_lattice,      ONLY:bz_samp,bz_map,nXkbz
 use D_lattice,      ONLY:a
 use vec_operate,    ONLY:get_id_perpendicular
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PAR_IND_Xk_ibz
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:NL_bands
 !
 implicit none           
 !
 type(levels),  intent(in)  :: en
 type(bz_samp), intent(in)  :: Xk
 type(bz_map),  intent(in)  :: k_map
 complex(SP),   intent(out) :: NL_J(3)
 complex(SP),   intent(in)  :: V_bands(NL_bands(2),en%nbf,QP_nk,n_sp_pol)
 !
 ! Work Space
 !
 integer       :: ik,i1,iv,isp,ikbz,Nperpend,id,id2,id3,is
 integer       :: id_red,istep,max_step
 complex(SP)   :: NL_J_red(3)
 complex(SP)   :: VH_bands(NL_bands(2),en%nbf,Xk%nibz,n_sp_pol)
 complex(SP)   :: V_tilde(en%nbf,NL_bands(2),2)
 complex(SP)   :: delta_V_tilde(en%nbf,NL_bands(2),2)
 !
 ! Build the < v_n | H_k | 
 !
 VH_bands=cZERO
 do isp=1,n_sp_pol
   do ik=1,Xk%nibz
     !
     if(.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     do i1=1,NL_bands(2)
       VH_bands(i1,:,ik,isp)=en%E(i1,ik,isp)*conjg(V_bands(i1,:,ik,isp))
     enddo
     !
   enddo
 enddo
 !
 call PP_redux_wait(VH_bands,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 NL_J     = cZERO
 NL_J_red = cZERO
 !
 isp  =1
 !
 do id_red=1,3
   !
   if(k_map%max_kdir(id_red)/=1) then   
     !
     call get_id_perpendicular(id_red,id2,id3)
     !
     Nperpend=k_map%max_kdir(id2)*k_map%max_kdir(id3)       
     !
     max_step=1
!     if(k_map%max_kdir(id_red)>=6.and..not.l_force_SndOrd) max_step=2
     !
     do ikbz=1,nXkbz
       !
!       if(.not.PAR_IND_Xk_bz%element_1D(ikbz)) cycle
       !
       do istep=1,max_step
         call Build_tilde_vbands(V_bands,en,Xk,ikbz,isp,istep,id_red,V_tilde)
         delta_V_tilde(:,:,istep)=(V_tilde(:,:,1)-V_tilde(:,:,2))/dble(istep)
       enddo
       !
       ik = Xk%sstar(ikbz,1)
       !
       if(max_step==1) then
         do iv=1,en%nbf
           NL_J_red(id_red)=NL_J_red(id_red)+sum(VH_bands(:,iv,ik,isp)*delta_V_tilde(iv,:,1))
         enddo
       elseif(max_step==2) then
         do iv=1,en%nbf
           NL_J_red(id_red)=NL_J_red(id_red)+sum(VH_bands(:,iv,ik,isp)*(4._SP*delta_V_tilde(iv,:,1)-delta_V_tilde(iv,:,2)))/3._SP
         enddo
       endif
       !
     enddo
     !
   endif
   !
   ! Notice that the 2.0 originates from H i\der_k +i \der_k H = 2 * Re[H i\der_k]
   !
   NL_J_red(id_red)=2.0*spin_occ*real(NL_J_red(id_red),SP)/dble(Nperpend)
   !
 enddo
 !
 ! Transform in cartesian coordinates
 ! 
 do id=1,3
   NL_J(id)=-sum(NL_J_red(:)*a(:,id))/(4.0*pi)
 enddo
 !
! call PP_redux_wait(NL_J,COMM=PAR_COM_Xk_bz_INDEX%COMM)
 !
end subroutine NL_Berry_current
