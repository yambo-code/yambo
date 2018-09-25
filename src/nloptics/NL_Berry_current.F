!
!        Copyright (C) 2000-2018 the LUMEN team
!              http://www.attaccalite.com/lumen/ 
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
 use pars,           ONLY:DP,SP,cI,cZERO,pi,cONE
 use electrons,      ONLY:n_sp_pol,levels,spin_occ,n_spin
 use R_lattice,      ONLY:bz_samp,bz_map,nXkbz
 use D_lattice,      ONLY:a,DL_vol
 use vec_operate,    ONLY:get_id_perpendicular
 use electric,       ONLY:Sm1_plus,Sm1_minus,E_minus,E_plus
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_bz,PAR_COM_Xk_ibz_INDEX
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:NL_bands
 use wrapper_omp,    ONLY:V_dot_V_omp
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
 integer       :: id2,id3,Nperpend,i1,i2,i3,idx(3),ikbz,id,id_red
 integer       :: ik_bz,iv
 complex(SP)   :: NL_J_red(3)
 !
 ! Build the E_minus and E_plus
 !
 NL_J=0._SP
 ! 
 E_minus=cZERO
 E_plus =cZERO
 !
 do ik_bz=1,nXkbz
   !
   if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
   !
   call Build_E_matrix(en,Xk,ik_bz,V_bands,E_minus(:,:,:,ik_bz,1),E_plus(:,:,:,ik_bz,1))
   !
 enddo
 !
 call PP_redux_wait( E_minus(:,:,:,:,1),COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait( E_plus (:,:,:,:,1),COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 NL_J     = cZERO
 NL_J_red = cZERO
 !
 do id=1,3
   !
   call get_id_perpendicular(id,id2,id3)
   !
   Nperpend=k_map%max_kdir(id2)*k_map%max_kdir(id3)
   !
   do i2=1,k_map%max_kdir(id2)
     do i3=1,k_map%max_kdir(id3)
       !
       idx(id2)=i2
       idx(id3)=i3
       !
       ! Loop on the string
       !
       do i1=1,k_map%max_kdir(id)
         !
         idx(id)=i1
         ikbz=k_map%k_map_dir(idx(1),idx(2),idx(3))
         !
         do iv=1,en%nbf
!           NL_J_red(id)=NL_J_red(id)+V_dot_V_omp(en%nbf,Sm1_plus (iv,:,id,ikbz,1),E_plus (:,iv,id,ikbz,1))  !&
           NL_J_red(id)=NL_J_red(id)+conjg(V_dot_V_omp(en%nbf,Sm1_minus(iv,:,id,ikbz,1),E_minus(:,iv,id,ikbz,1))) &
                                          +V_dot_V_omp(en%nbf,Sm1_plus (iv,:,id,ikbz,1),E_plus (:,iv,id,ikbz,1))  
         enddo
         !
       enddo
       !
     enddo
   enddo
   !
   NL_J_red(id)=aimag(NL_J_red(id))/dble(Nperpend)   
   !
 enddo
 !
 ! Transform in cartesian coordinates
 ! 
 do id=1,3
   NL_J(id)=sum(NL_J_red(:)*a(:,id))/DL_Vol
 enddo
 !
 NL_J=-NL_J/(4.0*pi)
 !
end subroutine NL_Berry_current
