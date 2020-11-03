!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine ELPH_databases_IO_transfer_and_write(IDB,k,E,q,GKKP)
 !
 use pars,                ONLY:SP,DP,rZERO
 use D_lattice,           ONLY:n_atoms
 use R_lattice,           ONLY:bz_samp
 use ELPH,                ONLY:GKKP_me
 use YPP_ELPH,            ONLY:DB_ph_pol_vec,DB_E_k,DB_kind,DB_K_map,DB_nk,DB_gkkp,&
&                              DB_nb,DB_ph_modes,DB_grad_at_gamma,DB_E_k_plus_q,DB_Q_map
 use electrons,           ONLY:levels
 !
 implicit none
 !
 integer       :: IDB
 type(bz_samp) :: k,q
 type(levels)  :: E
 type(GKKP_me) :: GKKP
 !
 ! Work Space
 !
 integer     :: ia,ja,i,j,ik_db,ik_bz,ib1,ib2,il,iq
 complex(DP) :: DW_ws_L(n_atoms),DW_ws_R(n_atoms)
 real(SP)    :: E_shift
 !
 iq=DB_Q_map(IDB)
 !
 ! GKKP & Levels
 !===============
 do ik_db=1,DB_nk
   !
   ik_bz=DB_K_map(ik_db)
   !
   ! Let's remember it again:
   !                                ib1                             ib2
   !                                |                               | 
   ! el_ph_mat(i,j,k,I)= <\psi(k+q) n_i|dV_{SCF}/du^q_{i a}|\psi(k) n_j>
   !                           |                          
   !                           ik_db[GS]/DB_K_map(ik_db)[YAMBO]  
   !
   ! I = (i,a)
   !
   ! In GS we define
   !
   !  gkkp_disk(i,j,k,l) = el_ph_mat(i,j,k,I) u(I,l)^* eps_I(q l)/sqrt(M_a)
   !
   ! However YAMBO table describe the k->k-q transitions and not k+q. So we 
   ! define
   !
   ! g_ijk^{qI}|_YAMBO= g_ijk^{-qI}|_GS = <k-q n_i|dV_{SCF}/du^-q_{i a}|k n_j>
   !
   ! where k_bz = DB_K_map(ik_db). Note that this procedure implies that YAMBO {q}'s
   ! are -{q}'s in GS (note the MinusQ flag).
   !
   GKKP%E_kpq(iq)%E(:,ik_bz,1)=rZERO
   !
   if (trim(DB_kind)=="PWscf") then
     !
     ! To allign correctly the E(k+q) energies I use the shift between
     ! the ABSOLUTE YAMBO energy levels at k and the GS ones.
     !
     E_shift=DB_E_k(1,ik_db)-(E%E(1,k%sstar(ik_bz,1),1)+E%E_Fermi)
     !
     GKKP%E_kpq(iq)%E(:,ik_bz,1)=DB_E_k_plus_q(:,ik_db)-E_shift
     !
   endif
   !
   do ib1=1,DB_nb
     do ib2=1,DB_nb
       !
       GKKP%dVc(:,ib1,ib2,ik_bz,1)=DB_gkkp(ib1,ib2,:,ik_db)
       !
       if (trim(GKKP%KIND)=="dV_bare") cycle
       !
       ! Debye Waller Term
       !
       GKKP%dVr(:,ib1,ib2,ik_bz,1)=rZERO
       !
       do il=1,DB_ph_modes
         !
         DW_ws_L=(0._DP,0._DP)
         DW_ws_R=(0._DP,0._DP)
         !
         do ja=1,n_atoms
           do ia=1,n_atoms
             do i=1,3
               DW_ws_L(ja)=DW_ws_L(ja)+DB_grad_at_gamma(ib1,ib2,ia,i,ik_db)*DB_ph_pol_vec(IDB,il,ia,i)
            enddo
           enddo
         enddo
         !
         do ja=1,n_atoms
           do j=1,3
             DW_ws_R(ja)=DW_ws_R(ja)+DB_grad_at_gamma(ib2,ib1,ja,j,ik_db)*conjg(DB_ph_pol_vec(IDB,il,ja,j))
          enddo
         enddo
         !
         do ja=1,n_atoms
           GKKP%dVr(il,ib1,ib2,ik_bz,1)=GKKP%dVr(il,ib1,ib2,ik_bz,1)+2._SP*real( DW_ws_L(ja)*DW_ws_R(ja) , SP )
         enddo
       enddo
       !
     enddo 
   enddo
 enddo
 !
end subroutine
