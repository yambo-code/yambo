!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine DIPOLE_expand_and_project(Xk,Dip)
 !
 use pars,           ONLY:SP,cZERO,cONE
 use drivers,        ONLY:l_col_cut,l_magnons
 use parallel_m,     ONLY:PAR_K_scheme
 use electrons,      ONLY:n_sp_pol
 use D_lattice,      ONLY:nsym,i_time_rev,dl_sop
 use R_lattice,      ONLY:bz_samp,q0_def_norm,q_norm,bare_qpg
 use DIPOLES,        ONLY:DIP_projected,DIP_alloc,DIPOLE_t,DIP_iR,DIP_v
 use X_m,            ONLY:global_gauge
 use vec_operate,    ONLY:v_norm
 use com,            ONLY:secnm
#if defined _RT
 use pars,           ONLY:cI
 use matrix_operate, ONLY:m3det
 use MAGNONS,        ONLY:DIP_Splus,DIP_Smins,MAGNONS_alloc
 use DIPOLES,        ONLY:DIP_spin
#endif
#if defined _KERR
 use pars,           ONLY:pi
 use vec_operate,    ONLY:rot_v_xy_plane
 use drivers,        ONLY:l_kerr
 use KERR,           ONLY:DIP_projected_r90,KERR_alloc
#endif
 !
 implicit none
 !
 type(bz_samp), intent(inout) :: Xk
 type(DIPOLE_t),intent(inout) :: Dip
 !
 ! Work Space
 !
 integer                   :: ik,ik_bz,ic,iv,is,i_sp_pol,ik_mem,ik_bz_mem,i_star
 real(SP)                  :: field_dir(3)
 complex(SP)               :: dipole(3),dipole_rot(3)
#if defined _RT
 complex(SP)               :: tmp_sop(3,3),dipole_m(3),dipole_m_rot(3)
#endif
 logical                   :: t_rev,use_dip_iR,use_dip_v
#if defined _KERR
 real(SP)                  :: field_dir_r90(3)
#endif
 !
 ! The field direction and the gamma-point norm must be renormalized here in case the
 ! oscillator strengths have been calculated using the shifted grids.
 ! In this case q0_def_norm is not the default one but corresponds
 ! to the norm of the grid shift.
 !
 use_dip_iR=trim(global_gauge)=='length'   .or. .not.(index(secnm,"absorption")/=0)
 use_dip_v =trim(global_gauge)=='velocity'.and.      (index(secnm,"absorption")/=0)
 !
 if(use_dip_iR) field_dir=Dip%q0/v_norm(Dip%q0)*q0_def_norm
 if(use_dip_v ) field_dir=Dip%q0/v_norm(Dip%q0)
 !
#if defined _KERR
 if(l_kerr) call rot_v_xy_plane(-pi/2._SP,field_dir,field_dir_r90)
#endif
 !
 if (     l_col_cut) q_norm(1)    =q0_def_norm
 if (.not.l_col_cut) bare_qpg(1,1)=q0_def_norm
 !
 ! Calculate the q-dependent oscillators
 !
 ! This is q       *iR in the length gauge
 !         q_versor*P  in the velocity
 !
 call DIP_alloc('DIP_projected',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nbz/))
 DIP_projected = cZERO
 !
#if defined _KERR
 if(l_kerr) then
   call KERR_alloc('DIP_projected',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nbz/))
   DIP_projected_r90 = cZERO
 endif
#endif
 !
#if defined _RT
 if(l_magnons) then
   call MAGNONS_alloc('DIP_S',(/Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_K_scheme%nbz/))
   DIP_Splus = cZERO
   DIP_Smins = cZERO
 endif
#endif
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz
     !
     do iv=Dip%ib(1),Dip%ib_lim(1) 
       !
       do ic=Dip%ib_lim(2),Dip%ib(2)
         !
         dipole = cZERO
         !
         if (PAR_K_scheme%IND_ibz%element_1D(ik)) then
           !      
           ik_mem = PAR_K_scheme%ibz_index(ik)
           if (use_dip_iR) dipole = DIP_iR(:,ic,iv,ik_mem,i_sp_pol)
           if (use_dip_v ) dipole = DIP_v (:,ic,iv,ik_mem,i_sp_pol)
           !
#if defined _RT
           if (l_magnons ) dipole_m = DIP_spin(:,ic,iv,ik_mem,i_sp_pol)
#endif
           !
         endif
         !
         do i_star=1,Xk%nstar(ik)
           !
           is                = Xk%star(ik,i_star)
           ik_bz             = i_star
           if ( ik>1 ) ik_bz = sum(Xk%nstar(1:ik-1))+i_star
           !
           if (.not.PAR_K_scheme%IND_bz%element_1D(ik_bz)) cycle
           !
           ik_bz_mem=PAR_K_scheme%bz_index(ik_bz)
           !
           if ( is<= nsym/(i_time_rev+1) ) t_rev=.false.
           if ( is > nsym/(i_time_rev+1) ) t_rev=.true.
           !
           dipole_rot = matmul( dl_sop(:,:,is), dipole )
           if(t_rev) dipole_rot=conjg(dipole_rot)
           !
           DIP_projected(ic,iv,ik_bz_mem,i_sp_pol) = dot_product( field_dir, dipole_rot )
           if (  iv==ic  ) DIP_projected(ic,iv,ik_bz_mem,i_sp_pol) = cONE
           !
#if defined _KERR
           if(l_kerr) then
             DIP_projected_r90(ic,iv,ik_bz_mem,i_sp_pol) = dot_product( field_dir_r90, dipole_rot )
             if (  iv==ic  ) DIP_projected_r90(ic,iv,ik_bz_mem,i_sp_pol) = cONE
           endif
#endif
           !
#if defined _RT
           if(l_magnons) then
             tmp_sop(:,:)=dl_sop(:,:,is)*m3det(dl_sop(:,:,is))
             if( t_rev ) tmp_sop(:,:)=-tmp_sop(:,:)
             dipole_m_rot = matmul( tmp_sop, dipole_m )
             if( t_rev ) dipole_m_rot = conjg(dipole_m_rot)
             DIP_Splus(ic,iv,ik_bz_mem,i_sp_pol) = 0.5_SP*(dipole_m_rot(1)+cI*dipole_m_rot(2))
             DIP_Smins(ic,iv,ik_bz_mem,i_sp_pol) = 0.5_SP*(dipole_m_rot(1)-cI*dipole_m_rot(2))
           endif
#endif
           !
         enddo ! ic
         !
       enddo ! iv
       !
     enddo ! ik
     !
   enddo ! i_sp_pol
   !
 enddo
 !
end subroutine DIPOLE_expand_and_project
