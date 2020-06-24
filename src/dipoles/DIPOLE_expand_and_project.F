!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use drivers,        ONLY:l_col_cut
 use parallel_m,     ONLY:PAR_K_scheme
 use electrons,      ONLY:n_sp_pol
 use D_lattice,      ONLY:nsym,i_time_rev,dl_sop,sop_inv
 use R_lattice,      ONLY:bz_samp,q0_def_norm,q_norm,bare_qpg
 use DIPOLES,        ONLY:DIP_projected,DIP_alloc,DIPOLE_t,DIP_iR,DIP_v
 use X_m,            ONLY:global_gauge
 use vec_operate,    ONLY:v_norm
 use com,            ONLY:secnm
#if defined _KERR
 use pars,           ONLY:pi
 use vec_operate,    ONLY:rot_v_xy_plane
 use drivers,        ONLY:l_kerr
 use KERR,           ONLY:DIP_projected_r90,KERR_alloc,KERR_alloc
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
 real(SP)                  :: field_dir(3),field_dir_rot(3)
 complex(SP)               :: dipole(3)
 logical                   :: t_rev,use_dip_iR,use_dip_v
#if defined _KERR
 real(SP)                  :: dipole_dir(3),dipole_dir_rot(3)
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
 if(l_kerr) call rot_v_xy_plane(-pi/2._SP,field_dir,dipole_dir)
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
         endif
         !
         do i_star=1,Xk%nstar(ik)
           !
           is                = sop_inv(Xk%star(ik,i_star))
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
           field_dir_rot = matmul( dl_sop(:,:,is), field_dir )
           !
           if (.not.t_rev) DIP_projected(ic,iv,ik_bz_mem,i_sp_pol) = dot_product( field_dir_rot, dipole )
           if (     t_rev) DIP_projected(ic,iv,ik_bz_mem,i_sp_pol) = dot_product( dipole, field_dir_rot ) 
           if (  iv==ic  ) DIP_projected(ic,iv,ik_bz_mem,i_sp_pol) = cONE
           !
#if defined _KERR
           if(l_kerr) then
             dipole_dir_rot = matmul( dl_sop(:,:,is), dipole_dir )
             if (.not.t_rev) DIP_projected_r90(ic,iv,ik_bz_mem,i_sp_pol) = dot_product( dipole_dir_rot, dipole )
             if (     t_rev) DIP_projected_r90(ic,iv,ik_bz_mem,i_sp_pol) = dot_product( dipole, dipole_dir_rot )
             if (  iv==ic  ) DIP_projected_r90(ic,iv,ik_bz_mem,i_sp_pol) = cONE
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
