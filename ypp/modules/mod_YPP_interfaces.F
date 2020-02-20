!
!        Copyright (C) 2000-2019 the YAMBO team
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
module YPP_interfaces
 !
 implicit none
 !
 interface
   !
   subroutine electrons_dos_elemental(k,E,bands,el_dos,dos_E,WF_fac,PDOS_fac,USE_the_DbGd,QUIET,FORCE_occ)
     use pars,           ONLY:SP
     use electrons,      ONLY:levels,n_spin
     use R_lattice,      ONLY:bz_samp 
     use electrons,      ONLY:n_spin
     use YPPm,           ONLY:DOS_E_steps
     implicit none
     type(bz_samp), intent(in) :: k
     type(levels),  intent(in) :: E
     integer,       intent(in) :: bands(2)
     real(SP),      intent(in) :: dos_E(DOS_E_steps)
     real(SP),      intent(out):: el_dos(DOS_E_steps,n_spin)
     real(SP), pointer,  intent(in) :: WF_fac(:,:,:)
     real(SP), pointer,  intent(in) :: PDOS_fac(:,:,:)
     logical,  optional, intent(in) :: USE_the_DbGd,QUIET,FORCE_occ
   end subroutine electrons_dos_elemental
   !
   subroutine excitons_sort_and_report(iq,BS_R,BS_E,BS_E_SOC_corr,EXC_spin)
     use pars,          ONLY:SP
     use BS,            ONLY:BS_H_dim
     use YPPm,          ONLY:EXCITON_spin
     implicit none
     integer                      :: iq
     complex(SP)                  :: BS_R(BS_H_dim),BS_E(BS_H_dim)
     real(SP),   optional,pointer :: BS_E_SOC_corr(:,:)
     type(EXCITON_spin),optional  :: EXC_spin
   end subroutine
   !
   subroutine PROJECTION_setup( what, bands)
     character(*)       :: what
     integer   ,optional:: bands(2)
   end subroutine
   !
   subroutine PROJECTION_plot( what, N , bands, W, TIME, OBS_K, OBS_B)
     use pars,           ONLY:SP
     use YPPm,           ONLY:PROJECT_depth
     use interpolate,    ONLY:GRID_k
     use electrons,      ONLY:n_spin
     character(*)       :: what
     integer            :: N,bands(2)
     real(SP)  ,optional:: W(N),TIME(N)
     real(SP)  ,optional:: OBS_K(GRID_k%nbz/PROJECT_depth,N,n_spin)
     real(SP)  ,optional:: OBS_B(bands(1):bands(2),GRID_k%nbz/PROJECT_depth,n_spin)
   end subroutine
   !
 end interface
 !
 contains
   !
   integer function PROJECTION_k_on_the_path_or_plane(ik)
     use YPPm,          ONLY:PROJECT_depth
     integer :: ik
     if (PROJECT_depth==1) then
       PROJECTION_k_on_the_path_or_plane=ik
       return
     endif
     if (mod(ik,PROJECT_depth)/=1) then
       PROJECTION_k_on_the_path_or_plane=0
       return
     endif
     PROJECTION_k_on_the_path_or_plane=ik/PROJECT_depth+1
   end function
   !
end module YPP_interfaces
