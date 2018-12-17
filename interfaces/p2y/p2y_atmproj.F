!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine p2y_atmproj(filename,k)
 !
 use pars,                  ONLY:SP,DP,schlen
 use R_lattice,             ONLY:bz_samp
 use electrons,             ONLY:n_sp_pol
 use com,                   ONLY:error
 use atom_proj,             ONLY:atomic_projections,n_atom_wfc,n_proj_bands
 use IO_m,                  ONLY:OP_WR_CL,OP_APP_CL,REP,io_control
 !
 use atmproj_tools_module,  ONLY:atmproj_read_ext
 !
#include<memory.h>
 !
 character(schlen), intent(in) :: filename
 type(bz_samp),     intent(in) :: k
 !
 ! Work space
 !
 integer  :: nbnd,nkpts,nspin,ik,i_sp_pol
 !
 ! I/O
 !
 integer           :: io_err,ID
 integer, external :: io_ATMPROJ_pwscf
 !
 ! Allocs
 !
 complex(DP), allocatable  :: atomic_projections_dp(:,:,:,:)
 !
 ! (a) get dimensions
 !
 call atmproj_read_ext(filename,NBND=n_proj_bands,NKPT=nkpts,NSPIN=nspin,NATOMWFC=n_atom_wfc,IERR=io_err)
 !
 if (nspin/=n_sp_pol) call error(' nspin is not consistent with n_sp_pol')
 !
 ! (b) get projetions
 !
 YAMBO_ALLOC(atomic_projections_dp,(n_atom_wfc,n_proj_bands,nkpts,nspin))
 !
 call atmproj_read_ext(filename,proj=atomic_projections_dp,IERR=io_err)
 !
 ! (c) store them in yambo format
 !
 YAMBO_ALLOC(atomic_projections,(n_atom_wfc,n_proj_bands,nkpts,nspin))
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_ATMPROJ_pwscf(ID)
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,nkpts
     !
     atomic_projections(:,:,ik,i_sp_pol)=cmplx(atomic_projections_dp(:,:,ik,i_sp_pol),kind=SP)
     !
     call io_control(ACTION=OP_APP_CL,COM=REP,SEC=(/ik+(i_sp_pol-1)*nkpts+1/),ID=ID)
     io_err=io_ATMPROJ_pwscf(ID)
     !
   enddo
 enddo
 !
end subroutine p2y_atmproj
