!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS FP
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
!! Code taken from branch phys-excitons
!! file ypp/excitons/exciton_symmetries.f90
!
subroutine ELEC_WF_phases(E,k,l_min_mem,ID_phases)
 !
 use D_lattice,      ONLY:nsym
 use R_lattice,      ONLY:bz_samp
 use wave_func,      ONLY:io_WF_phases,WF_phases_b_map,WF_phases
 use electrons,      ONLY:levels,n_sp_pol 
 use BS,             ONLY:BS_bands
 use IO_m,           ONLY:RD,OP_RD,DUMP,REP
 use IO_int,         ONLY:io_control
 !
#include<y_memory.h>
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k
 integer,       intent(out):: ID_phases
 logical,       intent(in) :: l_min_mem
 !
 ! Work Space
 !
 integer :: nsz(5)
 integer :: io_err
 integer :: i_sp_pol,ik,il1
 !
 ! Electronic part of the symmetries
 !
 call k_build_up_BZ_tables(k)
 call k_ibz2bz(k,'iku',.true.)
 call k_small_group(k)
 !
 call WF_phase_matrices(E,k,BS_bands,(/1,k%nibz/))
 !
 YAMBO_ALLOC(WF_phases_b_map,(BS_bands(1):BS_bands(2),k%nibz,n_sp_pol,2))
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1,2/),MODE=DUMP,ID=ID_phases)
 io_err=io_WF_phases(BS_bands,0,0,0,ID_phases,(/0,0,0,0,0/))
 !
 if (l_min_mem) return
 !
 nsz(1:2)=maxval(WF_phases_b_map(:,:,:,1))
 nsz(3)  =maxval(WF_phases_b_map(:,:,:,2))
 nsz(4)  =nsym
 nsz(5)  =k%nbz
 YAMBO_ALLOC(WF_phases,(nsz(1),nsz(2),nsz(3),nsz(4),nsz(5)))
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,k%nibz
     nsz(1:2)=maxval(WF_phases_b_map(:,ik,i_sp_pol,1))
     nsz(3)  =maxval(WF_phases_b_map(:,ik,i_sp_pol,2))
     nsz(4)  =nsym
     nsz(5)  =k%nstar(ik)
     il1=1
     if(ik>0) il1= sum(k%nstar(:ik-1))+1
     call io_control(ACTION=RD,COM=REP,SEC=(/3/),MODE=DUMP,ID=ID_phases)
     io_err=io_WF_phases( BS_bands,ik,0,i_sp_pol,ID_phases,nsz, &
&    WF_phases(:nsz(1),:nsz(2),:nsz(3),:,il1:il1+nsz(5)-1) )
   enddo
 enddo
 !
end subroutine