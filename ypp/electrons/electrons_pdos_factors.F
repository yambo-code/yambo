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
subroutine electrons_pdos_factors(kpts,bands,PDOS_fac)
 !
 use pars,           ONLY:SP,cZERO
 use electrons,      ONLY:n_sp_pol
 use R_lattice,      ONLY:nkibz
 use com,            ONLY:error
 use IO_m,           ONLY:OP_RD_CL,REP,NONE,DUMP,io_control
 use YPP,            ONLY:PDOS_atoms,PDOS_kinds,PDOS_wfcs,PDOS_l,PDOS_j,PDOS_m
 use atom_proj
 !
 implicit none
 !
 integer,  intent(in)  :: kpts(2),bands(2)
 real(SP), intent(out) :: PDOS_fac(bands(1):bands(2),kpts(1):kpts(2),n_sp_pol)
 !
 ! Workspace
 !
 integer     :: i_sp_pol,ik,ib,iproj,iatom,ikind,iwfc,j2,il,im
 real(SP)    :: tmp_fac
 !
 ! I/O
 !
 integer           :: io_err,ID
 integer, external :: io_ATMPROJ_pwscf
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
 io_err=io_ATMPROJ_pwscf(ID)
 !
 if (bands(2)>n_proj_bands) call error(' not enought projections for PDOS')
 !
 do i_sp_pol=1,n_sp_pol
   do ik=kpts(1),kpts(2)
     call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/ik+(i_sp_pol-1)*nkibz+1/),ID=ID)
     io_err=io_ATMPROJ_pwscf(ID)
   enddo
 enddo
 !
 PDOS_fac=0._SP
 do i_sp_pol=1,n_sp_pol
   do ik=kpts(1),kpts(2)
     do ib=bands(1),bands(2)
       !
       tmp_fac=0._SP
       !
       do iproj=1,n_atom_wfc
         !
         iatom=atomic_table(1,iproj)
         ikind=atomic_table(2,iproj)
         iwfc =atomic_table(3,iproj)
         j2   =atomic_table(4,iproj)
         il   =atomic_table(5,iproj)
         im   =atomic_table(6,iproj)
         !
         if(all(PDOS_atoms(:)/=-1) .and. (iatom<PDOS_atoms(1).or.iatom>PDOS_atoms(2)) ) cycle
         if(all(PDOS_kinds(:)/=-1) .and. (ikind<PDOS_kinds(1).or.ikind>PDOS_kinds(2)) ) cycle
         if(all(PDOS_wfcs(:) /=-1) .and. (iwfc <PDOS_wfcs(1) .or.iwfc >PDOS_wfcs(2) ) ) cycle
         !
         if(all(PDOS_l(:)/=-1) .and. (il<         1+PDOS_l(1) .or.il>         1+PDOS_l(2) ) ) cycle
         if(all(PDOS_j(:)/=-1) .and. (j2<nint(2._SP*PDOS_j(1)).or.j2>nint(2._SP*PDOS_j(2))) ) cycle
         !
         !PDOS_m(2)
         !
         tmp_fac=tmp_fac+abs(atomic_projections(iproj,ib,ik,i_sp_pol))**2
         !
       enddo
       !
       PDOS_fac(ib,ik,i_sp_pol)=tmp_fac
       !
       ! DEBUG <
       ! write(*,*) i_sp_pol,ik,ib,PDOS_fac(ib,ik,i_sp_pol)
       ! DEBUG >
       !
     enddo
   enddo
 enddo
 !
end subroutine electrons_pdos_factors
