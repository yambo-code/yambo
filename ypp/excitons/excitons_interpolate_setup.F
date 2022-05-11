!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
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
subroutine excitons_interpolate_setup(k,Xk,en,Xen,q,ID_INTERP_EXC,report)
 !
 ! This subroutine read and interpolate exciton 
 !
 use pars,          ONLY:SP,schlen
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels
 use interpolate,   ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients
 use YPPm,          ONLY:BS_E,EXCITONS_user_indexes,EXCITONS_n_user_states,coo_out,K_transform
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en
 logical, intent(in) :: report
 integer, intent(inout) :: ID_INTERP_EXC
 !
 ! Work space
 !
 integer :: iq,i_c,i_l,io_err
 real(SP), allocatable    :: BS_all_E(:,:)
 real(SP), allocatable    :: values(:)
 real(SP) :: tmp_q(3)
 character(schlen), allocatable :: headings(:)
 !
 ! Allocate arrays for all eigenvalues/eigenvectors
 ! 
 YAMBO_ALLOC(BS_all_E,(EXCITONS_n_user_states,q%nibz))
 !
 call section('*','Excitons Interpolation')
 !
 call excitons_read(k,Xk,en,Xen,1,"clean",io_err)
 !
 ! Read all eigenvalues and eigenvectors
 !
 do iq=1,q%nibz
   call msg('s','Reading excitons at @ Q-index #',iq)
   call excitons_read(k,Xk,en,Xen,iq,"check",io_err)
   call excitons_read(k,Xk,en,Xen,iq,"eigenvalues",io_err)
   do i_c=1,EXCITONS_n_user_states
     i_l=EXCITONS_user_indexes(i_c)
     BS_all_E(i_c,iq)=BS_E(i_l)
   enddo
   call excitons_read(k,Xk,en,Xen,iq,"clean",io_err)
   !
 enddo
 !
 if(report.eqv..true.) then
   !
   ! Write in the report the exciton energies
   !
   YAMBO_ALLOC(values,(EXCITONS_n_user_states+4))
   allocate(headings(EXCITONS_n_user_states+4))
   !
   call msg('nr','Excitons Energies:')
   headings(1:3)=(/"q_x ("//trim(coo_out)//")","q_y ("//trim(coo_out)//")","q_z ("//trim(coo_out)//")"/)
   !
   do i_c=1,EXCITONS_n_user_states
      i_l=EXCITONS_user_indexes(i_c)
      headings(i_c+3)=' e'//trim(intc(i_l))//' [eV]'
   enddo
   call msg('r',"#",headings(1:EXCITONS_n_user_states+3))
   do iq=1,q%nibz
     tmp_q=q%pt(iq,:3)
     call K_transform(tmp_q,'iku')
     values(1:3)=tmp_q
     do i_c=1,EXCITONS_n_user_states
        i_l=EXCITONS_user_indexes(i_c)
        values(i_c+3)=BS_all_E(i_c,iq)*HA2EV
     enddo
     call msg('r',' ',values(1:EXCITONS_n_user_states+3))
   enddo
   !
   YAMBO_FREE(values)
   deallocate(headings)
   !
 endif
 !
 ! Fourier interpolation of exciton dispersion
 !
 call INTERPOLATION_BZ_setup(q)
 INTERP_obj(ID_INTERP_EXC)%what="excitons"
 call INTERPOLATION_coefficients(R1D=BS_all_E,k=q,NK=q%nibz,ID=ID_INTERP_EXC,ID_obj=ID_INTERP_EXC)
 !
end subroutine
