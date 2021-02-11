!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine excitons_bands(k,Xk,en,Xen,q)
 !
 use pars,          ONLY:SP,DP,schlen,pi,cZERO,cONE,rZERO
 use LIVE_t,        ONLY:live_timing
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use interpolate,   ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients
 use electrons,     ONLY:levels,n_sp_pol
 use functions,     ONLY:Fermi_fnc_derivative
 use units,         ONLY:HA2EV
 use YPPm,          ONLY:BS_E,EXCITONS_user_indexes,EXCITONS_n_user_states,coo_in,coo_out, &
&                        BANDS_steps,INTERP_grid,K_transform,CIRCUIT_k,DOS_E_step,DOS_E_range, &
&                        DOS_E_steps,DOS_broadening,USER_k
 use interpolate,   ONLY:nshells
 use parser_m,      ONLY:parser
 use com,           ONLY:msg,of_open_close
 use stderr,        ONLY:intc
 use vec_operate,   ONLY:iku_v_norm
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index

 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en
 !
 ! Work space
 !
 integer :: iq,i_c,i_l,ID_INTERP,ik,ib,i_E
 real(SP), allocatable    :: BS_all_E(:,:),BSE_interp_E(:,:),values(:)
 type(bz_samp)            :: GRID_q
 character(schlen)        :: coo_out_,file_name
 character(schlen), allocatable :: headings(:)
 logical                  :: GRID_interpolate,l_prt_dos
 real(SP)                 :: tmp_q(3)
 real(SP)                 :: dos_E(DOS_E_steps),exc_DOS(DOS_E_steps)
 type(PP_indexes)         :: px
 !
 ! Parse the different interpolations
 !
 call parser('PrtDOS',l_prt_dos)
 GRID_interpolate    = all(INTERP_grid>0)
 !
 if(l_prt_dos.and..not.GRID_interpolate) call error("You need to specify a Q-grid for the DOS")
 !
 ! Allocate arrays for all eigenvalues/eigenvectors
 ! 
 YAMBO_ALLOC(BS_all_E,(EXCITONS_n_user_states,q%nibz))
 YAMBO_ALLOC(values,(EXCITONS_n_user_states+4))
 allocate(headings(EXCITONS_n_user_states+4))
 !
 !
 call section('*','Excitons Interpolation')
 !
 call excitons_read(k,Xk,en,Xen,1,"clean")
 !
 ! Read all eigenvalues and eigenvectors
 !
 do iq=1,q%nibz
   call msg('s','Reading excitons at @ Q-index #',iq)
   call excitons_read(k,Xk,en,Xen,iq,"check")
   call excitons_read(k,Xk,en,Xen,iq,"eigenvalues")
   do i_c=1,EXCITONS_n_user_states
     i_l=EXCITONS_user_indexes(i_c)
     BS_all_E(i_c,iq)=BS_E(i_l)
   enddo
   call excitons_read(k,Xk,en,Xen,iq,"clean")
   !
 enddo
 !
 ! Fourier interpolation of exciton dispersion
 !
 call INTERPOLATION_BZ_setup(q)
 ID_INTERP=1
 INTERP_obj(ID_INTERP)%what="excitons"
 call INTERPOLATION_coefficients(R1D=BS_all_E,k=q,NK=q%nibz,ID=ID_INTERP,ID_obj=ID_INTERP)
 !
 ! Excitons Interpolation on GRID or CIRCUIT
 !
 if(BANDS_steps>0) call k_circuit_made_of_user_points( )
 !
 ! Interpolate and write on disk
 !
 if(USER_k%nbz>0)  then
   !
   call msg('s','Exciton bands interpolation')
   !   
   YAMBO_ALLOC(BSE_interp_E,(EXCITONS_n_user_states,CIRCUIT_k%nibz))      
   call INTERPOLATION_BZ(K=CIRCUIT_k,NK=CIRCUIT_k%nbz,R1D=BSE_interp_E,ID=ID_INTERP)
   !
   call print_BANDS()
   !
 endif
 !
 ! Interpolate exciton on a grid and calculate the DOS
 !
 if(l_prt_dos) then
   !      
   call msg('s','Exciton Density of States')
   !
   GRID_q%description="INTERP"
   call INTERPOLATION_grid(GRID_q,'i',INTERP_grid)
   YAMBO_ALLOC(BSE_interp_E,(EXCITONS_n_user_states,GRID_q%nibz))      
   call INTERPOLATION_BZ(K=GRID_q,NK=GRID_q%nibz,R1D=BSE_interp_E,ID=ID_INTERP)
   !
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/DOS_E_steps/))
   !
   call electrons_dos_setup(BSE_interp_E, dos_E, (/1,EXCITONS_n_user_states/))
   !
   exc_dos=rZERO
   !
   call live_timing("EXC DOS",px%n_of_elements(myid+1))
   !
   do i_E=1,DOS_E_steps
     if (.not.px%element_1D(i_E)) cycle
     do iq=1,GRID_q%nibz
       do i_c=1,EXCITONS_n_user_states
         exc_dos(i_E)=exc_dos(i_E)+Fermi_fnc_derivative((BSE_interp_E(i_c,iq)-dos_E(i_E))*HA2EV,DOS_broadening)*GRID_q%weights(iq)
       enddo
     enddo
     call live_timing(steps=1)
   enddo
   !
   call live_timing( )
   !
   call PP_redux_wait(exc_dos)
   !
   call print_DOS()
   !
 endif
 !
 YAMBO_FREE(values)
 deallocate(headings)
 !
 contains 
   !
   subroutine print_DOS()
     implicit none
     ! 
     file_name="excitons_dos"
     call of_open_close(trim(file_name),'ot')
     headings(1)=" E [eV] "
     headings(2)=" Dos    "
     call msg('o '//trim(file_name),"#",headings(1:2),INDENT=0,USE_TABS=.true.)
     call msg('o '//trim(file_name),"#")
     do i_E=1,DOS_E_steps
       values(1)=dos_E(i_E)*HA2EV
       values(2)=exc_dos(i_E)
       call msg('o '//trim(file_name),' ',values(1:2),INDENT=0,USE_TABS=.true.)
     enddo
     !
     call of_open_close(trim(file_name))
     !
   end subroutine print_DOS
   ! 
   subroutine print_BANDS()
     implicit none
     !
     headings(1)=" |q| (a.u.) "
     do i_c=1,EXCITONS_n_user_states
        i_l=EXCITONS_user_indexes(i_c)
        headings(i_c+1)=' e'//intc(i_l)
     enddo
     headings(EXCITONS_n_user_states+2:EXCITONS_n_user_states+4)=&
&           (/"q_x ("//trim(coo_out)//")","q_y ("//trim(coo_out)//")","q_z ("//trim(coo_out)//")"/)
     !
     file_name="excitons_interpolated"
     call of_open_close(trim(file_name),'ot')
     call msg('o '//trim(file_name),"#",headings,INDENT=0,USE_TABS=.true.)
     call msg('o '//trim(file_name),"#")
     !
     values=0._SP
     !
     do iq=1,CIRCUIT_k%nibz
       !
       if (iq>1) values(1)=values(1)+iku_v_norm(CIRCUIT_k%pt(iq,:3)-CIRCUIT_k%pt(iq-1,:3))
       !
       tmp_q=CIRCUIT_k%pt(iq,:3)
       !
       call K_transform(tmp_q,'iku')
       values(1+1:EXCITONS_n_user_states+1)=BSE_interp_E(:,iq)*HA2EV
       values(EXCITONS_n_user_states+2:EXCITONS_n_user_states+4)=tmp_q
       call msg('o '//trim(file_name),' ',values,INDENT=0,USE_TABS=.true.)
     enddo
     call of_open_close(trim(file_name))
     !
   end subroutine print_BANDS
 !
end subroutine
