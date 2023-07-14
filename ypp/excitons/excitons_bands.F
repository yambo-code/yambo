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
subroutine excitons_bands(k,Xk,en,Xen,q)
 !
 use pars,          ONLY:SP,schlen,rZERO
 use LIVE_t,        ONLY:live_timing
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:Boltz_Temp
 use interpolate,   ONLY:INTERPOLATION_BZ
 use electrons,     ONLY:levels,nel_cond
 use functions,     ONLY:Fermi_fnc_derivative,boltzman_f
 use units,         ONLY:HA2EV
 use YPPm,          ONLY:EXCITONS_user_indexes,EXCITONS_n_user_states,coo_out, &
&                        BANDS_steps,INTERP_grid,K_transform,CIRCUIT_k,DOS_E_step, &
&                        DOS_E_steps,DOS_broadening,USER_k,l_exc_occ_interp,EXC_min
 use parser_m,      ONLY:parser
 use com,           ONLY:msg,of_open_close
 use stderr,        ONLY:intc
 use vec_operate,   ONLY:iku_v_2D_norm,iku_v_norm
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index
#if defined _YPP_ELPH
 use EXCPH,         ONLY:alphaQ
#endif
 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en
 !
 ! Work space
 !
 integer :: iq,i_c,i_l,ID_INTERP_EXC,ID_INTERP_OCC,i_E
 real(SP), allocatable    :: BS_all_E(:,:),BSE_interp_E(:,:),EXC_occ_interp(:,:),values(:)
 type(bz_samp)            :: GRID_q
 character(schlen)        :: file_name,file_name2
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
 ! Read and interpolate excitons dispersion
 !
 ID_INTERP_EXC=1
 ID_INTERP_OCC=2
 call excitons_interpolate_setup(k,Xk,en,Xen,q,ID_INTERP_EXC,ID_INTERP_OCC,.TRUE.)
 !
 ! Check exciton occupations
 !
 if(Boltz_Temp/=rZERO.and.EXC_min/=rZERO) then
   call msg('rs','Boltzman occupation for the excitons')
   if(nel_cond/=rZERO) call error('NelCond /= 0 not compatible with Boltzman occupation ')
   l_exc_occ_interp=.true.
 endif
 !
 if(nel_cond/=rZERO.and.Boltz_Temp/=rZERO) call warning('Excitonic Occupation, EXCTemp ignored, use ElecTemp!')
 !
 ! Interpolate and write on disk
 !
 call k_circuit_made_of_user_points( )
 !
 if(USER_k%nbz>0.and.BANDS_steps>0)  then
   !
   call msg('s','Exciton bands')
   !   
   YAMBO_ALLOC(BSE_interp_E,(EXCITONS_n_user_states,CIRCUIT_k%nibz))      
   call INTERPOLATION_BZ(K=CIRCUIT_k,NK=CIRCUIT_k%nbz,R1D=BSE_interp_E,ID=ID_INTERP_EXC)
   !
#if defined _YPP_ELPH
   if(alphaQ/=rZERO) then
     call msg('rs','Excitonic 2D bands stretching :',alphaQ)
      do iq=1,CIRCUIT_k%nibz
        BSE_interp_E(:,iq)=BSE_interp_E(:,iq)+alphaQ*iku_v_2D_norm(CIRCUIT_k%pt(iq,:3))
      enddo
   endif
#endif
   !
   if(l_exc_occ_interp) then
     YAMBO_ALLOC(EXC_occ_interp,(EXCITONS_n_user_states,CIRCUIT_k%nibz))      
     if(nel_cond/=rZERO) then
        ! Excitonic occupations are interpolated     
        call INTERPOLATION_BZ(K=CIRCUIT_k,NK=CIRCUIT_k%nbz,R1D=EXC_occ_interp,ID=ID_INTERP_OCC)
     else
        ! Boltzman occupations are directly calculated
        do iq=1,CIRCUIT_k%nibz
          do i_c=1,EXCITONS_n_user_states
            EXC_occ_interp(i_c,iq)=boltzman_f(BSE_interp_E(i_c,iq)-EXC_min)
          enddo
        enddo
        !
     endif
   endif
   !
   call print_BANDS()
   !
 endif
 !
 ! Interpolate exciton on a grid
 !
 if (GRID_interpolate) then
   !      
   call msg('s','Exciton Grid')
   !
   GRID_q%description="INTERP"
   call INTERPOLATION_grid(GRID_q,'i',INTERP_grid)
   !
   YAMBO_ALLOC(BSE_interp_E,(EXCITONS_n_user_states,GRID_q%nibz))
   !
   call INTERPOLATION_BZ(K=GRID_q,NK=GRID_q%nibz,R1D=BSE_interp_E,ID=ID_INTERP_EXC)
   !
   call print_INTERP_GRID()
   !
 endif
 !
 ! Interpolate exciton on a grid and calculate the DOS
 !
 if(l_prt_dos) then
   !      
   call msg('s','Exciton Density of States')
   !
   call electrons_dos_setup(BSE_interp_E, dos_E, (/1,EXCITONS_n_user_states/))
   !
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/DOS_E_steps/))
   !
   exc_DOS=rZERO
   !
   call live_timing("EXC DOS",px%n_of_elements(myid+1))
   !
   do i_E=1,DOS_E_steps
     if (.not.px%element_1D(i_E)) cycle
     do iq=1,GRID_q%nibz
       do i_c=1,EXCITONS_n_user_states
         exc_DOS(i_E)=exc_DOS(i_E)+Fermi_fnc_derivative((BSE_interp_E(i_c,iq)-dos_E(i_E)),DOS_broadening)*GRID_q%weights(iq)
       enddo
     enddo
     call live_timing(steps=1)
   enddo
   !
   call live_timing( )
   !
   call PP_redux_wait(exc_DOS)
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
       values(2)=exc_DOS(i_E)
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
     if(l_exc_occ_interp) then
       file_name2="excitons_occupation"
       call of_open_close(trim(file_name2),'ot')
       call msg('o '//trim(file_name2),"#",headings,INDENT=0,USE_TABS=.true.)
       call msg('o '//trim(file_name2),"#")
     endif
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
       !
       if(l_exc_occ_interp) then
         values(1+1:EXCITONS_n_user_states+1)=abs(EXC_occ_interp(:,iq))
         values(EXCITONS_n_user_states+2:EXCITONS_n_user_states+4)=tmp_q
         call msg('o '//trim(file_name2),' ',values,INDENT=0,USE_TABS=.true.)
       endif
       !
     enddo
     call of_open_close(trim(file_name))
     if(l_exc_occ_interp) call of_open_close(trim(file_name2))
     !
   end subroutine print_BANDS
   !
   subroutine print_INTERP_GRID()
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
     file_name="excitons_interpolated_IBZ"
     call of_open_close(trim(file_name),'ot')
     call msg('o '//trim(file_name),"#",headings,INDENT=0,USE_TABS=.true.)
     call msg('o '//trim(file_name),"#")
     !
     do iq=1,GRID_q%nibz
       !
       if (iq>1) values(1)=iku_v_norm(GRID_q%pt(iq,:3))
       !
       tmp_q = GRID_q%pt(iq,:3)
       !
       call K_transform(tmp_q,'iku')
       values(1+1:EXCITONS_n_user_states+1)=BSE_interp_E(:,iq)*HA2EV
       values(EXCITONS_n_user_states+2:EXCITONS_n_user_states+4)=tmp_q
       call msg('o '//trim(file_name),' ',values,INDENT=0,USE_TABS=.true.)
     enddo
     call of_open_close(trim(file_name))
     !
   end subroutine print_INTERP_GRID
 !
end subroutine
