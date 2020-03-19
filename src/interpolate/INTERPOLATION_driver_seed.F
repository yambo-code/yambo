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
subroutine INTERPOLATION_driver_seed(operation,ID,dims,SEED_k,SEED_E,SEED_RT,SEED_QP,SEED_R3,&
&                                    NEAREST,QUIET,SERIAL)
 !
 ! BOLTZ structure:
 !
 ! INTERPOLATION_BZ_setup
 ! INTERPOLATION_coefficients
 !
 use pars,                 ONLY:SP
 use real_time,            ONLY:RT_t
 use QP_m,                 ONLY:QP_t
 use R_lattice,            ONLY:bz_samp
 use electrons,            ONLY:levels,n_sp_pol
 use interpolate_tools,    ONLY:init_ID,FILL_SEED_E,FILL_SEED_RT,FILL_SEED_QP
 use interpolate,          ONLY:INTERP_obj,INTERPOLATION_coefficients,max_interpls,&
&                               l_INTERP_f,l_INTERP_E,l_INTERP_dE,l_INTERP_W,INTERPOLATE_is_serial,&
&                               INTERPOLATE_initialized,l_INTERP_Z,INTERP_mode,INTERPOLATE_is_quiet
 use IO_m,                 ONLY:IO_and_Messaging_switch
 !
 implicit none
 !
 character(*),  intent(in)           :: operation
 integer,       intent(out)          :: ID
 integer,       intent(in)           :: dims(2)
 type(bz_samp), intent(in)           :: SEED_k
 type(RT_t),    intent(in), optional :: SEED_RT
 type(levels),  intent(in), optional :: SEED_E
 type(QP_t),    intent(in), optional :: SEED_QP
 real(SP),      intent(in), optional :: SEED_R3(:,:,:)
 logical,       intent(in), optional :: NEAREST
 logical,       intent(in), optional :: QUIET
 logical,       intent(in), optional :: SERIAL
 !
 ! Work Space
 !
 integer               :: i_dim,i_obj
 !
 ! Quiet?
 !========
 if (present(QUIET)) INTERPOLATE_is_quiet=QUIET
 if (INTERPOLATE_is_quiet) call IO_and_Messaging_switch("-log")
 !
 ! Serial?
 !=========
 if (present(SERIAL)) INTERPOLATE_is_serial=SERIAL
 !
 ! Actions
 !=========
 !
 l_INTERP_f =operation(1:1)== "f".or.operation(1:2)== "df"
 l_INTERP_E =operation(1:1)== "E"
 l_INTERP_Z =operation(1:1)== "Z"
 l_INTERP_W =operation(1:1)== "W"
 l_INTERP_dE=operation(1:2)== "dE"
 !
 if (index(operation,"_from_DB")>0) then
   l_INTERP_dE =index(operation(1:3),"dE")>0
   l_INTERP_W  =index(operation(1:3),"W")>0
   l_INTERP_Z  =index(operation(1:3),"Z")>0
 endif
 !
 ! ID to use
 !===========
 !
 ID=-1
 do i_obj=1,max_interpls
   if (INTERP_obj(i_obj)%in_use) cycle
   ID=i_obj
   exit
 enddo
 !
 ! SEED
 !======
 !
 if (present(SEED_E))  INTERP_obj(ID)%D=n_sp_pol
 if (present(SEED_RT)) INTERP_obj(ID)%D=n_sp_pol
 if (present(SEED_QP)) then
   INTERP_obj(ID)%D=0
   if (l_INTERP_dE) INTERP_obj(ID)%D=INTERP_obj(ID)%D+n_sp_pol
   if (l_INTERP_W ) INTERP_obj(ID)%D=INTERP_obj(ID)%D+n_sp_pol
   if (l_INTERP_Z ) INTERP_obj(ID)%D=INTERP_obj(ID)%D+n_sp_pol*2
 endif
 if (present(SEED_R3)) INTERP_obj(ID)%D=size(SEED_R3(1,1,:))
 !
 if (present(SEED_E))  call init_ID(ID,dims,SEED_k,operation,"E")
 if (present(SEED_RT)) call init_ID(ID,dims,SEED_k,operation,"RT")
 if (present(SEED_QP)) call init_ID(ID,dims,SEED_k,operation,"QP")
 !
 if (present(NEAREST)) then
   INTERP_obj(ID)%use_NN=NEAREST 
   if (NEAREST) INTERP_mode="NN"
 endif
 !
 if (.not.INTERPOLATE_initialized.and..not.INTERP_obj(ID)%use_NN) then
   !
   call INTERPOLATION_BZ_setup(SEED_k)
   !
   INTERPOLATE_initialized=.TRUE.
   !
 endif
 !
 ! I asssume here that SEED_E and SEED_RT lives on the same grid of SEED_k
 !
 if (present(SEED_E))   call FILL_SEED_E(ID,SEED_E)
 if (present(SEED_RT))  call FILL_SEED_RT(ID,SEED_RT)
 if (present(SEED_QP))  call FILL_SEED_QP(ID,SEED_QP)
 !
 if (present(SEED_R3)) then
   do i_dim=1,dims(2)-dims(1)+1
     INTERP_obj(ID)%SEED(dims(1)+i_dim-1,:,:)=SEED_R3(i_dim,:,:)
   enddo
 endif
 !
 if (INTERP_obj(ID)%use_NN) return
 !
 call INTERPOLATION_coefficients(R2D=INTERP_obj(ID)%SEED,k=SEED_k,ID=INTERP_obj(ID)%ID,ID_obj=ID)
 !
end subroutine INTERPOLATION_driver_seed
