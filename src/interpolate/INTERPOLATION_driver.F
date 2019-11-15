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
subroutine INTERPOLATION_driver_seed(dims,operation,SEED_k,SEED_E,SEED_RT,SEED_QP,SEED_R3,ID_op,NEAREST)
 !
 use pars,                 ONLY:SP
 use real_time,            ONLY:RT_t
 use QP_m,                 ONLY:QP_t
 use R_lattice,            ONLY:bz_samp
 use electrons,            ONLY:levels,n_sp_pol
 use interpolate_tools,    ONLY:init_ID,FILL_SEED_E,FILL_SEED_RT,FILL_SEED_QP
 use interpolate,          ONLY:INTERP_obj,INTERPOLATION_coefficients,max_interpls,&
&                               l_INTERP_f,l_INTERP_E,l_INTERP_dE,l_INTERP_W,&
&                               INTERPOLATE_initialized,l_INTERP_Z
 !
 implicit none
 !
 integer,       intent(in)           :: dims(2)
 character(*),  intent(in)           :: operation
 type(bz_samp), intent(in)           :: SEED_k
 type(RT_t),    intent(in), optional :: SEED_RT
 type(levels),  intent(in), optional :: SEED_E
 type(QP_t),    intent(in), optional :: SEED_QP
 real(SP),      intent(in), optional :: SEED_R3(:,:,:)
 logical,       intent(in), optional :: NEAREST
 integer,       intent(out),optional :: ID_op
 !
 ! Work Space
 !
 integer               :: i_dim,ID,i_obj
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
   if (.not.INTERP_obj(i_obj)%in_use) then 
     ID=i_obj
     exit
   endif
 enddo
 !
 if (present(ID_op)) ID_op=ID
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
 call init_ID(ID,dims,SEED_k,operation)
 !
 if (present(NEAREST)) INTERP_obj(ID)%use_NN=NEAREST 
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
!
!
subroutine INTERPOLATION_driver_do(operation,OUT_k,OUT_E,QUIET)
 !
 use R_lattice,            ONLY:bz_samp
 use electrons,            ONLY:levels
 use interpolate_tools,    ONLY:INTERPOLATION_nearest_k_report
 use interpolate,          ONLY:INTERP_obj,max_interpls,INTERPOLATION_coefficients,&
&                               INTERPOLATION_BZ,INTERPOLATION_nearest_k,&
&                               l_INTERP_f,l_INTERP_E,l_INTERP_dE,&
&                               l_integral_respect
 use IO_m,                 ONLY:IO_and_Messaging_switch
 !
#include<memory.h>
 !
 character(*),  intent(in)              :: operation
 type(bz_samp), intent(inout)           :: OUT_k
 type(levels),  intent(inout), optional :: OUT_E
 logical,       intent(in),    optional :: QUIET
 !
 ! Work Space
 !
 integer               :: ID,i_obj,n(2),nk
 logical               :: QUIET_
 !
 QUIET_=.false.
 if (present(QUIET)) QUIET_=QUIET
 !
 if (QUIET_) call IO_and_Messaging_switch("-log")
 !
 do i_obj=1,max_interpls
   if (operation==trim(INTERP_obj(i_obj)%what)) ID=i_obj
 enddo
 !
 ! ... Allocation
 !
 n =INTERP_obj(ID)%nb
 nk=INTERP_obj(ID)%nk
 !
 YAMBO_ALLOC(INTERP_obj(ID)%INTERP,(n(1):n(2),OUT_k%nibz,INTERP_obj(ID)%D))
 !
 ! ... Interpolation...
 !
 if (INTERP_obj(ID)%use_NN) then
   ! ... Using the Nearest point method
   call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%pt,INTERP_obj(ID)%INTERP)
   if (.not.QUIET_) call INTERPOLATION_nearest_k_report(ID)
   !
 else
   !
   ! ... Using BOLTZ method
   call INTERPOLATION_BZ(K=OUT_k,R2D=INTERP_obj(ID)%INTERP,ID=INTERP_obj(ID)%ID)
   !
 endif
 !
 ! f/dF normalization
 !====================
 if (l_INTERP_f) call INTERPOLATION_integral_respect(ID,OUT_k,INTERP_obj(ID)%INTERP,l_integral_respect)
 !
 ! store f/E/dE in OUT_E
 !=======================
 if (present(OUT_E)) then
   if (l_INTERP_E.or.l_INTERP_dE) then
     YAMBO_ALLOC(OUT_E%E,(n(1):n(2),OUT_k%nibz,INTERP_obj(ID)%D))
   else if (l_INTERP_f) then
     YAMBO_ALLOC(OUT_E%f,(n(1):n(2),OUT_k%nibz,INTERP_obj(ID)%D))
   endif
   OUT_E%nk=OUT_k%nibz
   OUT_E%nb=INTERP_obj(ID)%nb(2)
   if (l_INTERP_E.or.l_INTERP_dE) OUT_E%E=INTERP_obj(ID)%INTERP
   if (l_INTERP_f)                OUT_E%f=INTERP_obj(ID)%INTERP
 endif
 !
 call IO_and_Messaging_switch("+log")
 !
end subroutine INTERPOLATION_driver_do
!
!
subroutine INTERPOLATION_driver_create_grid(GRID)
 !
 use interpolate,          ONLY:GRID_k
 use R_lattice,            ONLY:bz_samp_reset
 !
 implicit none
 !
 integer, intent(in) :: GRID(3)
 !
 call bz_samp_reset(GRID_k)
 GRID_k%description="INTERP"
 call INTERPOLATION_create_the_grid(GRID_k,'i',GRID)
 !
end subroutine INTERPOLATION_driver_create_grid
!
!
subroutine INTERPOLATION_driver_end(ID_op)
 !
 use interpolate_tools,    ONLY:INTERPOLATION_reset
 use interpolate,          ONLY:INTERPOLATE_msg_fmt,INTERPOLATE_initialized,&
&                               lattice_vectors,INTERP_obj,max_interpls
 !
#include<memory.h>
 !
 integer, intent(in) :: ID_op
 !
 ! Work space
 !
 integer             :: i_obj
 !
 if (ID_op==0) then
   !
   if (ID_op==0)                      return 
   if (.not.INTERP_obj(ID_op)%in_use) return 
   !
   call INTERPOLATION_reset(ID_op)
   !
 else
   !
   do i_obj=1,max_interpls
     call INTERPOLATION_reset(i_obj)
   enddo
   !
   YAMBO_FREE_P(lattice_vectors)
   !
   INTERPOLATE_initialized=.FALSE.
   INTERPOLATE_msg_fmt="rs"
   !
 endif
 !
end subroutine INTERPOLATION_driver_end
