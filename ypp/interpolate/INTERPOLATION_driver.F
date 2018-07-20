!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine INTERPOLATION_driver(bands,SEED_k,SEED_E,SEED_RT,OUT_k,OUT_E,operation,QUIET)
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_t
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use electrons,      ONLY:levels,n_sp_pol
 use YPP,            ONLY:INTERP_grid
 use interpolate,    ONLY:GRID_k,INTERP_obj,max_interpls,INTERPOLATION_coefficients,&
&                         INTERPOLATION_reset,INTERPOLATION_BZ
 use IO_m,           ONLY:IO_and_Messaging_switch
 !
#include<memory.h>
 !
 integer,       intent(in),    optional :: bands(2)
 type(bz_samp), intent(in),    optional :: SEED_k
 type(bz_samp), intent(inout), optional :: OUT_k
 type(RT_t),    intent(in)   , optional :: SEED_RT
 type(levels),  intent(in)   , optional :: SEED_E
 type(levels),  intent(inout), optional :: OUT_E
 character(*),  intent(in)              :: operation
 logical,       intent(in)   , optional :: QUIET
 !
 ! Work Space
 !
 real(SP), allocatable :: SEED_obj(:,:,:)
 integer               :: i_sp_pol,ib,ik,ID_free,ID_to_use,i_obj,i_RT
 logical               :: l_f,l_E,l_dE,l_G,l_EQ
 !
 l_f   =operation(1:1)== "f"
 l_E   =operation(1:1)== "E"
 l_G   =operation(1:1)== "G"
 l_dE  =operation(1:2)== "dE"
 l_EQ  =index(operation,"EQ")>0
 !
 if (present(QUIET)) then
  if (QUIET) call IO_and_Messaging_switch("-log")
 endif
 !
 if (operation=="GRID") then
  !
  call bz_samp_reset(GRID_k)
  GRID_k%description="INTERP_GRID"
  call INTERPOLATION_create_the_grid(GRID_k,'i',INTERP_grid)
  !
  return
  !
 endif
 !
 if (operation=="END") then
   !
   do i_obj=1,max_interpls
     call INTERPOLATION_reset(i_obj)
   enddo
   !
   return
   !
 endif
 !
 if (operation=="INIT") then
   !
   call INTERPOLATION_BZ_setup(SEED_k) 
   !
   return
   !
 endif
 !
 do i_obj=1,max_interpls
   !
   if (.not.INTERP_obj(i_obj)%in_use) then 
     ID_free=i_obj
     exit
   endif
   !
   if (operation==trim(INTERP_obj(i_obj)%what)) then 
     ID_to_use=i_obj
   endif
   ! 
 enddo
 !
 if (present(SEED_k).and.(present(SEED_E).or.present(SEED_RT))) then
   !
   YAMBO_ALLOC(SEED_obj,(bands(1):bands(2),SEED_k%nibz,n_sp_pol))
   !
   INTERP_obj(ID_free)%n=bands
   INTERP_obj(ID_free)%in_use=.TRUE.
   INTERP_obj(ID_free)%what=operation
   !
   if (present(SEED_E)) then
     !
     do i_sp_pol=1,n_sp_pol
       do ib=bands(1),bands(2)
        do ik=1,SEED_k%nibz
           if (l_f ) SEED_obj(ib,ik,i_sp_pol)=SEED_E%f(ib,ik,i_sp_pol)
           if (l_E ) SEED_obj(ib,ik,i_sp_pol)=SEED_E%E(ib,ik,i_sp_pol)
           if (l_dE) SEED_obj(ib,ik,i_sp_pol)=SEED_E%E(ib,ik,i_sp_pol)-SEED_E%Eo(ib,ik,i_sp_pol)
         enddo
       enddo
     enddo
     !
   else if (present(SEED_RT)) then
     !
     do i_RT=1,SEED_RT%nstates
       ib      =SEED_RT%table(i_RT,1)
       ik      =SEED_RT%table(i_RT,3)
       i_sp_pol=SEED_RT%table(i_RT,4)
       if (l_G ) SEED_obj(ib,ik,i_sp_pol)=SEED_RT%NEQ_Lifetime(i_RT) 
     enddo
     !
   endif
   !
   call INTERPOLATION_coefficients(R2D=SEED_obj,k=SEED_k,Nk=SEED_k%nibz,ID=INTERP_obj(ID_free)%ID,ID_obj=ID_free)
   !
   YAMBO_FREE(SEED_obj)
   !
 endif
 !
 if (present(OUT_k).and.present(OUT_E)) then
   !
   if (l_E.or.l_dE) then
     YAMBO_ALLOC(OUT_E%E,(INTERP_obj(ID_to_use)%n(1):INTERP_obj(ID_to_use)%n(2),OUT_k%nbz,n_sp_pol))
     call INTERPOLATION_BZ(K=OUT_k,R2D=OUT_E%E,ID=INTERP_obj(ID_to_use)%ID)
   else if (l_f) then
     YAMBO_ALLOC(OUT_E%f,(INTERP_obj(ID_to_use)%n(1):INTERP_obj(ID_to_use)%n(2),OUT_k%nbz,n_sp_pol))
     call INTERPOLATION_BZ(K=OUT_k,R2D=OUT_E%f,ID=INTERP_obj(ID_to_use)%ID)
   endif
   !
   if (.not.allocated(OUT_k%weights)) then
     YAMBO_ALLOC(OUT_k%weights,(OUT_k%nbz))
     OUT_k%weights=1./real( OUT_k%nbz )
   endif
   !
   OUT_E%nk=OUT_k%nibz
   OUT_E%nb=INTERP_obj(ID_to_use)%n(2)
   !
 endif
 !
 call IO_and_Messaging_switch("+log")
 !
end subroutine INTERPOLATION_driver
