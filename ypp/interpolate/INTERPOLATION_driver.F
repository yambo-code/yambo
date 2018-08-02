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
subroutine INTERPOLATION_driver(bands,SEED_k,SEED_E,SEED_RT,SEED_R3,&
&                               OUT_k,OUT_E,operation,QUIET,ID_out,NEAREST)
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_t
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use electrons,      ONLY:levels,n_sp_pol
 use YPP,            ONLY:INTERP_grid
 use parser_m,       ONLY:parser
 use y_interpolate,  ONLY:INTERPOLATION_nearest_k
 use interpolate,    ONLY:GRID_k,INTERP_obj,max_interpls,INTERPOLATION_coefficients,&
&                         INTERPOLATION_reset,INTERPOLATION_BZ,interpls
 use IO_m,           ONLY:IO_and_Messaging_switch
 !
#include<memory.h>
 !
 integer,       intent(in),    optional :: bands(2)
 type(bz_samp), intent(in),    optional :: SEED_k
 type(bz_samp), intent(inout), optional :: OUT_k
 type(RT_t),    intent(in)   , optional :: SEED_RT
 type(levels),  intent(in)   , optional :: SEED_E
 real(SP),      intent(in)   , optional :: SEED_R3(:,:,:)
 type(levels),  intent(inout), optional :: OUT_E
 character(*),  intent(in)              :: operation
 logical,       intent(in)   , optional :: QUIET,NEAREST
 integer,       intent(out),   optional :: ID_out
 !
 ! Work Space
 !
 integer               :: i_s,ib,ik,ID,i_obj,i_RT,n(2)
 logical               :: l_f,l_E,l_dE,l_G,l_EQ,l_df,l_normalize_Nel
 !
 ! Actions
 !=========
 !
 ! Check if Nel has to be renormalized
 !
 call parser('NormN',l_normalize_Nel)
 !
 l_f   =operation(1:1)== "f"
 l_E   =operation(1:1)== "E"
 l_G   =operation(1:1)== "G"
 l_dE  =operation(1:2)== "dE"
 l_df  =operation(1:2)== "df"
 l_EQ  =index(operation,"EQ")>0
 !
 ! SETUP
 !=======
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
 ! SEED
 !======
 !
 if (present(SEED_k)) then
   !
   if (present(SEED_E).or.present(SEED_RT).or.present(SEED_R3)) then
     !
     do i_obj=1,max_interpls
       if (.not.INTERP_obj(i_obj)%in_use) then 
         ID=i_obj
         exit
       endif
     enddo
     !
     INTERP_obj(ID)%nb=bands
     !
     if (present(SEED_E).or.present(SEED_RT)) INTERP_obj(ID)%D=n_sp_pol
     if (present(SEED_R3))                    INTERP_obj(ID)%D=size(SEED_R3(1,1,:))
     !
     INTERP_obj(ID)%in_use=.TRUE.
     INTERP_obj(ID)%use_NN=.FALSE.
     if (present(NEAREST)) then
       INTERP_obj(ID)%use_NN=NEAREST 
     endif
     INTERP_obj(ID)%what=operation
     !
     if (present(ID_out)) then
       ID_out=ID
     endif
     !
   endif
   !
   ! I asssume here that SEED_E and SEED_RT lives on the same grid of SEED_k
   !
   if (present(SEED_E).or.present(SEED_RT)) then
     !
     YAMBO_ALLOC(INTERP_obj(ID)%SEED,(bands(1):bands(2),SEED_k%nibz,n_sp_pol))
     !
     if (present(SEED_E)) then
       !
       do i_s=1,n_sp_pol
         do ib=bands(1),bands(2)
          do ik=1,SEED_k%nibz
             if (l_f ) INTERP_obj(ID)%SEED(ib,ik,i_s)=SEED_E%f(ib,ik,i_s)
             if (l_E ) INTERP_obj(ID)%SEED(ib,ik,i_s)=SEED_E%E(ib,ik,i_s)
             if (l_dE) INTERP_obj(ID)%SEED(ib,ik,i_s)=SEED_E%E(ib,ik,i_s)-SEED_E%Eo(ib,ik,i_s)
           enddo
         enddo
       enddo
       !
     else if (present(SEED_RT)) then
       !
       do i_RT=1,SEED_RT%nstates
         ib   =SEED_RT%table(i_RT,1)
         ik   =SEED_RT%table(i_RT,3)
         i_s  =SEED_RT%table(i_RT,4)
         if (l_df) INTERP_obj(ID)%SEED(ib,ik,i_s)=SEED_RT%delta_f(i_RT) 
         if (l_G ) INTERP_obj(ID)%SEED(ib,ik,i_s)=SEED_RT%NEQ_Lifetime(i_RT) 
         !
       enddo
       !
     endif
     !
     call INTERPOLATION_coefficients(R2D=INTERP_obj(ID)%SEED,k=SEED_k,ID=INTERP_obj(ID)%ID,ID_obj=ID)
     !
     if (.not.INTERP_obj(ID)%use_NN) then
       !
       YAMBO_FREE(INTERP_obj(ID)%SEED)
       !
     endif
     !
   else if (present(SEED_R3)) then
     !
     if (.not.INTERP_obj(ID)%use_NN) then
       call INTERPOLATION_coefficients(R2D=SEED_R3,k=SEED_k,ID=INTERP_obj(ID)%ID,ID_obj=ID)
     endif
     !
   endif
   !
 endif
 !
 ! INTERPOLATION...
 !==================
 !
 if (present(OUT_k)) then
   !
   do i_obj=1,max_interpls
     if (operation==trim(INTERP_obj(i_obj)%what)) ID=i_obj
   enddo
   !
 endif
 !
 ! ... Using SEED and coefficients
 !
 if (present(OUT_k).and..not.INTERP_obj(ID)%use_NN) then
   !
   if (present(OUT_E)) then
     !
     if (l_E.or.l_dE) then
       YAMBO_ALLOC(OUT_E%E,(INTERP_obj(ID)%nb(1):INTERP_obj(ID)%nb(2),OUT_k%nibz,INTERP_obj(ID)%D))
       call INTERPOLATION_BZ(K=OUT_k,R2D=OUT_E%E,ID=INTERP_obj(ID)%ID)
     else if (l_f) then
       YAMBO_ALLOC(OUT_E%f,(INTERP_obj(ID)%nb(1):INTERP_obj(ID)%nb(2),OUT_k%nibz,INTERP_obj(ID)%D))
       call INTERPOLATION_BZ(K=OUT_k,R2D=OUT_E%f,ID=INTERP_obj(ID)%ID)
     endif
     !
     OUT_E%nk=OUT_k%nibz
     OUT_E%nb=INTERP_obj(ID)%nb(2)
     !
   else
     !
     n=INTERP_obj(ID)%nb
     !
     YAMBO_ALLOC(INTERP_obj(ID)%INTERP,(n(1):n(2),OUT_k%nibz,INTERP_obj(ID)%D))
     call INTERPOLATION_BZ(K=OUT_k,R2D=INTERP_obj(ID)%INTERP,ID=INTERP_obj(ID)%ID)
     !
   endif
   !
 endif
 !
 ! ... Using the Nearest point method (ONLY RT)
 !
 if (present(OUT_k).and.INTERP_obj(ID)%use_NN) then
   !
   n=INTERP_obj(ID)%nb
   !
   YAMBO_ALLOC(INTERP_obj(ID)%INTERP,(n(1):n(2),OUT_k%nibz,INTERP_obj(ID)%D))
   !
   call INTERPOLATION_nearest_k(K_in=SEED_RT%kpt,SHAPE_in=(/n(1),n(2)/),&
&                               K_out=OUT_k%pt,V_out=INTERP_obj(ID)%INTERP,SHAPE_out=(/n(1),n(2)/),&
&                               V_in=INTERP_obj(ID)%SEED,SAME_SUM=l_normalize_Nel)
   !
!    call Nearest_kpt_interpolation(SEED_RT%nk,OUT_k%nibz,n,&
!&                                  SEED_RT%nstates,1,1,SEED_RT%table,SEED_RT%kpt, &
!&                                  OUT_k%pt,SEED_RT%k_weight,OUT_k%weights,&
!&                                  SEED_RT%delta_f,INTERP_obj(ID)%INTERP,&
!&                                  "QUIET",l_normalize_Nel)
!   else if (l_G) then
!    call Nearest_kpt_interpolation(SEED_RT%nk,OUT_k%nibz,n,&
!&                                  SEED_RT%nstates,1,1,SEED_RT%table,SEED_RT%kpt, &
!&                                  OUT_k%pt,SEED_RT%k_weight,OUT_k%weights,&
!&                                  SEED_RT%NEQ_Lifetime,INTERP_obj(ID)%INTERP,&
!&                                  "QUIET",.FALSE.)
!  endif
   !
 endif
 !
 call IO_and_Messaging_switch("+log")
 !
end subroutine INTERPOLATION_driver
