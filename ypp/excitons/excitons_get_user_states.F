!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV DS
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
subroutine excitons_get_user_states(BS_E_degs,Res,n_eig,recompute)
 !
 use pars,          ONLY:SP,schlen
 use com,           ONLY:msg
 use stderr,        ONLY:STRING_split,intc
 use YPPm,          ONLY:EXCITONS_user_indexes,EXCITONS_n_user_states,EXCITON_E_treshold,&
                         EXCITON_Res_treshold,MAGNON_Res_threshold,state_ctl,BS_E,exc_kind
 !
 implicit none
 !
 integer, intent(in)    :: n_eig
 integer, intent(inout) :: BS_E_degs(n_eig)
 real(SP),intent(in)    :: Res(n_eig)
 logical, intent(in)    :: recompute
 !
 integer          ::i_start,i_end,i_str,i_st,TMP_index(n_eig),n_user
 character(schlen)::str_piece(50)
 !
 ! Sort energies to find degenerate states
 !========================================
 if(recompute) call excitons_find_degeneracies(BS_E,BS_E_degs,n_eig)
 !
 call STRING_split(state_ctl,str_piece)
 !
 EXCITONS_n_user_states=0
 n_user    =0
 TMP_index =0
 !
 ! 1. Using the "States" variable
 !--------------------------------
 i_str     =1
 do while (i_str<50)
   !
   if (len_trim(str_piece(i_str))==0) exit
   !
   if (trim(str_piece(i_str+1))=="-") then
     read(str_piece(i_str  ),*) i_start
     read(str_piece(i_str+2),*) i_end
     i_str=i_str+3
   else
     read(str_piece(i_str),*) i_start
     i_end=i_start
     i_str=i_str+1
   endif
   !
   do i_st=i_start,i_end
     !
     n_user=n_user+1
     !
     call ADD_me_to_the_list(i_st,TMP_index)
     !
   enddo
 enddo
 !
 if (EXCITONS_n_user_states>n_user) &
 & call warning(" "//trim(intc(EXCITONS_n_user_states-n_user))//&
 & ""//exc_kind//"ic states added to the USER selection to preserve degeneracy")
 !
 ! 2. Using the "En_treshold" variable
 !------------------------------------
 if (EXCITON_E_treshold>0.) then
   do i_st=1,n_eig
     if (real(BS_E(i_st))<EXCITON_E_treshold) call ADD_me_to_the_list(i_st,TMP_index)
   enddo
 endif
 !
 ! 3. Using the "Res_treshold" variable
 !--------------------------------------
 if (EXCITON_Res_treshold>0.) then
   do i_st=1,n_eig
     if (real(Res(i_st))>EXCITON_Res_treshold*maxval(Res)) call ADD_me_to_the_list(i_st,TMP_index)
   enddo
 endif
 !
 if (MAGNON_Res_threshold>0.) then
   do i_st=1,n_eig
     if (real(Res(i_st))>MAGNON_Res_threshold*maxval(Res)) call ADD_me_to_the_list(i_st,TMP_index)
   enddo
 endif
 !
 if (EXCITONS_n_user_states==0) return
 call msg("s"," "//trim(intc(EXCITONS_n_user_states))//" "//exc_kind//"ic states selected")
 !
 if(allocated(EXCITONS_user_indexes)) deallocate(EXCITONS_user_indexes)
 allocate(EXCITONS_user_indexes(EXCITONS_n_user_states))
 EXCITONS_user_indexes=TMP_index(:EXCITONS_n_user_states)
 !
 contains
   !
   subroutine ADD_me_to_the_list(i_state,TMP_index)
     !
     use YPPm,   ONLY:EXCITONS_n_user_states
     !
     integer i_st_p,i_state,TMP_index(n_eig)
     !
     do i_st_p=1,n_eig
       if (BS_E_degs(i_st_p)==BS_E_degs(i_state)) then
         if (.not.any(TMP_index==i_st_p)) then
           EXCITONS_n_user_states=EXCITONS_n_user_states+1
           TMP_index(EXCITONS_n_user_states)=i_st_p
         endif
       endif
     enddo
   end subroutine ADD_me_to_the_list
   !
end subroutine
