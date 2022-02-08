!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AC
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
!> @brief Calculate X(w) including ph-assisted transitions
!!
subroutine EXCPH_X_phass(wv,Epsilon_ii,PL_ii)
 !
 use frequency,     ONLY:w_samp
 use pars,          ONLY:SP,cZERO
 use functions,     ONLY:Lorentzian_func
 use IO_m,          ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,REP,VERIFY
 use IO_int,        ONLY:io_control
 use EXCPH,         ONLY:EXCPH_states,EXCPH_sum,BS_Sat_E,BS_Sat_WEIGHT,EXCPH_Renorm 
 use ELPH,          ONLY:elph_branches
 !
#include<memory.h>
 !
 type(w_samp), intent(in)  :: wv
 complex(SP),  intent(out) :: Epsilon_ii(wv%n_freqs,1),PL_ii(wv%n_freqs,2)
 !
 ! Work space
 !
 complex(SP) :: pole,W,Satellites_alpha
 complex(SP), external :: Lorentzian_FT
 integer :: i_alpha,i_beta,il,iq,iw,io_err,ID_exc_gkkp
 integer, external :: io_EXCPH_gkkp
 !
 Epsilon_ii=cZERO
 PL_ii     =cZERO
 !
 !
 ! Load the Exciton-phonon elements
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp)
 if(io_err/=0) call error("Error loading the gkkp matrix elements!! ")
 !
 do iw=1,wv%n_freqs
   !
   W=wv%p(iw)
   !
   do i_alpha=EXCPH_states(1),EXCPH_states(2)
     !
     !pole=BS_E(i_alpha)
     !
     ! Main peak
     !
     Epsilon_ii(iw,1)=Epsilon_ii(iw,1) + (1._SP-EXCPH_Renorm(i_alpha))*Lorentzian_FT(W,pole,'r')
     !
     ! Satellites
     !
     Satellites_alpha=cZERO
     do i_beta=EXCPH_sum(1),EXCPH_sum(2)
       do il=elph_branches(1),elph_branches(2)
!         pole=BS_Sat_E(il,i_beta,i_alpha)
         Satellites_alpha=Satellites_alpha+BS_Sat_WEIGHT(il,i_beta,i_alpha)*Lorentzian_FT(W,pole,'r')    
       enddo
     enddo
     !
     Epsilon_ii(iw,1)=Epsilon_ii(iw,1)+EXCPH_Renorm(i_alpha)*Satellites_alpha
     !
   enddo   
   !
 enddo   
 !
end subroutine EXCPH_X_phass
