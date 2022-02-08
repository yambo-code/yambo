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
subroutine EXCPH_X_phass(Epsilon_ii,PL_ii,wv)
  !
  use frequency,     ONLY:w_samp
  use pars,          ONLY:SP,cZERO
  use functions,     ONLY:Lorentzian_func
  !
#include<memory.h>
  !
  complex(SP),  intent(out) :: Epsilon_ii(wv%n_freq,1),PL_ii(wv%n_freq,2)
  type(bz_samp),intent(in)  :: wv
  !
  ! Work space
  !
  complex(SP) :: pole,W
  complex(SP), external :: Lorentzian_FT
  !
  Epsilon_ii=cZERO
  PL_ii     =cZERO
  !
  do iw=1,Xw%n_freqs
    !
    W=wv%p(iw)
    pole=0.0
    !
    Epsilon_ii(iw)=Epsilon_ii(iw) + Lorentzian_FT(W,pole,'r')
    PL_ii(iw,1)   =
    PL_ii(iw,2)   =
  enddo   
  !
end subroutine EXCPH_X_phass
