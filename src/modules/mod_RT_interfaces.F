!
!        Copyright (C) 2000-2020 the YAMBO team
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
module RT_interfaces
 !
 implicit none
 !
 interface
   !
   subroutine RT_average_operator(N_V,O_VALUE,V_VALUE,O_REAL,O_QP,O_MATRIX,&
&                                 V_MATRIX,PV_MATRIX,G,k,en,TRANS,Double_Grid,nK_mem)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp,nXkibz
     use QP_m,       ONLY:QP_table,QP_n_states,QP_nk
     use electrons,  ONLY:n_spin,n_sp_pol,levels
     use FFT_m,      ONLY:fft_size
     use real_time,  ONLY:RT_bands
     type(bz_samp), intent(in) :: k
     complex(SP),   intent(in) :: G(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),QP_nk)
     type(levels),  intent(in) :: en
     integer,       intent(in) :: nK_mem,N_V
     complex(SP),     optional,intent(out) :: O_VALUE,V_VALUE(N_V)
     complex(SP),     optional, intent(in) ::  V_MATRIX(N_V,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
     complex(SP),     optional, intent(in) :: PV_MATRIX(N_V,RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
     complex(SP),     optional, intent(in) ::  O_MATRIX(    RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nK_mem,n_sp_pol)
     complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
     real(SP),        optional, intent(in) :: O_REAL(fft_size,n_spin)
     logical,         optional, intent(in) :: TRANS
     character(6),    optional, intent(in) :: Double_Grid
   end subroutine RT_average_operator
   !
   subroutine RT_update(G_input,H_rotation,it,E,k,X,up_Gref,up_DIP,an_QP)
     use pars,       ONLY:SP
     use R_lattice,  ONLY:bz_samp
     use QP_m,       ONLY:QP_nk
     use electrons,  ONLY:levels,n_sp_pol
     use X_m,        ONLY:X_t
     use real_time,  ONLY:RT_bands
     type(X_t),      intent(inout)        :: X
     complex(SP),    intent(inout)        :: G_input(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),QP_nk)
     complex(SP),    intent(in)           :: H_rotation(RT_bands(2),RT_bands(2),QP_nk,n_sp_pol)
     type(bz_samp),  intent(in)           :: k
     type(levels),   intent(in)           :: E
     integer,        intent(in)           :: it
     logical,        optional, intent(in) :: up_Gref,up_DIP,an_QP
   end subroutine RT_update
   !
 end interface
 !
end module
