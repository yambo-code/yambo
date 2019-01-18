!
!        Copyright (C) 2000-2019 the YAMBO team
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
module TDDFT
 !
 use pars,     ONLY:SP,lchlen,schlen
 implicit none
 !
 character(schlen):: TDDFT_mode
 character(lchlen):: FXC_description(100)
 character(10)    :: FXC_type
 integer          :: FXC_n_descs
 integer          :: FXC_n_g_corr
 integer          :: FXC_n_mem_freqs
 integer          :: FXC_SVD_digits
 real(SP)         :: FXC_per_memstps
 real(SP)         :: FXC_LRC_alpha
 real(SP)         :: FXC_LRC_beta
 character(schlen):: FXC_PF_alpha
! logical          :: FXC_is_causal
 logical          :: FXC_is_retarded
 !
 real(SP),    allocatable :: FXC_K_diagonal(:)
 complex(SP), allocatable :: FXC(:,:,:)
 !
 interface 
   !
   integer function io_BS_Fxc(iq,W,ID,X)
     use frequency,  ONLY:w_samp
     use X_m,        ONLY:X_t
     integer            ::ID,iq
     type(w_samp)       ::W
     type(X_t), optional::X
   end function
   !
 end interface
 !
end module
