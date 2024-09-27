!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
module TDDFT
 !
 use descriptors,ONLY:IO_desc
 use pars,       ONLY:SP,DP,lchlen,schlen
 !
 implicit none
 !
 character(schlen):: TDDFT_mode
 type(IO_desc)    :: FXC_desc
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
 logical          :: l_Fxc_from_Vxc
 !
 real(SP),    allocatable :: FXC_K_diagonal(:)
 complex(SP), allocatable :: F_xc_gspace(:,:,:)
 !
 type tddft_wf_t
   complex(DP), allocatable :: rhotwr_DP(:)
   complex(SP), allocatable :: rhotwr1(:)
   complex(SP), allocatable :: rhotwr2(:)
   complex(SP), allocatable :: WF_symm1(:,:)
   complex(SP), allocatable :: WF_symm2(:,:)
 end type tddft_wf_t
 !
 character(12)     :: FXC_mode
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
