!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
!
subroutine XC_nlcc_setup()
 !
 ! reads the non-linear core-correction (NLCC) data
 ! in reciprocal space from DB (if present), and sets
 ! all the quantities needed to evaluate E_xc and V_xc 
 !
 use pars,          ONLY:DP
 use D_lattice,     ONLY:DL_vol
 use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table
 use com,           ONLY:msg
#if defined _FFTW
 use FFT_m,         ONLY:fftw_plan
#endif
 use pseudo,        ONLY:pp_rho_nlcc,pp_rhog_nlcc,&
&                        use_nlcc,pp_has_nlcc,pp_has_nlcc_atom
 use parser_m,      ONLY:parser
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_RD_CL,REP
#include <memory.h>
 !
 ! Work Space 
 !
 integer :: ng_,io_err,ID
 !
 integer, external :: io_NLCC_pwscf

 !
 ! perform main data IO
 !
 call parser("UseNLCC", use_nlcc)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_NLCC_pwscf(ID)
 !
 pp_has_nlcc=(io_err==0)
 if (.not.use_nlcc) pp_has_nlcc=.false.
 !
 if (.not.pp_has_nlcc) return
 !
 call msg('s','[xc] Including NLCC, core corrections density loaded from pseudo')
 !
 if (all(pp_has_nlcc_atom(:)==0)) call error(' [xc] Unexpected zero NLCC')
 !
 ! init core charge in direct space
 !
 if (.not.allocated(pp_rho_nlcc)) then
   YAMBO_ALLOC(pp_rho_nlcc,(fft_size))
 endif
 !
 pp_rho_nlcc=0.0
 !
 ng_ = min(size(pp_rhog_nlcc),size(fft_g_table,1))
 pp_rho_nlcc(fft_g_table(1:ng_,1))=pp_rhog_nlcc(1:ng_)
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
 call fft_3d(pp_rho_nlcc,fft_dim,1,fftw_plan)
#else
 call fft_3d(pp_rho_nlcc,fft_dim,1)
#endif  
 !
 pp_rho_nlcc=pp_rho_nlcc*DL_vol/real(fft_size,DP)
 !
end subroutine XC_nlcc_setup
