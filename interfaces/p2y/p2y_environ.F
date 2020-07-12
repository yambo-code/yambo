!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF,PDA
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
subroutine p2y_environ(filename)
 !
 use pars,           ONLY:SP,DP,schlen
 use vec_operate,    ONLY:c2a
 use IO_m,           ONLY:OP_WR_CL,OP_APP_CL,REP,io_control
 use pw_data,        ONLY:nr1_,nr2_,nr3_,a1_,a2_,a3_
 use R_lattice,      ONLY:b,g_vec,ng_vec
 use FFT_m,          ONLY:fft_size,fft_dim,fft_g_table,modx
#if defined _FFTW
 use FFT_m,          ONLY:fftw_plan
#endif
 use environ_m,      ONLY:nr1,nr2,nr3,ng_env,eps_env_g,environ_alloc,environ_free
 use cube_m,         ONLY:cube_read
 !
#include<memory.h>
 !
 character(schlen), intent(in) :: filename
 !
 logical, external :: file_exists

 !
 ! Work space
 !
 real(SP) :: al1(3),al2(3),al3(3),r0(3),v1(3)
 !
 ! I/O
 !
 integer           :: io_err,ID,ig,ir,i1,iv(3)
 integer, external :: io_ENVIRON_pwscf
 !
 ! Allocs
 !
 real(SP),    allocatable :: eps(:)
 complex(DP), allocatable :: eps_fft(:)
#if defined _FFTQE
 integer, external :: fft_setmap
#endif

 !
 ! checks and init
 !
 if (.not.file_exists(filename)) return

 !
 ! (a) get dimensions
 !
 call cube_read(filename,NR1=nr1,NR2=nr2,NR3=nr3,AL1=al1,AL2=al2,AL3=al3,IERR=io_err)
 if (io_err/=0) call error(' [ENVIRON] reading dims from '//trim(filename))
 !
 if (nr1/=nr1_.or.nr2/=nr2_.or.nr3/=nr3_) call error(' [ENVIRON] inconsistent fft grid 1')
 if (any(abs(nr1*al1-a1_)>1.0d-6)) call error(' [ENVIRON] inconsistent a1')
 if (any(abs(nr2*al2-a2_)>1.0d-6)) call error(' [ENVIRON] inconsistent a2')
 if (any(abs(nr3*al3-a3_)>1.0d-6)) call error(' [ENVIRON] inconsistent a3')
 !
 fft_size=nr1*nr2*nr3
 ng_env=fft_size
 fft_dim=(/nr1,nr2,nr3/)
 !
! call fft_setup(ng_vec,1,.false.)
! if (any(fft_dim/=(/nr1,nr2,nr3/))) call error(' [ENVIRON] inconsistent fft grid 2')
! if (fft_size /= nr1*nr2*nr3)       call error(' [ENVIRON] inconsistent fft grid 3')
 !
 ! regenerate fft_g_table to be consistent with Env fft
 ! the following lines are taken from fft_setup and should better be 
 ! made a separate subroutine to be simply re-used instead of replicated
 !
 YAMBO_FREE(fft_g_table)
 YAMBO_ALLOC(fft_g_table,(ng_vec,1))
 !
 do i1=1,ng_vec
   call c2a(b,g_vec(i1,:),v1,'ki2a')
   iv=nint(v1)
#if defined _FFTQE
   call error(' [ENVIRON] dffts not init')
   fft_g_table(i1,1)=fft_setmap(iv,dffts)
#else
   fft_g_table(i1,1)=1+modx(iv(1),fft_dim(1))+&
&                      modx(iv(2),fft_dim(2))*fft_dim(1)+&
&                      modx(iv(3),fft_dim(3))*fft_dim(1)*fft_dim(2)
#endif
 enddo

 !
 ! (b) get PCM epsilon(r)
 !
 YAMBO_ALLOC(eps,(fft_size))
 YAMBO_ALLOC(eps_fft,(fft_size))
 call environ_alloc(ng_vec)
 !
 call cube_read(filename,dat=eps,orig=r0,IERR=io_err)
 if (io_err/=0) call error(' [ENVIRON] reading data from '//trim(filename))
 !
 if (any(r0/=0.0)) call error(' [ENVIRON] invalid r0 /= 0')

 !
 ! (c) compute eps in reciprocal space
 !
 do ir = 1, fft_size
   eps_fft(ir)=cmplx(eps(ir),0.0_SP,kind=DP)
 enddo
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
 call fft_3d(eps_fft,fft_dim,-1,fftw_plan)
#else
 call fft_3d(eps_fft,fft_dim,-1)
#endif
 !
 eps_fft=eps_fft/real(fft_size,DP)
 !
 do ig = 1, ng_vec
   eps_env_g(ig)= cmplx(eps_fft(fft_g_table(ig,1)),kind=SP)
 enddo
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_ENVIRON_pwscf(ID)
 !
 YAMBO_FREE(eps)
 YAMBO_FREE(eps_fft)
 !
 call environ_free()
 !
end subroutine p2y_environ

