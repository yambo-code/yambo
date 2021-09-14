!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, PDA
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
subroutine environ_test(q,v_environ,label)
 !
 ! Given the initialized v_environ(q+G,q+G',w), and a test label ("onsager" | ...)
 ! 
 ! * a test charge density \rho_G' corresponding to label is initialized, 
 ! * the reaction potential v_G is computed
 !     v_G(w) = \sum_G' v_environ(q+G,q+G',w) \rho_G' 
 ! * the FFT of v_G is computed
 ! * the potential is plotted in real space and dumped in CUBE fmt
 ! 
 use pars,          ONLY:SP,DP,pi,cONE,cZERO,CI
 use com,           ONLY:msg
 use vec_operate,   ONLY:iku_v_norm,c2a
 use matrix,        ONLY:PAR_matrix,MATRIX_reset
 use wrapper,       ONLY:M_by_V
 use parallel_m,    ONLY:PP_redux_wait
 use linear_algebra,ONLY:INV
 use interfaces,    ONLY:LINEAR_ALGEBRA_driver,MATRIX_duplicate
 use environ_m,     ONLY:l_environ,eps_env_g, eps0, A0
 use R_lattice,     ONLY:b,G_m_G,g_vec,ng_vec,q_norm,minus_G
 use D_lattice,     ONLY:DL_vol
 use FFT_m,         ONLY:fft_size,fft_dim,fftw_plan,fft_g_table
#include<memory.h>
 !
 real(SP),         intent(in) :: q(3)
 type(PAR_matrix), intent(in) :: v_environ
 character(len=*), intent(in) :: label
 !
 ! Work Space
 !
 integer     :: ig1,ifft,ng_loc,iun
 real(SP)    :: q_loc(3)
 logical     :: l_this_is_q0
 real(SP)    :: arg,m_dip,z0
 real(SP),    allocatable :: g_vec_loc(:,:)
 complex(SP), allocatable :: rho_test(:), v_react(:)
 complex(DP), allocatable :: v_react_DP(:)
 character(256) :: filename

 if (.not.l_environ) return
 if (trim(v_environ%kind)=="NAN".or..not.allocated(v_environ%blc)) &
&   call error(' [ENVIRON] v_environ not initialized')
 !
 if (v_environ%Nb/=1) call error(' [ENVIRON] Nb > 1 not implemented')
 !
 call section('=','Testing v_environ')
 !
 ! handle units
 ! AF: to be checked whether we really need this
 !
 ng_loc=ng_vec
 ng_loc=min(ng_loc,v_environ%cols(2))
 ng_loc=min(ng_loc,v_environ%rows(2))
 !
 YAMBO_ALLOC(g_vec_loc,(3,ng_loc))
 do ig1=1,ng_loc
   call c2a(b,g_vec(ig1,:),g_vec_loc(:,ig1),'ki2c')
 enddo
 !
 ! here we deal with the q-> term numerically
 call c2a(b,q,q_loc,'ki2c')
 !
 ! AF: probably not needed if q(3) passed in input is already
 ! taking care of properly defining q0.
 !
 if (iku_v_norm(q)<1.0d-6) then
   q_loc(:)=q_norm(1)*(/1.0_SP,0.0_SP,0.0_SP/)
 endif
 !
 YAMBO_ALLOC(v_react,(ng_loc))
 YAMBO_ALLOC(rho_test,(ng_loc))

 !
 ! initialize test density
 !
 if ( trim(label)=="onsager" ) then
    !
    ! rho_G = i |m|/( sqrt(V)*z0) * sin( z0*G_z)
    !
    z0=1.0
    m_dip = 1.0
    !
    do ig1=1,ng_loc
       arg = z0*(q_loc(3)+g_vec_loc(3,ig1))
       rho_test(ig1) = CI * m_dip/(sqrt(DL_vol)*z0) * sin(arg)
    enddo
    !
 else if ( trim(label)=="onsager_elliptic" ) then
    call error(' [ENVIRON] onsager_elliptic not implemented')
 else
    call error(' [ENVIRON] invalid label: '//trim(label))
 endif

 !
 ! compute the potential
 !
 if ( v_environ%cols(1) > 1 )      call error(' [ENVIRON] invalid v_env%cols(1)')
 if ( v_environ%rows(1) > 1 )      call error(' [ENVIRON] invalid v_env%rows(1)')
 if ( v_environ%cols(2) < ng_loc ) call error(' [ENVIRON] invalid v_env%cols(2)')
 if ( v_environ%rows(2) < ng_loc ) call error(' [ENVIRON] invalid v_env%rows(2)')
 !
 ! AF: here for a single freq, to be generalized, if needed
! call M_by_V('N',ng_loc,ng_loc,cONE,v_environ%blc(:,:,1),size(v_environ%blc,1), &
!              rho_test,1,cZERO,v_react,1)
 call M_by_V('N',ng_loc,v_environ%blc(:,:,1),rho_test,v_react)
 
 !
 ! perform the FFT
 !
 YAMBO_ALLOC(v_react_DP,(fft_size))
 !
 v_react_DP=0.0_DP
 !
 do ig1 = 1, ng_loc
   ifft = fft_g_table(ig1,1)
   v_react_DP(ifft) = v_react(ig1) 
 enddo
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan=0
 call fft_3d(v_react_DP,fft_dim,+1,fftw_plan) 
#else
 call fft_3d(v_react_DP,fft_dim,+1)
#endif
 !
 v_react_DP=v_react_DP*DL_vol/real(fft_size,DP)

 !
 ! dump to disk
 !
 filename="v_react_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# fft_dim  ',3i8)") fft_dim
 write(iun,"('# fft_size ', i8)") fft_size
 write(iun,"('# ng       ',3i8)") ng_loc
 write(iun,"(2f15.9)") v_react_DP(:)
 close(iun)
 

 !
 ! cleanup
 !
 YAMBO_FREE(v_react)
 YAMBO_FREE(v_react_DP)
 YAMBO_FREE(rho_test)
 YAMBO_FREE(g_vec_loc)
 !
end subroutine environ_test


