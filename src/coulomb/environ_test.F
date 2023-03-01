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
 use pars,           ONLY:SP,DP,pi,cONE,cZERO,CI
 use com,            ONLY:msg
 use vec_operate,    ONLY:iku_v_norm,c2a
 use matrix,         ONLY:PAR_matrix,MATRIX_reset
 use wrapper,        ONLY:M_by_V
 use linear_algebra, ONLY:INV
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver,MATRIX_duplicate
 use environ_m,      ONLY:l_environ, eps0, A0, eps_env_RL
 use R_lattice,      ONLY:b,G_m_G,g_vec,ng_vec,q_norm,minus_G
 use D_lattice,      ONLY:DL_vol
 use FFT_m,          ONLY:fft_size,fft_dim,fftw_plan,fft_g_table
 !
#include<memory.h>
 !
 real(SP),         intent(in) :: q(3)
 type(PAR_matrix), intent(in) :: v_environ
 character(len=*), intent(in) :: label
 !
 ! Work Space
 !
 type(PAR_matrix) :: v_environ_react
 integer     :: ig1, ig2, ifft,ng_loc,iun
 real(SP)    :: q_loc(3), bare_qpg_v1(3), bare_qpg_v2(3)
 logical     :: l_this_is_q0
 real(SP)    :: arg,m_dip,z0,scal,scal2,gaussian,sigma
 complex(SP) :: increment
 real(SP),    allocatable :: g_vec_loc(:,:)
 complex(SP), allocatable :: rho_test(:), v_react(:), v_total(:), partial_sum(:)
 complex(DP), allocatable :: v_react_DP(:), v_total_DP(:), rho_DP(:), eps_env_DP(:)
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
 ! dump to disk epsilon in reciprocal space
 !
 filename="espilon_Gspace_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# ng       ',3i8)") ng_loc
 write(iun,"('# Gx, Gy, Gz, |G|, rho')")
 do ig1 = 1, ng_loc
   scal=sqrt(dot_product(g_vec_loc(:,ig1),g_vec_loc(:,ig1)))
   write(iun,"(3f25.15, f25.9, f25.9, 2f25.15)") g_vec_loc(:,ig1), scal, eps_env_RL(ig1)
 enddo
 close(iun)

 YAMBO_ALLOC(eps_env_DP,(fft_size))
 !
 eps_env_DP=0.0_DP
 !
 do ig1 = 1, ng_loc
   ifft = fft_g_table(ig1,1)
   eps_env_DP(ifft) = eps_env_RL(ig1)
 enddo
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan=0
 call fft_3d(eps_env_DP,fft_dim,+1,fftw_plan)
#else
 call fft_3d(eps_env_DP,fft_dim,+1)
#endif
 !
 ! dump to disk the epsilon in real space
 !
 filename="epsilon_Rspace_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# fft_dim  ',3i8)") fft_dim
 write(iun,"('# fft_size ', i8)") fft_size
 write(iun,"('# ng       ',3i8)") ng_loc
 write(iun,"(2f25.9)") eps_env_DP(:)
 close(iun)
 !
 YAMBO_ALLOC(v_react,(ng_loc))
 YAMBO_ALLOC(v_total,(ng_loc))
 YAMBO_ALLOC(rho_test,(ng_loc))
 YAMBO_ALLOC(partial_sum,(ng_loc))
 rho_test=0.0_SP
 !
 ! initialize test density: here we use a dipolar charge distribution
 ! where the single charges are represented as gaussians
 ! rho_G = i |m|/( sqrt(V)*z0) * sin( z0*G_z)*exp(-(sigma*|G|)^2/2)
 !
 if ( trim(label)=="onsager" ) then
    !
    z0=1.0
    m_dip = 1.0 
    sigma = 1.0 
    !
    do ig1=1,ng_loc
       arg = z0*g_vec_loc(3,ig1)      
       gaussian = dot_product(g_vec_loc(:,ig1),g_vec_loc(:,ig1))*sigma**2/2.0
       rho_test(ig1) = CI * m_dip/(sqrt(DL_vol)*z0) * sin(arg) * exp(-gaussian)
    enddo
    !
 else if ( trim(label)=="onsager_elliptic" ) then
    call error(' [ENVIRON] onsager_elliptic not implemented')
 else
    call error(' [ENVIRON] invalid label: '//trim(label))
 endif
 !
 ! dump to disk the density in reciprocal space
 !
 filename="rho_Gspace_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# ng       ',3i8)") ng_loc
 write(iun,"('# Gx, Gy, Gz, |G|, z0*Gz, rho')")
 do ig1 = 1, ng_loc
   scal=sqrt(dot_product(g_vec_loc(:,ig1),g_vec_loc(:,ig1)))
   scal2=z0*g_vec_loc(3,ig1)
   write(iun,"(3f25.15, f25.9, f25.9, 2f25.15)") g_vec_loc(:,ig1), scal, scal2, rho_test(ig1)
 enddo
 close(iun)

 YAMBO_ALLOC(rho_DP,(fft_size))
 !
 rho_DP=0.0_DP
 !
 do ig1 = 1, ng_loc
   ifft = fft_g_table(ig1,1)
   rho_DP(ifft) = rho_test(ig1)
 enddo
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan=0
 call fft_3d(rho_DP,fft_dim,+1,fftw_plan)
#else
 call fft_3d(rho_DP,fft_dim,+1)
#endif
 !
 rho_DP=rho_DP*DL_vol/real(fft_size,DP)
 !
 ! dump to disk the density in real space
 !
 filename="rho_Rspace_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# fft_dim  ',3i8)") fft_dim
 write(iun,"('# fft_size ', i8)") fft_size
 write(iun,"('# ng       ',3i8)") ng_loc
 write(iun,"(2f25.9)") rho_DP(:)
 close(iun)
 !
 partial_sum=0.0_SP
 increment=0.0_SP
 !
 do ig1=1,ng_loc
    increment=increment+v_environ%blc(1,ig1,1)*rho_test(ig1)
    partial_sum(ig1)=increment
 enddo
 !
 ! dump to disk the partial sums sum_G(V_0G*rho_G)
 ! in order to check if V_0 = sum_G(V_0G*rho_G) is correctly calculated.
 ! We could cancel it, but it turns out to be quite useful for debugging.
 !
 filename="V_0G_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# ng       ',3i8)") ng_loc
 write(iun,"('# |G|, Gx, Gy, Gz, V_0G, Im(rho_G), Im(partial_sum)')")
 do ig1 = 1, ng_loc
   scal=sqrt(dot_product(g_vec_loc(:,ig1),g_vec_loc(:,ig1)))
   write(iun,"(f25.9, 3f25.9, f25.9, f25.9, f15.9)") scal, g_vec_loc(:,ig1), real(v_environ%blc(1,ig1,1)),  &
                           &  aimag(rho_test(ig1)), aimag(partial_sum(ig1))
 enddo
 close(iun)
 !
 ! compute the total potential
 !
 if ( v_environ%cols(1) > 1 )      call error(' [ENVIRON] invalid v_env%cols(1)')
 if ( v_environ%rows(1) > 1 )      call error(' [ENVIRON] invalid v_env%rows(1)')
 if ( v_environ%cols(2) < ng_loc ) call error(' [ENVIRON] invalid v_env%cols(2)')
 if ( v_environ%rows(2) < ng_loc ) call error(' [ENVIRON] invalid v_env%rows(2)')
 !
 call M_by_V('N',ng_loc,v_environ%blc(:,:,1),rho_test,v_total)
 !
 !
 ! dump to disk the total potential in reciprocal space
 !
 filename="v_total_Gspace_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# ng       ',3i8)") ng_loc
 write(iun,"('# |G|, rho_G, V_G')")
 do ig1 = 1, ng_loc
   bare_qpg_v1(:)=q_loc(:)+g_vec_loc(:,ig1)
   scal=sqrt(dot_product(bare_qpg_v1,bare_qpg_v1))
   write(iun,"(f25.9, 2f25.9, 2f25.9)") scal, rho_test(ig1), v_total(ig1)
 enddo
 close(iun)
 !
 ! perform the FFT
 !
 YAMBO_ALLOC(v_total_DP,(fft_size))
 !
 v_total_DP=0.0_DP
 !
 do ig1 = 1, ng_loc
   ifft = fft_g_table(ig1,1)
   v_total_DP(ifft) = v_total(ig1)
 enddo
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan=0
 call fft_3d(v_total_DP,fft_dim,+1,fftw_plan)
#else
 call fft_3d(v_total_DP,fft_dim,+1)
#endif
 !
 v_total_DP=v_total_DP*DL_vol/real(fft_size,DP)
 !
 ! dump to disk the total potential in real space
 !
 filename="v_total_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# fft_dim  ',3i8)") fft_dim
 write(iun,"('# fft_size ', i8)") fft_size
 write(iun,"('# ng       ',3i8)") ng_loc
 write(iun,"(2f25.9)") v_total_DP(:)
 close(iun)
 !
 ! cleanup
 !
 YAMBO_FREE(v_total)
 YAMBO_FREE(v_total_DP)
 YAMBO_FREE(partial_sum)
 !
 call MATRIX_duplicate("+1",M_in=v_environ,M_out=v_environ_react)
 !
 ! compute the kernel of the reaction potential V^reac_G = V - 4pi/G^2
 !
 !$omp parallel do default(shared), &
 !$omp &           private(ig1,ig2,bare_qpg_v1,bare_qpg_v2,scal)
 do ig2=v_environ_react%cols(1),v_environ_react%cols(2)
   !
   bare_qpg_v2(:)=g_vec_loc(:,ig2)
   !
   do ig1=v_environ_react%rows(1),v_environ_react%rows(2)
     !
     bare_qpg_v1(:)=g_vec_loc(:,ig1)
     !
     if (ig1==ig2) then
       !
       if (ig1==1 .and. ig2==1) then
         v_environ_react%blc(ig1,ig2,1)=v_environ_react%blc(ig1,ig2,1) -4.0_SP*pi/q_norm(1)**2
       else
         !
         scal=dot_product(bare_qpg_v1,bare_qpg_v2)
         v_environ_react%blc(ig1,ig2,1)=v_environ_react%blc(ig1,ig2,1) - 4.0_SP*pi/scal
         !
       endif
     endif
     !
   enddo
   !
 enddo
 !$omp end parallel do

 !
 ! construct the react potential in reciprocal space
 ! AF: here for a single freq, to be generalized, if needed
 !
 call M_by_V('N',ng_loc,v_environ_react%blc(:,:,1),rho_test,v_react)
 !
 ! dump to disk the react potential in reciprocal space
 !
 filename="v_react_Gspace_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# ng       ',3i8)") ng_loc
 do ig1 = 1, ng_loc
   bare_qpg_v1(:)=q_loc(:)+g_vec_loc(:,ig1)
   scal=sqrt(dot_product(bare_qpg_v1,bare_qpg_v1))
   write(iun,"(f25.9, 2f25.9)") scal, v_react(ig1)
 enddo
 close(iun)
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
 ! dump to disk the react potential in real space
 !
 filename="v_react_"//trim(label)//".dat"
 iun=12345
 open(iun,file=filename)
 write(iun,"('# fft_dim  ',3i8)") fft_dim
 write(iun,"('# fft_size ', i8)") fft_size
 write(iun,"('# ng       ',3i8)") ng_loc
 write(iun,"(2f25.9)") v_react_DP(:)
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

