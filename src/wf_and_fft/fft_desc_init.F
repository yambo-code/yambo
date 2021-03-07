!
!        Copyright (C) 2000-2020 the YAMBO team 
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
subroutine fft_desc_init(n,iGo_max,dfft)
 !
 ! init dffts as required by the QE FFT lib
 !
 use pars,       ONLY:DP,pi
 use parallel_m, ONLY:mpi_comm_world
 use fft_types,  ONLY:fft_dlay_descriptor,fft_dlay_deallocate
 use fft_scalar, ONLY:good_fft_dimension
 use stick_set,  ONLY:pstickset
 use D_lattice,  ONLY:alat
 use R_lattice,  ONLY:b,g_vec,E_of_shell,n_g_shells,k_pt,nkibz
 !
 implicit none
 integer,       intent(in) :: n(3)
 integer,       intent(in) :: iGo_max
 type(fft_dlay_descriptor) :: dfft
 !
 type(fft_dlay_descriptor) :: dfftp_dum
 integer  :: ik, ig, ngw, ngm, ngs 
 real(DP) :: tpiba, bg(3,3), conv(3)
 real(DP) :: gcutm, gkcut, g0cut
    

 !
 ! clean existing data
 call fft_dlay_deallocate( dfft )
 !
 ! re-build the descriptor
 dfft%nr1=n(1)
 dfft%nr2=n(2)
 dfft%nr3=n(3)
 !
 ! some FFT libraries are more efficient with 
 ! a slightly larger workspace
 dfft%nr1x  = good_fft_dimension( dfft%nr1 )
 dfft%nr2x  = dfft%nr2
 dfft%nr3x  = good_fft_dimension( dfft%nr3 )
 !
 dfftp_dum%nr1=dfft%nr1
 dfftp_dum%nr2=dfft%nr2
 dfftp_dum%nr3=dfft%nr3
 dfftp_dum%nr1x=dfft%nr1x
 dfftp_dum%nr2x=dfft%nr2x
 dfftp_dum%nr3x=dfft%nr3x
 !
 tpiba=2.0_DP*PI/alat(1)
 bg=transpose(b)/tpiba
 !
 ! conversion factor for g_vec and k_pt
 conv(:)=2.0_DP*PI/alat(:)/tpiba
 !
 ! consider iGo_max
 g0cut = 0.0d0
 do ig = 1, iGo_max
     g0cut=max (g0cut, sqrt ( sum( (g_vec(ig,1:3)*conv(1:3))**2 ) ) )
 enddo
 !
 gkcut=0.0d0
 do ik = 1, nkibz
     gkcut = max (gkcut, sqrt ( sum( (k_pt(ik,1:3)*conv(1:3))**2 ) ) )
 enddo
 !
 ! QE routines use Ry-au units (then converted to internal QE units
 ! by taking tpiba into account)
 !
 !gcutm=( MAXVAL(E_of_shell(1:n_g_shells)) +0.5_DP*E_of_shell(2) )/tpiba**2 
 gcutm=( MAXVAL(E_of_shell(1:n_g_shells)) )/tpiba**2 
 ! Ha -> Ry
 gcutm = 2.0_DP*gcutm
 gcutm = ( sqrt(gcutm) + g0cut )**2
 ! 
 gkcut = ( sqrt(gcutm/4.0_DP) + gkcut )**2
 !
 ! set up fft descriptors, including parallel stuff: sticks, planes, etc.
 !
#ifndef _USE_3D_FFT
 call error(" [FFT] QE FFT must be used with USE_3D_FFT")
#endif
 CALL pstickset( .FALSE., bg, gcutm, gkcut, gcutm, &
                 dfftp_dum, dfft, ngw, ngm, ngs, 0, 0, 1, mpi_comm_world, 1 )
 !
 return
end subroutine fft_desc_init

