!
! Copyright (C) 2000-2005 C. Hogan and the SELF team
!         http://www.fisica.uniroma2.it/~self
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
 subroutine loc_spp
   use SPP
   use com,                     ONLY : error
   use pars,                    ONLY : SP, pi
   use surface_geometry,        ONLY : setup_gvecaff, gvecaff
   use wave_func,               ONLY : wf, wf_ng, wf_state, wf_load, wf_free, wf_norm_test
   use D_lattice,               ONLY : alat, DL_vol
   use R_lattice,               ONLY : g_vec, nkibz
   use IO_m,                    only : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP, DUMP, NONE
   use electrons,               ONLY : n_bands,n_spin,n_spinor,n_sp_pol
   implicit none
   ! 
   ! Work Space
   !
!  type(bz_samp),  intent(in) :: Xk
   real(SP)                 :: loc(n_bands,nkibz)
   real(SP)                 :: kg(wf_ng)
   real(SP)                 :: tpiba
   integer                  :: ib, ik, ig, ix,iy,iz, ID
   integer                  :: ibfft, io_err, ig1, ig2, ngz
   integer, external        :: io_loc
   complex(SP)              :: rhoint, int, gz
   complex(SP), parameter   :: ci=cmplx(0.0_SP,1.0_SP)
   !
   ! Define ix,iy, iz = normal
   ! 
   iz = normdir
   select case (normdir)
   case(1)
     ix = 2; iy = 3
   case(2)
     ix = 3; iy = 1
   case(3)
     ix = 1; iy = 2
   end select
 
   call setup_gvecaff

   wf_norm_test = .true.
#if !defined PJ_BIGSYS
   call wf_load(ngloc,1,(/1,n_bands/),(/1,nkibz/),space='G',&
&               title='-Localization/G space')
#endif

   do ik=1,nkibz
#if defined PJ_BIGSYS
     call wf_load(ngloc,1,(/1,n_bands/),(/ik,ik/),space='G',&
&                 title='-Localization/G space')
#endif
     do ib=1,n_bands
       ibfft=wf_state(ib,ik,1)
       rhoint = 0.0_SP
       do ig1=1,ngloc
         do ig2=1,ngloc
           if(gvecaff(ig1,ix).ne.gvecaff(ig2,ix).or. &
&             gvecaff(ig1,iy).ne.gvecaff(ig2,iy)) cycle

           ngz = gvecaff(ig2,iz) - gvecaff(ig1,iz)
           if(ngz.eq.0) then
             rhoint = rhoint + real(upperlim-lowerlim) * conjg(wf(ig2,ibfft))*wf(ig1,ibfft)
           else
             gz = real(ngz)*2.0_SP*pi
             int = -ci/gz*( exp(ci*gz*upperlim) - exp(ci*gz*lowerlim) )
             rhoint = rhoint + int * conjg(wf(ig2,ibfft))*wf(ig1,ibfft)
           endif

         enddo
       enddo

       loc(ib,ik) = real(rhoint)
!<--- DEBUG
!      write(87,'(i4,i4,2f15.9)') ik,ib,real(rhoint),aimag(rhoint)
!---> DEBUG
     enddo
   enddo

   call section('=','Create localization database.')
   call io_control(ACTION=OP_WR_CL, COM=NONE, SEC=(/1/), ID=ID) 
   io_err = io_loc(loc,upperlim, lowerlim, ID)
   if(io_err/=0) call error('Failed to write localization DB.')

   return
 end subroutine loc_spp
