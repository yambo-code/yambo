!
! Copyright (C) 2000-2013 C. Hogan and the YAMBO team
!              http://www.yambo-code.org
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
 subroutine ras_loc_ypp
   use YPP
   use com,                     ONLY : error, msg
   use pars,                    ONLY : SP, pi, schlen
   use parser_m,                ONLY : parser
   use surface_geometry,        ONLY : setup_gvecaff, gvecaff
   use wave_func,               ONLY : WF, wf_ng, wf_norm_test
   use D_lattice,               ONLY : alat, DL_vol
   use R_lattice,               ONLY : g_vec, nkibz
   use IO_m,                    ONLY : io_control, OP_RD_CL, OP_WR_CL, VERIFY, REP, DUMP, NONE
   use electrons,               ONLY : n_bands,n_spin,n_spinor,n_sp_pol
   use LIVE_t,                  ONLY : live_timing,live_timing_is_on
   use stderr,                  ONLY : intc
   use parallel_m,              ONLY : PP_redux_wait,PP_indexes,myid,PP_indexes_reset
   use interfaces,              ONLY : PARALLEL_index,WF_load
   implicit none
   ! 
   ! Work Space
   !
   real(SP)                 :: loc(n_bands,nkibz), scal, kg(wf_ng), tpiba
   integer                  :: ib, ik, ig, ix,iy,iz, ID, nk_loc
   integer                  :: ibfft, io_err, ig1, ig2, ngz
   integer, external        :: io_loc
   logical                  :: WrtLocDB
   complex(SP)              :: rhoint, int, gz
   complex(SP), parameter   :: ci=cmplx(0.0_SP,1.0_SP)
   character(schlen)        :: sch
   type(PP_indexes)             :: px

   call section('*',"== Slab state localization ==")
   call parser('WrtLocDB',WrtLocDB)
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
   loc(:,:) = 0.0_SP
   nk_loc = loc_kpts(2)-loc_kpts(1)+1
 
   call setup_gvecaff


   wf_norm_test = .true.

   do ik=loc_kpts(1),loc_kpts(2)
     call WF_load(WF,ngloc,1,loc_bands,(/ik,ik/),space='G',impose_free_and_alloc=.TRUE.,title='-LOC')
 
     call live_timing('Localization k('//trim(intc(ik))//')',loc_bands(2)-loc_bands(1)+1,SERIAL=.true.)
     do ib=loc_bands(1),loc_bands(2)
       ibfft=WF%index(ib,ik,1)
       rhoint = 0.0_SP
       do ig1=1,ngloc
         do ig2=1,ngloc
           if(gvecaff(ig1,ix).ne.gvecaff(ig2,ix).or. &
&             gvecaff(ig1,iy).ne.gvecaff(ig2,iy)) cycle
           ngz = gvecaff(ig2,iz) - gvecaff(ig1,iz)
           if(ngz.eq.0) then
             rhoint = rhoint + real(upperlim-lowerlim) * conjg(WF%c(ig2,ibfft))*WF%c(ig1,ibfft)
           else
             gz = real(ngz)*2.0_SP*pi
             int = -ci/gz*( exp(ci*gz*upperlim) - exp(ci*gz*lowerlim) )
             rhoint = rhoint + int * conjg(WF%c(ig2,ibfft))*WF%c(ig1,ibfft)
           endif

         enddo
       enddo
       call live_timing(steps=1)

       loc(ib,ik) = real(rhoint)
     enddo
     call live_timing
   enddo

   scal = 1.d0/maxval(loc)

   live_timing_is_on = .false.
   call section('=','State localization.')

   call msg("ns","",loc_kpts(2)-loc_kpts(1)+1)
   call msg("s","",loc_bands(2)-loc_bands(1)+1)
   do ik=loc_kpts(1),loc_kpts(2)
     do ib=loc_bands(1),loc_bands(2)
       write(sch,'(i7,i7,f10.5,f9.4)') ik,ib,loc(ib,ik),loc(ib,ik)*scal
       call msg("s",trim(sch))
     enddo
   enddo 
   live_timing_is_on = .true.

   if(WrtLocDB) then

     call section('=','Create localization database.')
     call io_control(ACTION=OP_WR_CL, COM=NONE, SEC=(/1/), ID=ID) 
     io_err = io_loc(loc,upperlim, lowerlim, ID)
     if(io_err/=0) call error('Failed to write localization DB.')
   
   endif

   return
 end subroutine ras_loc_ypp
