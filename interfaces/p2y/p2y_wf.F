!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH
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
 subroutine p2y_wf(wf_disk,ikibz, ib_grp)
 !
 ! Reads the WFs for single k-point ikibz and group of bands
 ! associated with group ib_grp
 !
 use P2Y,                   ONLY:pw_unit
 use pars,                  ONLY:SP,DP
 use electrons,             ONLY:n_bands, n_spin
 use R_lattice,             ONLY:nkibz
 use wave_func,             ONLY:wf_ncx,wf_nc_k, wf_igk, wf_nb_io
 use mod_com2y,             ONLY:verboseIO
#if defined _P2Y_EXPORT
 use pw_export
#else
 use qexml_module
#endif
 use pw_data,               only: n_spin_pw_, gamma_only_
 implicit none
 !
 integer,  intent(in)   :: ikibz, ib_grp
 real(SP), intent(out)  :: wf_disk(2,wf_nb_io,wf_ncx,n_spin)
 !
 ! Work Space
 !
 integer        :: ib, ikeff, ispin, npwk, ierr, ig
 integer        :: ic, ib1, ib2, n_b
 !
 ! holds the double precision pwscf record
 !
#if defined _P2Y_EXPORT
 complex(DP)    :: wf_disk_DP(wf_nc_k(ikibz))
#else
 complex(DP)   :: wf_disk_DP(wf_ncx, wf_nb_io)
#endif

 !---------------------------------------------------------------------*
 !    Set up main data
 !---------------------------------------------------------------------*

 npwk = wf_nc_k(ikibz)
 if(gamma_only_) npwk=(npwk-1)/2+1 ! PWs actually read from file
 wf_disk=(0._SP,0._SP)
 !
 ! Set the range of bands to be read from disk (ib1, ib2, n_b)
 !
 call set_band_block

 !---------------------------------------------------------------------*
 !    P2Y_EXPORT
 !---------------------------------------------------------------------*
#if defined _P2Y_EXPORT
 !
 ! Note that EXPORT does not support block splitting
 !
 if (ikibz==1) call pw_wfcstart(pw_unit)
 do ispin=1,n_spin
   ikeff = ikibz + (ispin-1)*nkibz
   call pw_wfcscan( pw_unit, ikeff ) ! scan for this ikibz
   do ib = 1, n_bands
     call pw_wfcread(pw_unit,ib,npwk,wf_disk_DP)
     wf_disk(1,ib,1:npwk,ispin) =  real( wf_disk_DP(1:npwk) )
     wf_disk(2,ib,1:npwk,ispin) = aimag( wf_disk_DP(1:npwk) )
     !call make_real_wf(ib,i,ikibz,wf_disk(:,ib,:wf_nc_k(ikibz),ispinor),wf_nc_k(ikibz))
   enddo
   call pw_wfcscanend(pw_unit,ikeff)
 enddo
 if (ikibz==nkibz) call pw_wfcstop(pw_unit)

 !---------------------------------------------------------------------*
 !    PW 3.1 and 3.1.1
 !---------------------------------------------------------------------*
#elif defined _P2Y_V31  || defined _P2Y_V311

 do ispin = 1, n_spin
     call qexml_read_wfc(ibnds=ib1, ibnde=ib2, ik=ikibz, ispin=ispin, &
&       npwk=npwk, igk = wf_igk(1:npwk,ikibz), wf = wf_disk_DP(1:npwk,1:n_b), ierr=ierr)
     if(ierr.ne.0) call errore('qexml_read_wfc','Check version of PW data!',ierr)
     !
     call sort_wf_disk
     !
 enddo

 !---------------------------------------------------------------------*
 !    PW 3.2 and later
 !---------------------------------------------------------------------*
#elif defined _P2Y_V32 || defined _P2Y_V40 || defined _P2Y_V50

 do ispin = 1, n_spin

     select case(n_spin_pw_)
     case(1)
       call qexml_read_wfc(ibnds=ib1, ibnde=ib2, ik=ikibz, &
&         igk = wf_igk(1:npwk,ikibz), wf = wf_disk_DP(1:npwk,1:n_b), ierr=ierr)
       if(ierr.ne.0) call errore('qexml_read_wfc',' Check version of PW data!',ierr)
     case(2) ! note ipol=ispin
       call qexml_read_wfc(ibnds=ib1, ibnde=ib2, ik=ikibz, ipol=ispin, &
&         igk = wf_igk(1:npwk,ikibz), wf = wf_disk_DP(1:npwk,1:n_b), ierr=ierr)
       if(ierr.ne.0) call errore('qexml_read_wfc',' Check ipol/PWscf version!',ierr)
     case(4) ! note ispin=ispin
       call qexml_read_wfc(ibnds=ib1, ibnde=ib2, ik=ikibz, ispin=ispin, &
&           igk = wf_igk(1:npwk,ikibz), wf = wf_disk_DP(1:npwk,1:n_b), ierr=ierr)
       if(ierr.ne.0) call errore('qexml_read_wfc',' Check ispin/PWscf version!',ierr)
     end select
     !
     call sort_wf_disk
     !
 enddo

#endif

 contains

   subroutine set_band_block
     use pars,                ONLY:schlen
     use com,                 only : msg
     character(schlen)       :: sch

     ib1 = (ib_grp-1)*wf_nb_io + 1 
     ib2 = min(ib_grp*wf_nb_io,n_bands)
     n_b = ib2-ib1+1

     if(verboseIO) then
       write(sch,'(a,i2,a,i4,a,i4,a)') &
&      "  Filling block ",ib_grp," bands: [",ib1,":",ib2,"]"
       call msg('s',trim(sch))
     endif
   end subroutine set_band_block

   subroutine sort_wf_disk
    
#if !defined _P2Y_EXPORT
     if(gamma_only_) then

       do ib=1, n_b
         wf_disk(1,ib,1,ispin) = real( wf_disk_DP(1,ib) )
         wf_disk(2,ib,1,ispin) =aimag( wf_disk_DP(1,ib) )
         do ic=2,npwk
           wf_disk(1,ib,2*ic-2,ispin) =   real(wf_disk_DP(ic,ib))
           wf_disk(2,ib,2*ic-2,ispin) =  aimag(wf_disk_DP(ic,ib))
           wf_disk(1,ib,2*ic-1,ispin) =   real(wf_disk_DP(ic,ib))
           wf_disk(2,ib,2*ic-1,ispin) = -aimag(wf_disk_DP(ic,ib))
         enddo
       enddo

     else

       do ib=1,n_b
         forall(ic=1:npwk) wf_disk(1,ib,ic,ispin) = real( wf_disk_DP(ic,ib) )
         forall(ic=1:npwk) wf_disk(2,ib,ic,ispin) =aimag( wf_disk_DP(ic,ib) )
       enddo

     endif
     !
     !   Lines relating to real WF workaround
     !
     !do ib=1,n_bands
     !  call make_real_wf(ib,ikibz,wf_disk(:,ib,:wf_nc_k(ikibz),ispin),wf_nc_k(ikibz))
     !enddo
     !
#endif
   end subroutine sort_wf_disk

 end subroutine p2y_wf
