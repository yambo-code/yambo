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
 subroutine p2s_wf(wf_disk,ikibz)
 !
 use P2S,                   only: pw_unit, verboseIO
 use pars,                  ONLY:SP,DP
 use electrons,             ONLY:n_bands, n_spin, n_sp_pol, n_spinor
 use D_lattice,             ONLY:n_atom_species,pp_n_l_comp_max,&
&                                PP_alloc,pp_kbs,pp_kb,pp_kbd
 use R_lattice,             ONLY:nkibz
 use wave_func,             ONLY:wf_ncx,wf_nc_k, wf_igk
#if defined _P2S_EXPORT
 use pw_export
#else
 use qexml_module
#endif
 use pw_data,               only: n_spin_pw_, gamma_only_
 use interfaces,            ONLY:make_real_wf, n_band_block, blocks, maxband
 use memory_m,              ONLY: mem_est
 implicit none
 !
 integer,  intent(in)   :: ikibz
 real(SP), intent(out)  :: wf_disk(2,n_bands,wf_ncx,n_spin)
 !
 ! Work Space
 !
 integer        :: ib, ikeff, ispin, npwk, ierr, ig
 integer        :: ic, ib1, ib2, iblock, n_b
 !
 ! holds the double precision pwscf record
 !
#if defined _P2S_EXPORT
 complex(DP)    :: wf_disk_DP(wf_nc_k(ikibz))
#else
!complex(DP)   :: wf_disk_DP(wf_nc_k(ikibz), n_bands) 
!complex(DP)   :: wf_disk_DP_T(n_bands,wf_nc_k(ikibz)) 
 complex(DP), allocatable   :: wf_disk_DP(:,:)
#endif

 !---------------------------------------------------------------------*
 !    Set up main data
 !---------------------------------------------------------------------*

#if !defined _P2S_EXPORT
 allocate(wf_disk_DP(wf_ncx,maxband))
 call mem_est("wf_disk_DP",(/size(wf_disk_DP)/),(/DP/))
#endif
 npwk = wf_nc_k(ikibz)
 if(gamma_only_) npwk=(npwk-1)/2+1 ! PWs actually read from file
 wf_disk=(0._SP,0._SP)

 !---------------------------------------------------------------------*
 !    P2S_EXPORT
 !---------------------------------------------------------------------*
#if defined _P2S_EXPORT

 if (ikibz==1) call pw_wfcstart(pw_unit)
 do ispin=1,n_spin
   ikeff = ikibz + (ispin-1)*nkibz
   call pw_wfcscan( pw_unit, ikeff ) ! scan for this ikibz
   do ib = 1, n_bands       ! loop over bands
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
#elif defined _P2S_V31  || defined _P2S_V311

 do ispin = 1, n_spin
   do iblock = 1, n_band_block
     call set_band_block
     call qexml_read_wfc(ibnds=ib1, ibnde=ib2, ik=ikibz, ispin=ispin, &
&       npwk=npwk, igk = wf_igk(1:npwk,ikibz), wf = wf_disk_DP(1:npwk,1:n_b), ierr=ierr)
!
! The original form without blocks
!
!   call qexml_read_wfc(ibnds=1, ibnde=n_bands, ik=ikibz, ispin=ispin, &
!&       npwk=npwk, igk = wf_igk(1:npwk,ikibz), wf = wf_disk_DP, ierr=ierr)
     if(ierr.ne.0) call errore('qexml_read_wfc','Check version of PW data!',ierr)
     call sort_wf_disk
   enddo
 
 enddo

 !---------------------------------------------------------------------*
 !    PW 3.2 and later
 !---------------------------------------------------------------------*
#elif defined _P2S_V32

 do ispin = 1, n_spin
   do iblock = 1, n_band_block

     call set_band_block

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
   !
 enddo

#endif

 contains

   subroutine set_band_block
     use pars,                  ONLY:schlen
     use com,                 only : msg
     character(schlen)       :: sch
     ib1 = blocks(1,iblock)
     ib2 = blocks(2,iblock)
     n_b = ib2-ib1+1
     if(verboseIO) then
       write(sch,'(a,i2,a,i4,a,i4,a)') &
&      "  Filling block ",iblock," bands: [",ib1,":",ib2,"]"
       call msg('s',trim(sch))
     endif
   end subroutine set_band_block

   subroutine sort_wf_disk
    
     if(gamma_only_) then
       do ib=ib1,ib2
         wf_disk(1,ib,1,ispin) = real( wf_disk_DP(1,ib-ib1+1) )
         wf_disk(2,ib,1,ispin) =aimag( wf_disk_DP(1,ib-ib1+1) )
         do ic=2,npwk
           wf_disk(1,ib,2*ic-2,ispin) =   real(wf_disk_DP(ic,ib-ib1+1))
           wf_disk(2,ib,2*ic-2,ispin) =  aimag(wf_disk_DP(ic,ib-ib1+1))
           wf_disk(1,ib,2*ic-1,ispin) =   real(wf_disk_DP(ic,ib-ib1+1))
           wf_disk(2,ib,2*ic-1,ispin) = -aimag(wf_disk_DP(ic,ib-ib1+1))
         enddo
       enddo
!
! Original lines without blocks
!
!      wf_disk(1,1:n_bands,1,ispin) =  real( wf_disk_DP(1,1:n_bands))
!      wf_disk(2,1:n_bands,1,ispin) = aimag( wf_disk_DP(1,1:n_bands))
!      do ig=2,npwk
!        wf_disk(1,1:n_bands,2*ig-2,ispin) =  real(wf_disk_DP(ig,1:n_bands))
!        wf_disk(2,1:n_bands,2*ig-2,ispin) = aimag(wf_disk_DP(ig,1:n_bands))
!        wf_disk(1,1:n_bands,2*ig-1,ispin) =  real(wf_disk_DP(ig,1:n_bands))
!        wf_disk(2,1:n_bands,2*ig-1,ispin) = -aimag(wf_disk_DP(ig,1:n_bands))
!      enddo

     else
!      wf_disk_DP_T=transpose(wf_disk_DP)
!      wf_disk(1,1:n_bands,1:npwk,ispin) =  real( wf_disk_DP_T(:,:) )
!      wf_disk(2,1:n_bands,1:npwk,ispin) = aimag( wf_disk_DP_T(:,:) )
       do ib=ib1,ib2
         forall(ic=1:npwk) wf_disk(1,ib,ic,ispin) = real( wf_disk_DP(ic,ib-ib1+1) )
         forall(ic=1:npwk) wf_disk(2,ib,ic,ispin) =aimag( wf_disk_DP(ic,ib-ib1+1) )
       enddo
     endif
     !
     !   Lines relating to real WF workaround
     !
     !do ib=1,n_bands
     !  call make_real_wf(ib,ikibz,wf_disk(:,ib,:wf_nc_k(ikibz),ispin),wf_nc_k(ikibz))
     !enddo
     !
   end subroutine sort_wf_disk

 end subroutine p2s_wf
