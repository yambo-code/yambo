!
!        Copyright (C) 2000-2014 the YAMBO team
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
module optcut
  !
  use pars,                  ONLY : schlen, SP, pi
  use electrons,             ONLY : levels
! use R_lattice,             ONLY : bz_samp
  use surface_geometry

  implicit none

  ! Limiting values of n1,n2,n3 for G=n1*b1 + n2*b2 + n3*b3
  ! in a list of G vectors (e.g. cutoff sphere)
  type G_box
     integer                :: max(3)
     integer                :: min(3)
  end type G_box

  type(G_box)               :: n,nxx  ! one k and overall limiting values of n
  type(G_box), allocatable  :: nxk(:) ! for each k

  logical, allocatable      :: gfound (:,:)
  integer, allocatable      :: n3limit(:,:,:)
  integer, allocatable      :: igtab(:,:,:)
  complex, allocatable      :: wfc1(:),wfc2(:)
  complex, allocatable      :: Fzg(:,:)
  complex, allocatable      :: Fzgp(:,:)

  complex(SP), allocatable  :: DIP_iR_cut(:,:,:,:,:)
  complex(SP), allocatable  :: DIP_q_dot_iR_cut(:,:,:,:)
  complex(SP), allocatable  :: cutmat(:,:)

  real(SP)                  :: dcf,dc ! fractional/cartesian cut length
  real(SP)                  :: z0f,z0 ! fractional/cartesian zero of cut box
  logical                   :: loptcut

  private
  save
  public  :: init_cutoff
  public  :: loptcut, dc  
  public  :: DIP_iR_cut, DIP_q_dot_iR_cut
  public  :: setup_gvecaff, nG_limits, setup_optcut, PScut , PScut_slow, setup_cutmat
  public  :: end_optcut, setup_cutoff, print_cutoff, z0f, dcf

contains 

subroutine init_cutoff(defs)
  use it_m,                  ONLY : it, initdefs, E_unit,G_unit,T_unit
  use parser_m,      only : parser
  implicit none
  integer, parameter             :: V_general=1, V_qp=2, V_io=3
  type(initdefs), intent(inout)  :: defs
  character(schlen)              :: loptcut_

  call it('f',defs,'Cutoff'  , '[RAS] Cutoff mode (Uncomment to use)' )
  call parser('Cutoff',loptcut)
  call it(defs,'CutZero' , '[RAS] Zero position of cutoff fn (Frac)', z0f  )
  call it(defs,'CutStep' , '[RAS] Width of cutoff function (Frac)',   dcf  )

  return
end subroutine init_cutoff

  subroutine setup_cutoff(lfail)
    implicit none
    logical, intent(out)      :: lfail

    lfail = .false.
    z0   = z0f*az  ! az is in a.u.
    dc   = dcf*az

    return
  end subroutine setup_cutoff


  subroutine setup_cutmat
    use wave_func,         ONLY : wf_ng
    implicit none
    complex(SP), parameter          :: ci = (0.0_SP,1.0_SP)
    real(SP)                        :: gz 
    integer                         :: ig1, ig2, ngz
    
! SLOW
    allocate(cutmat(wf_ng,wf_ng))
    do ig1 = 1, wf_ng
      do ig2 = 1, wf_ng
        if(gvecaff(ig1,ix).ne.gvecaff(ig2,ix).or. &
&          gvecaff(ig1,iy).ne.gvecaff(ig2,iy)) cycle
        ngz = gvecaff(ig2,iz) - gvecaff(ig1,iz)
        if(ngz.eq.0) then
          cutmat(ig1,ig2) = dcf
        else
          gz = real(ngz)*2.0_SP*pi
          cutmat(ig1,ig2) = -ci/gz * (exp(ci*gz*dcf) - 1.0_SP) * exp(ci*z0f)
        endif
      enddo
    enddo
! SLOW
    return
  end subroutine setup_cutmat


  subroutine print_cutoff
    implicit none
    character(schlen)           :: lch
    if (loptcut) then
      call msg('r','Using cut off function in matrix elements:')
      write(lch,100) 'Zero position of cut function [frac]',z0f,', [a.u.]:',z0
      call msg('r',trim(lch))
      write(lch,100) 'Width of cut (boxcar) function [frac]',dcf,', [a.u.]:',dc
      call msg('r',trim(lch))
    else
      call msg('r','No cut off function in matrix elements.')
    endif
    return
100 format(a,f6.3,a,f8.3)
  end subroutine print_cutoff

!<-------------------------------------------------------------------->! 

  subroutine nG_limits(nkibz)
    use com,               ONLY : msg
    use wave_func,         ONLY : wf_ng, wf_ncx
    implicit none
   
    integer, intent (in)       :: nkibz
!   Work space
    integer                    :: ngw_k, ik, i1

    allocate( nxk(nkibz) )
    ngw_k = wf_ng
    
    do ik = 1,nkibz

! wf and g_vec are on same 1:ngw_k grid (wf reordered on reading).
! Hence just need simple mask.
       nxk(ik)%min(1) = minval( gvecaff(1:ngw_k,ix) )
       nxk(ik)%min(2) = minval( gvecaff(1:ngw_k,iy) )
       nxk(ik)%min(3) = minval( gvecaff(1:ngw_k,iz) )
       nxk(ik)%max(1) = maxval( gvecaff(1:ngw_k,ix) )
       nxk(ik)%max(2) = maxval( gvecaff(1:ngw_k,iy) )
       nxk(ik)%max(3) = maxval( gvecaff(1:ngw_k,iz) )

    enddo

! Maximum values for array declarations
    nxx%min(1) = minval( nxk(:)%min(1) )
    nxx%min(2) = minval( nxk(:)%min(2) )
    nxx%min(3) = minval( nxk(:)%min(3) )
    nxx%max(1) = maxval( nxk(:)%max(1) )
    nxx%max(2) = maxval( nxk(:)%max(2) )
    nxx%max(3) = maxval( nxk(:)%max(3) )

    call msg('nr','Wavefunction nG limits: ',(/ (nxx%min(i1),nxx%max(i1),i1=1,3) /) )

    allocate( igtab  (nxx%min(1):nxx%max(1), nxx%min(2):nxx%max(2), nxx%min(3):nxx%max(3)) )
    allocate( n3limit(nxx%min(1):nxx%max(1), nxx%min(2):nxx%max(2), 2) )
    allocate( gfound (nxx%min(1):nxx%max(1), nxx%min(2):nxx%max(2)) )
    allocate( Fzg    (nxx%min(3):nxx%max(3), nxx%min(3):nxx%max(3)) )
    allocate( Fzgp   (nxx%min(3):nxx%max(3), nxx%min(3):nxx%max(3)) )
    allocate( wfc1   (nxx%min(3):nxx%max(3)) )
    allocate( wfc2   (nxx%min(3):nxx%max(3)) )

    return
  end subroutine nG_limits

!<-------------------------------------------------------------------->! 
   
  subroutine setup_optcut( ik )
    use com,   ONLY:msg
    use wave_func,         ONLY : wf_ng, wf_ncx
    implicit none
    integer, intent (in)       :: ik
!ws
    integer                    :: ig,ngw_k,n1,n2,n3
    
    ngw_k = wf_ng
    n = nxk(ik) 

    ! Set up igtab array
    n3limit(:,:,1) = +999
    n3limit(:,:,2) = -999
    gfound(:,:) = .false.     ! some n1,n2 will not exist within cutoff radius
    do ig=1,ngw_k             ! counter over all G for this wfc 
! Note: the wavevectors are already reordered (igk used) in wfload.
          n1 = gvecaff(ig,ix)
          n2 = gvecaff(ig,iy)
          n3 = gvecaff(ig,iz)
          gfound(n1,n2) = .true.     ! at least one point is enough
          igtab(n1,n2,n3) = ig ! this is the label needed in wfc(ib,G) and gvec
          if(n3.lt.n3limit(n1,n2,1)) n3limit(n1,n2,1) = n3
          if(n3.gt.n3limit(n1,n2,2)) n3limit(n1,n2,2) = n3
!DEBUG>
!         write(77,223) n1,n2,n3,ig,gvecaff(igtab(n1,n2,n3),xyz(:))  ! DEBUG
!  223 format(3i4,2x,i7,3i4,2x,3i4)  ! DEBUG
!DEBUG<
    enddo
    
    call check_index
   
!   Calculate Fz(gz) for all Gz,Gz'
    call calc_Fzg
   
    return
   
  contains
   
     subroutine calc_Fzg
       !
       ! For debugging, note: d0f = 1, z0f = 0
       ! should give same results as without cutoff.
       ! Hence Fzg(i,i) = 1; Fzg(i,j) = 0 (i .ne. j)
       ! Precision gives sin(2*pi) = 1E-7
       !
       implicit none
       integer                   :: n3,n3p
       real(SP)                  :: twopi
       complex, parameter        :: ci = (0,1)
       
       twopi = 2.d0*pi
       do n3 = n%min(3),n%max(3)
          Fzg(n3,n3) = dcf ! Gz.eq.Gz'
          Fzgp(n3,n3) = 0 ! Gz.eq.Gz'
          do n3p = n3+1,n%max(3) ! for all Gz,Gz', Gz.ne.Gz'
             Fzg(n3,n3p) = -ci/twopi/(n3p-n3) * &
&                  exp(ci*(n3p-n3)*twopi*z0f) * (exp(ci*(n3p-n3)*twopi*dcf)-1)
             Fzgp(n3,n3p) =  Fzg(n3,n3p)*(n3-n3p)*twopi/az ! -1*gz
          enddo
          do n3p = n%min(3),n3-1
             Fzg(n3,n3p) = -ci/twopi/(n3p-n3) * &
&                  exp(ci*(n3p-n3)*twopi*z0f) * (exp(ci*(n3p-n3)*twopi*dcf)-1)
             Fzgp(n3,n3p) =  Fzg(n3,n3p)*(n3-n3p)*twopi/az ! -1*gz
          enddo
       enddo
   
       return
     end subroutine calc_Fzg
   
     subroutine check_index
       implicit none
       integer                    :: icount
       integer                    :: n1,n2,n3,ig1
   
       icount = 0
       do n1 = n%min(1), n%max(1)
       do n2 = n%min(2), n%max(2)
          if ( gfound(n1,n2) ) then
             do n3 = n3limit(n1,n2,1), n3limit(n1,n2,2)
                if (n3.lt.n%min(3).or.n3.gt.n%max(3)) then
                   call msg('r','n3 out of bounds.')
                endif
                icount = icount + 1
                ig1=igtab(n1,n2,n3)
                if (ig1.lt.1.or.ig1.gt.wf_ng) then
!               if (ig1.lt.1.or.ig1.gt.wf_ncx) then !CHECK THIS
                 write (*,224) n1,n2,n3,ig1,gvecaff(ig1,xyz(:))
                 write (*,*) wf_ncx,wf_ng
                   call msg('sr','[ERROR] Bug found in check_index.')
                   stop
                endif
!DEBUG>
!                write (78,224) n1,n2,n3,ig1,gvecaff(ig1,xyz(:)) ! DEBUG
!DEBUG<
224 format(3i4,2x,i7,3i4,2x,3i4)
             enddo
          endif
       enddo
       enddo
       if(icount.ne.ngw_k) then
         call msg('sr','[WARNING] Wrong number of Gvectors in igtab.')
         stop
       endif
       return 
     end subroutine check_index
   
  end subroutine setup_optcut

!<-------------------------------------------------------------------->! 

  subroutine end_optcut
    implicit none
    if(allocated(igtab))  deallocate(igtab)
    if(allocated(gfound)) deallocate(gfound)
    if(allocated(n3limit)) deallocate(n3limit)
    if(allocated(Fzg)) deallocate(Fzg)
    if(allocated(Fzgp)) deallocate(Fzgp)
    if(allocated(wfc1)) deallocate(wfc1)
    if(allocated(wfc2)) deallocate(wfc2)
    if(allocated(nxk)) deallocate(nxk)
    return
  end subroutine end_optcut

!<-------------------------------------------------------------------->! 
!<-------------------------------------------------------------------->! 
  subroutine PScut_better(PS,wfv,wfc,kg)
    use wave_func,         ONLY : wf_ng, wf_ncx
    implicit none
    complex(SP),   intent(out)   :: PS(3)
    integer                         :: ig1,ig2
    complex(SP),   intent(in)       :: wfv(:),wfc(:)
    real(SP),      intent(in)       :: kg(:,:)
    complex(SP)                     :: PSx,PSy,PSz,cc
    complex(SP), parameter          :: ci = (0.0_SP,1.0_SP)

    PSx = 0
    PSy = 0
    PSz = 0
    do ig2 = 1, wf_ng
      do ig1 = 1, wf_ng
        if(gvecaff(ig1,ix).ne.gvecaff(ig2,ix).or. &
&          gvecaff(ig1,iy).ne.gvecaff(ig2,iy)) cycle
        cc = conjg(wfv(ig1)) * wfc(ig2)
        PSx = PSx + cc * kg(ix,ig2) * cutmat(ig1,ig2)
        PSy = PSy + cc * kg(iy,ig2) * cutmat(ig1,ig2)
      enddo
    enddo
    ! Convert to crystal frame (i.e the REAL cartesian axes)
    PS(ix) = PSx
    PS(iy) = PSy
    PS(iz) = PSz
    return
  end subroutine PScut_better





  subroutine PScut_slow(PS,wfv,wfc,kg)
    use wave_func,         ONLY : wf_ng, wf_ncx
    implicit none
    complex(SP),   intent(out)   :: PS(3)
    integer                         :: ig1,ig2, ngz
    complex(SP),   intent(in)       :: wfv(:),wfc(:)
    real(SP),      intent(in)       :: kg(:,:)
    complex(SP)                     :: PSx,PSy,PSz,cc
    complex(SP), parameter          :: ci = (0.0_SP,1.0_SP)
    real(SP)                        :: gz

    PSx = 0
    PSy = 0
    PSz = 0
    do ig1 = 1, wf_ng
      do ig2 = 1, wf_ng
        if(gvecaff(ig1,ix).ne.gvecaff(ig2,ix).or. &
&          gvecaff(ig1,iy).ne.gvecaff(ig2,iy)) cycle
        ngz = gvecaff(ig2,iz) - gvecaff(ig1,iz)
        cc = conjg(wfv(ig1)) * wfc(ig2)
        if(ngz.eq.0) then
          PSx = PSx + cc * kg(ix,ig2) * dcf
          PSy = PSy + cc * kg(iy,ig2) * dcf
        else
          gz = real(ngz)*2.0_SP*pi
          PSx = PSx + cc * kg(ix,ig2) * &
&               ci/gz * ( 1.0_SP - exp(ci*gz*dcf)) * exp(ci*z0f)
          PSy = PSy + cc * kg(iy,ig2) * &
&               ci/gz * ( 1.0_SP - exp(ci*gz*dcf)) * exp(ci*z0f)
        endif
      enddo
    enddo
    ! Convert to crystal frame (i.e the REAL cartesian axes)
    PS(ix) = PSx
    PS(iy) = PSy
    PS(iz) = PSz
    return
  end subroutine PScut_slow
   
  subroutine PScut(PS,wfv,wfc,kg)
    implicit none
    complex(SP),   intent(out)   :: PS(3)
    real(SP), optional, intent(in)    :: kg(:,:)
    complex(SP),   intent(in)    :: wfv(:),wfc(:)
!   logical,       intent(in)    :: use_trans_gauge
   ! ws
    integer                      :: n3min,n3max,ig1,n1,n2,n3,n3p
    real(SP)                     :: kgxy(3)
    complex(SP)                  :: PSx,PSy,PSz
   
    PSx = 0
    PSy = 0
    PSz = 0
    kgxy = 1.0_SP
    ! Double loop over n1,n2 (i.e. Gx,Gy)
    do n1 = n%min(1),n%max(1)
    do n2 = n%min(2),n%max(2)
       if(gfound(n1,n2)) then ! Avoid cases outside the cutoff

          n3min = n3limit(n1,n2,1)
          n3max = n3limit(n1,n2,2) 
!         Ignore Gz for now. Thus any n3 will do.
          if(present(kg)) kgxy(ix) = kg(ix,igtab(n1,n2,0)) ! crystal frame
          if(present(kg)) kgxy(iy) = kg(iy,igtab(n1,n2,0)) ! crystal frame
          do n3 = n3min,n3max
             ig1 = igtab(n1,n2,n3)
             wfc1(n3) = conjg(wfv(ig1))
             wfc2(n3) = wfc(ig1)
          enddo
          ! Calculate matrix element: replace 
          ! [loop on G,G'; if(G.ne.G')..] with known sum on n3/n3'
          do n3 = n3min,n3max
          do n3p = n3min,n3max
             PSx = PSx + wfc1(n3) * wfc2(n3p) * kgxy(ix)* Fzg(n3,n3p) ! surface frame
! PS(ix) = PS(ix) + wfc1(n3) * wfc2(n3p) * kg(ix)* Fzg(n3,n3p) ! crystal frame
             PSy = PSy + wfc1(n3) * wfc2(n3p) * kgxy(iy)* Fzg(n3,n3p) ! surface frame
! PSz = PSz + wfc1(n3) * wfc2(n3p) * kg(iz)* Fzg(n3,n3p) ! surface frame, iz incorrect
!            PSz = 0 ! if want this, need to code kg(n3p)
             if(present(kg)) kgxy(iz) = kg(iz,igtab(n1,n2,n3p)) ! crystal frame
!         PSz = PSz + wfc1(n3) * wfc2(n3p) * kgxy(iz)* Fzg(n3,n3p) 
          PSz = PSz + wfc1(n3) * wfc2(n3p) * kgxy(iz)* Fzg(n3,n3p) + &
 &                  0.5 * wfc1(n3) * wfc2(n3p) * Fzgp(n3,n3p)
!   write(*,*) "In pscut:",n1,n2,n3,PSx,PSy,wfc1(n3),wfc2(n3p),kgxy(ix),Fzg(n3,n3p)," XXX"
          enddo
          enddo
       endif ! end on gfound(n1,n2)
    enddo 
    enddo ! end loop n1,n2
   
   ! Convert to crystal frame (i.e the REAL cartesian axes)
    PS(ix) = PSx
    PS(iy) = PSy
    PS(iz) = PSz

   
    return
  end subroutine PScut
    
!<-------------------------------------------------------------------->! 
   
  subroutine endcut
    implicit none
    deallocate(gvecaff)
  end subroutine endcut

!<-------------------------------------------------------------------->! 
  
end module optcut
