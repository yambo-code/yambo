!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DK CA
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
subroutine bxsf_read(Xk,Xen)
 !
 use pars,                ONLY:SP,lchlen
 use units,               ONLY:pi,HA2EV,BO2ANG
 use R_lattice,           ONLY:bz_samp,b,k_grid,k_the_nearest
 use parser_m,            ONLY:parser
 use YPP,                 ONLY:wannier_bands, ord_dgrid_ipol
 use electrons,           ONLY:levels,n_spin
 use LIVE_t,              ONLY:live_timing
 use com,                 ONLY:file_exists,msg,error
 use vec_operate,         ONLY:c2a,rlu_k2bz,k2bz,v_is_zero
 use IO_m,                ONLY:io_control,NONE,OP_WR_CL
 !
#include<memory.h>
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 ! 
 character(lchlen) :: filename
 type(bz_samp) :: DbGd_k
 !
 integer :: readunit, ii, readband, iband, ikpt, kx, ky, kz,ID,i_err, ikbz, bxsf_bands
 integer :: nearest_to_g
 integer :: nbands,vband ! total number of bands, highest valence
 integer :: ndim ! total number of dimensions, iterator on dimensions
 integer, external :: io_Double_Grid
 integer, external :: double_k_grid
 integer :: err_dgrid
 real(SP) :: readene,deltaE
 integer, allocatable :: nkpoints_gengrid(:), &  ! number of points in kpoint
                                                  ! mesh of general grid
                          nkpoints(:)             ! -"- of periodic grid
 real(SP), allocatable :: recip_lattice(:,:), startkpoint(:)
 real(SP), allocatable :: ene(:,:), &           ! ene(band, kpoint)
                          kpoints(:,:)           ! kpoints([k1x,k1y,k1z],[k2x,k2y,k2z],... )
 character(lchlen) :: dummyline

 real(SP) :: efermitest

 YAMBO_ALLOC(nkpoints_gengrid,(3))
 YAMBO_ALLOC(nkpoints,(3))
 YAMBO_ALLOC(startkpoint,(3))
 
 nkpoints_gengrid = 0
 nkpoints = 0
 startkpoint = 0.0_SP
 !
 call parser('w90_fname',filename)
 !
 if(.not. file_exists(trim(filename))) then
    call msg("s",':: Do not find wannier90 file')
    return
 endif
 !
 call section('*','Read wannier interpolation file')
 ! TODO read&write according to yambo standards
 !call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=NONE,ID=readunit)
 readunit=22
 open(readunit,FILE=trim(filename), FORM='FORMATTED')
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,'(a20, F8.5)') dummyline, efermitest
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,*) dummyline
 read(readunit,*) bxsf_bands
 read(readunit,*) (nkpoints_gengrid(ii), ii=1,3)
 !
 nkpoints = nkpoints_gengrid-1
 ! xcrysden bxsf plots 3D by default, but it is convenient to have this 
 ! reading routine more flexible
 ndim = 0
 do ii = 1,3
  if(nkpoints(ii) >= 1) ndim = ndim + 1 
 enddo
 !
 YAMBO_ALLOC(recip_lattice,(ndim,ndim))
 ! set nbands
 !wannier_bands(2)=min(wannier_bands(2),bxsf_bands-wannier_bands(1)+1)
 wannier_bands(2)=min(wannier_bands(2),bxsf_bands)
 nbands=wannier_bands(2)-wannier_bands(1)+1
 !
 YAMBO_ALLOC(ene,(nbands,product(nkpoints)))
 YAMBO_ALLOC(kpoints,(product(nkpoints),ndim))
 ene = 0.0_SP
 kpoints = 0.0_SP
 recip_lattice = 0.0_SP 
 !
 read(readunit,*) (startkpoint(ii), ii=1,3)
 read(readunit,*) (recip_lattice(1,ii), ii=1,ndim)
 read(readunit,*) (recip_lattice(2,ii), ii=1,ndim)
 read(readunit,*) (recip_lattice(3,ii), ii=1,ndim)
 ! wannier90 outputs in angstrom and eV
 ! convert recip_lattice (ang^(-1)). 
 recip_lattice = recip_lattice*BO2ANG
 ! compare with internal reciprocal lattice vectors
 do ii=1,ndim
   if(.not.v_is_zero(recip_lattice(ii,:) - b(ii,:))) &
&     call error("The reciprocal lattices in db and bxsf are not the same") 
 enddo
 ! 
 do iband = 1, wannier_bands(2)
   read(readunit,*) dummyline, readband
   !
   if (readband.ne.iband) call error(" reading bxsf file, number of bands") 
   !
   if (readband .lt. wannier_bands(1)) then
     do ii = 1,product(nkpoints_gengrid)
       read(readunit,*) readene
     end do
   else 
     !
     ikpt = 0
     !
     do kx = 1, nkpoints_gengrid(1)
        do ky = 1, nkpoints_gengrid(2)
          do kz = 1, nkpoints_gengrid(3)
            read(readunit,*) readene
            ! general grid becomes a periodic one. last kpoint = first kpoint in
            ! each direction. should get rid of if block!!
            if(kx .ne. nkpoints_gengrid(1) .and. &
               ky .ne. nkpoints_gengrid(2) .and. &
               kz .ne. nkpoints_gengrid(3)) then
              ikpt = ikpt + 1
              ene(iband-wannier_bands(1)+1,ikpt)=readene
            end if
         end do
       end do
     end do
   end if
 end do
 !
 close(readunit)
 ikpt = 0
 do kx = 1, nkpoints(1)
    do ky = 1, nkpoints(2)
       do kz = 1, nkpoints(3)
          ikpt = ikpt + 1
          ! Monkhorst-Pack grid kpoint vector, reciprocal coord
          ! (reciprocal unit cell, not Brillouin zone)
          kpoints(ikpt,1) = real((kx-1))/nkpoints(1) 
          kpoints(ikpt,2) = real((ky-1))/nkpoints(2)
          kpoints(ikpt,3) = real((kz-1))/nkpoints(3)
       end do
    end do
 end do 
 !                 
 call msg('s',':: Wannier-interpolated K-points (BZ) : ',nkpoints)
 call msg('s',':: Internal (coarse) K-point grid (BZ): ',k_grid)
 call msg('s',':: Range of Wannier-interpolated bands: ',wannier_bands)
 call msg('s',':: Wannier-interpolated bands:          ',nbands)

 !call io_disconnect(readunit)
 !
 ! Xk (IBZ->BZ)
 !
 call k_ibz2bz(Xk,'i',.true.)
 !
 !
 !============== prepare DbGd_k
 ! DbGd_k are in reciproc unit cell, not Brillouin zone
 DbGd_k%nbz=size(kpoints, 1)
 DbGd_k%nibz=DbGd_k%nbz
 YAMBO_ALLOC(DbGd_k%ptbz,(DbGd_k%nbz,3))
 YAMBO_ALLOC(DbGd_k%weights,(DbGd_k%nibz))
 DbGd_k%ptbz = 0_SP
 DbGd_k%weights = 1_SP
 ! map read kpoints to BZ 
 do ii=1,DbGd_k%nbz
   call rlu_k2bz(v_out=DbGd_k%ptbz(ii,:),v_in=kpoints(ii,:))
 enddo
 !
 ! conversion kpoints from reciprocal coord to internal (cartesian-like) coord 
 do ii=1,DbGd_k%nbz
   call c2a(v_in=DbGd_k%ptbz(ii,:),mode='ka2i')
 enddo !
 ! map kpoints (in iku) to BZ
 do ii=1,DbGd_k%nbz
   call k2bz(v_in=DbGd_k%ptbz(ii,:))
 enddo
 !
 YAMBO_ALLOC(Xk%nbz_mapped,(Xk%nbz))
 Xk%nbz_mapped = 0
 Xen%nb_FineGd=nbands
 Xk%nbz_FineGd=DbGd_k%nbz
 !
 if (ord_dgrid_ipol .eq. 0) then
   !
   call section('*','Find nearest K-points in BZ')
   ! 
   call live_timing('BZ DbGd_k Tables',DbGd_k%nbz*2)
   ! 
   ! have to go twice through all elements as BZ_DbGd_k cannot be
   ! allocated without knowing the number of maximum elements. 
   !
   do while(.not.allocated(Xk%bz_DbGd_map)) 
     if (maxval(Xk%nbz_mapped)>0) then
       allocate(Xk%bz_DbGd_map(Xk%nbz,maxval(Xk%nbz_mapped)))
       Xk%bz_DbGd_map=0
     endif
     Xk%nbz_mapped=0
     do ii=1,DbGd_k%nbz
       !
       ikbz=k_the_nearest(DbGd_k%ptbz(ii,:),Xk%ptbz(:,:),Xk%nbz)
       Xk%nbz_mapped(ikbz)=Xk%nbz_mapped(ikbz)+1
       !
       if (allocated(Xk%bz_DbGd_map)) Xk%bz_DbGd_map(ikbz,Xk%nbz_mapped(ikbz))= ii
       !   
       call live_timing(steps=1)
       !
     enddo
   enddo
   call live_timing()
   ! 
   YAMBO_ALLOC(Xk%weights_ipol,(maxval(Xk%nbz_mapped)))
   Xk%weights_ipol = 1.0_SP
 else
   call section('*','Employ double grid method' )
   call msg('s',':: Perform polynomial interpolation of order :',ord_dgrid_ipol)
   ! fill DbGd_k according to double grid technique
   err_dgrid=double_k_grid(Xk,nkpoints,ord_dgrid_ipol)
   if(err_dgrid.ne.0) then
     call msg("s",':: ERROR while preparing double grid. Error:',err_dgrid) 
     return
   endif
 endif
 !
 !     
 call msg('s',':: Blocks filling range :',(/minval(Xk%nbz_mapped),maxval(Xk%nbz_mapped)/))
 if(minval(Xk%nbz_mapped) .ne. maxval(Xk%nbz_mapped)) then
   call msg('s',':: DbGd_k blocks are not filled uniformly.')
   if(ord_dgrid_ipol.gt.0)then
     call msg('s',':: Error: For double grid must be filled uniformely.')
     return
   endif
 endif
 !
 call section('*','Create & write BZ_DbGd')
 !
 YAMBO_ALLOC(Xen%E_FineGd,(Xen%nb_FineGd,Xen%nk,n_spin))
 Xen%E_FineGd(:,:,1)=ene/HA2EV+Xen%E_Fermi
 ! align last (partly) filled band at gamma.
 nearest_to_g=k_the_nearest(DbGd_k%ptbz(1,:),Xk%ptbz(:,:),Xk%nbz) 
 if(sum((DbGd_k%ptbz(1,:)-Xk%ptbz(nearest_to_g,:))**2).gt. epsilon(1._SP)) then
    call msg('s',':: Gamma is not part of your kgrid.')
    call msg('s',':: Probably you use a shifted grid. You should not!')
    call msg('s',':: Gamma :',DbGd_k%ptbz(1,:))
    call msg('s',':: Nearest to Gamma :',Xk%ptbz(nearest_to_g,:))
 end if 
 !
 vband=minloc(Xen%f(:,nearest_to_g,1),1)-1
 call msg('s',':: Internal and wannier bands to align (at Gamma):',(/vband,vband+wannier_bands(1)-1/))
 deltaE=ene(vband,1)/HA2EV-Xen%E(vband,nearest_to_g,1)
 Xen%E_FineGd(:,:DbGd_k%nbz,1)=Xen%E_FineGd(:,:DbGd_k%nbz,1)-deltaE
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=NONE,ID=ID)
 i_err=io_Double_Grid(Xen,Xk,ID)
 !
 ! clean up
 ! 
 call k_ibz2bz(Xk,'d',.false.)  
 YAMBO_FREE(ene)
 YAMBO_FREE(kpoints)
 YAMBO_FREE(nkpoints)
 YAMBO_FREE(nkpoints_gengrid)
 YAMBO_FREE(startkpoint)
 YAMBO_FREE(recip_lattice)
 YAMBO_FREE(Xk%nbz_mapped)
 YAMBO_FREE(Xk%bz_DbGd_map)
 YAMBO_FREE(Xk%weights_ipol)
 !
end subroutine
