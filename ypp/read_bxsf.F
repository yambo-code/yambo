!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
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
subroutine read_bxsf(Xk,Xen)
 !
 use pars,                ONLY:SP,DP,lchlen,schlen
 use units,               ONLY:pi,HA2EV,BO2ANG
 use R_lattice,           ONLY:bz_samp,b,k_grid
 use D_lattice,           ONLY:alat
 use parser_m,            ONLY:parser
 use YPP,                 ONLY:BZ_RIM_path, wannier_bands, ord_dgrid_ipol
 use electrons,           ONLY:levels,BZ_RIM_table,BZ_RIM_nkpt,&
                               BZ_RIM_max_filling,BZ_RIM_nbands, &
                               n_bands,BZ_RIM_tot_nkpts,BZ_RIM_ipol_weight,n_spin
 use timing,              ONLY:live_timing
 use com,                 ONLY:file_exists,msg,error
 use vec_operate,         ONLY:c2a,rlu_k2bz,k2bz,v_is_zero
 use IO_m,                ONLY:io_control,NONE,OP_WR_CL,OP_RD_CL,io_disconnect 
 !
 implicit none
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 ! 
 character(lchlen) :: filename
 type(bz_samp) :: RIM_k
 logical       :: l_prt_k_grids
 !
 integer :: readunit, ii, jj, readband, iband, ikpt, kx, ky, kz,ID,i_err, ikbz, bxsf_bands
 integer :: nearest_to_g
 integer :: nbands,vband ! total number of bands, highest valence
 integer :: ndim, idim  ! total number of dimensions, iterator on dimensions
 integer, external :: k_the_nearest
 integer, external :: ioE_RIM
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
 character(schlen) :: fname

 real(SP) :: efermitest

 allocate(nkpoints_gengrid(3))
 allocate(nkpoints(3))
 allocate(startkpoint(3))
 
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
 allocate(recip_lattice(ndim,ndim))
 ! set nbands
 !wannier_bands(2)=min(wannier_bands(2),bxsf_bands-wannier_bands(1)+1)
 wannier_bands(2)=min(wannier_bands(2),bxsf_bands)
 nbands=wannier_bands(2)-wannier_bands(1)+1
 !
 allocate(ene(nbands, product(nkpoints)))
 allocate(kpoints( product(nkpoints), ndim))
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
 !============== prepare RIM_k
 ! RIM_k are in reciproc unit cell, not Brillouin zone
 RIM_k%nbz=size(kpoints, 1)
 RIM_k%nibz=RIM_k%nbz
 allocate(RIM_k%ptbz(RIM_k%nbz,3))
 allocate(RIM_k%weights(RIM_k%nibz))
 RIM_k%ptbz = 0_SP
 RIM_k%weights = 1_SP
 ! map read kpoints to BZ 
 do ii=1,RIM_k%nbz
   call rlu_k2bz(v_out=RIM_k%ptbz(ii,:),v_in=kpoints(ii,:))
 enddo
 !
 ! conversion kpoints from reciprocal coord to internal (cartesian-like) coord 
 do ii=1,RIM_k%nbz
   call c2a(v_in=RIM_k%ptbz(ii,:),mode='ka2i')
 enddo !
 ! map kpoints (in iku) to BZ
 do ii=1,RIM_k%nbz
   call k2bz(v_in=RIM_k%ptbz(ii,:))
 enddo
 !
 
 allocate(BZ_RIM_nkpt(Xk%nbz))
 BZ_RIM_nkpt = 0
 BZ_RIM_nbands=nbands
 BZ_RIM_tot_nkpts=RIM_k%nbz
 !
 if (ord_dgrid_ipol .eq. 0) then
   !
   call section('*','Find nearest K-points in BZ')
   ! 
   call live_timing('BZ RIM Tables',RIM_k%nbz*2,SERIAL=.true.)  
   ! 
   ! have to go twice through all elements as BZ_RIM_table cannot be
   ! allocated without knowing the number of maximum elements. 
   !
   do while(.not.allocated(BZ_RIM_table)) 
     if (maxval(BZ_RIM_nkpt)>0) then
       allocate(BZ_RIM_table(Xk%nbz,maxval(BZ_RIM_nkpt)))
       BZ_RIM_table=0
     endif
     BZ_RIM_nkpt=0
     do ii=1,RIM_k%nbz
      !
       ikbz=k_the_nearest(RIM_k%ptbz(ii,:),Xk%ptbz(:,:),Xk%nbz,.FALSE.,i_err)
       BZ_RIM_nkpt(ikbz)=BZ_RIM_nkpt(ikbz)+1
       !
       if (allocated(BZ_RIM_table)) BZ_RIM_table(ikbz,BZ_RIM_nkpt(ikbz))= ii !RIM_k%sstar(i1,1)
       !   
       call live_timing(steps=1)
       !
     enddo
   enddo
   call live_timing()
   ! 
   BZ_RIM_max_filling=maxval(BZ_RIM_nkpt)
   allocate(BZ_RIM_ipol_weight(BZ_RIM_max_filling))
   BZ_RIM_ipol_weight = 1.0_SP
 else
   call section('*','Employ double grid method' )
   call msg('s',':: Perform polynomial interpolation of order :',ord_dgrid_ipol)
   ! fill RIM_table according to double grid technique
   err_dgrid=double_k_grid(Xk,nkpoints,ord_dgrid_ipol)
   if(err_dgrid.ne.0) then
     call msg("s",':: ERROR while preparing double grid. Error:',err_dgrid) 
     return
   endif
   BZ_RIM_max_filling=maxval(BZ_RIM_nkpt)
 endif
 !
 !     
 call msg('s',':: Blocks filling range :',(/minval(BZ_RIM_nkpt),BZ_RIM_max_filling/))
 if(minval(BZ_RIM_nkpt) .ne. BZ_RIM_max_filling) then
   call msg('s',':: RIM blocks are not filled uniformly.')
   if(ord_dgrid_ipol.gt.0)then
     call msg('s',':: Error: For double grid must be filled uniformely.')
     return
   endif
 endif
 !
 call section('*','Create & write BZ_RIM')
 !
 allocate(Xen%E_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_spin))
 Xen%E_RIM(:,:RIM_k%nbz,1)=ene(:,:RIM_k%nbz)/HA2EV+Xen%Efermi(1)
 ! align last (partly) filled band at gamma.
 nearest_to_g=k_the_nearest(RIM_k%ptbz(1,:),Xk%ptbz(:,:),Xk%nbz,.FALSE.,i_err) 
 if(sum((RIM_k%ptbz(1,:)-Xk%ptbz(nearest_to_g,:))**2).gt. epsilon(1._SP)) then
    call msg('s',':: Gamma is not part of your kgrid.')
    call msg('s',':: Probably you use a shifted grid. You should not!')
    call msg('s',':: Gamma :',RIM_k%ptbz(1,:))
    call msg('s',':: Nearest to Gamma :',Xk%ptbz(nearest_to_g,:))
 end if 
 !
 vband=minloc(Xen%f(:,nearest_to_g,1),1)-1
 call msg('s',':: Internal and wannier bands to align (at Gamma):',(/vband,vband+wannier_bands(1)-1/))
 deltaE=ene(vband,1)/HA2EV-Xen%E(vband,nearest_to_g,1)
 Xen%E_RIM(:,:RIM_k%nbz,1)=Xen%E_RIM(:,:RIM_k%nbz,1)-deltaE
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=NONE,ID=ID)
 i_err=ioE_RIM(Xen,ID)
 !
 ! clean up
 ! 
 call k_ibz2bz(Xk,'d',.false.)  
 deallocate(ene)
 deallocate(kpoints)
 deallocate(nkpoints)
 deallocate(nkpoints_gengrid)
 deallocate(startkpoint)
 deallocate(recip_lattice)
 deallocate(BZ_RIM_nkpt,BZ_RIM_table,BZ_RIM_ipol_weight)
 !
end subroutine
