!
! Copyright (C) 2000-2005 M. Bruno  and the SELF team
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
 subroutine pwf(excwf,sp,Xk,Xen)
!==================================
 use pars
 use com,   ONLY:msg
 use BS
 use iom
 use it_m
 use spp_m
 use header
 use preset_m,       ONLY:preset
 implicit none
 type(sprs)       :: sp              !symmetry operations
 type(bz_samp) :: Xk              !k/q points
 type(levels)     :: Xen             !these are needed
 type(excwf_t)    :: excwf           !input file informations
!
!workspace
!---------
 type(kb_pseudo)           :: pp          !atomic positions
 type(iospecs)        :: ppios, ios,ios_xsf
 complex(SP), allocatable :: bsR(:)      !residuals
 real(SP),allocatable     :: bsE(:)      !eigenvalues of bs%K
 character(schlen)    :: local_ch
!
!variables needed to sort eigenvalues
!------------------------------------
 integer,  allocatable :: indx(:)
 integer,  allocatable :: wf_indx(:)
 integer               :: ir, j1
 integer               :: nlines
 integer               :: jinv       !vectors are sorted in ascending order
                                     !I want to print the values in descending order
 integer               :: L, nL      !L=lambda
 integer               :: nLl, nLu   
 integer               :: W          !energy window to be taken into account
 real(SP), allocatable :: R(:)       !square residuals R
 real(SP), allocatable :: Rs(:)      !sorted square residuals Rs
 real(SP)              :: Rmax       !maximum value of the residuals 
!
!variables needed to plot the wavefunction
!-----------------------------------------
 integer               :: iv, ic 
 integer               :: iv_fft
 integer               :: ic_fft
 integer               :: ikbz, ikibz 
 integer               :: is, neh
 integer               :: ir_hole, iL
 integer               :: wf_L       !index of the single particle wavefunction
                                     !entering the excitonic wavefunction
 real(SP)              :: k_pt(3)
 real(SP)              :: r_eh(3)
 complex, allocatable  :: wf_eh(:)
 complex, allocatable  :: wf_vc(:)
 complex, allocatable  :: wf_average(:)
!
!variables needed in the big-grid
!--------------------------------
 integer               :: nrtot      !total number of points in the big-grid
 integer               :: nfft1, nfft2, nfft3 !fft mesh in x, y, z
 integer, allocatable  :: rindex(:)
 integer, allocatable  :: rindexm1(:,:,:,:)
 real(SP), allocatable :: rvecbig(:,:)
!
!imaginary i
!-----------
 complex, parameter    :: cI=(0.,1.)
!
!dummy arguments
!---------------
 integer               :: i1
 real(SP)              :: r1(3)
 real(SP)              :: kdum, kr
#if defined DP
 complex(DP):: zdotu
#else
 complex(SP):: cdotu
#endif
!
!============================================================
!
 call preset(pp); call preset(bs)
 call preset(ios); call preset(ppios)
 wf_ng=ng_closed
!
 call msg("nl"," == Excitonic WF plotting utility ==")
!
!--------------------------------------------
!I get the necessary informations from the db
!--------------------------------------------
!I read the position of the atoms in the unit-cell
 ppios%mode='orhc';ppios%verbose=1
 call io(pp,ppios)
!I read the bs database
 ios%mode='rdhc'
 call io(1,bsE,bsR,ios)
 if (.not.ios%lf) return
 allocate( bsR(BS_K_dim), bsE(BS_K_dim), BS_mat(BS_K_dim,BS_K_dim), BS_eh_table(BS_K_dim,3) )
 ios%mode='rc'
 call io(1,bsE,bsR,ios)
!
!-------------------------------------------------------
!Xcrysden formatted file
!------------------------------------------------------
 call preset(ios_xsf)
 ios_xsf%name='7excWf'
 ios_xsf%mode='ow'
 ios_xsf%form='f'
 call bios(ios_xsf)
 call bioo(ios_xsf)
 ioverb(1)=1
 msgunits(1)=-ios_xsf%unit

 call title(ios_xsf%unit,'#')
 call msg('o1','#',' .-Input file')
 do i1=1,nflines
  if (len_trim(flines(i1))>0) then
   write (local_ch,'(2a)') '| ',trim(flines(i1))
   call msg('o1','#',trim(local_ch))
  endif
 enddo
 call msg('o1','#','')
!
!-------------------------------------------------------
!Ordering of the residuals according to excwf%sort_mode
!------------------------------------------------------
!this part will fill the array indx, according to excwf%sort_mode,
!this array is needed in order to determine the wave-functions to be plotted
!
 L = excwf%lambda       !per chiarezza
 W = excwf%window/100   !now W varies between 0-1
 nLl = 0                !number of states with energy/intensity greater than the maximum
 nLu = 0                !number of states with energy/intensity lower than the maximum
!
 if (excwf%sort_mode == 'e') then
   do j1 = 1, BS_K_dim
     if( (bsE(j1).ge.(1-W)*bsE(L)).and.(bsE(j1).lt.bsE(L)) ) then
       nLl = nLl + 1
     else if ( (bsE(j1).gt.bsE(L)).and.(bsE(j1).le.(1+W)*bsE(L)) ) then
       nLu = nLu + 1
     end if
   end do
   nL = nLl + nLu + 1
!I will load in wf_indx the indexes of the eigenvalues of Hexc to be plotted
   allocate( wf_indx(nL) )
   do j1 = 1, nL
     wf_indx(j1) = L - nLl + j1 -1
   enddo
 else if (excwf%sort_mode == 'i') then
!I will normalize and order the |bsR(:)|^2 to determine the 'intensity'
!of each excitonic transition.
   Rmax=0
   allocate( R(BS_K_dim), Rs(BS_K_dim), indx(BS_K_dim) )
   R(:) = bsR(:)*conjg(bsR(:))
   call sort( BS_K_dim, R, Rs, indx )
   Rmax = maxval(Rs)
   Rs(:) = Rs(:)/Rmax
   do j1 = 1, BS_K_dim
     jinv = BS_K_dim - j1 + 1 
     if( (Rs(jinv).ge.(1-W)*Rs(BS_K_dim-L+1)).and.(Rs(jinv).lt.Rs(BS_K_dim-L+1)) ) then
       nLl = nLl + 1
     else if ( (Rs(jinv).gt.Rs(BS_K_dim-L+1)).and.(Rs(jinv).le.(1+W)*Rs(BS_K_dim-L+1)) ) then
       nLu = nLu + 1
     end if
   end do
   nL = nLl + nLu + 1
!I will load in wf_indx the indexes of the eigenvalues of Hexc to be plotted
   allocate(wf_indx(nL))
   do j1 = 1, nL
     jinv = BS_K_dim - j1 + 1 
     wf_indx(j1) = indx( jinv - (L - nLu -1) )
   end do
   deallocate( R, Rs, indx )
 end if
!
!------------------------------------
!Plot of the excitonic wave-function:
!-----------------------------------
!this part is divided in 5 steps each performing
!a different operation in order to gather the informations
!needed to generate the .xsf file (step 5)
!
!--------------------------------
!1) loading of the wave-functions
!--------------------------------
 wf_k = (/1, Xk%nibz/)
 wf_b = (/BS_bands(1), BS_bands(2)/)
! fft%type = 'r'
! fft%title = ': Exc-wf'
!
! call wfload( 0,1,sp,fft )
!
!-------------------------
!2) creation of a big-grid
!-------------------------
!to evaluate the excitonic wave-function in the 'expanded' zone.
!I will also define arrays of indexes to pass from the 'small' to the big-grid
!
 nrtot = fft_size*excwf%ncell(1)*excwf%ncell(2)*excwf%ncell(3)
 allocate ( rvecbig(3,nrtot), rindex(nrtot) )
 allocate ( rindexm1(fft_size, excwf%ncell(1), excwf%ncell(2), excwf%ncell(3)) )
!
 call expand_grid
!
!---------------------------------------------------
!3) find the position of the hole in the expanded zone
!----------------------------------------------------
!this can be done in a better way, I'll think about it
 kdum = 1000.
 do ir = 1, nrtot
   r1(:) = rvecbig(:,ir) - excwf%r_hole(:)
   if ( abs(dot_product(r1,r1)) .le. kdum ) then
     ir_hole = rindex(ir)
     kdum = abs(dot_product(r1,r1))
   endif
 enddo
 excwf%r_hole(:) = rvecbig(:,ir_hole)
!
!-------------------------------------------------
!4) wave-functions evaluation in the expanded zone
!-------------------------------------------------
 allocate(wf_eh(nrtot), wf_vc(BS_K_dim), wf_average(nrtot))
 wf_eh(:) = (0.0, 0.0)
 wf_vc(:) = (0.0, 0.0)
 wf_average(:) = (0.0, 0.0)
!
!I will take into account a linear combination of excitonic 
!wave-functions weighted with the residuals. So first of all
!I normalize the square of the residuals to their maximun value
!
 allocate( R(BS_K_dim) )
 R(:) = bsR(:)*conjg(bsR(:))
 Rmax = maxval(R)
 R(:) = R(:)/Rmax
!
!loop on the excitonic wavefunctions to be averaged, 
!the size of the loop is related to excwf%window
 do iL = 1, size(wf_indx)
   wf_L = wf_indx(iL)
   do ir = 1, nrtot
     r_eh(:) = rvecbig(:,ir) - excwf%r_hole(:)
     do neh = 1, BS_K_dim
       !these are the indexes I need to evaluate 
       !the single particle wavefunction
       ikbz = BS_eh_table(neh,1)
       iv = BS_eh_table(neh,2)
       ic = BS_eh_table(neh,3)
       ikibz = Xk%sstar(ikbz,1)
       is = Xk%sstar(ikbz,2)
       k_pt(:) = Xk%ptbz(ikbz,:)
       kr = dot_product(r_eh,k_pt)
       !conduction and valence band indexes for a given k-point
       iv_fft = wf_state(iv,ikibz)
       ic_fft = wf_state(ic,ikibz)
       !product of the valence and conduction single particle wavefunctions
       wf_vc(neh) = wf(fft_rot_r(sop_inv(is),rindex(ir)), ic_fft)&
&           *conjg( wf(fft_rot_r(sop_inv(is),ir_hole), iv_fft) )
!====================================================================================
!check trflag in the old version
!check what I should put instead of alat
!=====================================================================================
       wf_vc(neh) = exp(cI*kr*2*pi/alat(1))*wf_vc(neh)
     end do
#if defined DP
     wf_eh(ir) = zdotu(BS_K_dim,BS_mat(1,wf_L),1,wf_vc,1)
#else
     wf_eh(ir) = cdotu(BS_K_dim,BS_mat(1,wf_L),1,wf_vc,1)
#endif
   end do
   wf_average(:) = wf_average(:) + R(wf_L)*wf_eh(:)
 end do
!
!------------------------------------------------------------
!5) writing of the .xsf which can be visualized with XCrysden
!------------------------------------------------------------
 call s2xsf
!
!--------------------------------
!Deallocation of the local arrays
!--------------------------------
 deallocate( R, bsR, bsE, wf_indx, BS_mat, BS_eh_table )
 deallocate( rvecbig, rindex, rindexm1 )
 deallocate( wf_eh, wf_vc, wf_average )
!=======================================================
 contains
!
!--------------------------------------------------
 subroutine expand_grid
!--------------------------------------------------
 !create big grid
 !ws
 implicit none
 integer :: ir, ir1, ir2, ir3, i1, i2, i3, j1, j2, j3
 !
 nfft1 = fft_dim(1); nfft2 = fft_dim(2); nfft3 = fft_dim(3)
 !
 ir = 0
 do ir1 = 0, nfft1*excwf%ncell(1)-1
   do ir2 = 0, nfft2*excwf%ncell(2)-1
     do ir3 = 0, nfft3*excwf%ncell(3)-1
       ir = 1 + ir1 + ir2*nfft1*excwf%ncell(1) + ir3*nfft1*nfft2*excwf%ncell(1)*excwf%ncell(2)
       i1=mod(ir1,nfft1)
       i2=mod(ir2,nfft2)
       i3=mod(ir3,nfft3)
       j1=ir1/nfft1
       j2=ir2/nfft2
       j3=ir3/nfft3
       rindex(ir)=i1+nfft1*i2+nfft1*nfft2*i3+1
       rindexm1(rindex(ir),1+j1,1+j2,1+j3)=ir
       rvecbig(1,ir) = ir1*a(1,1)/nfft1 + ir2*a(2,1)/nfft2 + ir3*a(3,1)/nfft3
       rvecbig(2,ir) = ir1*a(1,2)/nfft1 + ir2*a(2,2)/nfft2 + ir3*a(3,2)/nfft3
       rvecbig(3,ir) = ir1*a(1,3)/nfft1 + ir2*a(2,3)/nfft2 + ir3*a(3,3)/nfft3
     end do
   end do
 end do
 end subroutine
!--------------------------------------------------------
 subroutine s2xsf
!--------------------------------------------------------
!I write the .xsf file, so I will be able to plot
!the excitonic wave-functions using XCrysden
!
 implicit none
!
 integer               :: i, i1, i2, i3
 integer               :: ir, ir1, ir2, ir3
 integer               :: n1, n2, n3
 real(SP), allocatable :: wf3d(:,:,:)
 real(SP)              :: a_angs(3,3)    !lattice vectors in Angstrom
 real(SP)              :: wf_max = 0.
!
!introduced just for chiarezza
 n1 = fft_dim(1)*excwf%ncell(1)
 n2 = fft_dim(2)*excwf%ncell(2)
 n3 = fft_dim(3)*excwf%ncell(3)
!
!the atomic cell is given in a.u. and I want
!to write in the output of xcrysden the cell and the
!coordinates of the atoms in Angstrom (remember bohr=0.529*A)
 a_angs(:,:) = 0.529*a(:,:)
!
!this is the 3d wavefunction
 allocate( wf3d(n1, n2, n3) )
 wf3d(:,:,:)=(0.d0)
!
!first of all I evaluate the maximum of wf3d in order to normalize it,
!so I will not have problems when I plot it
 ir = 0
 do ir1 = 0, n1-1
   do ir2 = 0, n2-1
     do ir3 = 0, n3-1
       ir = 1 + ir1 + ir2*n1 + ir3*n1*n2
       wf3d(ir1+1, ir2+1, ir3+1) = wf_average(ir)*conjg(wf_average(ir))
       if (wf3d(ir1+1, ir2+1, ir3+1) .ge. wf_max) wf_max = (wf3d(ir1+1,ir2+1,ir3+1))
     enddo
   enddo
 enddo
 wf3d(:,:,:) = wf3d(:,:,:)/wf_max 
!
! WRITING ...
!
!This is the output file
 call msg('o1','CRYSTAL')
 call msg('o1','PRIMVEC')
 call msg('o1','',excwf%ncell(1)*a_angs(1,:))
 call msg('o1','',excwf%ncell(2)*a_angs(2,:))
 call msg('o1','',excwf%ncell(3)*a_angs(3,:))
 call msg('o1','PRIMCOORD')
 call msg('o1','',(/n_atoms_max*excwf%ncell(1)*excwf%ncell(2)*excwf%ncell(3),1/))
!write the translated atoms of the cell
 do i=1,n_atoms_max
   do i1=0,excwf%ncell(1)-1
     do i2=0,excwf%ncell(2)-1
       do i3=0,excwf%ncell(3)-1
         write (ios_xsf%unit,'(i4,3f20.10)')  14,&
&          atom_pos(1,i,1)*0.529+i1*a_angs(1,1)+i2*a_angs(1,2)+i3*a_angs(1,3),&
&          atom_pos(2,i,1)*0.529+i1*a_angs(2,1)+i2*a_angs(2,2)+i3*a_angs(2,3),&
&          atom_pos(3,i,1)*0.529+i1*a_angs(3,1)+i2*a_angs(3,2)+i3*a_angs(3,3)
       end do
     end do
   end do
 end do
 call msg('o1','BEGIN_BLOCK_DATAGRID_3D')
 write (local_ch,'(a,3(i2.2,a))') 'Generated with SELF ',code_version(1),'.',code_version(2),'.',code_version(3)
 call msg('o1',local_ch)
 call msg('o1','BEGIN_DATAGRID_3D')
 call msg('o1','',(/n1,n2,n3/))
 call msg('o1','',(/0._dt,0._dt,0._dt/))
 call msg('o1','',excwf%ncell(1)*a_angs(1,:))
 call msg('o1','',excwf%ncell(2)*a_angs(2,:))
 call msg('o1','',excwf%ncell(3)*a_angs(3,:))
!plot of the 3d wavefunction
 ir = 0
 do ir1 = 0, n1-1
   do ir2 = 0, n2-1
     do ir3 = 0, n3-1
       ir = 1 + ir1 + ir2*n1 + ir3*n1*n2
       call msg('o1','',wf3d(ir1+1,ir2+1,ir3+1))
     enddo
   enddo
   call msg('o1','')
 enddo
 !
 call msg('o1','','END_DATAGRID_3D')
 call msg('o1','','END_BLOCK_DATAGRID_3D')
 !
 call bioc(ios_xsf)
 deallocate(wf3d)
 end subroutine
!===============================================================
!
 end subroutine
