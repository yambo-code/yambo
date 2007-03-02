!
! Copyright (C) 2000-2005 D. Varsano and the SELF team
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
 subroutine plot_exc_wf(Xk)
!=======================================
 use FFT_m,       ONLY : fft_size,fft_dim,fft_rot_r
 use pars,        ONLY : SP,HARTREE,IP,pi,lchlen,DP
 use D_lattice,   ONLY : a,sop_inv,i_time_rev,nsym,alat
 use wave_func,   ONLY : wf_load,wf_state,wf_ng,wf,WF_free
 use R_lattice,   ONLY : nXkibz,bz_samp
 use SPP,         ONLY : action_mode,ncell,lambda,window,r_hole,sort_mode,&
 &                      max_energy,p_format,p_dim,p_dir
 use IO_m,        ONLY : io_control,OP_RD_CL,DUMP,NONE,REP,VERIFY
 use BS,          ONLY : BS_K_dim,BS_bands,BS_q,BSS_n_descs,BSS_description,&
 &                       BS_eh_table,BS_mat,BS_K_coupling,BS_cpl_mode,&
 &                       BS_cpl_K_exchange,BS_cpl_K_corr
 use vec_operate, ONLY: sort
 use com,         ONLY: msg,of_open_close,error
 use X_m,         ONLY: X_t
 use memory_m,    ONLY: mem_est
 use timing,      ONLY: live_timing
 
 implicit none
 type(X_t)     ::Xbsk
 type(bz_samp) ::k,Xk,q
 
!
!parameters GPL_END
!-----------
 real(SP),     parameter    :: bohr=0.52917721
 complex(SP),  parameter    :: cI=(0.,1.)
!
!variables needed in the grid
!--------------------------------
 integer               :: ir,ir_hole
 integer               :: nfft1, nfft2, nfft3 !fft mesh in x, y, z
 integer               :: nr1,nr2,nr3,nrtot !total number of points in the big-grid
 integer, allocatable  :: rindex(:)
 integer, allocatable  :: rindexm1(:,:,:,:)
 real(SP), allocatable :: rvecbig(:,:)
!Dummy arguments 
 real(SP)              :: kdum,kr
 real(SP)              :: r1(3)
#if defined _DOUBLE
 complex(DP):: zdotu
#else
 complex(SP):: cdotu
#endif

!
! I/0
!------
 integer           ::io_err,ID
 integer, external ::ioBS_diago,ioBS

!
!Energies and Residulas
!----------------------
 complex(SP), allocatable ::BS_R(:)
 complex(SP), allocatable ::BS_E(:)
 integer iq
!
!Variables needed to plot the exc-wf
!-----------------------------------
 integer           ::iv,ic,iv_fft,ic_fft
 integer           ::ikbz,ikibz
 integer           ::is,neh
 real(SP)          ::k_pt(3)
 real(SP)          ::r_eh(3)
 complex(SP),  allocatable ::wf_vc(:)
 complex(SP),  allocatable ::wf_eh(:)
 real(SP),     allocatable ::wf2d(:,:)
!
!Variables needed to the report of eigenvectors
!-----------------------------------
 real(SP), allocatable ::A_weight(:)
 integer           ::j1
!
!-------------
 integer io_ID
!-----------------------
!
! Outputs
!
 character(lchlen) :: file_name
 character(lchlen) :: local_ch
 character(5)      :: extension
!
!--------Here I read information from the BS database
!
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=ioBS(1,Xbsk,io_ID)
 if (BS_cpl_K_exchange) BS_cpl_mode='x'
 if (BS_cpl_K_corr) BS_cpl_mode='c'
 if (BS_cpl_K_exchange.and.BS_cpl_K_corr) BS_cpl_mode='xc'
 if (BS_K_coupling) BS_K_dim=2*BS_K_dim
!
! Allocation of energies and residuum, then also eigenvector
!

 allocate(BS_R(BS_K_dim),BS_E(BS_K_dim))
 iq=BS_q(1)

!
!Description of the BS Kernel
 call K_descriptions(iq,q,Xbsk)
!

!
! Here I read energies and residuum
!

 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=ioBS_diago(iq,BS_K_dim,BS_E,BS_R,io_ID)


 if (action_mode == 's' ) then 
  call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
  io_err=ioBS_diago(iq,BS_K_dim,BS_E,BS_R,ID)
  call sort_energies(BS_R,BS_E)
  return
 else if (action_mode == 'p') then
  if (BS_K_coupling) call error('When coupling is present only sort mode works')
  call section('*','Excitonic Wave Function')
!===================================================
!    Loading tables and eigenvectors
!===================================================
  allocate (BS_eh_table(BS_K_dim,3),BS_mat(BS_K_dim,BS_K_dim))
  call mem_est("BS_eh_table",(/3*BS_K_dim/),(/IP/))
  call mem_est('BS_mat',(/BS_K_dim**2/))
 
  call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
  io_err=ioBS_diago(iq,BS_K_dim,BS_E,BS_R,ID)
  

 
!=============================================
!            Loading  Wf 
!=============================================
! 
 call wf_load(wf_ng,1,BS_bands,(/1,nXkibz/),space='R',title='-EXCWF')
!

!=============================================
!              Constructing Grid 
!=============================================

 call section('+','Expanding grid')
 call expand_grid()

!=============================================
!        Find hole in the big grid
! The hole is translated from the unit cell to the 
!         cell in the center of the big grid.
!=============================================
 
!
! Translation of the hole
! 
 r_hole(1)=r_hole(1)+a(1,1)*int(ncell(1)/2)+a(2,1)*int(ncell(2)/2)+&
&          a(3,1)*int(ncell(3)/2)
 r_hole(2)=r_hole(2)+a(1,2)*int(ncell(1)/2)+a(2,2)*int(ncell(2)/2)+&
&          a(3,2)*int(ncell(3)/2)
 r_hole(3)=r_hole(3)+a(1,3)*int(ncell(1)/2)+a(2,3)*int(ncell(2)/2)+&
&          a(3,3)*int(ncell(3)/2)

!
!Find the index of the traslated hole
! 
 kdum = 1000.
 do ir = 1, nrtot
   r1(:) = rvecbig(:,ir) - r_hole(:)
   if ( abs(dot_product(r1,r1)) .le. kdum ) then
     ir_hole = ir 
     kdum = abs(dot_product(r1,r1))
   endif
 enddo
 r_hole(:) = rvecbig(:,ir_hole)
 call msg('s',':: Hole placed at [a.u.]          :',r_hole)
!==================================================================
!                 Constructionof the Exc-Wf
!==================================================================
  call section('=','Excitonic Wave function costruction')
!Allocation
     
  allocate(wf_vc(BS_K_dim),wf_eh(nrtot))
  call k_ibz2bz(Xk,'i',.false.)

  call live_timing('Exc-Wfs',nrtot)  
  do ir=1,nrtot
   r_eh(:)=rvecbig(:,ir)-r_hole(:)
   do neh = 1,BS_K_dim
    ikbz = BS_eh_table(neh,1)
    iv = BS_eh_table(neh,2)
    ic = BS_eh_table(neh,3)
    ikibz = Xk%sstar(ikbz,1)
    is = Xk%sstar(ikbz,2)
    k_pt(:) = Xk%ptbz(ikbz,:)
    k_pt(:)=k_pt(:)/alat(:)
    kr = dot_product(r_eh,k_pt)
    iv_fft = wf_state(iv,ikibz,1)
    ic_fft = wf_state(ic,ikibz,1)
    wf_vc(neh) = wf(fft_rot_r(sop_inv(is),rindex(ir)), ic_fft)&
&           *conjg( wf(fft_rot_r(sop_inv(is),rindex(ir_hole)), iv_fft) )
    if (is > nsym/(i_time_rev+1)) wf_vc(neh) = conjg(wf_vc(neh))
    wf_vc(neh) = exp(cI*kr*2*pi)*wf_vc(neh)

   enddo  !Matrix elements
#if defined _DOUBLE
     wf_eh(ir) = zdotu(BS_K_dim,BS_mat(1,lambda),1,wf_vc,1)
#else
     wf_eh(ir) = cdotu(BS_K_dim,BS_mat(1,lambda),1,wf_vc,1)
#endif
  call live_timing(steps=1)
  enddo   !grid points

 call live_timing



!report on file of the weights A_cvk of excitonic wf

  allocate(A_weight(BS_K_dim))

  write(extension,'(a,i2.2)') '-',lambda
  write(file_name,'(a,a)') 'weight',trim(extension)
  call of_open_close(file_name,'ot')
  write(extension,'(a,i2.2)')'o -',lambda
  do j1=1,BSS_n_descs
   call msg(extension,"",trim(BSS_description(j1)),INDENT=0)
  enddo
  call msg(extension,'#')
  write(local_ch,'(a,i2.2,a)') 'Electron-Hole pairs that contributes &
                          to Excitonic State ',lambda,' more than 5%'
  call msg(extension,'#',local_ch,INDENT=0,USE_TABS=.TRUE.)
  call msg(extension,'#')
  write(local_ch,'(8x,a,6x,a,5x,a,8x,a,1x,a,6x,a)') 'Band_V','Band_C',&
                                        ' K_ibz','Sym','|','Weight '
  call msg(extension,'#',local_ch,INDENT=0,USE_TABS=.TRUE.)
  do neh = 1,BS_K_dim
   ikbz = BS_eh_table(neh,1)
   iv = BS_eh_table(neh,2)
   ic = BS_eh_table(neh,3)
   ikibz = Xk%sstar(ikbz,1)
   is = Xk%sstar(ikbz,2)
   A_weight(neh)=(BS_mat(neh,lambda)*conjg(BS_mat(neh,lambda)))
   if (A_weight(neh)>=5e-2) then
    write(local_ch,'(i,i,i,i,a,f)') iv,ic,ikibz,is,'  |',A_weight(neh)
    call msg(extension,'',local_ch,USE_TABS=.FALSE.)
   endif
  enddo
  call of_open_close(file_name) 
!deallocation
 deallocate(BS_eh_table,BS_mat,wf_vc,A_weight)
!PLOT

 call plot_driver()

!
! Deallocation
!
 deallocate(rvecbig,rindex,rindexm1)
 deallocate (wf_eh)
 call WF_free()
 endif


! Deallocation
 deallocate(BS_R,BS_E)
  




!
 contains
!===================
!
 subroutine expand_grid()
!--------------------------------------------------
!
!ws
!--
 implicit none
 integer ::  ir1, ir2, ir3, i1, i2, i3, j1, j2, j3


 nrtot = fft_size*ncell(1)*ncell(2)*ncell(3)
 allocate ( rvecbig(3,nrtot), rindex(nrtot) )
 allocate ( rindexm1(fft_size, ncell(1), ncell(2), ncell(3)) )
!
 nfft1 = fft_dim(1); nfft2 = fft_dim(2); nfft3 = fft_dim(3)
 nr1=ncell(1)*nfft1
 nr2=ncell(2)*nfft2
 nr3=ncell(3)*nfft3
!
!
 ir = 0
 do ir1 = 0, nr1-1
   do ir2 = 0, nr2-1
     do ir3 = 0, nr3-1
       ir = 1 + ir1 + ir2*nr1 + ir3*nr1*nr2
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


 subroutine sort_energies(BS_R,BS_E)
!----------------------------------------------
 implicit none 
 
 complex(SP) :: BS_R(BS_K_dim),BS_E(BS_K_dim)

!ws
!--
 integer j1,j2
 integer,  allocatable :: indx(:)
 real(SP), allocatable :: R(:)       !square residuals R
 real(SP), allocatable :: Rs(:),Rs2(:) !sorted square residuals Rs
 real(SP)              :: Rmax       !maximum value of the residuals

!
! Eventually in this subroutine window stuff
!

 
 call section('*','Sorting Energies and Strengths')
 
 if (sort_mode == 'e')   extension='-e'
 if (sort_mode == 'i')   extension='-i'
 write(file_name,'(a,a)') 'exc_sort',trim(extension)
 call of_open_close(file_name,'ot')
 if (sort_mode == 'e')   extension='o -e'
 if (sort_mode == 'i')   extension='o -i'
 
 
 do j1=1,BSS_n_descs
   call msg(extension,"",trim(BSS_description(j1)),INDENT=0)
 enddo
 call msg(extension,'#')
 write(local_ch,'(a)') '     E/ev[1]     Strength     |       Index '
 call msg(extension,'#',local_ch,INDENT=0,USE_TABS=.TRUE.)
 Rmax=0
 allocate( R(BS_K_dim), Rs(BS_K_dim),Rs2(BS_K_dim))
 R(:) = BS_R(:)*conjg(BS_R(:))
 Rmax = maxval(R)
 Rs(:) = R(:)/Rmax

 if (sort_mode == 'e') then
!
  do j1=1,BS_K_dim
   if (real(BS_E(j1))<=max_energy) then 
    write(local_ch,'(f,f,a,i)')real(BS_E(j1))*HARTREE ,Rs(j1),' |',j1
!Ask to Andrea if possbile to mix real and integere in msg   
    call msg(extension,'',local_ch,USE_TABS=.TRUE.)
   endif
  enddo

 else if (sort_mode == 'i') then
!
  allocate (indx(BS_K_dim))
  call sort( Rs,Rs2,indx)
  do j1=1,BS_K_dim
   j2=BS_K_dim-j1+1
   if (real(BS_E(indx(j2)))>=0.and.real(BS_E(indx(j2)))<=max_energy) then
    write(local_ch,'(f,f,a,i)')real(BS_E(indx(j2)))*HARTREE ,&
&                               Rs2(j2),' |',indx(j2)
    call msg(extension,'',local_ch,USE_TABS=.FALSE.)
   endif
  enddo 
 endif

  
 call of_open_close(file_name)
 deallocate(R, Rs,Rs2)
 if (allocated(indx)) deallocate(indx)

 end subroutine


 subroutine plot_driver()
!--------------------------------------
 implicit none
  
 
 if (p_dim==1) then
  if (p_format=='x') call error('For 1d plot you do not need xcrysden')
  call plot_gnu()
!
 elseif (p_dim==2) then   
  call plot_gnu() !here I just integrate
  if (p_format=='x') call plot_xcrysden()  
!
 elseif (p_dim==3) then  
  if (p_format=='g')  call error&
&              ('3d plot cannot be visualised with gnuplot')

  call plot_xcrysden()
 else 
  call error('Wrong dimension in input')  
 endif
 end subroutine


 subroutine plot_gnu()
!-----------------------------
 implicit none
!
!output
!------

 character(lchlen), allocatable :: title(:)
 real(SP), allocatable       ::od(:)
!
!ws
!---
 integer i1,ir1,ir2,ir3
 real(SP) rp(3),rplot1,rplot2
 real(SP), allocatable ::wf1d(:)
 real(SP)              :: wf_max = 0.
   
 call live_timing('plot',nr1)
!
!Open the output file
!
 if (p_format=='g') then
  write(extension,'(a,a)') '-',p_dir
  write(file_name,'(a,i2.2,a)') 'exc_wf',lambda,trim(extension)
  call of_open_close(file_name,'ot')
 endif
!
!titles
!  
 if (p_dim==1) then
  allocate (title(2),od(2))
! 
  if (p_dir=='x') then
   allocate (wf1d(nr1)) 
   extension='o -x'
   title(1)  ='a1 proj [A]'
  elseif (p_dir=='y') then
   allocate (wf1d(nr2)) 
   extension='o -y'
   title(1)  ='a2 proj [A]'
  elseif (p_dir=='z') then
   allocate (wf1d(nr3)) 
   extension='o -z'
   title(1)  ='a3 proj [A]'
  else
   call error('Check the Directions')
  endif
!
  do i1=1,BSS_n_descs
   call msg(extension,"",trim(BSS_description(i1)),INDENT=0)
  enddo
  call msg(extension,'#')
 
  title(2)='|wf|^2'
  call msg(extension,'#',title,INDENT=0,USE_TABS=.TRUE.)

!wf1d
  wf1d(:)=(0.)
!
  if (p_dir=='x') then
   ir=0
   do ir1 = 0,nr1-1
    do ir2 = 0,nr2-1
     do ir3 = 0,nr3-1
      ir = 1 + ir1 + ir2*nr1 + ir3*nr1*nr2
      wf1d(ir1+1)=wf1d(ir1+1)+abs(wf_eh(ir))**2
     enddo
    enddo
    call live_timing(steps=1)
   enddo
   
   do ir1=0,nr1-1
    rp(:)=ir1*a(1,:)/nfft1
    rplot1 = (sqrt(dot_product(rp,rp))-r_hole(1))*bohr
    od(1)=rplot1
    od(2)=abs(wf1d(ir1+1))
    call msg(extension,'',od,INDENT=0,USE_TABS=.TRUE.)
   enddo
 
  elseif (p_dir=='y') then !ydir
   ir=0
   do ir1 = 0,nr1-1
    do ir2 = 0,nr2-1
     do ir3 = 0,nr3-1
      ir = 1 + ir1 + ir2*nr1 + ir3*nr1*nr2
      wf1d(ir2+1)=wf1d(ir2+1)+abs(wf_eh(ir))**2
     enddo
    enddo
    call live_timing(steps=1)
   enddo
   
   do ir2=0,nr2-1
    rp(:)=ir2*a(2,:)/nfft2
    rplot1 = (sqrt(dot_product(rp,rp))-r_hole(2))*bohr
    od(1)=rplot1
    od(2)=abs(wf1d(ir2+1))
    call msg(extension,'',od,INDENT=0,USE_TABS=.TRUE.)
   enddo
  
  elseif (p_dir=='z') then !zdir
   ir=0
   do ir1 = 0,nr1-1
    do ir2 = 0,nr2-1
     do ir3 = 0,nr3-1
      ir = 1 + ir1 + ir2*nr1 + ir3*nr1*nr2
      wf1d(ir3+1)=wf1d(ir3+1)+abs(wf_eh(ir))**2
     enddo
    enddo
    call live_timing(steps=1)
   enddo
   
   do ir3=0,nr3-1
    rp(:)=ir3*a(3,:)/nfft3
    rplot1 = (sqrt(dot_product(rp,rp))-r_hole(3))*bohr
    od(1)=rplot1
    od(2)=abs(wf1d(ir3+1))
    call msg(extension,'',od,INDENT=0,USE_TABS=.TRUE.)
   enddo
  endif  !if on firections
! 
  call of_open_close(file_name) 
  deallocate(title,od,wf1d)

!wf2d
 else  ! p_dim=2
!
  if (p_dir=='xy') then 
   allocate (wf2d(nr1,nr2))
  elseif (p_dir=='xz') then 
    allocate (wf2d(nr1,nr3))
  elseif (p_dir=='yz') then 
    allocate (wf2d(nr2,nr3))
  else 
   call error('Check the Direction')
  endif
!
!titles
! 

  if (p_format=='g') then
   allocate (title(3),od(3)) 
   if (p_dir=='xy') then 
    extension='o -xy'
    title(1)  ='a1 proj [A]'
    title(2)  ='a2 proj [A]'
   elseif (p_dir=='xz') then 
    extension='o -xz'
    title(1)  ='a1 proj [A]'
    title(2)  ='a3 proj [A]'
   elseif (p_dir=='yz') then 
    extension='o -yz'
    title(1)  ='a2 proj [A]' 
    title(2)  ='a3 proj [A]'
   endif
!
   do i1=1,BSS_n_descs
    call msg(extension,"",trim(BSS_description(i1)),INDENT=0)
   enddo
   title(3)='|wf|^2'
  endif
!
  wf2d(:,:)=(0.) 
!
  if (p_dir=='xy') then !xy plane
   ir = 0
   do ir1 = 0,nr1-1
    do ir2 = 0,nr2-1
     do ir3 = 0, nr3-1
      ir = 1 + ir1 + ir2*nr1 + ir3*nr1*nr2
      wf2d(ir1+1,ir2+1)=wf2d(ir1+1,ir2+1)+abs(wf_eh(ir))**2
     enddo
    enddo
    call live_timing(steps=1)
   enddo

   if (p_format=='x') return
   wf_max=maxval(wf2d)
   wf2d(:,:)=wf2d(:,:)/wf_max
   write(local_ch,'(a,e)') 'Max value of the ExcWfs:  ',wf_max
   call msg(extension,'#',local_ch,INDENT=0,USE_TABS=.TRUE.)
   call msg(extension,'#')
   call msg(extension,'#',title,INDENT=0,USE_TABS=.TRUE.)
   do ir1=0,nr1-1
    do ir2=0,nr2-1
     rp(:)=ir1*a(1,:)/nfft1
     rplot1=(sqrt(dot_product(rp,rp))-r_hole(1))*bohr      
     rp(:)=ir2*a(2,:)/nfft2
     rplot2=(sqrt(dot_product(rp,rp))-r_hole(2))*bohr
     od(1)=rplot1
     od(2)=rplot2 
     od(3)=abs(wf2d(ir1+1,ir2+1))
     call msg(extension,'',od,INDENT=0,USE_TABS=.TRUE.)
    enddo
    call msg(extension,'')
   enddo
!
   elseif (p_dir=='xz') then   !xz plane
!
   ir = 0
   do ir1 = 0,nr1-1
    do ir2 = 0,nr2-1
     do ir3 = 0, nr3-1
      ir = 1 + ir1 + ir2*nr1 + ir3*nr1*nr2
      wf2d(ir1+1,ir3+1)=wf2d(ir1+1,ir3+1)+abs(wf_eh(ir))**2
     enddo
    enddo
    call live_timing(steps=1)
   enddo
!
   if (p_format=='x') return
   wf_max=maxval(wf2d)
   wf2d(:,:)=wf2d(:,:)/wf_max
   write(local_ch,'(a,e)') 'Max value of the ExcWfs:  ',wf_max
   call msg(extension,'#',local_ch,INDENT=0,USE_TABS=.TRUE.)
   call msg(extension,'#')
   call msg(extension,'#',title,INDENT=0,USE_TABS=.TRUE.)
   do ir1=0,nr1-1
    do ir3=0,nr3-1
     rp(:)=ir1*a(1,:)/nfft1
     rplot1=sqrt(dot_product(rp,rp))*bohr
     rp(:)=ir3*a(3,:)/nfft3
     rplot2=sqrt(dot_product(rp,rp))*bohr
     od(1)=rplot1
     od(2)=rplot2 
     od(3)=abs(wf2d(ir1+1,ir3+1))
     call msg(extension,'',od,INDENT=0,USE_TABS=.TRUE.)
    enddo
    call msg(extension,'')
   enddo
 !
  elseif (p_dir=='yz') then !yz plane
   ir = 0
   do ir1 = 0,nr1-1
    do ir2 = 0,nr2-1
     do ir3 = 0, nr3-1
      ir = 1 + ir1 + ir2*nr1 + ir3*nr1*nr2
      wf2d(ir2+1,ir3+1)=wf2d(ir2+1,ir3+1)+abs(wf_eh(ir))**2
     enddo
    enddo
    call live_timing(steps=1)
   enddo

   if (p_format=='x') return
   wf_max=maxval(wf2d)
   wf2d(:,:)=wf2d(:,:)/wf_max
   write(local_ch,'(a,e)') 'Max value of the ExcWfs:  ',wf_max
   call msg(extension,'#',local_ch,INDENT=0,USE_TABS=.TRUE.)
   call msg(extension,'#')
   call msg(extension,'#',title,INDENT=0,USE_TABS=.TRUE.)
   do ir2=0,nr2-1
    do ir3=0,nr3-1
     rp(:)=ir2*a(2,:)/nfft2
     rplot1=sqrt(dot_product(rp,rp))*bohr
     rp(:)=ir3*a(3,:)/nfft3
     rplot2=sqrt(dot_product(rp,rp))*bohr
     od(1)=rplot1
     od(2)=rplot2 
     od(3)=abs(wf2d(ir2+1,ir3+1))
     call msg(extension,'',od,INDENT=0,USE_TABS=.TRUE.)
    enddo
    call msg(extension,'')
   enddo
!
  endif !if on p_dir
!
  call of_open_close(file_name)
!
  if (p_format=='g') deallocate(title,od,wf2d)
!  
 endif !if on p_dim
!

 call live_timing 
 end subroutine

 subroutine plot_xcrysden()
!-----------------------------
 use com,         ONLY : code_version 
 use D_lattice,   ONLY : n_atom_species,n_atoms,atom_pos,znucl
 implicit none
!
!outputs
!
 
 character(lchlen), allocatable :: title(:)
 character(10)     :: extension2
 real(SP)          ::od(3)
!
!atomic structures
!
 integer               :: natom,ntype
 integer               :: n_atoms_tot
!
!ws
!
 integer               :: ir, ir1, ir2, ir3, i1, i2, i3
 real(SP), allocatable :: wf3d(:,:,:)
 real(SP)              :: a_angs(3,3)    !lattice vectors in Angstrom
 real(SP)              :: wf_max = 0.
!
 

! TO DO
! This subroutine has not yet been well tested 
! on non-cubic cell

!
!the atomic cell is given in a.u. and I want
!to write the output of xcrysden in \AA
 a_angs(:,:) = a(:,:)*bohr
!
!this is the 3d wavefunction

 if(p_dim==3) then
 allocate( wf3d(nr1, nr2, nr3) )
 wf3d(:,:,:)=(0.d0)
!
 ir = 0
 do ir1 = 0, nr1-1
   do ir2 = 0, nr2-1
     do ir3 = 0, nr3-1
       ir = 1 + ir1 + ir2*nr1 + ir3*nr1*nr2
       wf3d(ir1+1, ir2+1, ir3+1) = wf_eh(ir)*conjg(wf_eh(ir))
     enddo
   enddo
 enddo
 endif
!
!Open the output file
!
 if (p_dim==3) extension2='-3d.xsf'
 if (p_dim==2) write(extension2,'(a,a,a)') '-2d',p_dir,'.xsf'
 write(file_name,'(a,i2.2,a)') 'exc_wf',lambda,trim(extension2)
 extension2='o .xsf'
 call of_open_close(file_name,'ot')
 
   
  n_atoms_tot=0
  do ntype=1,n_atom_species
   n_atoms_tot=n_atoms_tot+n_atoms(ntype)
  enddo
 

!
! WRITING ...
!
!This is the output file common at both grids
 call msg(extension2,'CRYSTAL')
 call msg(extension2,'PRIMVEC')
 call msg(extension2,'',ncell(1)*a(1,:)*bohr)
 call msg(extension2,'',ncell(2)*a(2,:)*bohr)
 call msg(extension2,'',ncell(3)*a(3,:)*bohr)
 call msg(extension2,'PRIMCOORD')
 call msg(extension2,'',(/n_atoms_tot*ncell(1)*ncell(2)*ncell(3),1/))

!write the translated atoms of the cell
! 
 do ntype=1,n_atom_species
  do natom=1,n_atoms(ntype)
   do i1=0,ncell(1)-1
    do i2=0,ncell(2)-1
     do i3=0,ncell(3)-1
      od(1)=atom_pos(1,natom,ntype)*bohr+i1*a_angs(1,1)+i2*a_angs(1,2)+i3*a_angs(1,3)
      od(2)=atom_pos(2,natom,ntype)*bohr+i1*a_angs(2,1)+i2*a_angs(2,2)+i3*a_angs(2,3)
      od(3)=atom_pos(3,natom,ntype)*bohr+i1*a_angs(3,1)+i2*a_angs(3,2)+i3*a_angs(3,3)   
      write(local_ch,'(i,f,f,f)') znucl(ntype),od(1),od(2),od(3)
      call msg(extension2,'',local_ch,INDENT=0,USE_TABS=.FALSE.)
     enddo
    enddo
   enddo
  enddo
 enddo

!deallocation --> allocated in ioDB1
 deallocate(n_atoms,atom_pos,znucl)
!
!grids
!
 if (p_dim==3) then  !3d grid
  call msg(extension2,'BEGIN_BLOCK_DATAGRID_3D')
  write (local_ch,'(a,3(i2.2,a))') 'Generated with SELF ',&
&                  code_version(1),'.',code_version(2),'.',code_version(3)
  call msg(extension2,local_ch)
  call msg(extension2,'BEGIN_DATAGRID_3D')
  call msg(extension2,'',(/nr1,nr2,nr3/))
  call msg(extension2,'',(/0._SP,0._SP,0._SP/))
  call msg(extension2,'',ncell(1)*a_angs(1,:))
  call msg(extension2,'',ncell(2)*a_angs(2,:))
  call msg(extension2,'',ncell(3)*a_angs(3,:))
!
 elseif (p_dim==2) then   !2D grid 
!
  call msg(extension2,'BEGIN_BLOCK_DATAGRID_2D')
  write (local_ch,'(a,3(i2.2,a))') 'Generated with SELF ',&
                   code_version(1),'.',code_version(2),'.',code_version(3)
  call msg(extension2,local_ch)
  call msg(extension2,'BEGIN_DATAGRID_2D')
!
!here it depends on the plane
!number of data-points in each direction 
!
  if (p_dir=='xy') call msg(extension2,'',(/nr1,nr2/)) 
  if (p_dir=='xz') call msg(extension2,'',(/nr1,nr3/)) 
  if (p_dir=='yz') call msg(extension2,'',(/nr2,nr3/))
!origin of the datagrid
  if (p_dir=='xy') call msg(extension2,'',ncell(3)*a_angs(3,:)/2)
  if (p_dir=='xz') call msg(extension2,'',ncell(2)*a_angs(2,:)/2) 
  if (p_dir=='yz') call msg(extension2,'',ncell(1)*a_angs(1,:)/2)
!first spanning vector of the datagrid
  if (p_dir=='xy'.or.p_dir=='xz') call msg(extension2,'',ncell(1)*a_angs(1,:))
  if (p_dir=='yz') call msg(extension2,'',ncell(2)*a_angs(2,:))
!second spanning vector of the datagrid
  if (p_dir=='xy')call msg(extension2,'',ncell(2)*a_angs(2,:))
  if (p_dir=='xz'.or.p_dir=='yz') call msg(extension2,'',ncell(3)*a_angs(3,:))
 endif

 if (p_dim==3) then  !plot of the 3d wf

  wf_max=maxval(wf3d) 
  wf3d(:,:,:) = wf3d(:,:,:)/wf_max !normalization of wf3d

  do ir3 = 0, nr3-1
   do ir2 = 0, nr2-1
    do ir1 = 0, nr1-1
     call msg(extension2,'',wf3d(ir1+1,ir2+1,ir3+1))
    enddo
   enddo
   call msg(extension2,'')
  enddo
  deallocate(wf3d)
 endif


 if (p_dim==2) then ! plot of the 2d wf 

 wf_max=maxval(wf2d)
 wf2d(:,:) = wf2d(:,:)/wf_max !normalization
  if (p_dir=='xy') then  !xy plane
   do ir2=1,nr2 
    do ir1=1,nr1
     call msg(extension2,'',wf2d(ir1,ir2))
    enddo
   enddo
!
  elseif (p_dir=='xz') then !xz plane
! 
  do ir3=1,nr3 
    do ir1=1,nr1
     call msg(extension2,'',wf2d(ir1,ir3))
    enddo
   enddo
!
  elseif (p_dir=='yz') then !yz plane
!
   do ir3=1,nr3 
    do ir2=1,nr2
     call msg(extension2,'',wf2d(ir2,ir3))
    enddo
   enddo  
  endif ! if on p_dir
 deallocate(wf2d)
 endif !if on p_dim
! 
 if (p_dim==3) then 
  call msg(extension2,'','END_DATAGRID_3D')
  call msg(extension2,'','END_BLOCK_DATAGRID_3D')
 elseif (p_dim==2) then 
  call msg(extension2,'','END_DATAGRID_2D')
  call msg(extension2,'','END_BLOCK_DATAGRID_2D')
 endif
 
 end subroutine

!================================== GPL_START
!
  end subroutine
