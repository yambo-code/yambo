!
! Copyright (C) 2000-2005 D. Varsano, A. Marini and the SELF team
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
subroutine exc_wf_eval(Xk)
 !
 use pars,        ONLY : SP,HARTREE,IP,pi,lchlen,DP,schlen
 use stderr,      ONLY : intc
 use FFT_m,       ONLY : fft_rot_r,fft_dim
 use D_lattice,   ONLY : a,sop_inv,i_time_rev,nsym,alat,&
&                        n_atom_species,n_atoms,atom_pos,znucl,n_atoms_max
 use wave_func,   ONLY : wf_load,wf_state,wf,WF_free
 use R_lattice,   ONLY : nXkibz,bz_samp,ng_vec
 use SPP,         ONLY : action_mode,ncell,lambda,r_hole,sort_mode,&
&                        max_energy,nr,exc_wf,nr_tot
 use IO_m,        ONLY : io_control,OP_RD_CL,DUMP,NONE,REP,VERIFY
 use BS,          ONLY : BS_K_dim,BS_bands,BS_q,BSS_n_descs,BSS_description,&
&                        BS_eh_table,BS_mat,BS_K_coupling,BS_cpl_mode,&
&                        BS_cpl_K_exchange,BS_cpl_K_corr
 use com,         ONLY: msg,of_open_close,error,warning
 use X_m,         ONLY: X_t
 use memory_m,    ONLY: mem_est
 use timing,      ONLY: live_timing
 use vec_operate, ONLY: c2a
 !
 implicit none
 type(bz_samp) ::Xk
 !
 ! Work Space
 !
 ! ... parameters GPL_END
 !
 complex(SP),  parameter    :: cI=(0.,1.)
 !
 ! ... variables needed in the grid
 !
 integer               :: ir,ir_hole,j1
 integer, allocatable  :: rindex(:)
 real(SP), allocatable :: rvecbig(:,:)
 !
 !... Dummy arguments 
 !
 integer      :: iq,ia
 real(SP)     :: kr,r_hole_rlu(3)
#if defined _DOUBLE
 complex(DP)  :: zdotu
#else
 complex(SP)  :: cdotu
#endif
 type(bz_samp):: q
 !
 !... I/0
 !
 integer           ::io_err,ID,io_ID
 integer, external ::ioBSS_diago,ioBS
 type(X_t)         ::Xbsk
 character(schlen) ::extension,titles(5)
 !
 !... Energies and Residulas
 !
 complex(SP), allocatable ::BS_R(:)
 complex(SP), allocatable ::BS_E(:)
 !
 !... Variables needed to plot the exc-wf
 !
 integer           ::iv,ic,iv_fft,ic_fft,ikbz,ikibz,is,neh
 real(SP)          ::k_pt(3)
 real(SP)          ::r_eh(3)
 complex(SP),  allocatable ::wf_vc(:)
 !
 !... Variables needed to the report of eigenvectors
 !
 real(SP), allocatable ::A_weight(:)
 !
 ! ... Outputs
 !
 character(lchlen) :: file_name
 character(lchlen) :: local_ch
 !
 ! Here I read information from the BS database
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
 call mem_est('BS_R_E',(/2*BS_K_dim/))
 iq=BS_q(1)
 !
 !Description of the BS Kernel
 !
 call K_descriptions(iq,q,Xbsk)
 !
 ! Here I read energies and residuum
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=ioBSS_diago(iq,BS_K_dim,BS_E,BS_R,io_ID)
 !
 call section('*','Excitonic Wave Function')
 !
 ! Check that directions are OK
 !
 call exc_wf_plot_driver(.true.)
 !
 if (action_mode == 's' ) then 
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
   io_err=ioBSS_diago(iq,BS_K_dim,BS_E,BS_R,ID)
   call sort_energies(BS_R,BS_E)
   return
 endif
 !
 if (action_mode /= 'p') return
 !
 if (BS_K_coupling) call error('When coupling is present only sort mode works')
 !
 ! Loading tables and eigenvectors
 !
 allocate (BS_eh_table(BS_K_dim,3),BS_mat(BS_K_dim,BS_K_dim))
 call mem_est("BS_eh_table",(/3*BS_K_dim/),(/IP/))
 call mem_est('BS_mat',(/BS_K_dim**2/))
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
 io_err=ioBSS_diago(iq,BS_K_dim,BS_E,BS_R,ID)
 ! 
 ! Loading  Wf 
 !
 call wf_load(ng_vec,1,BS_bands,(/1,nXkibz/),space='R',title='-EXCWF')
 !
 ! Constructing Grid 
 !
 call section('+','Expanding grid')
 call expand_grid()
 !
 ! Translation & location in the big grid of the hole...
 ! 
 ! [1] Bare position pushed in the smallest cell 
 ! 
 call c2a(b_in=a,v_in=r_hole,v_out=r_hole_rlu,mode='kc2a')
 do j1=1,3
   r_hole_rlu(j1)=r_hole_rlu(j1)-int(r_hole_rlu(j1))
 enddo
 call c2a(b_in=a,v_in=r_hole_rlu,v_out=r_hole,mode='ka2c')
 call msg('s',':: Hole position in the DL cell  [cc]:',r_hole)
 !
 ! [2] Bare position in the FFT grid
 !
 call c2a(b_in=a,v_in=r_hole,v_out=r_hole_rlu,mode='kc2a')
 r_hole_rlu(:)=nint(r_hole_rlu(:)*fft_dim(:))
 ir_hole=1+r_hole_rlu(1)+ r_hole_rlu(2)*nr(1)+ r_hole_rlu(3)*nr(1)*nr(2)
 call msg('s','::      position in the FFT grid [cc]:',rvecbig(:,ir_hole))
 !
 ! [3] Translation in the center of the Big grid
 !
 do j1=1,3
   if (ncell(j1)==1) cycle
   !
   ! (***) Daniele 15/7/07:
   ! questo cambio l'ho fatto perche' generalemente (almeno come faccio io)
   ! quando si ripete la cella lo si fa un numero dispari di volte.
   ! In questo modo la hole che hai piazzato nella tua unit cell rimane
   ! li dov'e' ma traslata in un cella ripetuta che si trova nel mezzo
   ! della griglia grande.
   !
   !r_hole_rlu(j1)=r_hole_rlu(j1)+nr(j1)/2
   !
   r_hole_rlu(j1)=r_hole_rlu(j1)+fft_dim(j1)*(ncell(j1)-1)/2
   !
 enddo
 ir_hole=1+r_hole_rlu(1)+ r_hole_rlu(2)*nr(1)+ r_hole_rlu(3)*nr(1)*nr(2)
 r_hole=rvecbig(:,ir_hole)
 !
 call msg('s','::      translated position      [cc]:',r_hole)
 !
 ! Construction of the Exc-Wf
 !
 call section('=','Excitonic Wave function costruction')
 !
 ! Allocation
 !
 allocate(wf_vc(BS_K_dim),exc_wf(nr_tot),A_weight(BS_K_dim))
 call mem_est('wf_vc exc_wf A_weight',(/BS_K_dim,nr_tot,BS_K_dim/))
 call k_ibz2bz(Xk,'i',.false.)
 !
 call live_timing('Exc-Wfs',nr_tot)  
 !
 do ir=1,nr_tot
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
     !
     if (ir==1) A_weight(neh)=(BS_mat(neh,lambda)*conjg(BS_mat(neh,lambda)))
     !
     wf_vc(neh) = wf(fft_rot_r(sop_inv(is),rindex(ir)), ic_fft)*&
&          conjg( wf(fft_rot_r(sop_inv(is),rindex(ir_hole)), iv_fft) )
     if (is > nsym/(i_time_rev+1)) wf_vc(neh) = conjg(wf_vc(neh))
     wf_vc(neh) = exp(cI*kr*2*pi)*wf_vc(neh)
     !
   enddo  !Matrix elements
   !
#if defined _DOUBLE
   exc_wf(ir) = zdotu(BS_K_dim,BS_mat(1,lambda),1,wf_vc,1)
#else
   exc_wf(ir) = cdotu(BS_K_dim,BS_mat(1,lambda),1,wf_vc,1)
#endif
   call live_timing(steps=1)
 enddo   !grid points
 !
 call live_timing
 !
 ! report on file of the weights A_cvk of excitonic wf
 !
 !
 extension='weights_at_'//trim(intc(lambda))
 call of_open_close(trim(extension),'ot')
 do j1=3,BSS_n_descs
   call msg('o weight',"",trim(BSS_description(j1)),INDENT=0)
 enddo
 call msg('o weight',&
& '# Electron-Hole pairs that contributes to Excitonic State '//trim(intc(lambda))//' more than 5%')
 call msg('o weight','#')
 titles(1:4)=(/'Band_V','Band_C','K  ibz','Symm. '/)
 titles(5)='Weight'
 !
 call msg('o weight','#',titles(:5),INDENT=0,USE_TABS=.true.)
 call msg('o weight','#','',INDENT=0,USE_TABS=.true.)
 !
 do neh = 1,BS_K_dim
     ikbz = BS_eh_table(neh,1)
     iv = BS_eh_table(neh,2)
     ic = BS_eh_table(neh,3)
     ikibz = Xk%sstar(ikbz,1)
     is = Xk%sstar(ikbz,2)
   if (A_weight(neh)>=5e-2) then
     call msg('o weight','',(/real(iv),real(ic),real(ikibz),real(is),A_weight(neh)/),&
&            INDENT=-2,USE_TABS=.FALSE.)
   endif
 enddo
 call of_open_close(trim(extension)) 
 !
 ! CLEAN
 !
 deallocate(BS_eh_table,BS_mat,wf_vc,A_weight)
 call mem_est('BS_eh_table BS_mat wf_vc A_weight')
 !
 ! PLOT
 !
 call exc_wf_plot_driver(.false.)
 !
 ! More CLEAN
 !
 deallocate(rvecbig,rindex,exc_wf,BS_R,BS_E)
 call mem_est('exc_wf BS_R_E')
 call WF_free()
 !
 contains
   !
   subroutine expand_grid()
     !
     use FFT_m,       ONLY:fft_size
     implicit none
     ! 
     ! Work Space
     !
     integer :: ir1, ir2, ir3, i1, i2, i3, j1, j2, j3
     logical :: warning_
     !
     warning_=.TRUE. 
     ! 
     do j1=1,3
       if (ncell(j1)<=0) ncell(j1)=1
       !
       ! Comment at (***)
       !
       if (ncell(j1)/=1) then
         if (warning_) call warning('Forced even number of cell replicas')
         warning_=.FALSE.
         ncell(j1)=int( real(ncell(j1))/real(2.) ) * 2
       endif
       !
     enddo
     !
     nr_tot = fft_size*ncell(1)*ncell(2)*ncell(3)
     allocate ( rvecbig(3,nr_tot), rindex(nr_tot) )
     !
     nr=(/ncell(1)*fft_dim(1),ncell(2)*fft_dim(2),ncell(3)*fft_dim(3)/)
     call msg('s',':: Extended grid :',nr)
     !
     ir = 0
     do ir1 = 0, nr(1)-1
       do ir2 = 0, nr(2)-1
         do ir3 = 0, nr(3)-1
           ir = 1 + ir1 + ir2*nr(1) + ir3*nr(1)*nr(2)
           i1=mod(ir1,fft_dim(1))
           i2=mod(ir2,fft_dim(2))
           i3=mod(ir3,fft_dim(3))
           j1=ir1/fft_dim(1)
           j2=ir2/fft_dim(2)
           j3=ir3/fft_dim(3)
           rindex(ir)=i1+fft_dim(1)*i2+fft_dim(1)*fft_dim(2)*i3+1
           rvecbig(:,ir) = ir1*a(1,:)/fft_dim(1) + ir2*a(2,:)/fft_dim(2) + ir3*a(3,:)/fft_dim(3)
         end do
       end do
     end do
     !
   end subroutine
   !
   subroutine sort_energies(BS_R,BS_E)
     !
     use vec_operate, ONLY:sort
     !
     implicit none 
     complex(SP) :: BS_R(BS_K_dim),BS_E(BS_K_dim)
     !
     ! Work Space
     !
     integer               :: j1,j2
     integer,  allocatable :: indx(:)
     real(SP), allocatable :: R(:)         !square residuals R
     real(SP)              :: Rmax         !maximum value of the residuals
     real(SP), allocatable :: v2sort(:)
     !
     call section('+','Sorting Energies and Strengths')
     !
     if (sort_mode /= 'e'.and.sort_mode/='i') sort_mode='e'
     if (sort_mode == 'e')   extension='-e'
     if (sort_mode == 'i')   extension='-i'
     call of_open_close('exc_sort'//trim(extension),'ot')
     !
     do j1=3,BSS_n_descs
       call msg('o sort',"",trim(BSS_description(j1)),INDENT=0)
     enddo
     do is=1,n_atom_species
       do ia=1,n_atoms(is)
         call msg('o sort',"# Atom "//trim(intc(ia))//" with Z "//trim(intc(znucl(is)))//" [cc]:",&
&                 atom_pos(:,ia,is),INDENT=0)
       enddo
     enddo
     call msg('o sort','#','',INDENT=0)
     !
     titles(1)='E [ev]'
     titles(2)='Width [meV]'
     titles(3)='Strength'
     titles(4)='Index'
     !
     call msg('o sort','#',titles(:4),INDENT=0,USE_TABS=.true.)
     call msg('o sort','#','',INDENT=0)
     !
     Rmax=0
     allocate( R(BS_K_dim), v2sort(BS_K_dim),indx(BS_K_dim))
     R(:) = BS_R(:)*conjg(BS_R(:))
     Rmax = maxval(R)
     R(:) = R(:)/Rmax
     !
     if (sort_mode == 'e') v2sort=real(BS_E)
     if (sort_mode == 'i') v2sort=R
     !
     call sort(arrin=v2sort,indx=indx)
     !
     do j1=BS_K_dim,1,-1
       !
       j2=indx(j1)
       if (sort_mode == 'e') j2=indx(-j1+BS_K_dim+1)
       !
       if (real(BS_E(j2))>max_energy.or.real(BS_E(j2))<0.) cycle 
       call msg('o sort','',&
&               (/real(BS_E(j2))*HARTREE,abs(aimag(BS_E(j2)))*HARTREE*1000.,&
&                 R(j2),real(j2)/),INDENT=-2,USE_TABS=.TRUE.)
       !
     enddo
     ! 
     call of_open_close('exc_sort'//trim(extension))
     deallocate(R,v2sort,indx)
     !
   end subroutine
   !
end subroutine
