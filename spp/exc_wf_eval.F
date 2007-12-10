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
subroutine exc_wf_eval(Xk,Xen)
 !
 use pars,          ONLY:SP,HARTREE,IP,pi,lchlen,DP,schlen,BOHR
 use stderr,        ONLY:intc
 use FFT_m,         ONLY:fft_rot_r,fft_dim
 use D_lattice,     ONLY:a,i_time_rev,nsym,alat,&
&                        n_atom_species,n_atoms,atom_pos,znucl
 use wave_func,     ONLY:wf_load,wf_state,wf,WF_free
 use R_lattice,     ONLY:nXkibz,bz_samp,ng_vec,q0_def_norm,d3k_factor
 use SPP,           ONLY:action_mode,ncell,lambda,r_hole,sort_mode,&
&                        max_energy,nr,exc_wf_sq,nr_tot,n_lambda_deg,lambda_deg,&
&                        R_normalize,deg_energy
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,NONE,REP,VERIFY
 use BS,            ONLY:BS_K_dim,BS_bands,BS_q,BSS_n_descs,BSS_description,&
&                        BS_eh_table,BS_mat,BS_K_coupling,BS_cpl_mode,&
&                        BS_cpl_K_exchange,BS_cpl_K_corr,BSS_write_eig_2_db
 use electrons,     ONLY:levels,spin_occ
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use com,           ONLY:msg,of_open_close,error,warning
 use X_m,           ONLY:X_t
 use memory_m,      ONLY:mem_est
 use timing,        ONLY:live_timing
 use vec_operate,   ONLY:c2a,sort
 !
 implicit none
 type(bz_samp) ::Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 ! ... parameters
 !
 complex(SP),  parameter    :: cI=(0.,1.)
 !
 ! ... variables needed in the grid
 !
 integer               :: ir,ir_hole,j1
 integer,  allocatable :: rindex(:)
 real(SP), allocatable :: r_big_cell_cc(:,:)
 !
 ! Excitonc amplitude
 !
 integer, parameter :: amp_steps=1000
 integer      :: amp_n_trans
 real(SP)     :: amp_range(2),amp_damping,amp_I
 real(SP),allocatable :: amp_trans(:,:)
 complex(SP)          :: amp_E
 !
 !... Dummy arguments 
 !
 integer      :: iq,ia,i1,i2,i_l
 real(SP)     :: k_dot_r,r_hole_rlu(3)
#if defined _DOUBLE
 complex(DP)  :: zdotu
#else
 complex(SP)  :: cdotu
#endif
 type(bz_samp)    ::q
 type(pp_indexes) ::px
 logical          ::TR_apply
 !
 !... I/0
 !
 integer           ::io_err,ID,io_ID
 integer, external ::ioBSS_diago,ioBS
 type(X_t)         ::Xbsk
 character(schlen) ::extension(2),titles(5)
 !
 !... Energies and Residulas
 !
 complex(SP), allocatable ::BS_R(:)
 complex(SP), allocatable ::BS_E(:)
 !
 !... Variables needed to plot the exc-wf
 !
 integer           ::iv,ic,iv_fft,ic_fft,ikbz,ikibz,is,neh
 real(SP)          ::r_eh(3)
 complex(SP)       ::wf_sq
 complex(SP),  allocatable ::wf_vc(:)
 !
 !... Variables needed to the report of eigenvectors
 !
 real(SP)              ::K_weight(Xk%nibz)
 integer,  allocatable ::A_indx(:)
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
 if (io_err/=0) return
 if (BS_cpl_K_exchange) BS_cpl_mode='x'
 if (BS_cpl_K_corr) BS_cpl_mode='c'
 if (BS_cpl_K_exchange.and.BS_cpl_K_corr) BS_cpl_mode='xc'
 if (BS_K_coupling) BS_K_dim=2*BS_K_dim
 !
 ! Allocation of energies and residuum, then also eigenvector
 !
 allocate(BS_R(BS_K_dim),BS_E(BS_K_dim),amp_trans(BS_K_dim,2))
 call mem_est('BS_R_E',(/2*BS_K_dim/))
 iq=BS_q(1)
 !
 ! Here I read energies and residuum
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=ioBSS_diago(iq,BS_K_dim,BS_E,BS_R,io_ID)
 if (io_err/=0) return
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
   call sort_energies(BS_R,BS_E,.true.)
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
 if (.not.BSS_write_eig_2_db) call error('Diago database does not conatind excitonic wfs')
 !
 ! Sort energies to find degenerate states
 !
 call sort_energies(BS_R,BS_E,.false.)
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
 call msg('s','::      position in the FFT grid [cc]:',r_big_cell_cc(:,ir_hole))
 !
 ! [3] Translation in the center of the Big grid
 !
 do j1=1,3
   if (ncell(j1)==1) cycle
   !
   ! (***) Daniele 15/7/07 rev Andrea 12/07:
   ! 
   ! The number of cells are always odd so that the hole can
   ! be placed in the middle.
   !
   r_hole_rlu(j1)=r_hole_rlu(j1)+ncell(j1)/2*fft_dim(j1)
   !
 enddo
 ir_hole=1+r_hole_rlu(1)+ r_hole_rlu(2)*nr(1)+ r_hole_rlu(3)*nr(1)*nr(2)
 r_hole=r_big_cell_cc(:,ir_hole)
 !
 call msg('s','::      translated position      [cc]:',r_hole)
 call msg('s','::                             [bohr]:',r_hole*BOHR)
 !
 ! Construction of the Exc-Wf
 !
 call section('=','Excitonic Wave function costruction')
 !
 ! Allocation
 !
 allocate(wf_vc(BS_K_dim),exc_wf_sq(nr_tot),A_weight(BS_K_dim),A_indx(BS_K_dim))
 call mem_est('wf_vc exc_wf_sq A_weight',(/2*BS_K_dim,nr_tot,BS_K_dim/),(/SP,SP,SP/))
 call k_ibz2bz(Xk,'i',.false.)
 !
 ! Par Proc
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/nr_tot/))
 call pp_redux_wait
 !
 call live_timing('Exc-Wfs',px%stps(myid+1))  
 !
 A_weight   =0._SP
 exc_wf_sq  =0._SP
 wf_vc      =(0._SP,0._SP)
 !
 do ir=1,nr_tot
   !
   if (.not.px%i1p(ir)) cycle
   !
   r_eh(:)=r_big_cell_cc(:,ir)-r_hole(:)
   do neh = 1,BS_K_dim
     ikbz = BS_eh_table(neh,1)
     iv = BS_eh_table(neh,2)
     ic = BS_eh_table(neh,3)
     ikibz = Xk%sstar(ikbz,1)
     is = Xk%sstar(ikbz,2)
     if (is > nsym/(i_time_rev+1)) is=is-nsym/(i_time_rev+1) 
     TR_apply=is/=Xk%sstar(ikbz,2)
     !
     k_dot_r = dot_product(r_eh,Xk%ptbz(ikbz,:)/alat(:))*2.*pi
     !
     iv_fft = wf_state(iv,ikibz,1)
     ic_fft = wf_state(ic,ikibz,1)
     !
     if (ir==1) then
       do i_l=1,n_lambda_deg
         A_weight(neh)=A_weight(neh)+BS_mat(neh,lambda_deg(i_l))*&
&                              conjg(BS_mat(neh,lambda_deg(i_l)))
       enddo
     endif
     !
     wf_vc(neh) = wf(fft_rot_r(is,rindex(ir)), ic_fft)*&
&          conjg( wf(fft_rot_r(is,rindex(ir_hole)), iv_fft) )
     !
     if (TR_apply) wf_vc(neh) = conjg(wf_vc(neh))
     !
     wf_vc(neh) = exp(cI*k_dot_r)*wf_vc(neh)
     !
   enddo  !Matrix elements
   !
   do i_l=1,n_lambda_deg
#if defined _DOUBLE
     wf_sq = zdotu(BS_K_dim,BS_mat(1,lambda_deg(i_l)),1,wf_vc,1)
#else
     wf_sq = cdotu(BS_K_dim,BS_mat(1,lambda_deg(i_l)),1,wf_vc,1)
#endif
     exc_wf_sq(ir) = exc_wf_sq(ir)+abs(wf_sq)**2.
   enddo
   !
   call live_timing(steps=1)
   !
 enddo   !grid points
 !
 call live_timing()
 !
 call pp_redux_wait(exc_wf_sq)
 call pp_indexes_reset(px)
 !
 ! Sort the weights
 !
 if (trim(R_normalize)=='yes') A_weight=A_weight/maxval(A_weight)
 call sort(arrin=A_weight,indx=A_indx)
 !
 ! report on file the weights and the amplitude 
 ! of the excitonic state...
 !
 ! ... first open the file.
 !
 extension(1)='weights_at_'//trim(intc(lambda))
 extension(2)='amplitude_at_'//trim(intc(lambda))
 call of_open_close(trim(extension(1)),'ot')
 call of_open_close(trim(extension(2)),'ot')
 !
 do j1=1,BSS_n_descs
   call msg('o weight amp',"#",trim(BSS_description(j1)),INDENT=0)
 enddo
 call msg('o weight amp',"#")
 call msg('o weight amp',&
& '# Electron-Hole pairs that contributes to Excitonic State '//trim(intc(lambda))//' more than 5%')
 call msg('o weight amp','#')
 !
 ! First summarize the total weight vs K-points
 !
 K_weight=0.
 do neh = 1,BS_K_dim
   ikbz = BS_eh_table(A_indx(neh),1)
   ikibz = Xk%sstar(ikbz,1)
   K_weight(ikibz)=K_weight(ikibz)+A_weight(neh)
 enddo
 K_weight=K_weight/maxval(K_weight)
 titles(1:4)=(/'','K-point [iku]','','Weight       '/)
 call msg('o weight','#',titles(:4),INDENT=0,USE_TABS=.true.)
 do ikibz=1,Xk%nibz
   if (K_weight(ikibz)>0.05) then
     call msg('o weight','# ',(/Xk%pt(ikibz,:),K_weight(ikibz)/),INDENT=0,USE_TABS=.true.)
   endif
 enddo
 !
 ! Then report the detailed list of transitions & calculate the amplitude
 !
 call msg('o weight','#','',INDENT=0,USE_TABS=.true.)
 titles(1:4)=(/'Band_V','Band_C','K  ibz','Symm. '/)
 titles(5)='Weight'
 call msg('o weight','#',titles(:5),INDENT=0,USE_TABS=.true.)
 titles(1:2)=(/'E    [eV]','Amplitude  '/)
 call msg('o amp','#',titles(:2),INDENT=0,USE_TABS=.true.)
 call msg('o weight amp','#','',INDENT=0,USE_TABS=.true.)
 !
 amp_n_trans=0
 amp_trans=0.
 !
 do neh = BS_K_dim,1,-1
   !
   if (A_weight(neh)<0.02) cycle
   !
   ikbz = BS_eh_table(A_indx(neh),1)
   iv = BS_eh_table(A_indx(neh),2)
   ic = BS_eh_table(A_indx(neh),3)
   ikibz = Xk%sstar(ikbz,1)
   is = Xk%sstar(ikbz,2)
   !
   amp_n_trans=amp_n_trans+1
   amp_trans(amp_n_trans,1)=Xen%E(ic,ikibz,1)-Xen%E(iv,ikibz,1)
   amp_trans(amp_n_trans,2)=A_weight(neh)
   !
   if (A_weight(neh)<0.05) cycle
   !
   call msg('o weight','',(/real(iv),real(ic),real(ikibz),real(is),A_weight(neh)/),&
&          INDENT=-2,USE_TABS=.FALSE.)
 enddo
 !
 ! Excitonic Amplitude
 ! 
 amp_range=(/minval(amp_trans(:amp_n_trans,1))-0.5/HARTREE,&
&            maxval(amp_trans(:amp_n_trans,1))+0.5/HARTREE/)
 amp_damping=(amp_range(2)-amp_range(1))/50.
 do i2=1,amp_steps
   amp_E=amp_range(1)+(i2-1)*(amp_range(2)-amp_range(1))/real(amp_steps)+&
&        (0.,1.)*amp_damping
   amp_I=0.
   do i1=1,amp_n_trans
     amp_I=amp_I-aimag(amp_trans(i1,2)/(amp_E-amp_trans(i1,1)))
   enddo
   call msg('o amp','',(/real(amp_E)*HARTREE,amp_I/),INDENT=-2,USE_TABS=.FALSE.)
 enddo
 !
 ! CLEAN
 !
 call of_open_close(trim(extension(1))) 
 call of_open_close(trim(extension(2))) 
 !
 deallocate(BS_eh_table,BS_mat,wf_vc,A_weight,A_indx,amp_trans)
 call mem_est('BS_eh_table BS_mat wf_vc A_weight')
 !
 ! PLOT
 !
 call exc_wf_plot_driver(.false.)
 !
 ! More CLEAN
 !
 deallocate(r_big_cell_cc,rindex,exc_wf_sq,BS_R,BS_E)
 call mem_est('exc_wf_sq BS_R_E')
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
       if ( int(real(ncell(j1))/2.)*2== ncell(j1) ) then
         if (warning_) call warning('Forced odd number of cell replicas')
         warning_=.FALSE.
         ncell(j1)=int( real(ncell(j1))/2. )*2+1
       endif
       !
     enddo
     !
     nr_tot = fft_size*ncell(1)*ncell(2)*ncell(3)
     allocate ( r_big_cell_cc(3,nr_tot), rindex(nr_tot) )
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
           r_big_cell_cc(:,ir) = ir1*a(1,:)/fft_dim(1) +&
&                                ir2*a(2,:)/fft_dim(2) +&
&                                ir3*a(3,:)/fft_dim(3)
         end do
       end do
     end do
     !
   end subroutine
   !
   subroutine sort_energies(BS_R,BS_E,full_run)
     !
     implicit none 
     complex(SP) :: BS_R(BS_K_dim),BS_E(BS_K_dim)
     !
     ! Work Space
     !
     integer               ::j1,j2
     integer,  allocatable ::E_indx(:)
     real(SP), allocatable ::R(:)         !square residuals R
     real(SP)              ::Rmax         !maximum value of the residuals
     real(SP), allocatable ::v2sort(:)
     logical               ::full_run
     character(schlen)     ::ch_dummy
     !
     ! Perform perliminar sort of energies
     !
     if (.not.full_run) then
       !
       ! Search for degenerate states
       !
       lambda_deg(1)=lambda
       !
       do j1=1,BS_K_dim
         !
         if (j1==lambda) cycle
         !
         if ( abs( real(BS_E(j1))-real(BS_E(lambda)) )<deg_energy) then
           n_lambda_deg=n_lambda_deg+1
           lambda_deg(n_lambda_deg)=j1
         endif
       enddo
       !
       if (n_lambda_deg>1) then
         ch_dummy=' '
         do j1=2,n_lambda_deg
           ch_dummy=trim(ch_dummy)//' '//trim(intc(lambda_deg(j1)))
         enddo
         call msg('s',':: Mergering with '//trim(intc(n_lambda_deg-1))//' state(s) :'//&
&                     trim(ch_dummy))
       endif
       return
     endif
     !
     call section('+','Sorting Energies and Strengths')
     !
     if (sort_mode /= 'e'.and.sort_mode/='i') sort_mode='e'
     if (sort_mode == 'e')   extension(1)='-e'
     if (sort_mode == 'i')   extension(1)='-i'
     call of_open_close('exc_sort'//trim(extension(1)),'ot')
     !
     do j1=1,BSS_n_descs
       call msg('o sort',"#",trim(BSS_description(j1)),INDENT=0)
     enddo
     call msg('o sort',"#")
     do is=1,n_atom_species
       do ia=1,n_atoms(is)
         call msg('o sort',"# Atom "//trim(intc(ia))//" with Z "//trim(intc(znucl(is)))//" [cc]:",&
&                 atom_pos(:,ia,is),INDENT=0)
       enddo
     enddo
     call msg('o sort','#','',INDENT=0)
     !
     titles(1)='E [ev]'
     titles(2)='W [meV]'
     titles(3)='Strength'
     titles(4)='Index'
     !
     call msg('o sort','#',titles(:4),INDENT=0,USE_TABS=.true.)
     call msg('o sort','#','',INDENT=0)
     !
     allocate(R(BS_K_dim))
     R(:) = BS_R(:)*conjg(BS_R(:))
     R(:) = R(:)*real(spin_occ)/(2.*pi)**3.*&
&           d3k_factor*4.*pi/q0_def_norm**2*HARTREE*1000.
     Rmax = maxval(R)
     !
     if (trim(R_normalize)=='yes') R=R/Rmax
     !
     if (sort_mode == 'i') then
       !
       allocate(E_indx(BS_K_dim),v2sort(BS_K_dim))
       v2sort=R
       call sort(arrin=v2sort,indx=E_indx)
       !
     endif
     !
     do j1=BS_K_dim,1,-1
       !
       j2=BS_K_dim-j1+1
       if (sort_mode == 'i') j2=E_indx(j1)
       !
       if (real(BS_E(j2))>max_energy.or.real(BS_E(j2))<0.) cycle 
       !
       call msg('o sort','',&
&               (/real(BS_E(j2))*HARTREE,abs(aimag(BS_E(j2)))*HARTREE*1000.,&
&                 R(j2),real(j2)/),INDENT=-2,USE_TABS=.TRUE.)
       !
     enddo
     ! 
     if (sort_mode == 'i') deallocate(v2sort,E_indx)
     deallocate(R)
     !
     call of_open_close('exc_sort'//trim(extension(1)))
     !
   end subroutine
   !
end subroutine
