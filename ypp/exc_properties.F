!
! Copyright (C) 2000-2008 D. Varsano, A. Marini and the YAMBO team
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
subroutine exc_properties(Xk,Xen)
 !
 use pars,          ONLY:SP,IP,pi,BOHR
 use stderr,        ONLY:intc
 use FFT_m,         ONLY:fft_rot_r,fft_dim
 use D_lattice,     ONLY:a,i_time_rev,nsym,alat
 use wave_func,     ONLY:wf_load,wf_state,wf,WF_free,wf_ng
 use R_lattice,     ONLY:nXkibz,bz_samp
 use YPP,           ONLY:l_sort,l_wf,l_spin,ncell,lambda,r_hole,&
&                        nr,exc_wf,nr_tot,state_ctl,&
&                        exc_wf_sort_and_report,l_amplitude,l_free_hole
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,NONE,REP,VERIFY
 use BS,            ONLY:BS_K_dim,BS_bands,BS_q,&
&                        BS_eh_table,BS_mat,BS_K_coupling,BS_cpl_mode,&
&                        BS_cpl_K_exchange,BS_cpl_K_corr,BSS_write_eig_2_db
 use electrons,     ONLY:levels,n_sp_pol,spin,n_spinor
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use com,           ONLY:msg,error,warning
 use X_m,           ONLY:X_t
 use memory_m,      ONLY:mem_est
 use timing,        ONLY:live_timing
 use vec_operate,   ONLY:c2a
 !
 implicit none
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 integer              :: n_lambda,i_lambda
 integer ,allocatable :: lambda_s(:)
 !
 ! ... parameters
 !
 complex(SP),  parameter    :: cI=(0.,1.)
 !
 ! ... variables needed in the grid
 !
 integer               :: ir,ir_hole,j1
 integer,  allocatable :: rindex(:)
 real(SP), allocatable :: r_cell_cc(:,:)
 !
 ! Degenerations
 !
 integer              :: n_lambda_deg
 integer ,allocatable :: BS_E_degs(:)
 !
 ! Excitonc Spin
 !
 real(SP),allocatable :: S_z(:),S_sq(:)
 !
 !... Dummy arguments 
 !
 integer      :: iq,ia,i1,i2,i_l,i_spin
 real(SP)     :: k_dot_r,r_hole_rlu(3)
#if defined _DOUBLE
 complex(SP)  :: zdotu
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
 complex(SP)       ::wf_
 complex(SP),  allocatable ::wf_vc(:)
 !
 !... Variables needed to the report of eigenvectors
 !
 real(SP), allocatable ::A_weight(:)
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
 ! Get state indexes
 !
 n_lambda = get_lambda()
 !
 if (.not.any((/l_sort,(l_wf.and.n_lambda>0),(l_amplitude.and.n_lambda>0),l_spin/))) return
 !
 ! Allocation of energies and residuum, then also eigenvector
 !
 allocate(BS_R(BS_K_dim),BS_E(BS_K_dim))
 call mem_est('BS_R_E',(/2*BS_K_dim/))
 iq=BS_q(1)
 !
 ! Here I read energies and residuum
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=ioBSS_diago(iq,BS_K_dim,BS_E,BS_R,io_ID)
 if (io_err/=0) return
 !
 call section('*','Excitonic Properties')
 !
 if (l_sort) then
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
   io_err=ioBSS_diago(iq,BS_K_dim,BS_E,BS_R,ID)
   call exc_wf_sort_and_report(Xk,Xen,BS_R,BS_E)
   deallocate(BS_R,BS_E)
   call mem_est('BS_R_E')
   return
 endif
 !
 if (BS_K_coupling) call error('When coupling is present only sort mode works')
 !
 ! Loading tables and eigenvectors
 !
 allocate(BS_eh_table(BS_K_dim,3+n_sp_pol-1),BS_mat(BS_K_dim,BS_K_dim),&
          BS_E_degs(BS_K_dim))
 call mem_est("BS_eh_table",(/(3+n_sp_pol-1)*BS_K_dim/),(/IP/))
 call mem_est('BS_mat',(/BS_K_dim**2/))
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
 io_err=ioBSS_diago(iq,BS_K_dim,BS_E,BS_R,ID)
 !
 if (.not.BSS_write_eig_2_db) call error('Diago database does not contain excitonic wfs')
 !
 ! Sort energies to find degenerate states
 !========================================
 !
 call exc_wf_sort_and_report(Xk,Xen,BS_R,BS_E,BS_E_degs=BS_E_degs)
 !
 ! GPL_EXCLUDE_START
 !
 ! Exciton's spin (collinear case)
 !================================
 !
 if (l_spin.and.n_spinor==1) then
   allocate(S_z(BS_K_dim),S_sq(BS_K_dim))
   if (n_sp_pol==2)  call exc_collinear_spin(BS_E_degs,S_z,S_sq)
   call exc_wf_sort_and_report(Xk,Xen,BS_R,BS_E,S_sq=S_sq,S_z=S_z)
   deallocate(S_z,S_sq)
   return
 endif
 ! 
 ! GPL_EXCLUDE_END 
 ! 
 ! Loading  Wf 
 !
 if (l_wf) call wf_load(wf_ng,1,BS_bands,(/1,nXkibz/),space='R',title='-EXCWF')
 !
 ! GPL_EXCLUDE_START
 !
 ! Exciton's spin (not collinear case)
 !=====================================
 !
 if (l_spin.and.n_spinor==2) then
   !
   allocate(S_z(BS_K_dim),S_sq(BS_K_dim))
   allocate (lambda_s(n_lambda)) 
   !
   n_lambda = get_lambda()
   ! 
   call exc_non_collinear_spin(Xk,lambda_s,n_lambda,S_z,S_sq)
   call exc_wf_sort_and_report(Xk,Xen,BS_R,BS_E,S_sq=S_sq,S_z=S_z)
   !
   deallocate(lambda_s)
   deallocate(S_z,S_sq)
   !
   return
 endif
 !
 ! GPL_EXCLUDE_END
 !
 call k_ibz2bz(Xk,'i',.false.)
 !
 ! Exciton's Amplitude
 !=====================================
 !
 if (l_amplitude) then
   !
   allocate (lambda_s(n_lambda)) 
   !
   n_lambda = get_lambda()
   !
   call section('+','Excitonic Amplitude')
   !
   call msg('s','Processing '//trim(intc(n_lambda))//' states')
   !
   allocate(A_weight(BS_K_dim))
   call mem_est('A_weight',(/BS_K_dim/),(/SP/))
   !
   do i_lambda=1,n_lambda
     !
     A_weight   =0._SP
     !
     lambda=lambda_s(i_lambda)
     !
     n_lambda_deg=count(BS_E_degs==BS_E_degs(lambda))
     !
     if (n_lambda_deg>0) call msg('s',':: State '//trim(intc(lambda))//' Merged with states '//&
&                                     trim(intc(BS_E_degs(lambda)))//' -> '//&
&                                     trim(intc(BS_E_degs(lambda)+n_lambda_deg-1)))
     do neh = 1,BS_K_dim
       !
       do i_l=BS_E_degs(lambda),BS_E_degs(lambda)+n_lambda_deg-1
         A_weight(neh)=A_weight(neh)+BS_mat(neh,i_l)*conjg(BS_mat(neh,i_l))
       enddo
       !
     enddo
     !
     call exc_wf_sort_and_report(Xk,Xen,BS_R,BS_E,A_weight=A_weight)
     !
   enddo
   !
   ! CLEAN
   !
   deallocate(BS_R,BS_E,lambda_s)
   call mem_est('BS_R_E')
   deallocate(BS_eh_table,BS_mat,A_weight)
   call mem_est('BS_eh_table BS_mat A_weight')
   !
   return
   !
 endif
 !
 !############################################################################
 ! EXCITONIC WF SECTION
 !############################################################################
 !
 if (.not.l_wf) return
 !
 call section('+','Excitonic Wave Function')
 !
 ! Check that directions are OK
 !
 call exc_wf_plot_driver(.true.)
 !
 ! Constructing Grid 
 !
 call section('+','Real-Space grid setup')
 !
 if (l_free_hole) ncell=1
 call expand_grid()
 !
 if (.not.l_free_hole) then
   !
   ! Translation & location in the big grid of the hole...
   !======================================================
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
   call msg('s','::      position in the FFT grid [cc]:',r_cell_cc(:,ir_hole))
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
   r_hole=r_cell_cc(:,ir_hole)
   !
   call msg('s','::      translated position      [cc]:',r_hole)
   call msg('s','::                             [bohr]:',r_hole*BOHR)
   !
 endif
 !
 ! Allocation
 !
 allocate(wf_vc(BS_K_dim),exc_wf(nr_tot))
 call mem_est('wf_vc exc_wf ',(/2*BS_K_dim,nr_tot/),(/SP,SP/))
 !
 ! Par Proc
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/nr_tot/))
 call pp_redux_wait
 !
 ! Loop on exc states
 !===================
 !
 allocate (lambda_s(n_lambda)) 
 !
 n_lambda = get_lambda()
 !
 call msg('s','Processing '//trim(intc(n_lambda))//' states')
 !
 do i_lambda=1,n_lambda
   !
   lambda=lambda_s(i_lambda)
   !
   n_lambda_deg=count(BS_E_degs==BS_E_degs(lambda))
   !
   if (n_lambda_deg>0) call msg('s',':: State '//trim(intc(lambda))//' Merged with states '//&
&                                   trim(intc(BS_E_degs(lambda)))//' -> '//&
&                                   trim(intc(BS_E_degs(lambda)+n_lambda_deg-1)))
   !
   call live_timing('ExcWF@'//trim(intc(lambda)),px%stps(myid+1))  
   !
   exc_wf  =0._SP
   wf_vc   =(0._SP,0._SP)
   !
   do ir=1,nr_tot
     !
     if (.not.px%i1p(ir)) cycle
     !
     if (l_free_hole) then
       r_eh(:)=0.
       ir_hole=ir
     else
       r_eh(:)=r_cell_cc(:,ir)-r_hole(:)
     endif
     !
     do neh = 1,BS_K_dim
       !
       ikbz  = BS_eh_table(neh,1)
       iv    = BS_eh_table(neh,2)
       ic    = BS_eh_table(neh,3)
       !
       ! GPL_EXCLUDE_START
       !
       i_spin= spin(BS_eh_table(neh,:))
       !
       ! GPL_EXCLUDE_END
       ! GPL_INCLUDE_START
       !i_spin = 1
       ! GPL_INCLUDE_END
       !
       ikibz = Xk%sstar(ikbz,1)
       is    = Xk%sstar(ikbz,2)
       !
       if (is > nsym/(i_time_rev+1)) is=is-nsym/(i_time_rev+1) 
       TR_apply=is/=Xk%sstar(ikbz,2)
       !
       k_dot_r = dot_product(r_eh,Xk%ptbz(ikbz,:)/alat(:))*2.*pi
       !
       iv_fft = wf_state(iv,ikibz,i_spin)
       ic_fft = wf_state(ic,ikibz,i_spin)
       !
       wf_vc(neh) = wf(fft_rot_r(is,rindex(ir)), ic_fft)*&
&            conjg( wf(fft_rot_r(is,rindex(ir_hole)), iv_fft) )
       !
       if (TR_apply) wf_vc(neh) = conjg(wf_vc(neh))
       !
       wf_vc(neh) = exp(cI*k_dot_r)*wf_vc(neh)
       !
     enddo  !Matrix elements
     !
     do i_l=BS_E_degs(lambda),BS_E_degs(lambda)+n_lambda_deg-1
#if defined _DOUBLE
       wf_ = zdotu(BS_K_dim,BS_mat(1,i_l),1,wf_vc,1)
#else
       wf_ = cdotu(BS_K_dim,BS_mat(1,i_l),1,wf_vc,1)
#endif
       !
       !if (.not.l_free_hole) exc_wf(ir) = exc_wf(ir)+abs(wf_)**2.  
       !if (l_free_hole)      exc_wf(ir) = exc_wf(ir)+aimag(wf_)+real(wf_)
       !
       exc_wf(ir) = exc_wf(ir)+abs(wf_)**2.  
       !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo   !grid points
   !
   call live_timing()
   !
   call pp_redux_wait(exc_wf)
   !
   ! PLOT
   !
   call exc_wf_plot_driver(.false.)
   !
 enddo
 !
 call pp_indexes_reset(px)
 !
 ! CLEAN
 !
 deallocate(r_cell_cc,rindex)
 deallocate(exc_wf,BS_R,BS_E,lambda_s)
 call mem_est('exc_wf BS_R_E')
 deallocate(BS_eh_table,BS_mat,wf_vc)
 call mem_est('BS_eh_table BS_mat wf_vc')
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
     integer :: ir1, ir2, ir3, i3, j1, j2, j3
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
     allocate ( r_cell_cc(3,nr_tot), rindex(nr_tot) )
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
           r_cell_cc(:,ir) = ir1*a(1,:)/fft_dim(1) +&
&                            ir2*a(2,:)/fft_dim(2) +&
&                            ir3*a(3,:)/fft_dim(3)
         end do
       end do
     end do
     !
   end subroutine
   !
   integer function get_lambda()
     use pars,   ONLY:schlen
     use stderr, ONLY:string_split
     !
     integer          ::i_start,i_end,i_str,i_st
     character(schlen)::str_piece(50)
     !
     call string_split(state_ctl,str_piece)
     !
     get_lambda=0
     if (allocated(lambda_s)) lambda_s=0
     !
     i_str=1
     !
     do while (i_str<50)
       !
       if (len_trim(str_piece(i_str))==0) exit
       !
       if (trim(str_piece(i_str+1))=="-") then
         read(str_piece(i_str  ),*) i_start
         read(str_piece(i_str+2),*) i_end
         i_str=i_str+3
       else
         read(str_piece(i_str),*) i_start
         i_end=i_start
         i_str=i_str+1
       endif
       !
       do i_st=i_start,i_end
         !
         if (allocated(BS_E_degs)) then
           if (i_st>i_start.and.BS_E_degs(i_st)/=i_st) cycle
         endif
         !
         if (allocated(lambda_s)) then
           if (.not.any(lambda_s==i_st)) then
             get_lambda=get_lambda+1
             lambda_s(get_lambda)=i_st
           endif
         else
           get_lambda=get_lambda+1
         endif
       enddo
     enddo
     !
   end function
   !
end subroutine
