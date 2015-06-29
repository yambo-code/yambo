!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine excitons_driver(k,Xk,en,Xen,q)
 !
 use pars,          ONLY:SP,IP,pi
 use units,         ONLY:BO2ANG
 use parser_m,      ONLY:parser
 use stderr,        ONLY:intc
 use FFT_m,         ONLY:fft_dim
 use D_lattice,     ONLY:a,alat
 use wave_func,     ONLY:WF,WF_symm,wf_ng
 use R_lattice,     ONLY:nXkibz,bz_samp
 use YPP,           ONLY:l_sort,l_exc_wf,l_spin,ncell,lambda,r_hole,&
&                        nr,v2plot,nr_tot,state_ctl,BS_H_dim,n_lambda,&
&                        excitons_sort_and_report,l_amplitude,l_free_hole,&
&                        output_fname,use_xcrysden,use_gnuplot,use_cube,plot_dim
#if defined _YPP_ELPH
 use YPP,           ONLY:l_eliashberg,l_gkkp
#endif
#if defined _YPP_RT
 use YPP,           ONLY:l_RT_abs
#endif
 use QP_CTL_m,      ONLY:QP_apply
 use IO_m,          ONLY:io_control,OP_RD_CL,DUMP,NONE,REP,VERIFY
 use BS,            ONLY:BS_K_dim,BS_bands,BS_q,&
&                        BSS_eh_table,BS_mat,BS_K_coupling,BS_cpl_mode,&
&                        BS_cpl_K_exchange,BS_cpl_K_corr,BSS_write_eig_2_db,&
&                        BSS_n_descs,BSS_description,io_BSS_diago
 use electrons,     ONLY:levels,n_sp_pol,spin,n_spinor
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index,WF_load
 use com,           ONLY:msg,error,warning,of_open_close
 use X_m,           ONLY:X_t,DIP_projected
 use memory_m,      ONLY:mem_est
 use LIVE_t,        ONLY:live_timing
 use vec_operate,   ONLY:c2a
 use wrapper,       ONLY:V_dot_V
 !
 implicit none
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en
 !
 ! Work Space
 !
 integer              :: i_lambda
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
 integer      :: iq,i1,i2,i_l,i_spin
 real(SP)     :: k_dot_r,r_hole_rlu(3),norm_
 !
 type(PP_indexes) ::px
 !
 !... I/0
 !
 integer           ::io_err,io_ID
 integer, external ::io_BS
 type(X_t)         ::Xbsk
 !
 !... Energies and Residulas
 !
 complex(SP), allocatable ::BS_R(:)
 complex(SP), allocatable ::BS_E(:)
 !
 !... Variables needed to plot the exc-wf
 !
 integer           ::iv,ic,ikbz,ikibz,is,neh
 real(SP)          ::r_eh(3)
 complex(SP)       ::wf_
 complex(SP)       ::WF1_ir(n_spinor),WF2_ir(n_spinor)
 !
 complex(SP),  allocatable ::wf_vc(:)
 !
 !... Variables needed to the report of eigenvectors
 !
 logical          :: l_weight_with_dipoles,l_flag
 complex(SP)      :: dip_factor
 real(SP), allocatable ::A_weight(:)
 !
 ! Here I read information from the BS database
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=io_BS(1,Xbsk,io_ID) 
 !
 ! X type. Also used in PARALLEL setup
 !
 Xbsk%whoami=2
 !
 if (io_err/=0) return
 !
 ! Apply quasi-particle correction if presents
 ! 
 call QP_apply(BS_bands,Xen,Xk,'G',msg_fmt='s')
 !
 if (BS_cpl_K_exchange) BS_cpl_mode='x'
 if (BS_cpl_K_corr) BS_cpl_mode='c'
 if (BS_cpl_K_exchange.and.BS_cpl_K_corr) BS_cpl_mode='xc'
 !
 call parser('DipWeight',l_weight_with_dipoles)
 !
 BS_H_dim=BS_K_dim
 if (BS_K_coupling) BS_H_dim=2*BS_K_dim
 !
 ! Get state indexes
 !
 n_lambda = get_lambda()
 !
 l_flag=l_sort .or. (l_exc_wf.and.n_lambda>0) .or. (l_amplitude.and.n_lambda>0) .or. l_spin
#if defined _YPP_ELPH
 l_flag=l_flag .or. l_eliashberg .or. l_gkkp
#endif
#if defined _YPP_RT
 l_flag=l_flag .or. l_RT_abs
#endif
 if(.not.l_flag) return
 !
 ! Allocation of energies and residuum, then also eigenvector
 !
 allocate(BS_R(BS_H_dim),BS_E(BS_H_dim))
 call mem_est('BS_R_E',(/2*BS_H_dim/))
 iq=BS_q(1)
 !
 ! Here I read energies and residuum
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_err=io_BSS_diago(iq,io_ID,BS_H_dim,BS_E,BS_R)
 if (io_err/=0) return
 !
 call section('*','Excitonic Properties')
 !
 if (l_sort) then
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=io_ID)
   io_err=io_BSS_diago(iq,io_ID,BS_H_dim,BS_E,BS_R)
   call excitons_sort_and_report(BS_H_dim,Xk,Xen,BS_R,BS_E)
   deallocate(BS_R,BS_E)
   call mem_est('BS_R_E')
   return
 endif
 !
 ! Loading tables and eigenvectors
 !
 allocate(BSS_eh_table(BS_H_dim,3+n_sp_pol-1),BS_mat(BS_H_dim,BS_H_dim),&
          BS_E_degs(BS_H_dim))
 call mem_est("BSS_eh_table",(/(3+n_sp_pol-1)*BS_H_dim/),(/IP/))
 call mem_est('BS_mat',(/BS_H_dim**2/))
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=io_ID)
 io_err=io_BSS_diago(iq,io_ID,BS_H_dim,BS_E,BS_R)
 !
 if(l_weight_with_dipoles) then
   !call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
   !io_err=io_DIPOLES(Xdip,Xen,io_ID)
   !allocate(q_norm(1),bare_qpg(1,1))
   !Wd=(0._SP,0._SP)
   !call K_Transitions_setup(1,Xen,Xk,Wd,.false.)
   !call PARALLEL_global_indexes(Xen,Xk,q,"Response_T_space",X=Xdip)
   !call Dipole_driver(Xen,Xk,Xdip,BSS_q0)
   !deallocate(q_norm,bare_qpg)
 endif
 !
 ! Create the anti-resonant part of the eh_table
 !
 if (BS_K_coupling) then
   do neh=BS_K_dim+1,BS_H_dim
     BSS_eh_table(neh,:)=BSS_eh_table(neh-BS_K_dim,1)
     !
     ! invert conduction <-> valence order
     !
     BSS_eh_table(neh,2)=BSS_eh_table(neh-BS_K_dim,3)
     BSS_eh_table(neh,3)=BSS_eh_table(neh-BS_K_dim,2)
   enddo
 endif
 !
 !
 if (.not.BSS_write_eig_2_db) call error(&
&   'Diagonalization database does not contain excitonic eigenstates. Re-run yambo using the WRbsWF flag in the input file.')
 !
 ! Sort energies to find degenerate states
 !========================================
 !
 call excitons_sort_and_report(BS_H_dim,Xk,Xen,BS_R,BS_E,BS_E_degs=BS_E_degs)
 !
 ! Excitonic Eliashberg function 
 !================================
 !
 !GPL_EXCLUDE_START
 !
#if defined _YPP_ELPH
 if (l_eliashberg.or.l_gkkp) then
   allocate (lambda_s(n_lambda)) 
   n_lambda = get_lambda()
   if (l_eliashberg) call ELPH_general_gFsq(k,en,Xk,Xen,q,lambda_s,BS_E_degs)
   if (l_gkkp)       call ELPH_excitonic_gkkp(Xk,lambda_s)
   return
 endif
#endif
 !
#if defined _YPP_RT
 !
 if(l_RT_abs) then
   call RT_transient_absorption(Xbsk,Xen,Xk,q,BS_H_dim,BS_E,BS_R)
   return
 endif
 !
#endif
 !
 !GPL_EXCLUDE_END
 !
 ! Exciton's spin (collinear case)
 !================================
 !
 if (l_spin.and.n_spinor==1) then
   allocate(S_z(BS_H_dim),S_sq(BS_H_dim))
   if (n_sp_pol==2)  call excitons_collinear(BS_E_degs,S_z,S_sq)
   call excitons_sort_and_report(BS_H_dim,Xk,Xen,BS_R,BS_E,S_sq=S_sq,S_z=S_z)
   deallocate(S_z,S_sq)
   return
 endif
 ! 
 ! Loading  Wf 
 !
 if (l_exc_wf.or.(l_spin.and.n_spinor==2)) call WF_load(WF,wf_ng,1,BS_bands,(/1,nXkibz/),space='R',title='-EXCWF')
 !
 ! Exciton's spin (not collinear case)
 !=====================================
 if (l_spin.and.n_spinor==2) then
   !
   allocate(S_z(BS_H_dim),S_sq(BS_H_dim))
   allocate (lambda_s(n_lambda)) 
   !
   n_lambda = get_lambda()
   ! 
   call excitons_non_collinear(Xk,lambda_s,n_lambda,S_z,S_sq)
   call excitons_sort_and_report(BS_H_dim,Xk,Xen,BS_R,BS_E,S_sq=S_sq,S_z=S_z)
   !
   deallocate(lambda_s)
   deallocate(S_z,S_sq)
   !
   return
 endif
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
   allocate(A_weight(BS_H_dim))
   call mem_est('A_weight',(/BS_H_dim/),(/SP/))
   !
   do i_lambda=1,n_lambda
     !
     A_weight   =0._SP
     !
     lambda=lambda_s(i_lambda)
     !
     n_lambda_deg=count(BS_E_degs==BS_E_degs(lambda))
     !
     if (n_lambda_deg>1) call msg('s',':: State '//trim(intc(lambda))//' Merged with states '//&
&                                     trim(intc(BS_E_degs(lambda)))//' -> '//&
&                                     trim(intc(BS_E_degs(lambda)+n_lambda_deg-1)))
     do neh = 1,BS_H_dim
       !
       dip_factor=1._SP
       !
       if(l_weight_with_dipoles) then
         ikbz  = BSS_eh_table(neh,1)
         iv    = BSS_eh_table(neh,2)
         ic    = BSS_eh_table(neh,3)
         i_spin= spin(BSS_eh_table(neh,:))
         dip_factor=-conjg(DIP_projected(ic,iv,ikbz,i_spin))
       endif
       !
       do i_l=BS_E_degs(lambda),BS_E_degs(lambda)+n_lambda_deg-1
         A_weight(neh)=A_weight(neh)+(BS_mat(neh,i_l)*dip_factor)*conjg(BS_mat(neh,i_l)*dip_factor)
       enddo
       !
     enddo
     !
     norm_ = sqrt(dot_product(A_weight,A_weight))
     if(l_weight_with_dipoles.and.norm_>0._SP) A_weight=A_weight/norm_
     !
     call excitons_sort_and_report(BS_H_dim,Xk,Xen,BS_R,BS_E,A_weight=A_weight,verbose=i_lambda==1)
     !
   enddo
   !
   ! CLEAN
   !
   deallocate(BS_R,BS_E,lambda_s)
   call mem_est('BS_R_E')
   deallocate(BSS_eh_table,BS_mat,A_weight)
   call mem_est('BSS_eh_table BS_mat A_weight')
   !
   return
   !
 endif
 !
 !############################################################################
 ! EXCITONIC WF SECTION
 !############################################################################
 !
 if (.not.l_exc_wf) return
 !
 call section('+','Excitonic Wave Function')
 !
 ! Check that directions are OK
 !
 call plot_check_and_launch(.true.)
 !
 ! Constructing Grid 
 !
 call section('+','Real-Space grid setup')
 !
 if (l_free_hole) ncell=1
 !
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
     ! The number of cells is always odd so that the hole can
     ! be placed in the middle.
     !
     r_hole_rlu(j1)=r_hole_rlu(j1)+ncell(j1)/2*fft_dim(j1)
     !
   enddo
   ir_hole=1+r_hole_rlu(1)+ r_hole_rlu(2)*nr(1)+ r_hole_rlu(3)*nr(1)*nr(2)
   r_hole=r_cell_cc(:,ir_hole)
   !
   call msg('s','::      translated position      [cc]:',r_hole)
   call msg('s','::                                [A]:',r_hole*BO2ANG)
   !
 endif
 !
 ! Allocation
 !
 allocate(wf_vc(BS_H_dim),v2plot(nr_tot))
 call mem_est('wf_vc v2plot ',(/2*BS_H_dim,nr_tot/),(/SP,SP/))
 !
 ! Par Proc
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/nr_tot/))
 call PP_redux_wait
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
   if (n_lambda_deg>1) call msg('s',':: State '//trim(intc(lambda))//' Merged with states '//&
&                                   trim(intc(BS_E_degs(lambda)))//' -> '//&
&                                   trim(intc(BS_E_degs(lambda)+n_lambda_deg-1)))
   !
   call live_timing('ExcWF@'//trim(intc(lambda)),px%n_of_elements(myid+1))  
   !
   v2plot  =0._SP
   wf_vc   =(0._SP,0._SP)
   !
   do ir=1,nr_tot
     !
     if (.not.px%element_1D(ir)) cycle
     !
     if (l_free_hole) then
       r_eh(:)=0.
       ir_hole=ir
     else
       r_eh(:)=r_cell_cc(:,ir)-r_hole(:)
     endif
     !
     do neh = 1,BS_H_dim
       !
       ikbz  = BSS_eh_table(neh,1)
       iv    = BSS_eh_table(neh,2)
       ic    = BSS_eh_table(neh,3)
       i_spin= spin(BSS_eh_table(neh,:))
       !
       ikibz = Xk%sstar(ikbz,1)
       is    = Xk%sstar(ikbz,2)
       !
       k_dot_r = dot_product(r_eh,Xk%ptbz(ikbz,:)/alat(:))*2.*pi
       !
       WF1_ir=WF_symm(rindex(ir_hole),(/iv,ikibz,is,i_spin/))
       WF2_ir=WF_symm(rindex(ir     ),(/ic,ikibz,is,i_spin/))
       !
       wf_vc(neh) = conjg(WF1_ir(1))*WF2_ir(1)
       if(n_spinor==2) wf_vc(neh) = wf_vc(neh) + conjg(WF1_ir(2))*WF2_ir(2)
       !
       wf_vc(neh) = wf_vc(neh)*exp(cI*k_dot_r)
       ! 
     enddo  !Matrix elements
     !
     do i_l=BS_E_degs(lambda),BS_E_degs(lambda)+n_lambda_deg-1
       !
       wf_ = V_dot_V(BS_H_dim,BS_mat(1,i_l),wf_vc)
       !
       v2plot(ir) = v2plot(ir)+abs(wf_)**2.  
       !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo   !grid points
   !
   call live_timing()
   !
   call PP_redux_wait(v2plot)
   !
   ! PLOT
   !
   if (use_cube) output_fname='exc_'//trim(intc(plot_dim))//'d_'//trim(intc(lambda))//'.cube'
   if (use_xcrysden) output_fname='exc_'//trim(intc(plot_dim))//'d_'//trim(intc(lambda))//'.xsf'
   if (use_gnuplot)  output_fname='exc_'//trim(intc(plot_dim))//'d_'//trim(intc(lambda))
   !
   if (use_cube) then 
     call of_open_close(trim(output_fname),'o')
   else
    call of_open_close(trim(output_fname),'ot')
    do i1=1,BSS_n_descs
      call msg('o exc',"#",trim(BSS_description(i1)),INDENT=0)
    enddo
      call msg('o exc',"#")
   endif
   !
   call plot_check_and_launch(.false.)
   !
   call of_open_close(trim(output_fname))
   !
 enddo
 !
 call PP_indexes_reset(px)
 !
 ! CLEAN
 !
 deallocate(r_cell_cc,rindex)
 deallocate(v2plot,BS_R,BS_E,lambda_s)
 call mem_est('v2plot BS_R_E')
 deallocate(BSS_eh_table,BS_mat,wf_vc)
 call mem_est('BSS_eh_table BS_mat wf_vc')
 call WF_free(WF)
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
