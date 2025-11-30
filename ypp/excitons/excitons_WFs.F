!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DV
!
! headers
!
#include<y_memory.h>
!
subroutine exciton_WFs(Xk,BS_E_degs,iq)
 !
 use pars,          ONLY:SP,cZERO,pi,schlen
 use R_lattice,     ONLY:bz_samp,qindx_X
 use units,         ONLY:BO2ANG
 use stderr,        ONLY:intc
 use D_lattice,     ONLY:a,alat
 use FFT_m,         ONLY:fft_dim,fft_size
 use com,           ONLY:msg,of_open_close
 use electrons,     ONLY:n_spinor,n_sp_pol
 use YPPm,          ONLY:v2plot,ncell,r_fixed,l_free_hole,AVE_exc,center_kind,&
&                        nr,nr_tot,output_fname,l_average_eh,elec_spin,hole_spin,&
&                        use_xcrysden,use_gnuplot,use_cube,plot_dim,&
&                        EXCITONS_user_indexes,EXCITONS_n_user_states
 use BS_solvers,    ONLY:BSS_eh_table,BSS_eh_table_m1,BS_mat,BSS_desc,BSS_n_eig
 use BS,            ONLY:BS_bands,BS_H_dim
 use interfaces,    ONLY:WF_load,WF_free,DESC_write
 use wave_func,     ONLY:WF,wf_ng
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_wait,PP_redux_wait,PARALLEL_index
 use openmp,        ONLY:OPENMP_update,master_thread
 use LIVE_t,        ONLY:live_timing
 use wrapper,       ONLY:V_dot_V
 use vec_operate,   ONLY:c2a
 use y_memory_alloc
 !
 implicit none
 !
 type(bz_samp)     :: Xk
 integer           :: BS_E_degs(BSS_n_eig),iq
 ! 
 ! Workspace
 !
 type(PP_indexes)  :: px
 logical           :: fixed_elec_plot,fixed_hole_plot
 integer           :: iv,ic,ikbz,ikibz,is,neh,i1,i2,i_lambda,j_lambda,i_sp_c,i_sp_v,ir,ir_fixed,j1,&
&                     n_lambda_deg,BS_E_deg_done,neh1,iv1,ic1,ikbz1,ikibz1,i_sp_c1,i_sp_v1,is1,&
&                     i_spin_hole,i_spin_elec,n1_lambda,n2_lambda
 integer           :: ikpbz,ikp,isp,i_spinor,j_spinor,rh_size,ir_fixed_rlu(3)
 character(schlen) :: exc_index_msg
 real(SP)          :: r_fixed_rlu(3),k1(3),k2(3)
 complex(SP)       :: B_coeff
 !
 integer,  allocatable :: rindex(:)
 real(SP), allocatable :: r_cell_cc(:,:),kp_dot_r_h(:),k_dot_r_e(:),alpha(:)
 complex(SP), allocatable :: wf_tmp(:,:,:),wf_tmp_uc(:,:,:),wf_vc(:,:,:),WF1(:,:),WF2(:,:),phase(:)
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
 if (l_free_hole.or.l_average_eh) ncell=1
 !
 fixed_hole_plot=trim(center_kind)=="Hole"
 fixed_elec_plot=trim(center_kind)=="Electron"
 !
 if (.not.(fixed_hole_plot.or.fixed_elec_plot)) call error("Wrong CenterKind in input")
 !
 call WF_load(WF,wf_ng,1,BS_bands,(/1,Xk%nibz/),space='R',title='-EXCWF')
 !
 call expand_grid()
 !
 call k_ibz2bz(Xk,'i',.false.)
 !
 if (.not.l_free_hole.and..not.l_average_eh) then
   !
   ! Translation & location in the big grid of the hole...
   !======================================================
   ! 
   ! [1] Bare position pushed in the smallest cell 
   ! 
   call c2a(b_in=a,v_in=r_fixed,v_out=r_fixed_rlu,mode='kc2a')
   do j1=1,3
     r_fixed_rlu(j1)=r_fixed_rlu(j1)-int(r_fixed_rlu(j1))
   enddo
   call c2a(b_in=a,v_in=r_fixed_rlu,v_out=r_fixed,mode='ka2c')
   call msg('s',' '//center_kind(1:4)//' position in the DL cell      ',r_fixed,"[c.c.]")
   !
   ! [2] Bare position in the FFT grid
   !
   call c2a(b_in=a,v_in=r_fixed,v_out=r_fixed_rlu,mode='kc2a')
   ir_fixed_rlu(:)=nint(r_fixed_rlu(:)*fft_dim(:))
   ir_fixed=1+r_fixed_rlu(1)+ r_fixed_rlu(2)*nr(1)+ r_fixed_rlu(3)*nr(1)*nr(2)
   if (ir_fixed.lt.0) call error(' Fixed '//trim(center_kind)//' coordinates in input must be positive') 
   call msg('s','      position in the FFT grid     ',r_cell_cc(ir_fixed,:),"[c.c.]")
   !
   ! [3] Translation in the center of the Big grid
   !
   do j1=1,3
     if (ncell(j1)==1) cycle
     !
     ! The number of cells is always odd so that the hole can
     ! be placed in the middle.
     !
     ir_fixed_rlu(j1)=ir_fixed_rlu(j1)+ncell(j1)/2*fft_dim(j1)
     !
   enddo
   ir_fixed=1+ir_fixed_rlu(1)+ ir_fixed_rlu(2)*nr(1)+ ir_fixed_rlu(3)*nr(1)*nr(2)
   r_fixed=r_cell_cc(ir_fixed,:)
   !
   call msg('s','      translated position          ',r_fixed,"[c.c.]")
   call msg('s','                                   ',r_fixed*BO2ANG,"[A]")
   !
 endif
 !
 i_spin_hole=-1
 if(trim(hole_spin)=="up")      i_spin_hole=1
 if(trim(hole_spin)=="dn")      i_spin_hole=2
 if(trim(hole_spin)=="average") i_spin_hole=0
 if(trim(hole_spin)=="diag")    i_spin_hole=-10
 !
 i_spin_elec=-1
 if(trim(elec_spin)=="up")      i_spin_elec=1
 if(trim(elec_spin)=="dn")      i_spin_elec=2
 if(trim(elec_spin)=="average") i_spin_elec=0
 if(trim(elec_spin)=="diag")    i_spin_elec=-10
 !
 if (i_spin_elec==-1) call error(" wrong electron spin in input")
 if (i_spin_hole==-1) call error(" wrong hole spin in input")
 !
 ! DS note: to change this for magnons
 if (n_sp_pol==2 .and. (i_spin_elec+i_spin_hole)==3) then
   call error("Spin polarized case: electron and hole cannot have opposite spin")
 endif
 !
 if (n_spinor==2 .and. (i_spin_elec+i_spin_hole)<0) then
   call warning("Spinorial case: diagonal approximation in spin space")
   i_spin_elec=-10
   i_spin_hole=-10
 endif
 !
 if (l_free_hole) then
   call warning("Free hole case, imposing elec spin to be identical to hole spin")
   i_spin_elec=i_spin_hole
 endif
 !
 ! Allocation
 !
 YAMBO_ALLOC(v2plot,(nr_tot))
 !
 YAMBO_ALLOC(WF1,(fft_size,n_spinor))
 YAMBO_ALLOC(WF2,(fft_size,n_spinor))
 !
 if(.not.l_average_eh) then
   rh_size=1
   if (l_free_hole) rh_size=nr_tot
   YAMBO_ALLOC(k_dot_r_e,(nr_tot))
   YAMBO_ALLOC(kp_dot_r_h,(rh_size))
   !
   i1=n_spinor
   if(l_free_hole) i1=1
   YAMBO_ALLOC(wf_vc,(fft_size,n_spinor,i1))
   YAMBO_ALLOC(wf_tmp_uc,(fft_size,n_spinor,i1))
   YAMBO_ALLOC(alpha,(nr_tot))
   YAMBO_ALLOC(phase,(nr_tot))
   YAMBO_ALLOC(wf_tmp,(nr_tot,n_spinor,i1))
 endif
 !
 call build_inverse_BS_eh_table_from_BSS_table(Xk%nbz)
 !
 ! Par Proc
 !
 if(l_average_eh.or.Xk%nbz==1) then
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/BS_H_dim/))
   call PP_wait()
 else
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/Xk%nbz/))
   call PP_wait()
 endif
 !
 ! Loop on exc states
 !===================
 !
 call msg('s','Processing '//trim(intc(EXCITONS_n_user_states))//' states')
 !
 BS_E_deg_done=-1
 !
 do i_lambda=1,EXCITONS_n_user_states
   !
   j_lambda=EXCITONS_user_indexes(i_lambda)
   !
   if (BS_E_degs(j_lambda)/=BS_E_deg_done) i1=0
   !
   BS_E_deg_done=BS_E_degs(j_lambda)
   !
   i1=i1+1
   !
   n_lambda_deg=count(BS_E_degs==BS_E_degs(j_lambda))
   !
   n1_lambda=BS_E_degs(j_lambda)
   n2_lambda=BS_E_degs(j_lambda)+n_lambda_deg-1
   !
   if(l_average_eh) then
     !
     if(i1==1) v2plot  =0._SP
     !
     call live_timing('ExcWF@'//trim(intc(j_lambda)),px%n_of_elements(myid+1))  
     !
     do neh = 1,BS_H_dim
       !
       if (.not.px%element_1D(neh)) cycle
       !
       ikbz  = BSS_eh_table(neh,1)
       iv    = BSS_eh_table(neh,2)
       ic    = BSS_eh_table(neh,3)
       i_sp_c= BSS_eh_table(neh,4)
       i_sp_v= BSS_eh_table(neh,5)
       !
       ikibz = Xk%sstar(ikbz,1)
       is    = Xk%sstar(ikbz,2)
       !
       do neh1 = 1,BS_H_dim
         !
         ikbz1  = BSS_eh_table(neh1,1)
         iv1    = BSS_eh_table(neh1,2)
         ic1    = BSS_eh_table(neh1,3)
         i_sp_c1= BSS_eh_table(neh1,4)
         i_sp_v1= BSS_eh_table(neh1,5)
         !
         ikibz1 = Xk%sstar(ikbz1,1)
         is1    = Xk%sstar(ikbz1,2)
         !
         if(ikbz/=ikbz1) cycle
         !      
         ! Build the B_{vvk} (or B_{cck} ) matrix        
         !
         if (ic1==ic.and.trim(AVE_exc)=='h') then
           !
           if (n_sp_pol==2 .and. i_spin_hole>0) then
             if (i_sp_v /=i_spin_hole) cycle
             if (i_sp_v1/=i_spin_hole) cycle
           endif
           !
           call WF_apply_symm((/iv, ikibz, is, i_sp_v /),WF1)
           call WF_apply_symm((/iv1,ikibz1,is1,i_sp_v1/),WF2)
           !
           B_coeff = BS_mat(neh, j_lambda)* conjg(BS_mat(neh1, j_lambda))
           !
           do i_spinor=1,n_spinor
             do j_spinor=1,n_spinor
               if (n_spinor==2 .and. i_spin_hole>0) then
                 if(i_spinor /= i_spin_hole) cycle
                 if(j_spinor /= i_spin_hole) cycle
                 if(i_spinor /= j_spinor .and. i_spin_hole==-10) cycle
               endif
               v2plot(:) = v2plot(:)+B_coeff*WF1(:,i_spinor)*conjg(WF2(:,i_spinor))
             enddo
           enddo
           !
         elseif(iv1==iv.and.trim(AVE_exc)=='e') then
           !
           if (n_sp_pol==2 .and. i_spin_elec>0) then
             if (i_sp_c /=i_spin_elec) cycle
             if (i_sp_c1/=i_spin_elec) cycle
           endif
           !
           call WF_apply_symm((/ic, ikibz, is, i_sp_c /),WF1)
           call WF_apply_symm((/ic1,ikibz1,is1,i_sp_c1/),WF2)
           !
           B_coeff = conjg(BS_mat(neh,j_lambda))* BS_mat(neh1,j_lambda)
           !
           do i_spinor=1,n_spinor
             do j_spinor=1,n_spinor
               if (n_spinor==2) then
                 if(i_spinor /= i_spin_elec .and. i_spin_elec>0) cycle
                 if(j_spinor /= i_spin_elec .and. i_spin_elec>0) cycle
                 if(i_spinor /= j_spinor .and. i_spin_elec==-10) cycle
               endif
               v2plot(:) = v2plot(:)+B_coeff*conjg(WF1(:,i_spinor))*WF2(:,j_spinor)
             enddo
           enddo
           !
         endif
         !
       enddo ! loop on neh1
       !
       call live_timing(steps=1)
       !
     enddo ! loop on neh
     !
     call live_timing()
     !
     call PP_redux_wait(v2plot)
     !
   else
     !   
     !
     call live_timing('ExcWF@'//trim(intc(j_lambda)),px%n_of_elements(myid+1))
     !
     wf_tmp= cZERO
     !
     !$omp parallel default(shared), &
     !$omp private(k_dot_r_e,kp_dot_r_h,alpha,phase,ir,neh,&
     !$omp         wf_tmp_uc,WF1,WF2,wf_vc,ikibz,is,ikpbz,ikp,isp,k1,k2,&
     !$omp         ikbz,iv,ic,i_sp_c,i_sp_v,i_spinor,j_spinor), &
     !$omp reduction(+:wf_tmp)
     !
     WF1 = cZERO
     WF2 = cZERO
     !
     call OPENMP_update(master_thread)
     !
     do ikbz  = 1,Xk%nbz
       !
       if (Xk%nbz>1) then
         if (.not.px%element_1D(ikbz)) cycle
       endif
       !
       ikibz = Xk%sstar(ikbz,1)
       is    = Xk%sstar(ikbz,2)
       !
       ikpbz = qindx_X(iq,ikbz,1)
       ikp   = Xk%sstar(ikpbz,1)
       isp   = Xk%sstar(ikpbz,2)
       !
       k1=Xk%ptbz(ikbz,:)/alat(:)*2._SP*pi
       k2=Xk%ptbz(ikpbz,:)/alat(:)*2._SP*pi
       !
       wf_tmp_uc= cZERO
       !
     do iv    = BS_bands(1),BS_bands(2)
     do ic    = BS_bands(1),BS_bands(2)
     do i_sp_c= 1,n_sp_pol
     do i_sp_v= 1,n_sp_pol
       !
       neh=BSS_eh_table_m1(ikbz,iv,ic,i_sp_c,i_sp_v)
       if (neh==0) cycle
       !
       if (Xk%nbz==1) then
         if (.not.px%element_1D(neh)) cycle
       endif
       !
       if (n_sp_pol==2 .and. i_spin_hole>0 .and. i_sp_v /=i_spin_hole) cycle
       !
       call WF_apply_symm((/iv,ikp,  isp,i_sp_v/),WF1)
       call WF_apply_symm((/ic,ikibz,is, i_sp_c/),WF2)
       !
       ! All the following operations are only up to fft_size
       wf_vc=cZERO
       if (l_free_hole) then
         do i_spinor=1,n_spinor
           if (n_spinor==2 .and. i_spin_hole>0 .and. i_spinor /= i_spin_hole) cycle
           wf_vc(:,i_spinor,1) = wf_vc(:,i_spinor,1) + conjg(WF1(:,i_spinor))*WF2(:,i_spinor)
         enddo
       else if (fixed_hole_plot) then
         do i_spinor=1,n_spinor
           if (n_spinor==2 .and. i_spin_hole>0 .and. i_spinor /= i_spin_hole) cycle
           do j_spinor=1,n_spinor
             if (n_spinor==2 .and. i_spin_elec>0 .and. j_spinor /= i_spin_elec) cycle
             if (n_spinor==2 .and. i_spin_elec==-10 .and. i_spinor/= j_spinor) cycle
             wf_vc(:,i_spinor,j_spinor) = wf_vc(:,i_spinor,j_spinor) + &
             &      conjg(WF1(rindex(ir_fixed),i_spinor))*WF2(:,j_spinor)
           enddo
         enddo
       else if (fixed_elec_plot) then
         do i_spinor=1,n_spinor
           if (n_spinor==2 .and. i_spin_hole>0 .and. i_spinor /= i_spin_hole) cycle
           do j_spinor=1,n_spinor
             if (n_spinor==2 .and. i_spin_elec>0 .and. j_spinor /= i_spin_elec) cycle
             if (n_spinor==2 .and. i_spin_elec==-10 .and. i_spinor/= j_spinor) cycle
             wf_vc(:,i_spinor,j_spinor) = wf_vc(:,i_spinor,j_spinor) + &
             &      conjg(WF1(:,i_spinor))*WF2(rindex(ir_fixed),j_spinor)
           enddo
         enddo
       endif
       !
       if (l_free_hole) then
         !
         do i_spinor=1,n_spinor
           if (n_spinor==2 .and. i_spin_hole>0 .and. i_spinor /= i_spin_hole) cycle
           wf_tmp_uc(:,i_spinor,i_spinor) = wf_tmp_uc(:,i_spinor,i_spinor)+  &
             &  BS_mat(neh,j_lambda)*wf_vc(:,i_spinor,1)
         enddo
         !
       else
         !
         do i_spinor=1,n_spinor
           if (n_spinor==2 .and. i_spin_hole>0 .and. i_spinor /= i_spin_hole) cycle
           do j_spinor=1,n_spinor
             if (n_spinor==2 .and. i_spin_elec>0 .and. j_spinor /= i_spin_elec) cycle
             if (n_spinor==2 .and. i_spin_elec==-10 .and. i_spinor/= j_spinor) cycle
             wf_tmp_uc(:,i_spinor,j_spinor) = wf_tmp_uc(:,i_spinor,j_spinor)+&
                &  BS_mat(neh,j_lambda)*wf_vc(:,i_spinor,j_spinor)
           enddo
         enddo
         !
       endif
       !
       if (master_thread.and.Xk%nbz==1) call live_timing(steps=1)
       !
     enddo
     enddo
     enddo
     enddo
       !
       if (l_free_hole) then
         !
         do i_spinor=1,n_spinor
           if (n_spinor==2 .and. i_spin_hole>0 .and. i_spinor /= i_spin_hole) cycle
           !$omp do
           do ir=1,nr_tot
             k_dot_r_e(ir) =r_cell_cc(ir,1)*k1(1)+r_cell_cc(ir,2)*k1(2)+r_cell_cc(ir,3)*k1(3)
             kp_dot_r_h(ir)=r_cell_cc(ir,1)*k2(1)+r_cell_cc(ir,2)*k2(2)+r_cell_cc(ir,3)*k2(3)
             alpha(ir)=k_dot_r_e(ir)-kp_dot_r_h(ir)
             phase(ir)=cmplx(cos(alpha(ir)),sin(alpha(ir)),kind=SP)
             wf_tmp(ir,i_spinor,i_spinor) = wf_tmp(ir,i_spinor,i_spinor)+  &
             &  wf_tmp_uc(rindex(ir),i_spinor,1)*phase(ir)
           enddo
         enddo
         !
       else if (fixed_hole_plot) then
         ! 
         kp_dot_r_h(1) = dot_product(r_cell_cc(ir_fixed,:),k2(:))
         !
         do i_spinor=1,n_spinor
           if (n_spinor==2 .and. i_spin_hole>0 .and. i_spinor /= i_spin_hole) cycle
           do j_spinor=1,n_spinor
             if (n_spinor==2 .and. i_spin_elec>0 .and. j_spinor /= i_spin_elec) cycle
             if (n_spinor==2 .and. i_spin_elec==-10 .and. i_spinor/= j_spinor) cycle
             !$omp do
             do ir=1,nr_tot
               k_dot_r_e(ir) =r_cell_cc(ir,1)*k1(1)+r_cell_cc(ir,2)*k1(2)+r_cell_cc(ir,3)*k1(3)
               alpha(ir)=k_dot_r_e(ir)-kp_dot_r_h(1)
               phase(ir)=cmplx(cos(alpha(ir)),sin(alpha(ir)),kind=SP)
               wf_tmp(ir,i_spinor,j_spinor) = wf_tmp(ir,i_spinor,j_spinor)+&
               &  wf_tmp_uc(rindex(ir),i_spinor,j_spinor)*phase(ir)
             enddo
           enddo
         enddo
         !
       else if (fixed_elec_plot) then
         ! 
         k_dot_r_e(1) = dot_product(r_cell_cc(ir_fixed,:),k1(:))
         !
         do i_spinor=1,n_spinor
           if (n_spinor==2 .and. i_spin_hole>0 .and. i_spinor /= i_spin_hole) cycle
           do j_spinor=1,n_spinor
             if (n_spinor==2 .and. i_spin_elec>0 .and. j_spinor /= i_spin_elec) cycle
             if (n_spinor==2 .and. i_spin_elec==-10 .and. i_spinor/= j_spinor) cycle
             !$omp do
              do ir=1,nr_tot
                kp_dot_r_h(ir) =r_cell_cc(ir,1)*k2(1)+r_cell_cc(ir,2)*k2(2)+r_cell_cc(ir,3)*k2(3)
                alpha(ir)=k_dot_r_e(1)-kp_dot_r_h(ir)
                phase(ir)=cmplx(cos(alpha(ir)),sin(alpha(ir)),kind=SP)
                wf_tmp(ir,i_spinor,j_spinor) = wf_tmp(ir,i_spinor,j_spinor)+&
                &  BS_mat(neh,j_lambda)*wf_vc(rindex(ir),i_spinor,j_spinor)*phase(ir)
             enddo
           enddo
         enddo
         !
       endif
       !
       if (master_thread.and.Xk%nbz>1) call live_timing(steps=1)
       !
     enddo
     !$omp end parallel
     !
     call live_timing()
     !
     call PP_wait()
     !
     call PP_redux_wait(wf_tmp)
     !
     if (i1==1) v2plot= 0._SP
     !
     do i_spinor=1,n_spinor
       if (n_spinor==2 .and. i_spin_hole>0 .and. i_spinor /= i_spin_hole) cycle
       do j_spinor=1,n_spinor
         if (n_spinor==2 .and. i_spin_elec>0 .and. j_spinor /= i_spin_elec) cycle
         if (n_spinor==2 .and. i_spin_elec==-10 .and. i_spinor/= j_spinor) cycle
         do ir=1,nr_tot
           v2plot(ir) = v2plot(ir)+abs(wf_tmp(ir,i_spinor,j_spinor))**2
         enddo
       enddo
     enddo
     !
   endif
   !
   if (i1/=n_lambda_deg) cycle
   !
   if (n_lambda_deg==1) exc_index_msg=trim(intc(j_lambda))
   if (n_lambda_deg> 1) exc_index_msg=trim(intc(n1_lambda))//'-'//trim(intc(n2_lambda))
   !
   call msg('s','Plotting state(s) '//trim(exc_index_msg))
   !
   ! PLOT
   !
   output_fname='exc_qpt'//trim(intc(iq))//"_"//trim(intc(plot_dim))//'d_'//trim(exc_index_msg)
   !
   if (use_cube)      output_fname=trim(output_fname)//'.cube'
   if (use_xcrysden)  output_fname=trim(output_fname)//'.xsf'
   if (use_gnuplot)   output_fname=trim(output_fname)
   !
   if (use_cube) then 
     call of_open_close(trim(output_fname),'o')
   else
    call of_open_close(trim(output_fname),'ot')
    call DESC_write('o exc','#',BSS_desc)
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
 YAMBO_FREE(v2plot)
 YAMBO_FREE(r_cell_cc)
 YAMBO_FREE(rindex)
 !
 YAMBO_FREE( k_dot_r_e)
 YAMBO_FREE(kp_dot_r_h)
 !
 YAMBO_FREE(WF1)
 YAMBO_FREE(WF2)
 !
 YAMBO_FREE(wf_vc)
 YAMBO_FREE(wf_tmp)
 YAMBO_FREE(wf_tmp_uc)
 !
 call WF_free(WF)
 !
 contains
   !
   subroutine expand_grid()
     !
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
     YAMBO_ALLOC(r_cell_cc,(nr_tot,3))
     YAMBO_ALLOC(rindex,(nr_tot))
     !
     nr=(/ncell(1)*fft_dim(1),ncell(2)*fft_dim(2),ncell(3)*fft_dim(3)/)
     call msg('s',' Extended grid ',nr)
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
           r_cell_cc(ir,:) = ir1*a(1,:)/fft_dim(1) +&
&                            ir2*a(2,:)/fft_dim(2) +&
&                            ir3*a(3,:)/fft_dim(3)
         end do
       end do
     end do
     !
   end subroutine
   !
end subroutine exciton_WFs
