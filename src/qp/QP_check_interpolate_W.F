!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AG GS
!
! AF: This source needs cleanup
!     - proper indentation
!     - del of old/unused source code
!     - ...
!
subroutine check_W_interpolation(q,Xw,f_coeff,delta_q,vX,idx_q,idx_is,idx_G)
 !
 use pars,          ONLY:SP,pi,zero_dfl,schlen,DP
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,RIM_W_ng,g_vec,b,k_grid_b,ng_vec,rl_sop,bare_qpg,&
&                        RIM_W,q0_def_norm,cut_is_slab,idir,k_grid,RIM_id_epsm1_reference,RIM_epsm1,rimw_type
 use vec_operate,   ONLY:c2a,v_is_zero,v_norm,iku_v_norm
 use X_m,           ONLY:X_mat,X_t
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:OP_RD_CL,NONE,RD_CL,OP_RD,RD_CL_IF_END,&
&                        deliver_IO_error_message,OP_WR_CL,REP,VERIFY
 use stderr,        ONLY:STRING_split
 use D_lattice,     ONLY:alat,a
 use matrix_operate, ONLY:m3inv,mat_transpose
 !
 implicit none
 type(bz_samp) :: q
 type(w_samp)  :: Xw
 complex(DP)   :: f_coeff(20,RIM_W_ng,RIM_W_ng,q%nibz,Xw%n_freqs)
 real(SP)      :: delta_q(3,3)
 complex(DP)   :: vX(7,q%nibz,RIM_W_ng,RIM_W_ng,Xw%n_freqs)
 integer       :: idx_q(RIM_W_ng,q%nibz,7),idx_G(RIM_W_ng,q%nibz,7),idx_is(RIM_W_ng,q%nibz,7)
 !
 ! Work Space
 !
 integer  :: out_unit_W_fit, out_unit_W_num,npoints(3),find,iq,i_int,iomega
 integer  :: Np, i, ig1, ig2, iqibz, is, gmax, igr, igc, pos, i_dir,vecsign1,vecsign2, j
 real(SP) :: dq, q_sampl(3),q_rlu(3),lcut,slab_vz1, slab_vplane1,slab_vz2,symminv(3,3),&
&            slab_vplane2,pre_factor,r1,vslab,q_num_norm,q_out_norm
 real(SP) :: g1_2D_mod, g2_2D_mod, qpGr(3), qpGc(3), idx_q1, q_out(3),q_num(3), &
&            qpG_trial(3),anis_fact,em1_anis(3),q_min
 complex(SP):: func,epsm1_sampl,W_sampl,f_coeff_loc(20)
 character(schlen):: str_piece1,str_piece2,str_piece3,file1,file2
 character(1) :: dir(3)
 integer, allocatable :: indexes(:,:,:)
 integer :: q_nn, Gr_nn, Gc_nn, n_indx_steps(3)
 integer :: iq_trial,iq_ibz_trial,is_trial,ig_trial
 !
 Np = 51
 dq = 1./(Np-1)
 q_min=1.E-5
 q_sampl = (/0.,0.,0./)
 gmax = RIM_W_ng
 lcut=alat(idir(1))/2._SP
 dir = (/"1","2","3"/)
 !out_unit_W_fit=(/200,300/)
 !out_unit_W_num=(/400,500/)
 out_unit_W_fit=200
 out_unit_W_num=400
 call k_ibz2bz(q,'i',.true.)
 !
 !Anisotropy initialization
 em1_anis=RIM_epsm1(:)-1._SP
 !
 if (RIM_id_epsm1_reference<0.or.RIM_id_epsm1_reference>3) RIM_id_epsm1_reference=0
 if (RIM_id_epsm1_reference==0) em1_anis=0.
 !
 n_indx_steps = k_grid/2
 !Allocate indexes
 !if (delta_q(1,1) > delta_q(2,2)) then
 !  n_indx_steps(1) = min(k_grid(1),k_grid(2))/2-1+1
 !  n_indx_steps(2) = max(k_grid(1),k_grid(2))/2-1+1
 !else
 !  n_indx_steps(1) = max(k_grid(1),k_grid(2))/2-1+1
 !  n_indx_steps(2) = min(k_grid(1),k_grid(2))/2-1+1
 !end if
 npoints = (n_indx_steps)*2+1
 allocate(indexes(max(npoints(1),npoints(2),npoints(3)),4,3))
 call msg('r', "npoints_1 = ", npoints(1))
 call msg('r', "npoints_2 = ", npoints(2))
 call msg('r', "npoints_3 = ", npoints(3))
 call msg('r', "n_indx_steps_1 = ", n_indx_steps(1))
 call msg('r', "n_indx_steps_2 = ", n_indx_steps(2))
 call msg('r', "n_indx_steps_3 = ", n_indx_steps(3))
 !
 !Print to file interpolated W data
 do iomega=1,Xw%n_freqs
 do i_dir=1,3
   !
   do igr=1,gmax
     !
     do igc=igr,gmax
       !
       !Open output files
       write(str_piece1, '(I2.2)') igr
       write(str_piece2, '(I2.2)') igc
       write(str_piece3, '(I2.2)') iomega
       file1="W_w="//trim(str_piece3)//"_"//dir(i_dir)//"_fit_g"// &
&             trim(str_piece1)//"_g"//trim(str_piece2)//".dat"
       file2="W_w="//trim(str_piece3)//"_"//dir(i_dir)//"_num_g"// &
&             trim(str_piece1)//"_g"//trim(str_piece2)//".dat"
       open (unit=out_unit_W_fit,file=file1, action="write",status="replace")
       open (unit=out_unit_W_num,file=file2, action="write",status="replace")
       !
       !
       !###################################
       !# Get the indexes of the q points #
       !###################################
       !
       do i=1,npoints(i_dir)
         !
         !First: get the (q+Gr,q+Gc) point
         !
         q_num = (/0.,0.,0./)
         q_num = delta_q(i_dir,:)*(i-1-n_indx_steps(i_dir))
         call c2a(b_in=b,v_in=q_num,mode='ka2i')
         !
         qpGr  = q_num + g_vec(igr,:)
         qpGc  = q_num + g_vec(igc,:)
         !
         vecsign1 = sign(1,i-n_indx_steps(i_dir)-1)
         !
         !Get q output in cc
         !
         call c2a(b_in=b,v_in=q_num,mode='ki2c')
         q_num_norm = v_norm(q_num)*vecsign1
         !
         ! Second: reduce (q+Gr,q+Gc) to the iBZ and store indexes
         !
         find = 0
         !
         !Find nn indexes
         !
         do ig_trial=1,ng_vec
           !
           do iq_trial=1,q%nbz
             !
             iq_ibz_trial = q%sstar(iq_trial,1)
             is_trial = q%sstar(iq_trial,2)
             !
             qpG_trial = matmul(rl_sop(:,:,is_trial),q%pt(iq_ibz_trial,:)+g_vec(ig_trial,:))
             !
             if (v_is_zero(qpGr-qpG_trial)) then
             !
             indexes(i,1,i_dir) = iq_ibz_trial
             indexes(i,2,i_dir) = is_trial
             indexes(i,3,i_dir) = ig_trial
             !
             find = find+1
             !
             end if
             !
             if (v_is_zero(qpGc-qpG_trial)) then
             !
             !Select the indexes
             indexes(i,4,i_dir) = ig_trial
             find = find+1
             !
             end if
             !
             if (find == 2) exit
             !
           end do
           !
           if (find == 2) exit
           !
         end do
         if (find /= 2) call error('I have not find n.n. for the plotting of W')
         ! Rename index variables
         iq  = indexes(i,1,i_dir)
         is  = indexes(i,2,i_dir)
         ig1 = indexes(i,3,i_dir)
         ig2 = indexes(i,4,i_dir)
         !
         if(ig1 > RIM_W_ng.or.ig2 > RIM_W_ng) then
           ! call msg("rs", "skipped ig1 =",ig1)
           ! call msg("rs", "skipped ig2 =",ig2)
           cycle
           ! call msg("rs", "not skipped ig2")
         end if
         !----------------------------------
         ! Third: Evaluate numerical f,vc,W
         !----------------------------------
         !
         ! Evaluate vslab
         !
         vslab = 4._DP*pi/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)
         !
         ! Evaluate func
         !
         func = bare_qpg(iq,ig1)*bare_qpg(iq,ig2)/(4._DP*pi)*vX(1,iq,ig1,ig2,iomega)/(vX(1,iq,ig1,ig2,iomega)+1._DP)
         !
         ! Evaluate W and eps^-1 (for metals if ig1==ig2 save W, otherwise Wcorr)
         !
         epsm1_sampl = vX(1,iq,ig1,ig2,iomega)
         W_sampl = vX(1,iq,ig1,ig2,iomega)*vslab
         !
         if (ig1==ig2 .and. iomega==1) then
           !
           select case (trim(rimw_type))
           !
           case ("metal")
               !
               epsm1_sampl = 1._SP + vX(1,iq,ig1,ig2,iomega)
               !
               W_sampl = (1._SP+vX(1,iq,ig1,ig2,iomega))*vslab
               !
           end select
           !
         end if
         !
         ! For metals, regularize the numerical limit of vc and W with Thomas-Fermi
         !
         if (iq==1 .and. ig1==1 .and. ig2==1 .and. iomega==1) then
           !
           select case (trim(rimw_type))
           !
           case ("metal")
           !
           func = bare_qpg(iq+1,ig1)*bare_qpg(iq+1,ig2)/(4._DP*pi)*vX(1,iq+1,ig1,ig2,iomega)/(vX(1,iq+1,ig1,ig2,iomega)+1._DP)
           !
           if (cut_is_slab) then
             !
             epsm1_sampl = q_num_norm/(q_num_norm-2._SP*pi*func*alat(idir(1)))
             !
             W_sampl = 2._SP*pi/(q_num_norm-2._SP*pi*func*alat(idir(1)))
             !
           else
           epsm1_sampl = q_num_norm**2/(q_num_norm**2-4._SP*pi*func)
           !
           W_sampl = 4._SP*pi/(q_num_norm**2-4._SP*pi*func)
           !
           end if
         end select
         !
         else if (iq==1 .and. ig1==1 .and. ig2.ne.1 .and. iomega==1 .and. .not. cut_is_slab) then
           select case (trim(rimw_type))
           !
           case ("metal")
           ! For metals, regularize the numerical limit through neighbours
           ! Store f as f/V00
           func = bare_qpg(iq+1,ig2)/bare_qpg(iq+1,ig1)/(4._DP*pi)*vX(1,iq+1,ig1,ig2,iomega)/(vX(1,iq+1,ig1,ig2,iomega)+1._DP)
           !
           epsm1_sampl = func/bare_qpg(iq,ig2)*bare_qpg(iq,ig1)
           !
           W_sampl = (4._SP*pi)**2*func/bare_qpg(iq,ig2)**2
           !
           func = func*bare_qpg(iq,ig1)**2
           !
           end select
         end if
         !
         !Write output
         !
         write(out_unit_W_num,"(I4, 11E15.6)") &
&                         i, q_num(1),q_num(2),q_num(3),q_num_norm,vslab,real(func),aimag(func),real(epsm1_sampl),&
&                         aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
         !
         ! For first and last points, skip the fit
         !
         !if(i == 1 .or. i == npoints(i_dir)) cycle
         !
         !-------------------------------
         ! Fourth: Evaluate fitted f,vc,W
         !-------------------------------
         do i_int=1,Np
           !
           if (i == n_indx_steps(i_dir)+1) then
             vecsign2 = sign(1,i_int-Np/2-1)
           else
             vecsign2=1
           end if
           !
           !q_sampl in rlu
           q_sampl = (/0.,0.,0./)
           q_sampl = (dq*(i_int-1)-0.5)*delta_q(i_dir,:)
           !
           ! keep q_sampl in rlu
           q_rlu=q_sampl
           ! Convert q_sampl to iku
           call c2a(b_in=b,v_in=q_sampl,mode='ka2i')
           !
           ! Save norm in rlu
           q_out = q_sampl
           call c2a(b_in=b,v_in=q_out,mode='ki2c')
           q_out = q_num+q_out
           q_out_norm = v_norm(q_out)*vecsign1*vecsign2
           !
           ! Evaluate v_col
           !=================
           r1 = 1.
           if (iku_v_norm(qpGr+q_sampl) < q0_def_norm) then
             r1 = r1*q0_def_norm
           else
             r1 = r1*iku_v_norm(qpGr+q_sampl)
           end if
           if (iku_v_norm(qpGc+q_sampl) < q0_def_norm) then
             r1 = r1*q0_def_norm
           else
             r1 = r1*iku_v_norm(qpGc+q_sampl)
           end if
           !
           vslab=4._DP*pi
           !
           if(cut_is_slab) then
             !r1=iku_v_norm(qpGr+q_sampl)*iku_v_norm(qpGc+q_sampl)
             !kz
             slab_vz1=(q_sampl(idir(1))+qpGr(idir(1)))*2.*pi/alat(idir(1))
             slab_vz2=(q_sampl(idir(1))+qpGc(idir(1)))*2.*pi/alat(idir(1))
             !kxy
             slab_vplane1=sqrt(((q_sampl(idir(2))+qpGr(idir(2)))*2.*pi/alat(idir(2)))**2+&
&                              ((q_sampl(idir(3))+qpGr(idir(3)))*2.*pi/alat(idir(3)))**2)
             !
             slab_vplane2=sqrt(((q_sampl(idir(2))+qpGc(idir(2)))*2.*pi/alat(idir(2)))**2+&
&                              ((q_sampl(idir(3))+qpGc(idir(3)))*2.*pi/alat(idir(3)))**2)
             !
             if ((slab_vplane1 < q0_def_norm) .and. (i_dir .ne. 3)) slab_vplane1 = q0_def_norm
             if ((slab_vplane2 < q0_def_norm) .and. (i_dir .ne. 3)) slab_vplane2 = q0_def_norm
             !
             pre_factor = sqrt(1.-exp(-slab_vplane1*lcut)*cos(slab_vz1*lcut))*&
&                         sqrt(1.-exp(-slab_vplane2*lcut)*cos(slab_vz2*lcut))
             !
             vslab=vslab*pre_factor
           endif
           !
           ! Evaluate func
           !===============
           !
           !Transform f_coeff from iBZ to BZ
           !
           call m3inv(rl_sop(:,:,is),symminv)
           !
           call trans_f_coeff(f_coeff(:,ig1,ig2,iq,iomega),f_coeff_loc,symminv)
           !
           func = f_coeff_loc(1) &
&                 +q_sampl(1)*(f_coeff_loc(2)+q_sampl(1)*f_coeff_loc(5)+2_DP*q_sampl(2)*f_coeff_loc(6)) &
&                 +q_sampl(2)*(f_coeff_loc(3)+q_sampl(2)*f_coeff_loc(8)+2_DP*q_sampl(3)*f_coeff_loc(9)) &
&                 +q_sampl(3)*(f_coeff_loc(4)+q_sampl(3)*f_coeff_loc(10)+2_DP*q_sampl(1)*f_coeff_loc(7)) &
&                 +q_sampl(1)**2*(q_sampl(1)*f_coeff_loc(11)+3_DP*q_sampl(2)*f_coeff_loc(12)+3_DP*q_sampl(3)*f_coeff_loc(13)) &
&                 +q_sampl(2)**2*(q_sampl(2)*f_coeff_loc(15)+3_DP*q_sampl(3)*f_coeff_loc(16)+3_DP*q_sampl(1)*f_coeff_loc(14)) &
&                 +q_sampl(3)**2*(q_sampl(3)*f_coeff_loc(19)+3_DP*q_sampl(1)*f_coeff_loc(17)+3_DP*q_sampl(2)*f_coeff_loc(18)) &
&                 +6*q_sampl(1)*q_sampl(2)*q_sampl(3)*f_coeff_loc(20)
           !
           if (iq==1) then
             !
             ! Head
             !
             if (ig1==1.and.ig2==1) then
               !
               ! Metal case, static and finite frequency
               !
               select case (trim(rimw_type))
               case ("metal")
                 !
                 if (iomega==1) then
                   !
                   q_out = q_sampl
                   call c2a(b_in=b,v_in=q_out,mode='ki2c')
                   func = f_coeff_loc(1) + f_coeff(2,ig1,ig2,iq,iomega)*abs(q_rlu(1))                             &
&                       + f_coeff(3,ig1,ig2,iq,iomega)*abs(q_rlu(2))+f_coeff(4,ig1,ig2,iq,iomega)*abs(q_rlu(3))   &
&                       + f_coeff(5,ig1,ig2,iq,iomega)*(q_rlu(1))**2 + f_coeff(6,ig1,ig2,iq,iomega)*(q_rlu(2))**2 &
&                       + f_coeff(7,ig1,ig2,iq,iomega)*(q_rlu(3))**2
                   !
                   epsm1_sampl = 1._SP/(1._SP-vslab/r1*func)
                   W_sampl= vslab/r1/(1._SP-vslab/r1*func)
                   !
                   if (iku_v_norm(q_sampl) < q0_def_norm) then
                     if(cut_is_slab) then
                       !
                       epsm1_sampl = q_out_norm/(q_out_norm-2._SP*pi*func*alat(idir(1)))
                       !
                       W_sampl = 2._SP*pi*a(idir(1),idir(1))/(q_out_norm-2._SP*pi*func*alat(idir(1)))
                       !
                     else
                       !
                       epsm1_sampl = q_out_norm**2/(q_out_norm**2-4._SP*pi*func)
                       !
                       W_sampl = 4._SP*pi/(q_out_norm**2-4._SP*pi*func)
                       !
                     endif
                   endif
                   !
                   write(out_unit_W_fit,"(I5, 11E15.6)") &
&                             i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func),&
&                             aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
                   cycle
                   !
                 else
                   !
                   func =  f_coeff(1,1,1,1,iomega)
                   !
                   if (iku_v_norm(q_sampl) < q0_def_norm) then
                     !
                     epsm1_sampl = vslab*func/(1.0-vslab*func)
                     W_sampl = func/(1.0-vslab*func)*vslab**2/q0_def_norm**2
                     func= f_coeff(1,1,1,1,iomega)*iku_v_norm(q_sampl)**2
                     !func= func*iku_v_norm(q_sampl)**2
                     !
                     write(out_unit_W_fit,"(I5, 11E15.6)") &
&                             i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func),&
&                             aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
                     cycle
                     !
                   endif
                   ! 
                   func = (func+f_coeff(5,1,1,1,iomega)*abs(q_rlu(1)) + f_coeff(6,1,1,1,iomega)*abs(q_rlu(2)) +      &
&                          f_coeff(7,1,1,1,iomega)*abs(q_rlu(3)))*exp(sign(1._DP,real(f_coeff(3,1,1,1,iomega)))      &
&                          *sqrt(f_coeff(2,1,1,1,iomega)**2*(q_rlu(1))**2 + f_coeff(3,1,1,1,iomega)**2*(q_rlu(2))**2 + &
&                          f_coeff(4,1,1,1,iomega)**2*(q_rlu(3))**2 ))
                   !
                   func = func*iku_v_norm(q_sampl)**2
                   epsm1_sampl = func*vslab/r1/(1._SP-vslab/r1*func)
                   W_sampl= vslab/r1*func*vslab/r1/(1._SP-vslab/r1*func)
                   !
                   write(out_unit_W_fit,"(I5, 11E15.6)") &
&                            i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func),&
&                            aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
                   cycle
                   !
                 endif
                 !
               end select
               !
               ! Semiconductors and Dirac cone, static case and finite frequency
               !
               if (iku_v_norm(q_sampl) < 1.e-5) then
                 !
                 epsm1_sampl = vslab*f_coeff(1,1,1,1,iomega)/(1.0-vslab*f_coeff(1,1,1,1,iomega))
                 W_sampl = f_coeff(1,1,1,1,iomega)/(1.0-vslab*f_coeff(1,1,1,1,iomega))*vslab**2/q0_def_norm**2
                 !
                 if (iomega==1) then
                   select case (trim(rimw_type))
                   case ("Dirac")
                     epsm1_sampl = vslab/q0_def_norm*f_coeff(1,1,1,1,iomega)/(1.0-vslab/q0_def_norm*f_coeff(1,1,1,1,iomega))
                     W_sampl = f_coeff(1,1,1,1,iomega)/(1.0-vslab/q0_def_norm*f_coeff(1,1,1,1,iomega))*vslab**2/q0_def_norm**3
                   end select
                 endif
                 !
                 if (RIM_id_epsm1_reference /= 0 .and. iomega == 1) then
                   W_sampl = W_sampl*em1_anis(i_dir)
                 endif
                 !
                 write(out_unit_W_fit,"(I5, 11E15.6)") &
&                      i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1,0.0,&
&                      0.0,real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
                 cycle
                 !
               endif
               !
               if (cut_is_slab) then
                 !
                 func= f_coeff(1,1,1,1,iomega)*exp(sign(1._DP,real(f_coeff(2,1,1,1,iomega),DP))*sqrt(  &
&                      f_coeff(2,1,1,1,iomega)**2*(q_rlu(1))**2+f_coeff(3,1,1,1,iomega)**2*(q_rlu(2))**2))
                 !
               else
                 ! 
                 func= f_coeff(1,1,1,1,iomega)*exp(sign(1._DP,real(f_coeff(2,1,1,1,iomega),DP))*sqrt(f_coeff(2,1,1,1,iomega)**2 &
&                      *(q_rlu(1))**2 + f_coeff(3,1,1,1,iomega)**2*(q_rlu(2))**2 + f_coeff(4,1,1,1,iomega)**2*(q_rlu(3))**2))

                 !
               endif
               !
               if (RIM_id_epsm1_reference /= 0 .and. iomega == 1) then
                 anis_fact = dot_product(em1_anis,(2._SP*pi*q_sampl/alat)**2)/iku_v_norm(q_sampl)**2
                 func = func*anis_fact
               endif
               !
               if (iomega==1) then
                 !
                 select case (trim(rimw_type))
                 case ("semiconductor")
                   call msg("rs", "Entered in the head check mode")
                   func = func*iku_v_norm(q_sampl)**2
                   !
                 case ("Dirac")
                   call msg("rs", "Entered in the head check mode")
                   func = func*iku_v_norm(q_sampl)
                   !
                 case default
                   func = f_coeff_loc(1) +q_sampl(1)*(f_coeff_loc(2)+q_sampl(1)*f_coeff_loc(5) + 2_DP*q_sampl(2)   &
&                         *f_coeff_loc(6)) +q_sampl(2)*(f_coeff_loc(3)+q_sampl(2)*f_coeff_loc(8) + 2_DP*q_sampl(3) &
&                         *f_coeff_loc(9))+q_sampl(3)*(f_coeff_loc(4)+q_sampl(3)*f_coeff_loc(10) + 2_DP*q_sampl(1) &
&                         *f_coeff_loc(7))
                 end select
                 !
               else
                 !
                 select case (trim(rimw_type))
                 case ("semiconductor","Dirac")
                   func = func*iku_v_norm(q_sampl)**2
                   !
                 case default
                   func = f_coeff_loc(1) +q_sampl(1)*(f_coeff_loc(2)+q_sampl(1)*f_coeff_loc(5) + 2_DP*q_sampl(2)   &
&                         *f_coeff_loc(6)) +q_sampl(2)*(f_coeff_loc(3)+q_sampl(2)*f_coeff_loc(8) + 2_DP*q_sampl(3) &
&                         *f_coeff_loc(9))+q_sampl(3)*(f_coeff_loc(4)+q_sampl(3)*f_coeff_loc(10) + 2_DP*q_sampl(1) &
&                         *f_coeff_loc(7))
                 end select
                 !
               endif
               !
             endif
             !
             ! wings
             !
             if (ig1==1 .and. ig2/=1) then  
               !
               select case (trim(rimw_type))
               case ("metal")
               !
               if (cut_is_slab) then
                 !
                 if (iomega ==1) then
                   func = real(f_coeff(2,1,ig2,iq,iomega))*abs(q_rlu(1)) + real(f_coeff(3,1,ig2,iq,iomega))*abs(q_rlu(2)) &
&                       + real(f_coeff(4,1,ig2,iq,iomega))*abs(q_rlu(3)) + aimag(f_coeff(2,1,ig2,iq,iomega))*q_rlu(1)     &                
&                       + aimag(f_coeff(3,1,ig2,iq,iomega))*q_rlu(2) + aimag(f_coeff(4,1,ig2,iq,iomega))*q_rlu(3)         &
&                       + f_coeff(5,1,ig2,iq,iomega)*(q_rlu(1))**2 + f_coeff(6,1,ig2,iq,iomega)*(q_rlu(2))**2             &
&                       + f_coeff(7,1,ig2,iq,iomega)*(q_rlu(3))**2 + f_coeff(8,1,ig2,iq,iomega)*sign(q_rlu(1)**2,q_rlu(1))& 
&                       + f_coeff(9,1,ig2,iq,iomega)*sign(q_rlu(2)**2,q_rlu(2)) + f_coeff(10,1,ig2,iq,iomega)             &
&                       * sign(q_rlu(3)**2,q_rlu(3))
                   !
                 else
                   !
                   if ((g_vec(ig2,idir(2))==g_vec(ig2,idir(3))) .and. (g_vec(ig2,idir(2))==0)) then
                     !
                     func= ((f_coeff(2,1,ig2,iq,iomega)+f_coeff(5,1,ig2,iq,iomega)+ (f_coeff(2,1,ig2,iq,iomega) & 
&                         -f_coeff(5,1,ig2,iq,iomega))*sign(1._SP,q_rlu(1)))*(q_rlu(1))**2   +                   &
&                          (f_coeff(3,1,ig2,iq,iomega)+f_coeff(6,1,ig2,iq,iomega)+ (f_coeff(3,1,ig2,iq,iomega)   &
&                         -f_coeff(6,1,ig2,iq,iomega))*sign(1._SP,q_rlu(2)))*(q_rlu(2))**2   +                   &
&                          (f_coeff(4,1,ig2,iq,iomega)+f_coeff(7,1,ig2,iq,iomega)+ (f_coeff(4,1,ig2,iq,iomega)   &
&                         -f_coeff(7,1,ig2,iq,iomega))*sign(1._SP,q_rlu(3)))*(q_rlu(3))**2   +                   &
&                          (f_coeff(8,1,ig2,iq,iomega)+f_coeff(11,1,ig2,iq,iomega)+ (f_coeff(8,1,ig2,iq,iomega)  &
&                         -f_coeff(11,1,ig2,iq,iomega))*sign(1._SP,q_rlu(1)))*(q_rlu(1))**3   +                  &
&                          (f_coeff(9,1,ig2,iq,iomega)+f_coeff(12,1,ig2,iq,iomega)+ (f_coeff(9,1,ig2,iq,iomega)  &
&                         -f_coeff(12,1,ig2,iq,iomega))*sign(1._SP,q_rlu(2)))*(q_rlu(2))**3   +                  &
&                          (f_coeff(10,1,ig2,iq,iomega)+f_coeff(13,1,ig2,iq,iomega)+ (f_coeff(10,1,ig2,iq,iomega)&
&                         -f_coeff(13,1,ig2,iq,iomega))*sign(1._SP,q_rlu(3)))*(q_rlu(3))**3)/2.0_SP
                     !
                   else 
                     !
                     func= ((f_coeff(2,1,ig2,iq,iomega)+f_coeff(5,1,ig2,iq,iomega)+ (f_coeff(2,1,ig2,iq,iomega)  &
&                         -f_coeff(5,1,ig2,iq,iomega))*sign(1._SP,q_rlu(1)))*(q_rlu(1))      +                   &
&                          (f_coeff(3,1,ig2,iq,iomega)+f_coeff(6,1,ig2,iq,iomega)+ (f_coeff(3,1,ig2,iq,iomega)   &
&                         -f_coeff(6,1,ig2,iq,iomega))*sign(1._SP,q_rlu(2)))*(q_rlu(2))      +                   &
&                          (f_coeff(4,1,ig2,iq,iomega)+f_coeff(7,1,ig2,iq,iomega)+ (f_coeff(4,1,ig2,iq,iomega)   &
&                         -f_coeff(7,1,ig2,iq,iomega))*sign(1._SP,q_rlu(3)))*(q_rlu(3))      +                   &
&                          (f_coeff(8,1,ig2,iq,iomega)+f_coeff(11,1,ig2,iq,iomega)+ (f_coeff(8,1,ig2,iq,iomega)  &
&                         -f_coeff(11,1,ig2,iq,iomega))*sign(1._SP,q_rlu(1)))*(q_rlu(1))**2  +                   &
&                          (f_coeff(9,1,ig2,iq,iomega)+f_coeff(12,1,ig2,iq,iomega)+ (f_coeff(9,1,ig2,iq,iomega)  &
&                         -f_coeff(12,1,ig2,iq,iomega))*sign(1._SP,q_rlu(2)))*(q_rlu(2))**2  +                   &
&                          (f_coeff(10,1,ig2,iq,iomega)+f_coeff(13,1,ig2,iq,iomega)+ (f_coeff(10,1,ig2,iq,iomega)&
&                         -f_coeff(13,1,ig2,iq,iomega))*sign(1._SP,q_rlu(3)))*(q_rlu(3))**2)/2.0_SP
                     !
                   endif
                   !
                 endif
                 !
               endif
               !
               if (iku_v_norm(q_sampl) < q0_def_norm) then
                 !
                 if (cut_is_slab) then
                   !
                   if (iomega ==1) then
                     !
                     ! System 2D, f func behaves as fcoeff*q
                     !
                     func= (real(f_coeff(2,1,ig2,iq,iomega))*abs(q_rlu(1)) + real(f_coeff(3,1,ig2,iq,iomega))*abs(q_rlu(2)) &
&                        + real(f_coeff(4,1,ig2,iq,iomega))*abs(q_rlu(3)) + aimag(f_coeff(2,1,ig2,iq,iomega))*q_rlu(1)      &
&                        + aimag(f_coeff(3,1,ig2,iq,iomega))*q_rlu(2) + aimag(f_coeff(4,1,ig2,iq,iomega))*q_rlu(3))         &
&                         /iku_v_norm(q_sampl)
                     !
                     W_sampl = func/iku_v_norm(qpGc+q_sampl)**2*vslab**2/q0_def_norm
                     epsm1_sampl = func/iku_v_norm(qpGc+q_sampl)*vslab*sqrt(iku_v_norm(q_sampl)/q0_def_norm)
                     func=func*iku_v_norm(q_sampl)
                     !
                   else
                     !
                     if ( (iomega .ne. 1) .and. (g_vec(ig2,idir(2))==g_vec(ig2,idir(3))) .and. (g_vec(ig2,idir(2))==0)) then
                       W_sampl = 0
                       epsm1_sampl = 0
                       func=0               
                     else
                       func = ((f_coeff(2,1,ig2,iq,iomega)-f_coeff(5,1,ig2,iq,iomega))*abs(q_rlu(1)) + &
&                           + (f_coeff(3,1,ig2,iq,iomega)-f_coeff(6,1,ig2,iq,iomega))*abs(q_rlu(2)) +  &
&                           + (f_coeff(4,1,ig2,iq,iomega)-f_coeff(7,1,ig2,iq,iomega))*abs(q_rlu(3)))   &
                             /iku_v_norm(q_sampl)/2.0_SP
                       W_sampl = func/iku_v_norm(qpGc+q_sampl)**2*vslab**2/q0_def_norm
                       epsm1_sampl = func/iku_v_norm(qpGc+q_sampl)*vslab*sqrt(iku_v_norm(q_sampl)/q0_def_norm)
                       func=func*iku_v_norm(q_sampl)
                     endif
                     !
                   endif
                   !
                 else
                   !
                   ! System 3D, the static f func behaves  as fcoeff*q^2
                   ! Dinamically, f func has a linear term that however cancels out due to symmetry
                   !
                   func= (q_sampl(1)*(q_sampl(1)*f_coeff_loc(5) + 2_SP*q_sampl(2)*f_coeff_loc(6)) &
&                      + q_sampl(2)*(q_sampl(2)*f_coeff_loc(8)  + 2_SP*q_sampl(3)*f_coeff_loc(9)) &
&                      + q_sampl(3)*(q_sampl(3)*f_coeff_loc(10) + 2_SP*q_sampl(1)*f_coeff_loc(7)))/iku_v_norm(q_sampl)**2
                   !
                   W_sampl = func/iku_v_norm(qpGc+q_sampl)**2*vslab**2
                   epsm1_sampl = func/iku_v_norm(qpGc+q_sampl)*vslab*iku_v_norm(q_sampl)
                   func=func*iku_v_norm(q_sampl)**2
                   !
                 endif
                 !
                 write(out_unit_W_fit,"(I5, 11E15.6)") &
&                      i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func), &
&                      aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
                 cycle
                 !
               endif
               !
               end select
               !
             endif
             !
           endif
           !
           epsm1_sampl = func*vslab/r1/(1._SP-vslab/r1*func)
           W_sampl= vslab/r1*func*vslab/r1/(1._SP-vslab/r1*func)
           !
           if (ig1==ig2 .and. iomega==1) then
             !
             select case (trim(rimw_type))
             case ("metal")
               !
               epsm1_sampl = 1._SP/(1._SP-vslab/r1*func)
               W_sampl= vslab/r1/(1._SP-vslab/r1*func)
               !
             end select
             !
           endif
           !         
           !if(igr==1.and.igc == 1.and.iomega==1.and.i_int==1.and.i==1) then
           !  write(*,*) "f_coeff_loc"
           !  write(*,*) f_coeff_loc
           !end if
           !if(igr==1.and.igc == 1.and.iomega==1.and.i_int==1.and.i==1) then
           !  write(*,*) "f_coeff"
           !  write(*,*) f_coeff(:,ig1,ig2,iq,iomega)
           !  call msg("rs","Wrong point")
           !  call msg("rs","iq = ", iq)
           !  call msg("rs","is = ", is)
           !  call msg("rs","ig1 = ", ig1)
           !  call msg("rs","ig2 = ", ig2)
           !  call msg("rs","qpG = ",qpGr)
           !  call msg("rs","nearest neighbours")
           !  call msg("rs","iq = ", idx_q(ig1,iq,2))
           !  call msg("rs","is = ", idx_is(ig1,iq,2))
           !  call msg("rs","ig = ", idx_G(ig1,iq,2))
           !  call msg("rs","Re[vX] = ",  real(vX(2,iq,ig1,ig2,iomega)))
           !  call msg("rs","Im[vX] = ", aimag(vX(2,iq,ig1,ig2,iomega)))
           !  call msg("rs","--------------------")
           !  call msg("rs","iq = ", idx_q(ig1,iq,3))
           !  call msg("rs","is = ", idx_is(ig1,iq,3))
           !  call msg("rs","ig = ", idx_G(ig1,iq,3))
           !  call msg("rs","Re[vX] = ",  real(vX(3,iq,ig1,ig2,iomega)))
           !  call msg("rs","Im[vX] = ", aimag(vX(3,iq,ig1,ig2,iomega)))
           !  call msg("rs","--------------------")
           !  call msg("rs","iq = ", idx_q(ig1,iq,4))
           !  call msg("rs","is = ", idx_is(ig1,iq,4))
           !  call msg("rs","ig = ", idx_G(ig1,iq,4))
           !  call msg("rs","Re[vX] = ",  real(vX(4,iq,ig1,ig2,iomega)))
           !  call msg("rs","Im[vX] = ", aimag(vX(4,iq,ig1,ig2,iomega)))
           !  call msg("rs","--------------------")
           !  call msg("rs","iq = ", idx_q(ig1,iq,5))
           !  call msg("rs","is = ", idx_is(ig1,iq,5))
           !  call msg("rs","ig = ", idx_G(ig1,iq,5))
           !  call msg("rs","Re[vX] = ",  real(vX(5,iq,ig1,ig2,iomega)))
           !  call msg("rs","Im[vX] = ", aimag(vX(5,iq,ig1,ig2,iomega)))
           !end if
           !
           !Convert q_sampl to iku
           !call c2a(b_in=b,v_in=q_sampl,mode='ka2i')
           !
           write(out_unit_W_fit,"(I5, 11E15.6)") &
&                           i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func), &
&                           aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
         enddo
         !
       enddo
       !
       ! Close output files
       !
       close(unit=out_unit_W_fit)
       close(unit=out_unit_W_num)
       !
     enddo
   enddo
 enddo
 enddo
end subroutine
!
subroutine trans_f_coeff(f_func,f_func_loc,symi)
 use pars,          ONLY:SP,zero_dfl,schlen,DP
 !
#include<y_memory.h>
 !
 ! Routine to perform the computation of f_coeff in BZ from iBZ
 !
 complex(DP) :: f_func(20)
 complex(SP) :: f_func_loc(20)
 real(SP)    :: symi(3,3)
 integer :: i1,i2,i3,i4,i5,nn
 !
 f_func_loc(1) = f_func(1)
 f_func_loc(2) = symi(1,1)*f_func(2)+symi(2,1)*f_func(3)+symi(3,1)*f_func(4)
 f_func_loc(3) = symi(1,2)*f_func(2)+symi(2,2)*f_func(3)+symi(3,2)*f_func(4)
 f_func_loc(4) = symi(1,3)*f_func(2)+symi(2,3)*f_func(3)+symi(3,3)*f_func(4)
 !
 f_func_loc(5) = symi(1,1)*symi(1,1)*f_func(5)+symi(1,1)*symi(2,1)*f_func(6)*2   &
 &               + symi(1,1)*symi(3,1)*f_func(7)*2+symi(2,1)*symi(2,1)*f_func(8) &
 &               + symi(2,1)*symi(3,1)*f_func(9)*2+symi(3,1)*symi(3,1)*f_func(10)
 !
 f_func_loc(6) = symi(1,1)*symi(1,2)*f_func(5)+(symi(1,1)*symi(2,2)+symi(2,1)*symi(1,2))*f_func(6)    &
 &               + (symi(1,1)*symi(3,2)+symi(3,1)*symi(1,2))*f_func(7)+symi(2,1)*symi(2,2)*f_func(8)  &
 &               + (symi(2,1)*symi(3,2)+symi(3,1)*symi(2,2))*f_func(9)+symi(3,1)*symi(3,2)*f_func(10)
 !
 f_func_loc(7) = symi(1,1)*symi(1,3)*f_func(5)+(symi(1,1)*symi(2,3)+symi(2,1)*symi(1,3))*f_func(6)    &
 &               + (symi(1,1)*symi(3,3)+symi(3,1)*symi(1,3))*f_func(7)+symi(2,1)*symi(2,3)*f_func(8)  &
 &               + (symi(2,1)*symi(3,3)+symi(3,1)*symi(2,3))*f_func(9)+symi(3,1)*symi(3,3)*f_func(10)
 !
 f_func_loc(8) = symi(1,2)*symi(1,2)*f_func(5)+symi(1,2)*symi(2,2)*f_func(6)*2   &
 &               + symi(1,2)*symi(3,2)*f_func(7)*2+symi(2,2)*symi(2,2)*f_func(8) &
 &               + symi(2,2)*symi(3,2)*f_func(9)*2+symi(3,2)*symi(3,2)*f_func(10)
 !
 f_func_loc(9) = symi(1,2)*symi(1,3)*f_func(5)+(symi(1,2)*symi(2,3)+symi(2,2)*symi(1,3))*f_func(6)    &
 &               + (symi(1,2)*symi(3,3)+symi(3,2)*symi(1,3))*f_func(7)+symi(2,2)*symi(2,3)*f_func(8)  &
 &               + (symi(2,2)*symi(3,3)+symi(3,2)*symi(2,3))*f_func(9)+symi(3,2)*symi(3,3)*f_func(10)
 !
 f_func_loc(10)= symi(1,3)*symi(1,3)*f_func(5)+symi(1,3)*symi(2,3)*f_func(6)*2   &
 &               + symi(1,3)*symi(3,3)*f_func(7)*2+symi(2,3)*symi(2,3)*f_func(8) &
 &               + symi(2,3)*symi(3,3)*f_func(9)*2+symi(3,3)*symi(3,3)*f_func(10)
 !
 f_func_loc(11:20)=0
 do i1=1,3
   do i2=1,3
     do nn=11,19
       !
       if (mod(nn-11,4)==0) then
         i3=(nn-11)/4+1
         f_func_loc(7+3*i1+i2) = f_func_loc(7+3*i1+i2) + f_func(nn)*symi(i3,i1)*symi(i3,i1)*symi(i3,i2)
       else
         i4=mod(nn-11,3)+1
         i3=(nn-10-i4)/3+1
         f_func_loc(7+3*i1+i2) = f_func_loc(7+3*i1+i2) + f_func(nn)*(2.0_SP*symi(i4,i1)*symi(i3,i1)*symi(i3,i2) &
&                              + symi(i4,i2)*symi(i3,i1)*symi(i3,i1))
       endif
       !
     enddo
     !
     do i3=0,5
       i4=(i3-mod(i3,2))/2+2+mod(i3,2)
       i5=i4+1-2*mod(i3,2)
       if (i4>3) i4=i4-3
       if (i5>3) i5=i5-3
       f_func_loc(7+3*i1+i2) = f_func_loc(7+3*i1+i2) + &
&                              f_func(20)*symi((i3-mod(i3,2))/2+1,i1)*symi(i4,i1)*symi(i5,i2)
     enddo
   enddo
 enddo
 !
 do nn=11,19
   !
   if (mod(nn-11,4)==0) then
     i3=(nn-11)/4+1
     f_func_loc(20) = f_func_loc(20) + f_func(nn)*symi(i3,1)*symi(i3,2)*symi(i3,3)
   else
     i4=mod(nn-11,3)+1
     i3=(nn-10-i4)/3+1
     f_func_loc(20) = f_func_loc(20) + f_func(nn)*(symi(i3,1)*symi(i3,2)*symi(i4,3) + symi(i3,1)*symi(i4,2)*symi(i3,3) &
&                   + symi(i4,1)*symi(i3,1)*symi(i3,3))
   endif
   !
 enddo
 !
 do i3=0,5
   i4=(i3-mod(i3,2))/2+2+mod(i3,2)
   i5=i4+1-2*mod(i3,2)
   if (i4>3) i4=i4-3
   if (i5>3) i5=i5-3
   f_func_loc(20) = f_func_loc(20) + f_func(20)*symi((i3-mod(i3,2))/2+1,1)*symi(i4,2)*symi(i5,3)
 enddo
 !
end subroutine
