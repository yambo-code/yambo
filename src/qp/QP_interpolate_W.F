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
subroutine QP_interpolate_W(X,Xw,q,mode)
 !
 use pars,          ONLY:SP,pi,zero_dfl,schlen,DP,rZERO
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,RIM_W_ng,g_vec,b,k_grid_b,bare_qpg,&
&                        RIM_W_is_diagonal,RIM_W,f_coeff,idir,RIM_W_E,cut_is_slab,&
&                        RIM_id_epsm1_reference,RIM_epsm1,Dirac_v_f,RIM_qpg,rimW_type
 use vec_operate,   ONLY:c2a,v_norm
 use ALLOC,         ONLY:X_ALLOC_elemental
 use X_m,           ONLY:X_mat,X_t
 use frequency,     ONLY:w_samp
 use IO_m,          ONLY:OP_RD_CL,NONE,RD_CL,OP_RD,RD_CL_IF_END,OP_WR_CL,&
&                        deliver_IO_error_message,manage_action,REP,VERIFY
 use IO_int,        ONLY:io_control
 use stderr,        ONLY:intc,STRING_pack,gen_fmt
 use D_lattice,     ONLY:alat,a
 use timing_m,      ONLY:timing
 use parallel_m,    ONLY:master_cpu
 use parser_m,      ONLY:parser
 !
#include<dev_defs.h>
#include<y_memory.h>
 !
 type(X_t)    :: X
 type(w_samp) :: Xw
 type(bz_samp):: q
 character(3) :: mode
 !
 ! Work Space
 !
 integer           :: iq1,ig1,iq2,ig2,iq,ig,i1,i2,iq0,iq3,iqn1,iqn2
 integer           :: ig1_tmp,iq2_tmp,ig2_tmp
 integer           :: nn,idm,igr,igc,ig2max,iomega
 integer           :: idx_q(RIM_W_ng,q%nibz,7),idx_G(RIM_W_ng,q%nibz,7),idx_is(RIM_W_ng,q%nibz,7)
 integer           :: idx_q_2(RIM_W_ng,q%nibz,13),idx_G_2(RIM_W_ng,q%nibz,13),idx_is_2(RIM_W_ng,q%nibz,13)
 real(SP)          :: dummy(2),anis_factor,em1_anis(3)
 complex(SP)       :: f_func(13),dp_dummy(13),dp_temp(3)
 complex(DP)       :: vX_nn(7,q%nibz,RIM_W_ng,RIM_W_ng,Xw%n_freqs),vX_nnn(13,q%nibz,RIM_W_ng,RIM_W_ng,Xw%n_freqs)
 real(SP)          :: v1(3),r_dum(2)
 real(SP)          :: Dirac_v_f_au,X0_Dirac
 real(SP)          :: q_grid_b_rlu(3,3),q_grid_b_iku(3,3),q_grid_b_cc(3,3)
 integer           :: ID, IO_ACT,io_err, ng_save,G_max_ibz(q%nibz), G_max
 integer, external :: io_X, io_RIM_W
 character(schlen) :: ch_dum,fmt_dum,msg_dum(2)
 logical           :: l_check,q0_neighbour,q0_snd_neighbour
 !
 call timing('RIM-W-coeff',OPR='start')
 !
 YAMBO_ALLOC(f_coeff,(20,RIM_W_ng,RIM_W_ng,q%nibz,Xw%n_freqs))
 !
 call section("+", "RIM-W interpolation")
 !
 call msg("r", trim(rimW_type))
 !
 !Load q_grid_b in rlu
 do idm=1,3
   v1 = k_grid_b(idm,:)
   call c2a(v_in=v1,mode='kc2a')
   !
   if((abs(v1(2))<zero_dfl).and.(abs(v1(3))<zero_dfl)) then
     !
     q_grid_b_rlu(:,1) = abs(v1)
     !
   elseif((abs(v1(1))< zero_dfl).and.(abs(v1(3))< zero_dfl)) then
     !
     q_grid_b_rlu(:,2) = abs(v1)
     !
   elseif((abs(v1(1))< zero_dfl).and.(abs(v1(2))< zero_dfl)) then
     !
     q_grid_b_rlu(:,3) = abs(v1)
     !
   else
     call error('Error in loading q_min')
   endif
 enddo
 !
 ! GS needed later for if clause, apparently idir not defined unless a cutoff is there?
 !
 if  (.not.  cut_is_slab) then 
  idir(1)=1
  idir(2)=2
  idir(3)=3
 end if
 !
 !Get q_grid_b in iku and cc
 do idm=1,3
   call c2a(v_in=q_grid_b_rlu(:,idm),v_out=q_grid_b_iku(:,idm),mode='ka2i')
   call c2a(v_in=q_grid_b_rlu(:,idm),v_out=q_grid_b_cc (:,idm),mode='ka2c')
 enddo
 !
 call warning ('RIM-W works only for 3D or 2D with slab cutoff')
 !call msg('r', 'Found compatible q grid: 2D')
 call msg('r', 'q_min[ 1 ]', q_grid_b_rlu(:,1), '[rlu]')
 call msg('r', '          ', q_grid_b_iku(:,1), '[iku]')
 call msg('r', 'q_min[ 2 ]', q_grid_b_rlu(:,2), '[rlu]')
 call msg('r', '          ', q_grid_b_iku(:,2), '[iku]')
 call msg('r', 'q_min[ 3 ]', q_grid_b_rlu(:,3), '[rlu]')
 call msg('r', '          ', q_grid_b_iku(:,3), '[iku]')
 call msg('r', 'G-vectors ', RIM_W_ng)
 !
 !Check if RIM-W has been alreay calculated
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_RIM_W(ID,mode,Xw)
 !
 if (io_err==0) then
   call timing('RIM-W-coeff',OPR='stop')
   YAMBO_FREE(f_coeff)
   return
 endif
 !
 !Calculation of the nearest neighbours indexes
 call find_q_nns(q,q_grid_b_iku,idx_q,idx_is,idx_G)
 call find_q_nnns(q,q_grid_b_iku,idx_q_2,idx_is_2,idx_G_2)
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Load of X_mat
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 ng_save = X%ng
 !
 !Initialize G_max_ibz to RIM_W_ng
 G_max_ibz(1:q%nibz) = RIM_W_ng
 !
 ! Look if G_max_ibz must be increased
 !
 do iq=1,q%nibz
!  if (v_norm(q%pt(iq,:))<0.2) then
   do ig=1,RIM_W_ng
     do nn=2,7
       if (G_max_ibz(idx_q(ig,iq,nn)) < idx_G(ig,iq,nn)) then
         G_max_ibz(idx_q(ig,iq,nn)) = idx_G(ig,iq,nn)
       endif 
     enddo
!       do iqn2=1,7
!       !
!       if (idx_q(1,iq,iqn2)==1) then
!         do nn=2,7
!          if (G_max_ibz(idx_q(ig,iq,nn)) < idx_G_2(ig,iq,nn)) then
!            G_max_ibz(idx_q(ig,iq,nn)) = idx_G_2(ig,iq,nn)
!          endif
!         enddo   
!       end if
!     end do   
   enddo
!  end if
 enddo
 !
 G_max=maxval(G_max_ibz)
 !
 if (G_max > X%ng) call error(' G_max '//trim(intc(G_max))//&
 &                             ' for RIM-W is higher than G_max '//trim(intc(X%ng))//' of vX')
 !
 call msg('r', 'G-vectors loaded', maxval(G_max_ibz))
 call msg('r', 'Number of interpolated frequencies', Xw%n_freqs)
 !
 !
 !Load only the G needed
 X%ng = G_max
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_X(X,Xw,ID)
 !
 do iq=1,q%nibz
   !
   ! I/O of X
   !
   !call msg("rs","iq =",  iq)
   !call msg("rs","Gmax",  G_max_ibz(iq))
   !
   IO_ACT=manage_action(RD_CL_IF_END,iq,1,q%nibz)
   call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2*iq,2*iq+1/),ID=ID)
   !if (iq==1) then
   !   call io_control(ACTION=OP_RD,COM=REP,       SEC=(/1,2,2*iq+1/),ID=ID)
   ! else if (q%nibz==1) then
   !   call io_control(ACTION=OP_RD_CL,COM=NONE,    SEC=(/1,2,3/),ID=ID)
   ! else if (iq > 1) then
   !   call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iq,2*iq+1/),ID=ID)
   !endif
   !
   !
   io_err=io_X(X,Xw,ID)
   !
   !
   !call msg('r', 'Re frequency 1', real(Xw%p(1)))
   !call msg('r', 'Im frequency 1', aimag(Xw%p(1)))
   !
   call deliver_IO_error_message(io_err,'PP/Em1s',STOP_it=.TRUE.)
   !
   !call msg("rs","shape X_mat",shape(X_mat))
   !
   !Assign X
   do iomega=1,Xw%n_freqs
   do ig1=1,RIM_W_ng
     !
     ig2max = RIM_W_ng
     if (RIM_W_is_diagonal) ig2max=ig1
     do iq1=1,q%nibz
!     if (v_norm(q%pt(iq1,:))<0.2) then
     do ig2=1,ig2max
!    old version do ig2=ig1,ig2max
       !
       do nn=1,7
         !
         if (idx_q(ig1,iq1,nn) == iq) then
           vX_nn(nn,iq1,ig1,ig2,iomega) = X_mat(idx_G(ig1,iq1,nn),idx_G(ig2,iq1,nn),iomega)
         endif
         if (idx_q_2(ig1,iq1,nn) == iq) then
               vX_nnn(nn,iq1,ig1,ig2,iomega) = X_mat(idx_G_2(ig1,iq1,nn),idx_G_2(ig2,iq1,nn),iomega)
         end if
         !
       enddo
         ! 
         ! Calculation of next nearest neighbours vX needed only for the q=0 term and its neighbours
         ! in the head term
       if (ig1==1 .and. ig2==1) then 
        do iqn2=1,7
          if (idx_q(1,iq,iqn2)==1) then
            do nn=8,13
              if (idx_q_2(ig1,iq1,nn) == iq) then
                vX_nnn(nn,iq1,ig1,ig2,iomega) = X_mat(idx_G_2(ig1,iq1,nn),idx_G_2(ig2,iq1,nn),iomega)
              end if
            end do 
            cycle         
          end if         
        end do  
       end if
     end do
     end do
     ! 
     ! Calculation of next nearest neighbours vX needed only for the q=0 term and the neighboursing q-points
     ! for the wings and the head term 
     !
 !     do iqn2=1,7 
 !       !
!        if (idx_q(1,iq1,iqn2)==1) then
!          do nn=1,13
 !           if (idx_q_2(1,iq1,nn) == iq) then
 !              vX_nnn(nn,iq1,1,ig1,iomega) = X_mat(idx_G_2(1,iq1,nn),idx_G_2(ig1,iq1,nn),iomega)
 !           end if
!          end do  
 !       end if
!      end do
     !
!     end if
!      end do
     !
   enddo
   enddo
   !
   call X_ALLOC_global('X')
   !if(iq==15) then
   !  write(*,*) "Re[X_mat]"
   !  write(*,*)  real(X_mat(:,:,1))
   !  write(*,*) "Im[X_mat]"
   !  write(*,*) aimag(X_mat(:,:,1))
   !end if
   !
 enddo
 !call msg("rs","Re[vX] incrimined",  real(vX_nn(1,16,1,1,1)))
 !call msg("rs","Im[vX] incrimined", aimag(vX_nn(1,16,1,1,1)))
 !call msg("rs","Re[vX] incrimined",  real(vX_nn(2,16,1,1,1)))
 !call msg("rs","Im[vX] incrimined", aimag(vX_nn(2,16,1,1,1)))
 !call msg("rs","Re[vX] incrimined",  real(vX_nn(3,16,1,1,1)))
 !call msg("rs","Im[vX] incrimined", aimag(vX_nn(3,16,1,1,1)))
 !call msg("rs","Re[vX] incrimined",  real(vX_nn(4,16,1,1,1)))
 !call msg("rs","Im[vX] incrimined", aimag(vX_nn(4,16,1,1,1)))
 !call msg("rs","Re[vX] incrimined",  real(vX_nn(5,16,1,1,1)))
 !call msg("rs","Im[vX] incrimined", aimag(vX_nn(5,16,1,1,1)))
 !
 X%ng = ng_save
 YAMBO_FREE(X_mat)
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of the interpolation coefficients
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 !Anisotropy initialization
 em1_anis=RIM_epsm1(:)-1._SP
 !
 if (RIM_id_epsm1_reference<0.or.RIM_id_epsm1_reference>3) RIM_id_epsm1_reference=0
 if (RIM_id_epsm1_reference==0) em1_anis=0.
 !
 do iomega=1,Xw%n_freqs
   !call msg('r','iomega',iomega)
   do igr=1,RIM_W_ng
    !
    ig2max = RIM_W_ng
    if (RIM_W_is_diagonal) ig2max=igr
    do igc=1,ig2max
    ! old version do igc=igr,ig2max
      !
      do iq1=1,q%nibz
!      if (v_norm(q%pt(iq1,:))<0.2) then
        q0_neighbour=.false.
        q0_snd_neighbour=.false.
        !cut_is_slab
        ! Evaluate f_func nearest neighbour
        !
        do nn=1,7
          !
          !Select the index of the reference/n.n.
          iq2 = idx_q(igr,iq1,nn)
          ig1 = idx_G(igr,iq1,nn)
          ig2 = idx_G(igc,iq1,nn) 
          !
          f_func(nn) = real(bare_qpg(iq2,ig1)*bare_qpg(iq2,ig2),kind=DP)/(4._DP*pi)*&
&                      vX_nn(nn,iq1,igr,igc,iomega)/(vX_nn(nn,iq1,igr,igc,iomega)+1._DP)
          !
          ! For metals, special treatment if a neighbour is the q-->0 term at zero frequency, 
          ! as this term is ill defined in metals both in the case of the head and wings
          !
          if ((iq1==1) .and. (iq2==1) .and. (ig1==1) .and. (ig2 .ne. 1) .and. (iomega .ne. 1)) then
            f_func(nn)=0
          end if
          !
          ! The q-->0 term is a neighbour
          if ( (iq1 .ne. 1) .and. (iq2==1)) then
            select case (trim(rimw_type))
            case ("semiconductor")
            !
            if ( (ig1==1) .and. (ig2==1)) then
             if  (.not.  cut_is_slab) then
              q0_neighbour=.true.
              iq0=nn
              f_func(nn)=vX_nn(1,1,1,1,iomega)/(4._DP*pi*(1._DP+vX_nn(1,1,1,1,iomega)))
             end if
            end if
            !
            case ("metal")
              ! Head
              if ( (ig1==1) .and. (ig2==1)) then
                 if (iomega==1) then
                   f_func(nn)=f_coeff(1,1,1,1,1)
                 else 
                   f_func(nn)=0
                   q0_neighbour=.true.
                   iq0=nn
                 end if
              ! Wing
              else if ((ig1==1) .and. (ig2 .ne. 1) ) then
                 f_func(nn)=f_coeff(1,ig1,ig2,1,iomega)
                 if (iomega==1) then  
                    f_func(nn)=0
                    if  (.not.  cut_is_slab) then
                     q0_neighbour=.true.
                     iq0=nn
                    end if
                 end if
              end if 
            end select
          end if
          !
          !
          if ((ig1==1) .and. (ig2==1)  .and. (iomega .ne. 1) .and. (iq2 .ne. 1) .and. (iq1 .ne. 1)) then
            iq3 = idx_q(ig1,iq2,nn)
            if (iq3==1) then
              q0_snd_neighbour=.true.
              iq0=nn
            end if
          end if           
        enddo
        !
        !test = (iq1==1.and.igr == 1.and.igc == 1)
        !call msg('r','iq',iq1)
        !call msg('r','igr',igr)
        !call msg('r','igc',igc)
        !call msg('r','test',test)
        !
        ! Coefficients calculated from Taylor expansion
        ! Coefficients in rlu
        !
        dp_dummy(:) = 0
        dp_dummy(1) = f_func(1)                                                          !f0
        dp_dummy(2) = (f_func(2)-f_func(5))/(2._DP*q_grid_b_rlu(1,1))                    !f1
        dp_dummy(3) = (f_func(3)-f_func(6))/(2._DP*q_grid_b_rlu(2,2))                    !f2
        dp_dummy(4) = (f_func(4)-f_func(7))/(2._DP*q_grid_b_rlu(3,3))                    !f3
        dp_dummy(5) = (f_func(2)-2._DP*f_func(1)+f_func(5))/(2._DP*q_grid_b_rlu(1,1)**2) !f11
        dp_dummy(6) = (f_func(3)-2._DP*f_func(1)+f_func(6))/(2._DP*q_grid_b_rlu(2,2)**2) !f22
        dp_dummy(7) = (f_func(4)-2._DP*f_func(1)+f_func(7))/(2._DP*q_grid_b_rlu(3,3)**2) !f33
        !
        ! for 3D
        ! if nearest neighbour is q=0 along a certain direction, use cubic expansion of the f functions
        ! when the lowest order is quadratic
        !
        if (q0_neighbour) then
            dp_dummy(2:4)=0
         if (iq0>=5) then
            i1=iq0-3
            dp_temp(1)= 0
            dp_temp(2)= (8*f_func(1)-f_func(i1))/(4*q_grid_b_rlu(i1-1,i1-1)**2)   
            dp_temp(3)= (f_func(i1)-4*f_func(1))/(4*q_grid_b_rlu(i1-1,i1-1)**3)  
            ! 
            dp_dummy(1) = dp_temp(1)*q_grid_b_rlu(i1-1,i1-1)+dp_temp(2)*q_grid_b_rlu(i1-1,i1-1)**2 +  &
&                         dp_temp(3)*q_grid_b_rlu(i1-1,i1-1)**3                                   
            dp_dummy(i1) = dp_temp(1)+2*dp_temp(2)*q_grid_b_rlu(i1-1,i1-1) +  &
&                          3*dp_temp(3)*q_grid_b_rlu(i1-1,i1-1)**2 
            dp_dummy(i1+3) = dp_temp(2)+3*dp_temp(3)*q_grid_b_rlu(i1-1,i1-1)                        
            dp_dummy(i1+6) = dp_temp(3)
            !   
            ! a mixing quadratic term qi1*qi2 is needed in case the i1 direction is not orthogonal to the other directions
            do idm=i1-1,i1
                dp_dummy(8+i1+mod(idm,3)) = (f_func(2+mod(idm,3))-f_func(5+mod(idm,3)))/   &                  
&                                           (4._DP*(q_grid_b_rlu(i1-1,i1-1)*q_grid_b_rlu(1+mod(idm,3),1+mod(idm,3))))
                dp_dummy(2+mod(idm,3)) = 2.0*dp_dummy(8+i1+mod(idm,3))*q_grid_b_rlu(i1-1,i1-1)
            end do
            !
            ! in case the i1 direction orthogonal to the other two directions, but the direction i2 and i3 are not between 
            ! each other a mixing quadratic term of the kind qi2*qi3 is needed
            !           
            if ( (q_grid_b_rlu(i1-1,1+mod(i1-1,3))==0) .and. (q_grid_b_rlu(1+mod(i1-1,3),i1-1)==0) .and.  &
&                (q_grid_b_rlu(i1-1,1+mod(i1,3))==0) .and. (q_grid_b_rlu(1+mod(i1,3),i1-1)==0) .and.      &
&                (q_grid_b_rlu(1+mod(i1-1,3),1+mod(i1,3)) .ne. 0) .or. (q_grid_b_rlu(1+mod(i1,3),1+mod(i1-1,3)) .ne. 0)) then 
               !  
               iqn1 = idx_q_2(1,iq1,12-i1)
               ! 
               f_func(5+mod(i1-1,3))= real(bare_qpg(iqn1,1)*bare_qpg(iqn1,1),kind=DP)/(4._DP*pi)* &
&                      vX_nnn(12-i1,iq1,1,1,iomega)/(vX_nnn(12-i1,iq1,1,1,iomega)+1._DP)
               ! 
               iqn2 = idx_q_2(1,iq1,15-i1)
               ! 
               f_func(5+mod(i1,3))= real(bare_qpg(iqn2,1)*bare_qpg(iqn2,1),kind=DP)/(4._DP*pi)* &
&                      vX_nnn(15-i1,iq1,1,1,iomega)/(vX_nnn(15-i1,iq1,1,1,iomega)+1._DP)
               ! 
               dp_dummy(15-i1) = (f_func(5+mod(i1-1,3))-f_func(5+mod(i1,3)))/ &
&                                (4._DP*(q_grid_b_rlu(1+mod(i1,3),1+mod(i1,3))*q_grid_b_rlu(1+mod(i1-1,3),1+mod(i1-1,3))))
            end if
         else 
            i1=iq0
            dp_temp(1)=0
            dp_temp(2)=  (8*f_func(1)-f_func(iq0+3))/(4*q_grid_b_rlu(i1-1,i1-1)**2)   
            dp_temp(3)= -(f_func(iq0+3)-4*f_func(1))/(4*q_grid_b_rlu(i1-1,i1-1)**3)  
            dp_dummy(1) = -dp_temp(1)*q_grid_b_rlu(i1-1,i1-1)+dp_temp(2)*q_grid_b_rlu(i1-1,i1-1)**2 -  &
&                      dp_temp(3)*q_grid_b_rlu(i1-1,i1-1)**3                                    
            dp_dummy(i1) = dp_temp(1)-2*dp_temp(2)*q_grid_b_rlu(i1-1,i1-1) +  &
&                          3*dp_temp(3)*q_grid_b_rlu(i1-1,i1-1)**2 
            dp_dummy(i1+3) = dp_temp(2)-3*dp_temp(3)*q_grid_b_rlu(i1-1,i1-1)                        
            dp_dummy(i1+6) = dp_temp(3) 
            !   
            ! in case the i1 direction is not orthogonal to the other two directions, a mixing term is needed
            do idm=i1-1,i1
                dp_dummy(8+i1+mod(idm,3)) = (f_func(2+mod(idm,3))-f_func(5+mod(idm,3)))/   &                  
&                                           (4._DP*(q_grid_b_rlu(i1-1,i1-1)*q_grid_b_rlu(1+mod(idm,3),1+mod(idm,3))))
                dp_dummy(2+mod(idm,3)) = -2.0*dp_dummy(8+i1+mod(idm,3))*q_grid_b_rlu(i1-1,i1-1)
            end do 
            !
            ! in case the i1 direction orthogonal to the other two directions, but the direction i2 and i3 are not between 
            ! each other a mixing quadratic term of the kind qi2*qi3 is needed
            !
            if ( (q_grid_b_rlu(i1-1,1+mod(i1-1,3))==0) .and. (q_grid_b_rlu(1+mod(i1-1,3),i1-1)==0) .and. &
&                (q_grid_b_rlu(i1-1,1+mod(i1,3))==0) .and. (q_grid_b_rlu(1+mod(i1,3),i1-1)==0)  .and.    &
&                (q_grid_b_rlu(1+mod(i1-1,3),1+mod(i1,3)) .ne. 0) .or. (q_grid_b_rlu(1+mod(i1,3),1+mod(i1-1,3)) .ne. 0)) then
               !  
               iqn1 = idx_q_2(1,iq1,12-i1)
               ! 
               f_func(5+mod(i1-1,3))= real(bare_qpg(iqn1,1)*bare_qpg(iqn1,1),kind=DP)/(4._DP*pi)* &
&                      vX_nnn(12-i1,iq1,1,1,iomega)/(vX_nnn(12-i1,iq1,1,1,iomega)+1._DP)
               ! 
               iqn2 = idx_q_2(1,iq1,15-i1)
               ! 
               f_func(5+mod(i1,3))= real(bare_qpg(iqn2,1)*bare_qpg(iqn2,1),kind=DP)/(4._DP*pi)* &
&                      vX_nnn(15-i1,iq1,1,1,iomega)/(vX_nnn(15-i1,iq1,1,1,iomega)+1._DP)
               ! 
               dp_dummy(15-i1) = (f_func(5+mod(i1-1,3))-f_func(5+mod(i1,3)))/ &
&                                (4._DP*(q_grid_b_rlu(1+mod(i1,3),1+mod(i1,3))*q_grid_b_rlu(1+mod(i1-1,3),1+mod(i1-1,3))))
            end if
         end if
        end if
        ! 
        if (q0_snd_neighbour) then
            dp_dummy(2:4)=0
            dp_dummy(5) = (f_func(2)-2._DP*f_func(1)+f_func(5))/(2._DP*q_grid_b_rlu(1,1)**2) !f11
            dp_dummy(6) = (f_func(3)-2._DP*f_func(1)+f_func(6))/(2._DP*q_grid_b_rlu(2,2)**2) !f22
            dp_dummy(7) = (f_func(4)-2._DP*f_func(1)+f_func(7))/(2._DP*q_grid_b_rlu(3,3)**2) !f33
            dp_dummy(8) = (f_func(2)-f_func(5))/(2._DP*q_grid_b_rlu(1,1)**3) !f111
            dp_dummy(9) = (f_func(3)-f_func(6))/(2._DP*q_grid_b_rlu(2,2)**3) !f222
            dp_dummy(10) = (f_func(4)-f_func(7))/(2._DP*q_grid_b_rlu(3,3)**3) !f333
         if (iq0 >=5) then
            i1=iq0-3
            dp_dummy(i1) = (-6*f_func(iq0)+3._DP*f_func(1)+2*f_func(i1))/(6._DP*q_grid_b_rlu(i1-1,i1-1)) 
            dp_dummy(i1+3)=  (f_func(iq0)-2._DP*f_func(1)+f_func(i1))/(2._DP*q_grid_b_rlu(i1-1,i1-1)**2)  
            dp_dummy(i1+6)=  (3*f_func(iq0)-3._DP*f_func(1)+f_func(i1))/(6._DP*q_grid_b_rlu(i1-1,i1-1)**3)                               
         else 
            i1=iq0
            dp_dummy(i1) = (-6*f_func(iq0)+3._DP*f_func(1)+2*f_func(i1+3))/(6._DP*q_grid_b_rlu(i1-1,i1-1)) 
            dp_dummy(i1+3)=  (f_func(iq0)-2._DP*f_func(1)+f_func(i1+3))/(2._DP*q_grid_b_rlu(i1-1,i1-1)**2)  
            dp_dummy(i1+6)=  (3*f_func(iq0)-3._DP*f_func(1)+f_func(i1+3))/(6._DP*q_grid_b_rlu(i1-1,i1-1)**3) 
         end if
        end if
        !
        if (iq1==1)   then 
          !
          ! Head
          !======
          !

          if ( (igr==1.and.igc==1) &
&            .or. ( (igr==1) .and. (igc/= 1) .and. (cut_is_slab) &
&                   .and. (g_vec(igc,idir(2))==g_vec(igc,idir(3))) .and. (g_vec(igc,idir(2))==0) .and. (iomega==1) ) &
&            .or. ( (igr==1) .and. (igc .ne. 1) .and. (cut_is_slab) .and. (g_vec(igc,idir(2))==g_vec(igc,idir(3))) &
&                   .and. (g_vec(igc,idir(2))==0) .and. (iomega==1) ) ) then
          !
          ! Special treatment of the q-->0 term
          !=====================================
          ! Evaluation of f_func of q-->0 term uses next nearest neighbour
          ! Index ordering of NEXT nearest-neighbour maps
          !
          !       6     7
          !       ^    /
          !       |   /
          !       3  4
          !       ^ /
          !       |/
          !       1 --> 2 --> 5
          !
          !
          idx_q_2 (igc,iq1,1) = iq1
          idx_is_2(igc,iq1,1) = 1
          idx_G_2 (igc,iq1,1) = igc
          !
          !Calculate qpG and qpG nearest neighbour
          !qpG(:,1) = q%pt(iq1,:)+g_vec(ig1,:)
          !
          ! Find nearest 1 --> 2
          !
          iq2_tmp = idx_q(igr,iq1,2)
          ig1_tmp = idx_G(igr,iq1,2)
          ig2_tmp = idx_G(igc,iq1,2)
          !
          ! Find next nearest 2 --> 5
          !
          iq2 = idx_q_2(igr,iq1,5)
          ig1 = idx_G_2(igr,iq1,5)
          ig2 = idx_G_2(igc,iq1,5)
          !
          !call msg('r','iq  =',iq1)
          !call msg('r','igr =',igr)
          !call msg('r','igc =',igc)
          !
          !call msg('r','iq_nn  =',iq2_tmp)
          !call msg('r','igr_nn =',ig1_tmp)
          !call msg('r','igc_nn =',ig2_tmp)
          !call msg('r','q_nn =',q%pt(iq2_tmp,:))
          !
          !call msg('r','iq_nnn  =',iq2)
          !call msg('r','igr_nnn =',ig1)
          !call msg('r','igc_nnn =',ig2)
          !call msg('r','q_nnn =',q%pt(iq2,:))
          !
          f_func(5) = real(bare_qpg(iq2,ig1)*bare_qpg(iq2,ig2),kind=DP)/(4._DP*pi)*&
&                     vX_nnn(5,iq1,igr,igc,iomega)/(vX_nnn(5,iq1,igr,igc,iomega)+1._DP)
          !
          !Find nearest 1 --> 3
          !
          iq2_tmp = idx_q(igr,iq1,3)
          ig1_tmp = idx_G(igr,iq1,3)
          ig2_tmp = idx_G(igc,iq1,3)
          !
          !Find next nearest 3 --> 6
          !
          iq2 = idx_q_2(igr,iq1,6)
          ig1 = idx_G_2(igr,iq1,6)
          ig2 = idx_G_2(igc,iq1,6)
          !
          !call msg('r','iq_nn  =',iq2_tmp)
          !call msg('r','igr_nn =',ig1_tmp)
          !call msg('r','igc_nn =',ig2_tmp)
          !call msg('r','q_nn =',q%pt(iq2_tmp,:))
          !
          !call msg('r','iq_nnn  =',iq2)
          !call msg('r','igr_nnn =',ig1)
          !call msg('r','igc_nnn =',ig2)
          !call msg('r','q_nnn =',q%pt(iq2,:))
          !
          f_func(6) = real(bare_qpg(iq2,ig1)*bare_qpg(iq2,ig2),kind=DP)/(4._DP*pi)*&
&                     vX_nnn(6,iq1,igr,igc,iomega)/(vX_nnn(6,iq1,igr,igc,iomega)+1._DP)
          !
          !Find nearest 1 --> 4
          !
          iq2_tmp = idx_q(igr,iq1,4)
          ig1_tmp = idx_G(igr,iq1,4)
          ig2_tmp = idx_G(igc,iq1,4)
          !
          !Find next nearest 4 --> 7
          !
          iq2 = idx_q_2(igr,iq1,7)
          ig1 = idx_G_2(igr,iq1,7)
          ig2 = idx_G_2(igc,iq1,7)
          !        
          !call msg('r','iq_nn  =',iq2_tmp)
          !call msg('r','igr_nn =',ig1_tmp)
          !call msg('r','igc_nn =',ig2_tmp)
          !call msg('r','q_nn =',q%pt(iq2_tmp,:))
          !
          !call msg('r','iq_nnn  =',iq2)
          !call msg('r','igr_nnn =',ig1)
          !call msg('r','igc_nnn =',ig2)
          !call msg('r','q_nnn =',q%pt(iq2,:))
          !
          f_func(7) = real(bare_qpg(iq2,ig1)*bare_qpg(iq2,ig2),kind=DP)/(4._DP*pi)*&
&                     vX_nnn(7,iq1,igr,igc,iomega)/(vX_nnn(7,iq1,igr,igc,iomega)+1._DP)
          end if 
          if (igr==1.and.igc==1) then
          if(iomega==1) then
            select case (trim(rimw_type))
              !
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              case ("semiconductor")
                !
                call msg("r","Semiconducting q --> 0 shape of f")
                !
                if (cut_is_slab) then
                !Save f as VXV
                if(RIM_id_epsm1_reference/=0) then
                  !
                  !vX is included in the anis_factor
                  !
                  f_coeff(1,1,1,1,1)=(4._DP*pi)/(bare_qpg(1,1)*bare_qpg(1,1)*(2._DP*pi*alat(idir(1)))**2)
                else
                  f_coeff(1,1,1,1,1)=vX_nn(1,1,1,1,1)*(4._DP*pi) / &
&                                    (bare_qpg(1,1)*bare_qpg(1,1)*(2._DP*pi*alat(idir(1)))**2)
                end if
                !
                do nn=2,4
                  idm = nn-1
                  !
                  if(RIM_id_epsm1_reference/=0 .and. iomega==1) then
                    anis_factor=dot_product(em1_anis,q_grid_b_cc(:,idm)**2)
                    f_coeff(nn,1,1,1,1)=1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                         (f_coeff(1,1,1,1,1)*anis_factor))
                  else
                    f_coeff(nn,1,1,1,1)=1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                         (v_norm(q_grid_b_cc(:,idm))**2*f_coeff(1,1,1,1,1)))
                  end if
                  !
                 end do
                else
                !Save f as VX
                if(RIM_id_epsm1_reference/=0) then
                   !
                   !vX is included in the anis_factor
                   !
                   f_coeff(1,1,1,1,1)=1.0/(4*pi*(1.0+vX_nn(1,1,1,1,1)))
                   call msg('r','an f_lim  =',real(f_coeff(1,1,1,1,1),SP))
                else
                   f_coeff(1,1,1,1,1)=vX_nn(1,1,1,1,1)/(4*pi*(1.0+vX_nn(1,1,1,1,1)))
                   call msg('r','f_lim  =',real(f_coeff(1,1,1,1,1),SP))
                end if
                   !
                do nn=2,4
                  idm = nn-1
                  !
                  if(RIM_id_epsm1_reference/=0 .and. iomega==1) then
                    anis_factor=dot_product(em1_anis,q_grid_b_cc(:,idm)**2)
                    f_coeff(nn,1,1,1,1)=1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                         (f_coeff(1,1,1,1,1)*anis_factor))
                    call msg('r','an f_nn  =',real(f_coeff(nn,1,1,1,1),SP))
                  else
                    f_coeff(nn,1,1,1,1)=1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                         (v_norm(q_grid_b_cc(:,idm))**2*f_coeff(1,1,1,1,1)))
                    call msg('r','f_nn  =',real(f_coeff(nn,1,1,1,1),SP))
                  end if
                   !
                enddo
                end if
              !
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              case ("metal")
                !
                call msg("r","Metallic q --> 0 shape of f")
                !
                ! The first point in metals is computed with a first order Taylor expansion 
                ! Coefficients in rlu (linear)
                !
                ! We make average to compute the constant value, that is linked to density at Fermi.   
                ! Average along the three directions if 3D, only along the planar directions if 2D.
                !
                if (cut_is_slab) then 
                  f_coeff(1,1,1,1,1) = ((2._DP*f_func(idir(2)+1)-f_func(idir(2)+4))+ &
&                                                  (2._DP*f_func(idir(3)+1)-f_func(idir(3)+4)))/2._DP 
                  call msg('r','2D Thomas-Fermi const  =',-real(pi*f_coeff(1,igr,igc,iq1,iomega)*alat(idir(1)),SP))
                else 
                  f_coeff(1,1,1,1,1) = ((2._DP*f_func(2)-f_func(5))+(2._DP*f_func(3)-f_func(6)) + &
&                                                  (2._DP*f_func(4)-f_func(7)))/3._DP   
                  call msg('r','3D Thomas-Fermi const  =',-real(4*pi**2*f_coeff(1,igr,igc,iq1,iomega),SP)) 
                end if  
                !f_coeff(1,1,1,1,1)=0._DP  
                ! 
                !
                ! quadratic dispersion to the neighbours
                f_coeff(2,igr,igc,iq1,iomega) = (4._DP*f_func(2)-f_func(5)-3._DP*f_coeff(1,1,1,1,1))/(2.0*q_grid_b_rlu(1,1))   ! f1
                f_coeff(3,igr,igc,iq1,iomega) = (4._DP*f_func(3)-f_func(6)-3._DP*f_coeff(1,1,1,1,1))/(2.0*q_grid_b_rlu(2,2))   ! f2
                f_coeff(4,igr,igc,iq1,iomega) = (4._DP*f_func(4)-f_func(7)-3._DP*f_coeff(1,1,1,1,1))/(2.0*q_grid_b_rlu(3,3))   ! f3
                f_coeff(5,igr,igc,iq1,iomega) = (f_func(5)-2._DP*f_func(2)+f_coeff(1,1,1,1,1))/(2.0*q_grid_b_rlu(1,1)**2)      ! f11
                f_coeff(6,igr,igc,iq1,iomega) = (f_func(6)-2._DP*f_func(3)+f_coeff(1,1,1,1,1))/(2.0*q_grid_b_rlu(2,2)**2)      ! f22
                f_coeff(7,igr,igc,iq1,iomega) = (f_func(7)-2._DP*f_func(4)+f_coeff(1,1,1,1,1))/(2.0*q_grid_b_rlu(3,3)**2)      ! f33
                !              
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              case ("Dirac")
                !
                ! Dirac cone treatment, a Fermi velocity has to be provided
                call msg('r','Graphene q--> 0 term included')
                call parser('v_fermi',Dirac_v_f)
                Dirac_v_f_au = Dirac_v_f/2.187691_SP
                X0_Dirac = -1.0/(4._SP*Dirac_v_f_au*alat(idir(1)))
                !
                call msg("r","Dirac cone q --> 0 shape of f")
                call msg("r","Fermi velocity vf ",Dirac_v_f_au)
                !
                !f_coeff(1,1,1,1,1) = (vX_nn(1,1,1,1,1)+VX_Dirac)/((1._DP+(vX_nn(1,1,1,1,1)+VX_Dirac)) &
!&                                       *2._DP*pi*alat(idir(1)))
                f_coeff(1,1,1,1,1) = X0_Dirac*2._DP*pi*alat(idir(1))
                !
                call msg("r","Dirac f head",real(vX_nn(1,1,1,1,1),SP))
                !
                do nn=2,3
                  idm = nn-1
                  !
                  f_coeff(nn,1,1,1,1)=1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                       (v_norm(q_grid_b_cc(:,idm))*f_coeff(1,1,1,1,1)))
                  !
                enddo
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              case default
                !
                call msg("r","No specific q --> 0 shape of f")
              !
              end select
            !Do not convert static head to iku
            cycle
            !
            end if
            !
            ! Finite frequency, as semiconductors
            ! The finite frequency term should be united to the term for semiconductors, but there is to see the role of anistropy
            !
            if(igr==1.and.igc==1.and.iomega.ne.1) then
              select case (trim(rimw_type))
              !
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              case ("semiconductor","Dirac") 
              !
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                !
                if (cut_is_slab) then
                  !Save f as VXV
                 if(RIM_id_epsm1_reference/=0) then
                   !
                   !vX is included in the anis_factor
                   !
                   f_coeff(1,1,1,1,iomega)=(4._DP*pi)/(bare_qpg(1,1)*bare_qpg(1,1)*(2._DP*pi*alat(idir(1)))**2)
                 else
                   f_coeff(1,1,1,1,iomega)=vX_nn(1,1,1,1,iomega)*(4._DP*pi) / &
&                                  (bare_qpg(1,1)*bare_qpg(1,1)*(2._DP*pi*alat(idir(1)))**2)
                 end if
                 !
                 do nn=2,4
                   !
                   idm=nn-1
                   !
                   if(RIM_id_epsm1_reference/=0 .and. iomega==1) then
                   anis_factor=dot_product(em1_anis,q_grid_b_cc(:,idm)**2)
                   f_coeff(nn,1,1,1,iomega)=1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                         (f_coeff(1,1,1,1,iomega)*anis_factor))
                   else
                    f_coeff(nn,1,1,1,iomega)=1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                         (v_norm(q_grid_b_cc(:,idm))**2*f_coeff(1,1,1,1,iomega)))
                   end if
                   !
                   enddo
               else 
                   !Save f as VX
                   f_coeff(1,1,1,1,iomega)=vX_nn(1,1,1,1,iomega)/(4*pi*(1.0+vX_nn(1,1,1,1,iomega)))
                   !
                   do nn=2,4
                   !
                   idm=nn-1
                   !
                   if(RIM_id_epsm1_reference/=0 .and. iomega==1) then
                    anis_factor=dot_product(em1_anis,q_grid_b_cc(:,idm)**2)
                    f_coeff(nn,1,1,1,iomega)=1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                         (f_coeff(1,1,1,1,iomega)*anis_factor))
                   else
                    f_coeff(nn,1,1,1,iomega)=1._DP/q_grid_b_rlu(idm,idm)*log(f_func(nn)/ &
&                                         (v_norm(q_grid_b_cc(:,idm))**2*f_coeff(1,1,1,1,iomega)))
                   end if
                   !
                   enddo
                end if
                cycle
              !
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              case ("metal")
              !
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                !
                ! As in the static case, the first point in metals is missing
                ! We know f behaves quadratically for q->0
                ! We fix the q->0 value from third order expansion with nnn
                !
                if (cut_is_slab) then 
                f_coeff(1,1,1,1,iomega)=  (8._DP*f_func(idir(2)+1)-f_func(idir(2)+4))/(8._DP*   &
&                                          v_norm(q_grid_b_cc(:,idir(2)))**2)                   &
&                                       + (8._DP*f_func(idir(3)+1)-f_func(idir(3)+4))/(8._DP*   &
&                                          v_norm(q_grid_b_cc(:,idir(3)))**2) 
                else 
                f_coeff(1,1,1,1,iomega)=  (8._DP*f_func(2)-f_func(5))/(12._DP*v_norm(q_grid_b_cc(:,1))**2) &
&                                       + (8._DP*f_func(3)-f_func(6))/(12._DP*v_norm(q_grid_b_cc(:,2))**2) &
&                                       + (8._DP*f_func(4)-f_func(7))/(12._DP*v_norm(q_grid_b_cc(:,3))**2) 
                end if    

                ! Alternative expansion through third order, easier for inclusion of anistropy but more difficult to use
                ! f_coeff(5,1,1,1,iomega)=  (8._DP*f_func(2)-f_func(5))/(4._DP*v_norm(q_grid_b_rlu(:,1))**2) 
                !f_coeff(6,1,1,1,iomega)=  (8._DP*f_func(3)-f_func(6))/(4._DP*v_norm(q_grid_b_rlu(:,2))**2) 
                !f_coeff(7,1,1,1,iomega)=  (8._DP*f_func(4)-f_func(7))/(4._DP*v_norm(q_grid_b_rlu(:,3))**2) 
                !f_coeff(8,1,1,1,iomega)=  (f_func(5)-4._DP*f_func(2))/(4._DP*v_norm(q_grid_b_rlu(:,1))**3) 
                !f_coeff(9,1,1,1,iomega)=  (f_func(6)-4._DP*f_func(3))/(4._DP*v_norm(q_grid_b_rlu(:,2))**3) 
                !f_coeff(10,1,1,1,iomega)= (f_func(7)-4._DP*f_func(4))/(4._DP*v_norm(q_grid_b_rlu(:,3))**3) 
                do nn=2,4
                 !
                   idm=nn-1
                   f_coeff(nn,1,1,1,iomega)= 1.0/q_grid_b_rlu(idm,idm)*log((2*f_func(nn)-sqrt(4*f_func(nn)**2          &   
&                                            -v_norm(q_grid_b_cc(:,idm))**2*f_coeff(1,1,1,1,iomega)* f_func(nn+3)))    &
&                                            /(2._DP*v_norm(q_grid_b_cc(:,idm))**2*f_coeff(1,1,1,1,iomega)))
                   f_coeff(nn+3,1,1,1,iomega)=  (4*f_func(nn)**2-f_func(nn+3)*v_norm(q_grid_b_cc(:,idm))**2*f_coeff(1,1,1,     &           
&                                                1,iomega)+2*f_func(nn)*sqrt(4*f_func(nn)**2-v_norm(q_grid_b_cc(:,idm))**2     &
&                                               *f_coeff(1,1,1,1,iomega)*f_func(nn+3)))/                                       &
&                                               (f_func(nn+3)*v_norm(q_grid_b_cc(:,idm))**2*q_grid_b_rlu(idm,idm))
                 end do
                !
                ! keep the coefficients in rlu
                cycle
            end select
            !
            end if
            end if
            !
            ! Wings 
            if ( ( (igr==1) .and. (igc .ne. 1) ) .or. ( (igc==1) .and. (igr .ne. 1) ) ) then
              select case (trim(rimw_type))
              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ! case ("semiconductor")
                !
                ! Wings for semiconductor (now removed from the calculation)
                !
                !=======
                !  dp_dummy(1) = f_func(1)                                                              !f0
                !  dp_dummy(2) = (-f_func(5)+4._DP*f_func(3)-3._DP*f_func(1))/(2._DP*q_grid_b_rlu(1,1)) !f1
                !  dp_dummy(3) = (-f_func(4)+4._DP*f_func(2)-3._DP*f_func(1))/(2._DP*q_grid_b_rlu(2,2)) !f2
                !  dp_dummy(4) = (f_func(5)-2._DP*f_func(3)+f_func(1))/(2._DP*q_grid_b_rlu(1,1)**2)     !f11
                !  dp_dummy(5) = (f_func(4)-2._DP*f_func(2)+f_func(1))/(2._DP*q_grid_b_rlu(2,2)**2)     !f22

              !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                case ("metal")
                  !
                  dp_dummy(:) = 0.0  
                  !
                  if (cut_is_slab) then
                   if (iomega==1) then
                     !
                     ! Static wings in 2D metals computed with Taylor expansion, vanish linearly for q --> 0
                     !
                     if ( (igr==1 .and. (g_vec(igc,idir(2))==g_vec(igc,idir(3))) .and. (g_vec(igc,idir(2))==0)) .or. &
&                       (igc==1 .and. (g_vec(igr,idir(2))==g_vec(igr,idir(3))) .and. (g_vec(igr,idir(2))==0)) ) then
                      ! Out of plane wings
                      ! Coefficients in rlu 
                      f_coeff(1,igr,igc,1,iomega)= 0
                      dp_dummy(2)=(4.0*f_func(2)-f_func(5))/(2._DP*q_grid_b_rlu(1,1))  
                      dp_dummy(3)=(4.0*f_func(3)-f_func(6))/(2._DP*q_grid_b_rlu(2,2))
                      dp_dummy(4)=(4.0*f_func(4)-f_func(7))/(2._DP*q_grid_b_rlu(3,3))
                      dp_dummy(5)=(f_func(5)-2.0*f_func(2))/(2._DP*q_grid_b_rlu(1,1)**2)  
                      dp_dummy(6)=(f_func(6)-2.0*f_func(3))/(2._DP*q_grid_b_rlu(2,2)**2)
                      dp_dummy(7)=(f_func(7)-2.0*f_func(4))/(2._DP*q_grid_b_rlu(3,3)**2)
                      ! linear at leading order, there is a small anistropy that is accounted averaging
                       
                      f_coeff(idir(2)+1,igr,igc,1,iomega) = (dp_dummy(idir(2)+1)+dp_dummy(idir(3)+1))/2._DP     !f idir2
                      f_coeff(idir(3)+1,igr,igc,1,iomega) = (dp_dummy(idir(2)+1)+dp_dummy(idir(3)+1))/2._DP     !f idir3
                      f_coeff(idir(1)+1,igr,igc,1,iomega) = 0        
                      f_coeff(idir(2)+4,igr,igc,1,iomega) = (dp_dummy(idir(2)+4)+dp_dummy(idir(3)+4))/2._DP      !f idir22
                      f_coeff(idir(3)+4,igr,igc,1,iomega) = (dp_dummy(idir(2)+4)+dp_dummy(idir(3)+4))/2._DP      !f idir33
                      f_coeff(idir(1)+4,igr,igc,1,iomega) = 0
                     else
                      ! On plane wings
                      !Coefficients in rlu
                      f_coeff(1,igr,igc,1,iomega)= 0
                      f_coeff(2,igr,igc,1,iomega) = (f_func(2)+f_func(5))/(2._DP*q_grid_b_rlu(1,1))         !f1
                      f_coeff(3,igr,igc,1,iomega) = (f_func(3)+f_func(6))/(2._DP*q_grid_b_rlu(2,2))         !f2
                      f_coeff(4,igr,igc,1,iomega) = (f_func(4)+f_func(7))/(2._DP*q_grid_b_rlu(3,3))         !f3
                      f_coeff(5:7,igr,igc,1,iomega)= 0
                      f_coeff(8,igr,igc,1,iomega) = (f_func(2)-f_func(5))/(2._DP*q_grid_b_rlu(1,1)**2)      !f11a
                      f_coeff(9,igr,igc,1,iomega) = (f_func(3)-f_func(6))/(2._DP*q_grid_b_rlu(2,2)**2)      !f22a
                      f_coeff(10,igr,igc,1,iomega) = (f_func(4)-f_func(7))/(2._DP*q_grid_b_rlu(3,3)**2)     !f33a
                      !
                     end if
                     ! keep the coefficients in rlu
                     cycle
                   else 
                     !
                     ! Wings f function, XV and W are anisotropic at finite frequency
                     ! Naive anisotropic expansion, split left and right neighbours
                     ! 
                     ! We need the second order neighbours along the left and right direction
                     do nn=2,3
                       iq2 = idx_q_2(igr,1,idir(nn)+1)
                       ig1 = idx_G_2(igr,1,idir(nn)+1)
                       ig2 = idx_G_2(igc,1,idir(nn)+1)
                       f_func(idir(nn)+7) = real(bare_qpg(iq2,ig1)*bare_qpg(iq2,ig2),kind=DP)/(4._DP*pi)*&
&                                  vX_nnn(idir(nn)+1,1,igr,igc,iomega)/(vX_nnn(idir(nn)+1,1,igr,igc,iomega)+1._DP)
                       iq2 = idx_q_2(igr,1,idir(nn)+4)
                       ig1 = idx_G_2(igr,1,idir(nn)+4)
                       ig2 = idx_G_2(igc,1,idir(nn)+4)
                       f_func(idir(nn)+10) = real(bare_qpg(iq2,ig1)*bare_qpg(iq2,ig2),kind=DP)/(4._DP*pi)*&
&                                  vX_nnn(idir(nn)+4,1,igr,igc,iomega)/(vX_nnn(idir(nn)+4,1,igr,igc,iomega)+1._DP)
                     end do
                     !
                     if ( (igr==1 .and. (g_vec(igc,idir(2))==g_vec(igc,idir(3))) .and. (g_vec(igc,idir(2))==0)) .or.  &
&                       (igc==1 .and. (g_vec(igr,idir(2))==g_vec(igr,idir(3))) .and. (g_vec(igr,idir(2))==0)) ) then
                      ! Coefficients kept in rlu
                      ! Out of plane plane wings quadratic + cubic (1-3 positive 5-7 negative quadratic direction)
                      ! (8-10 positive 11-13 negative cubic direction)
                      f_coeff(1:13,igr,igc,1,iomega) = 0._DP 
                      f_coeff(idir(2)+1,igr,igc,1,iomega) = (8*f_func(idir(2)+1)-f_func(idir(2)+7)) / &
&                                                           (4*q_grid_b_rlu(idir(2),idir(2))**2)    ! f idir22 r
                      f_coeff(idir(3)+1,igr,igc,1,iomega) = (8*f_func(idir(3)+1)-f_func(idir(3)+7)) / &
&                                                           (4*q_grid_b_rlu(idir(3),idir(3))**2)    ! f idir33 r
                      f_coeff(idir(2)+7,igr,igc,1,iomega) = (f_func(idir(2)+7)-4*f_func(idir(2)+1)) / &
&                                                           (4*q_grid_b_rlu(idir(2),idir(2))**3)    ! f idir222 r
                      f_coeff(idir(3)+7,igr,igc,1,iomega) = (f_func(idir(3)+7)-4*f_func(idir(3)+1)) / &
                                                            (4*q_grid_b_rlu(idir(3),idir(3))**3)    ! f idir333 r
                      ! 
                      f_coeff(idir(2)+4,igr,igc,1,iomega) = (8*f_func(idir(2)+4)-f_func(idir(2)+10)) / &
&                                                           (4*q_grid_b_rlu(idir(2),idir(2))**2)    ! f idir22 l
                      f_coeff(idir(3)+4,igr,igc,1,iomega) = (8*f_func(idir(3)+4)-f_func(idir(3)+10)) / &
&                                                           (4*q_grid_b_rlu(idir(3),idir(3))**2)    ! f idir33 l
                      f_coeff(idir(2)+10,igr,igc,1,iomega) = -(f_func(idir(2)+10)-4*f_func(idir(2)+4)) / &
&                                                             (4*q_grid_b_rlu(idir(2),idir(2))**3)  ! f idir222 l
                      f_coeff(idir(3)+10,igr,igc,1,iomega) = -(f_func(idir(3)+10)-4*f_func(idir(3)+4)) / &
&                                                             (4*q_grid_b_rlu(idir(3),idir(3))**3) ! f idir333 l
                      !
                     else
                      ! Coefficients kept in rlu
                      ! In plane wings linear + quadratic (1-3 positive 5-7 negative linear direction)
                      ! (7-9 positive 10-12 negative quadratic direction)
                      f_coeff(1:13,igr,igc,1,iomega) = 0._DP 
                      f_coeff(idir(2)+1,igr,igc,1,iomega) = (4*f_func(idir(2)+1)-f_func(idir(2)+7)) / &
&                                                           (2*q_grid_b_rlu(idir(2),idir(2)))       ! f idir2 r
                      f_coeff(idir(3)+1,igr,igc,1,iomega) = (4*f_func(idir(3)+1)-f_func(idir(3)+7)) / &
&                                                           (2*q_grid_b_rlu(idir(3),idir(3)))       ! f idir3 r
                      f_coeff(idir(2)+7,igr,igc,1,iomega) = (f_func(idir(2)+1)-2*f_func(idir(2)+7)) / &
&                                                           (2*q_grid_b_rlu(idir(2),idir(2))**2)    ! f idir22 r
                      f_coeff(idir(3)+7,igr,igc,1,iomega) = (f_func(idir(3)+1)-2*f_func(idir(3)+7)) / &
&                                                           (2*q_grid_b_rlu(idir(3),idir(3))**2)    ! f idir33 r
                      ! 
                      f_coeff(idir(2)+4,igr,igc,1,iomega) = -(4*f_func(idir(2)+4)-f_func(idir(2)+10)) / &
&                                                            (2*q_grid_b_rlu(idir(2),idir(2)))     ! f idir2 l
                      f_coeff(idir(3)+4,igr,igc,1,iomega) = -(4*f_func(idir(3)+4)-f_func(idir(3)+10)) / &
&                                                            (2*q_grid_b_rlu(idir(3),idir(3)))     ! f idir3 l
                      f_coeff(idir(2)+10,igr,igc,1,iomega) = (f_func(idir(2)+10)-2*f_func(idir(2)+4)) / &
&                                                            (2*q_grid_b_rlu(idir(2),idir(2))**2)  ! f idir22 l
                      f_coeff(idir(3)+10,igr,igc,1,iomega) = (f_func(idir(3)+10)-2*f_func(idir(3)+4)) / &
&                                                            (2*q_grid_b_rlu(idir(3),idir(3))**2)  ! f idir33 l
                     end if
                     ! keep the coefficients in rlu
                     cycle
                   end if
                   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  else
                   !
                   ! Static wings in 3D metals computed with Taylor expansion, vanish quadratically for q --> 0
                   ! Anistropy not relevant at leading order, quadratic behaviour originates from potential
                   !
                   !=======
                   !
                   !Coefficients in rlu
                   if (iomega==1) then
                    dp_dummy(5) = (f_func(5)+f_func(2))/(2._DP*q_grid_b_rlu(1,1)**2)       !f11
                    dp_dummy(6) = (f_func(6)+f_func(3))/(2._DP*q_grid_b_rlu(2,2)**2)       !f22
                    dp_dummy(7) = (f_func(7)+f_func(4))/(2._DP*q_grid_b_rlu(3,3)**2)       !f33
                    dp_dummy(8) = (f_func(2)-f_func(5))/(2._DP*q_grid_b_rlu(1,1)**3)       !f111
                    dp_dummy(9) = (f_func(3)-f_func(6))/(2._DP*q_grid_b_rlu(2,2)**3)       !f222
                    dp_dummy(10) = (f_func(4)-f_func(7))/(2._DP*q_grid_b_rlu(3,3)**3)      !f333
                   else
                    dp_dummy(2) = (f_func(2)-f_func(5))/(2._DP*q_grid_b_rlu(1,1))       !f1
                    dp_dummy(3) = (f_func(3)-f_func(6))/(2._DP*q_grid_b_rlu(2,2))       !f2
                    dp_dummy(4) = (f_func(4)-f_func(7))/(2._DP*q_grid_b_rlu(3,3))       !f3
                    dp_dummy(5) = (f_func(2)+f_func(5))/(2._DP*q_grid_b_rlu(1,1)**2)    !f11
                    dp_dummy(6) = (f_func(3)+f_func(6))/(2._DP*q_grid_b_rlu(2,2)**2)    !f22
                    dp_dummy(7) = (f_func(4)+f_func(7))/(2._DP*q_grid_b_rlu(3,3)**2)    !f33
                                                                               
                   end if
                   !
                   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  end if
              !
              end select
            !
            end if
          end if
        !
        ! set to zero coefficients in the non periodic direction
        if (cut_is_slab) then
            do nn=0,2 
             dp_dummy(1+3*nn+idir(1))=0
            end do
        end if
        !
        ! Coefficients in iku
        !  
        f_coeff(1,igr,igc,iq1,iomega) = dp_dummy(1)                                                                       !f  (iku)
        f_coeff(2,igr,igc,iq1,iomega) = dp_dummy(2)*a(1,1)/alat(1)+dp_dummy(3)*a(2,1)/alat(1)+dp_dummy(4)*a(3,1)/alat(1)  !fx (iku)
        f_coeff(3,igr,igc,iq1,iomega) = dp_dummy(3)*a(2,2)/alat(2)+dp_dummy(2)*a(1,2)/alat(2)+dp_dummy(4)*a(3,2)/alat(2)  !fy (iku)
        f_coeff(4,igr,igc,iq1,iomega) = dp_dummy(4)*a(3,3)/alat(3)+dp_dummy(2)*a(1,3)/alat(3)+dp_dummy(3)*a(2,3)/alat(3)  !fz (iku)
        !fxx(iku)
        f_coeff(5,igr,igc,iq1,iomega) = dp_dummy(5)*(a(1,1)/alat(1))**2+dp_dummy(6)*(a(2,1)/alat(1))**2 + dp_dummy(7)* &
&                                       (a(3,1)/alat(1))**2+2*dp_dummy(11)*(a(1,1)/alat(1))*(a(2,1)/alat(1)) + 2*dp_dummy(12)  & 
&                                      *(a(1,1)/alat(1))*(a(3,1)/alat(1))+ 2*dp_dummy(13)*(a(2,1)/alat(1))*(a(3,1)/alat(1)) 
        !fxy(iku)
        f_coeff(6,igr,igc,iq1,iomega) = dp_dummy(5)*a(1,1)/alat(1)*a(1,2)/alat(2)+dp_dummy(6)*a(2,2)/alat(2)*a(2,1)/alat(1)    &
&                                      +dp_dummy(7)*a(3,1)/alat(1)*a(3,2)/alat(2)+dp_dummy(11)*(a(1,1)/alat(1)*a(2,2)/alat(2)  &
&                                      +a(1,2)/alat(2)*a(2,1)/alat(1)) + dp_dummy(12)*(a(1,1)/alat(1)*a(3,2)/alat(2)           &
&                                      +a(1,2)/alat(2)*a(3,1)/alat(1)) + dp_dummy(13)*(a(3,1)/alat(1)*a(2,2)/alat(2)           &
&                                      +a(3,2)/alat(2)*a(2,1)/alat(1))                                                    
        !fxz(iku)
        f_coeff(7,igr,igc,iq1,iomega) = dp_dummy(5)*a(1,1)/alat(1)*a(1,3)/alat(3)+dp_dummy(7)*a(3,3)/alat(3) *a(3,1)/alat(1)   &
&                                      +dp_dummy(6)*a(2,1)/alat(1)*a(2,3)/alat(3)+dp_dummy(11)*(a(1,1)/alat(1)*a(2,3)/alat(3)  &
&                                      +a(1,3)/alat(3)*a(2,1)/alat(1)) + dp_dummy(12)*(a(1,1)/alat(1)*a(3,3)/alat(3)           &
&                                      +a(1,3)/alat(3)*a(3,1)/alat(1)) + dp_dummy(13)*(a(2,1)/alat(1)*a(3,3)/alat(3)           &
&                                      +a(2,3)/alat(3)*a(3,1)/alat(1))                                                    
        !fyy(iku)
        f_coeff(8,igr,igc,iq1,iomega) = dp_dummy(6)*(a(2,2)/alat(2))**2+dp_dummy(5)*(a(1,2)/alat(2))**2 + dp_dummy(7)*         &
&                                       (a(3,2)/alat(2))**2+2*dp_dummy(11)*(a(1,2)/alat(2))*(a(2,2)/alat(2)) + 2*dp_dummy(12)  & 
&                                      *(a(1,2)/alat(2))*(a(3,2)/alat(2))+ 2*dp_dummy(13)*(a(2,2)/alat(2))*(a(3,2)/alat(2)) 
        !fyz(iku)
        f_coeff(9,igr,igc,iq1,iomega) = dp_dummy(6)*a(2,2)/alat(2)*a(2,3)/alat(3)+dp_dummy(7)*a(3,3)/alat(3) *a(3,2)/alat(2)  &
&                                      +dp_dummy(5)*a(1,2)/alat(2)*a(1,3)/alat(3)+dp_dummy(11)*(a(1,3)/alat(3)*a(2,2)/alat(2) &
&                                      +a(1,2)/alat(2)*a(2,3)/alat(3)) + dp_dummy(12)*(a(1,2)/alat(2)*a(3,3)/alat(3)          &
&                                      +a(1,3)/alat(3)*a(3,2)/alat(2)) + dp_dummy(13)*(a(2,2)/alat(2)*a(3,3)/alat(3)          &
&                                      +a(2,3)/alat(3)*a(3,2)/alat(2))                                                    
        !fzz(iku)
        f_coeff(10,igr,igc,iq1,iomega)= dp_dummy(7)*(a(3,3)/alat(3))**2+dp_dummy(5)*(a(1,3)/alat(3))**2 + dp_dummy(6)*        &
&                                       (a(2,3)/alat(3))**2+2*dp_dummy(11)*(a(1,3)/alat(3))*(a(2,3)/alat(3)) + 2*dp_dummy(12) & 
&                                      *(a(1,3)/alat(3))*(a(3,3)/alat(3))+ 2*dp_dummy(13)*(a(2,3)/alat(3))*(a(3,3)/alat(3))
        !third order terms
        f_coeff(11:20,igr,igc,iq1,iomega)=0
        !
        do i1=1,3 !i
          do i2=1,3 !j            
            do nn=8,10
              f_coeff(7+3*i1+i2,igr,igc,iq1,iomega) = f_coeff(7+3*i1+i2,igr,igc,iq1,iomega) +  &
&                                                 dp_dummy(nn)*a(nn-7,i1)/alat(i1)*a(nn-7,i1)/alat(i1)*a(nn-7,i2)/alat(i2)  !fiij(iku)
            end do
          end do
        end do           
        do nn=8,10
           f_coeff(20,igr,igc,iq1,iomega) = f_coeff(20,igr,igc,iq1,iomega) + dp_dummy(nn)*a(nn-7,1)/alat(1)* &
&                                            a(nn-7,2)/alat(2)*a(nn-7,3)/alat(3) !fxyz(iku)
        end do
        ! 
!       old version , in general not true f_coeff(:,igc,igr,iq1,iomega) = f_coeff(:,igr,igc,iq1,iomega)
        !
!      end if
      enddo
    enddo
   enddo
           !call msg('r','f111',real(f_coeff(11,1,1,2,iomega)),SP)
           !call msg('r','f112',real(f_coeff(12,1,1,2,iomega)),SP)
           !call msg('r','f221',real(f_coeff(14,1,1,2,iomega)),SP)
           !call msg('r','f222',real(f_coeff(15,1,1,2,iomega)),SP)
           !call msg('r','f123',real(f_coeff(20,1,1,2,iomega)),SP)
 enddo
 !
 call timing('RIM-W-coeff',OPR='stop')
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of RIM-W integrals
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 call parser('rimw_check',l_check)
 if(l_check) call check_W_interpolation(q,Xw,f_coeff,q_grid_b_rlu,vX_nn,idx_q,idx_is,idx_G)
 !
 call rim('c',Xw)
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Debugging output
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 !
 if(allocated(RIM_qpg)) then
   call msg('nr','Comparison between head Wc = W - v averages: < Wc > [au] RIM-W/RIM')
   do iq1=1,q%nibz,2
       !
       msg_dum=' '
       !
       do iq2=iq1,min(iq1+1,q%nibz)
         !
         ch_dum=' < -Wc [Q = '//trim(intc(iq2))//'] >'
         if (iq1/=iq2) ch_dum=' * < -Wc [Q = '//trim(intc(iq2))//'] >'
         !
         r_dum(1)= -RIM_W(1,iq2,1,1)*2._SP*pi
         r_dum(2)= -vX_nn(1,iq2,1,1,1)*RIM_qpg(iq2,1,1)*2_SP*pi
         !
         fmt_dum='(a,2('//trim(gen_fmt(r_v=r_dum))//'))'
         write (msg_dum(iq2-iq1+1),trim(fmt_dum)) trim(ch_dum),r_dum
         !
       enddo
       if (iq1==1) call msg('nr',trim(STRING_pack(msg_dum(1),msg_dum(2))))
       if (iq1/=1) call msg(' r',trim(STRING_pack(msg_dum(1),msg_dum(2))))
   enddo
   call msg('r','')
   call msg('nr','Comparison between head Wc = W - v averages: < Wc > [au] RIM-W/RIM')
   do ig1=1,RIM_W_ng,2
       !
       msg_dum=' '
       !
       do ig2=ig1,min(ig1+1,RIM_W_ng)
         !
         ch_dum=' < -Wc [G = '//trim(intc(ig2))//',G`=0] >'
         if (ig1/=ig2) ch_dum=' * < -Wc [G = '//trim(intc(ig2))//',G`=0] >'
         !
         r_dum(1)= -RIM_W(1,1,ig2,1)*2._SP*pi
         r_dum(2)= -vX_nn(1,1,ig2,1,1)*RIM_qpg(1,ig2,1)*2_SP*pi
         !
         fmt_dum='(a,2('//trim(gen_fmt(r_v=r_dum))//'))'
         write (msg_dum(ig2-ig1+1),trim(fmt_dum)) trim(ch_dum),r_dum
         !
       enddo
       if (ig1==1) call msg('nr',trim(STRING_pack(msg_dum(1),msg_dum(2))))
       if (ig1/=1) call msg(' r',trim(STRING_pack(msg_dum(1),msg_dum(2))))
   enddo

 end if
 !
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !Calculation of RIM-W PPA energies (if needed)
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 !
 if (mode == 'PPA') then
   !
   YAMBO_ALLOC(RIM_W_E,(RIM_W_ng,RIM_W_ng))
   RIM_W_E = rZERO
   !
   do ig2=1,RIM_W_ng
     do ig1=1,RIM_W_ng
       !
       if(ig1==1.and.ig2==1) then
         !
         dummy(:) = f_coeff(1,1,1,1,:)
         if (RIM_id_epsm1_reference/=0) then
           if (cut_is_slab) then
             dummy(1) = dummy(1)*0.5*(em1_anis(idir(2))+em1_anis(idir(3)))
           else
             dummy(1) = dummy(1)/3.0*(em1_anis(1)+em1_anis(2)+em1_anis(3))
           end if
         end if
         !
       else
         !
         dummy(:) =  f_coeff(1,ig1,ig2,1,:) & 
                     + 1.E-5*(f_coeff(2,ig1,ig2,1,:)+f_coeff(3,ig1,ig2,1,:)+f_coeff(4,ig1,ig2,1,:) &
&                    + 1.E-5*(f_coeff(5,ig1,ig2,1,:)+f_coeff(8,ig1,ig2,1,:)+f_coeff(10,ig1,ig2,1,:) &
&                    + 2._SP*(f_coeff(6,ig1,ig2,1,:)+f_coeff(7,ig1,ig2,1,:)+f_coeff(9,ig1,ig2,1,:))))
         !
         !call msg("r","dummy",real(dummy)) 
         dummy(:) = dummy(:)*4._SP*pi/(bare_qpg(1,ig1)*bare_qpg(1,ig2))
         dummy(:) = dummy/(1._SP-dummy)
         !
       endif
       !
       if (dummy(1)/dummy(2)<=1._SP) then
         RIM_W_E(ig1,ig2)=X%ppaE
       else
         RIM_W_E(ig1,ig2)=sqrt(dummy(1)/dummy(2)-1._SP)
       endif
       !
     end do
   end do
   !
 end if
 !
 !Write the database
 !
 if (master_cpu) then
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=io_RIM_W(ID,mode,Xw)
 endif
 !
 YAMBO_FREE(f_coeff)
 !
end subroutine
!
subroutine find_q_nns(q,q_grid_b_iku,idx_q,idx_is,idx_G)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp,RIM_W_ng,g_vec,ng_vec,rl_sop
 use vec_operate,   ONLY:v_is_zero
 !
#include<y_memory.h>
 !
 type(bz_samp):: q
 real(SP)     :: q_grid_b_iku(3,3)
 !
 !The following variables map the extended BZ and n.n.
 !
 integer     :: idx_q (RIM_W_ng,q%nibz,7) !q    index irreduc. BZ
 integer     :: idx_is(RIM_W_ng,q%nibz,7) !Symm index
 integer     :: idx_G (RIM_W_ng,q%nibz,7) !G    index
 !
 ! Index ordering of nearest-neighbour maps
 !
 !       3  4
 !       ^ /
 !       |/
 ! 5 <-- 1 --> 2
 !      /|
 !     L v
 !    7  6
 !
 integer     :: iq,ig,iq_trial,ig_trial
 integer     :: iq_trial_ibz,is_trial,i_dummy,nn_find,nn
 real(SP)    :: qpG(3,7),qpG_trial(3)
 !
 !Cycles over qpG points
 do iq=1,q%nibz
   !
   do ig=1,RIM_W_ng
     !
     nn_find = 0
     !
     idx_q (ig,iq,1) = iq
     idx_is(ig,iq,1) = 1
     idx_G (ig,iq,1) = ig
     !
     !Calculate qpG and qpG nearest neighbour
     qpG(:,1) = q%pt(iq,:)+g_vec(ig,:)
     !
     qpG(:,2) = qpG(:,1) + q_grid_b_iku(:,1)
     qpG(:,3) = qpG(:,1) + q_grid_b_iku(:,2)
     qpG(:,4) = qpG(:,1) + q_grid_b_iku(:,3)
     qpG(:,5) = qpG(:,1) - q_grid_b_iku(:,1)
     qpG(:,6) = qpG(:,1) - q_grid_b_iku(:,2)
     qpG(:,7) = qpG(:,1) - q_grid_b_iku(:,3)
     !
     !Find nn indexes
     do i_dummy=1,ng_vec
       !
       !Try first ig2 near ig
       if (i_dummy < 2*ig) then
         ig_trial = (2*(i_dummy-2*(i_dummy/2))-1)*(i_dummy/2) + ig
       else
         ig_trial = i_dummy
       endif
       !
       do iq_trial=1,q%nbz
         !
         !Get ibz and is indexes
         iq_trial_ibz = q%sstar(iq_trial,1)
         is_trial = q%sstar(iq_trial,2)
         qpG_trial = matmul(rl_sop(:,:,is_trial),q%pt(iq_trial_ibz,:)+g_vec(ig_trial,:))
         !
         do nn=2,7
           if (v_is_zero(qpG_trial-qpG(:,nn))) then
             idx_q (ig,iq,nn) = iq_trial_ibz
             idx_is(ig,iq,nn) = is_trial
             idx_G (ig,iq,nn) = ig_trial
             nn_find = nn_find + 1
           endif
         enddo
         !
         if (nn_find == 6) exit
         !
       enddo
       !
       if (nn_find == 6) exit
       !
     enddo
     !
     if(nn_find < 6) call error('Nearest neighbours searching failed in the W interpolation')
     !
   enddo
   !
 enddo
 !
end subroutine
 !
subroutine find_q_nnns(q,q_grid_b_iku,idx_q,idx_is,idx_G)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp,RIM_W_ng,g_vec,ng_vec,rl_sop
 use vec_operate,   ONLY:v_is_zero
 !
#include<y_memory.h>
 !
 type(bz_samp):: q
 real(SP)     :: q_grid_b_iku(3,3)
 !
 !The following variables map the n.n.n
 !
 integer     :: idx_q (RIM_W_ng,q%nibz,13) !q    index irreduc. BZ
 integer     :: idx_is(RIM_W_ng,q%nibz,13) !Symm index
 integer     :: idx_G (RIM_W_ng,q%nibz,13) !G    index
 !
 ! Special treatment of the q-->0 term (only needed at zero frequency)
 !=====================================
 ! 
 ! Index ordering of NEXT nearest-neighbour along the three main directions
 !
 !             6     7                      10
 !             ^    /                      /
 !             |   /                      /
 !                                        --> 8  9
 !             ^ /                       ^      /
 !             |/                        |     /
 !  2 <--  <-- 1 -->  --> 5         13   1 -->     (mixed direction terms)
 !            /^                    ^  / ^   /
 !           / |                    | /  |  12   
 !                                         --> 11
 !         /   ^ 
 !        /    | 
 !       4     3          
 !
 ! Work Space
 !
 integer     :: iq,ig,iq_trial,ig_trial,max_numb
 integer     :: iq_trial_ibz,is_trial,i_dummy,nnn_find,nnn
 real(SP)    :: qpG(3,13),qpG_trial(3)
 !
 !Cycles over qpG points
 do iq=1,q%nibz
   !
   do ig=1,RIM_W_ng
     ! find in general 6 second order neighbours 
     max_numb=6
     ! For the neighbours of iq1, 12 second order neighbours are needed
     if (ig==1) max_numb=12
     !
     nnn_find = 0
     !
     idx_q (ig,iq,1) = iq
     idx_is(ig,iq,1) = 1
     idx_G (ig,iq,1) = ig
     !
     !Calculate qpG and qpG of next nearest neighbour
     qpG(:,1) = q%pt(iq,:)+g_vec(ig,:)
     !
     !Calculate qpG and qpG of next nearest neighbour
     qpG(:,2) = qpG(:,1) - 2.0*q_grid_b_iku(:,1)
     qpG(:,3) = qpG(:,1) - 2.0*q_grid_b_iku(:,2)
     qpG(:,4) = qpG(:,1) - 2.0*q_grid_b_iku(:,3)
     qpG(:,5) = qpG(:,1) + 2.0*q_grid_b_iku(:,1)
     qpG(:,6) = qpG(:,1) + 2.0*q_grid_b_iku(:,2)
     qpG(:,7) = qpG(:,1) + 2.0*q_grid_b_iku(:,3)
     if (ig==1) then
      qpG(:,8) = qpG(:,1) + q_grid_b_iku(:,1) + q_grid_b_iku(:,2)
      qpG(:,9) = qpG(:,1) + q_grid_b_iku(:,1) + q_grid_b_iku(:,3)
      qpG(:,10) = qpG(:,1) + q_grid_b_iku(:,2) + q_grid_b_iku(:,3)
      qpG(:,11) = qpG(:,1) - q_grid_b_iku(:,1) + q_grid_b_iku(:,2)
      qpG(:,12) = qpG(:,1) - q_grid_b_iku(:,1) + q_grid_b_iku(:,3)
      qpG(:,13) = qpG(:,1) - q_grid_b_iku(:,2) + q_grid_b_iku(:,3)
     end if
     !Find indexes
     do i_dummy=1,ng_vec
       !
       !Try first ig2 near ig
       if (i_dummy < 2*ig) then
         ig_trial = (2*(i_dummy-2*(i_dummy/2))-1)*(i_dummy/2) + ig
       else
         ig_trial = i_dummy
       endif
       !
       do iq_trial=1,q%nbz
         !
         !Get ibz and is indexes
         iq_trial_ibz = q%sstar(iq_trial,1)
         is_trial = q%sstar(iq_trial,2)
         qpG_trial = matmul(rl_sop(:,:,is_trial),q%pt(iq_trial_ibz,:)+g_vec(ig_trial,:))
         !
         ! nnn along positive suffice
         !
          do nnn=2,max_numb+1
           if (v_is_zero(qpG_trial-qpG(:,nnn))) then
             idx_q (ig,iq,nnn) = iq_trial_ibz
             idx_is(ig,iq,nnn) = is_trial
             idx_G (ig,iq,nnn) = ig_trial
             nnn_find = nnn_find + 1
           endif       
          enddo
          if (nnn_find == max_numb) exit
       enddo
       !
       if (nnn_find == max_numb) exit
       !
     enddo
     !
     if(nnn_find < max_numb) call error('Next nearest neighbours searching failed in the W interpolation')
     !
   enddo
   !
 enddo
 !
end subroutine
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
 character(schlen):: str_piece1,str_piece2,str_piece3
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
       open (unit=out_unit_W_fit,file="W_w="//trim(str_piece3)//"_"&
&//dir(i_dir)//"_fit_g"//trim(str_piece1)//"_g"//trim(str_piece2)//".dat",&
&          action="write",status="replace")
       open (unit=out_unit_W_num,file="W_w="//trim(str_piece3)//"_"&
&//dir(i_dir)//"_num_g"//trim(str_piece1)//"_g"//trim(str_piece2)//".dat",&
&          action="write",status="replace")
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
         write(out_unit_W_num,"(I4, E15.6, E15.6,E15.6,E15.6, E15.6, E15.6, E15.6, E15.6, E15.6, E15.6, E15.6)") &
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
           if(iq==1) then
             !
             ! Head
             !
             if(ig1==1.and.ig2==1) then
               !  
               ! Metal case, static and finite frequency 
               !
               select case (trim(rimw_type))
               !
               case ("metal")
               !
                if (iomega==1) then
                 !
                 q_out = q_sampl
                 call c2a(b_in=b,v_in=q_out,mode='ki2c')
                 func = f_coeff_loc(1) + f_coeff(2,ig1,ig2,iq,iomega)*abs(q_rlu(1))                             &
&                     + f_coeff(3,ig1,ig2,iq,iomega)*abs(q_rlu(2))+f_coeff(4,ig1,ig2,iq,iomega)*abs(q_rlu(3))   &
&                     + f_coeff(5,ig1,ig2,iq,iomega)*(q_rlu(1))**2 + f_coeff(6,ig1,ig2,iq,iomega)*(q_rlu(2))**2 &
&                     + f_coeff(7,ig1,ig2,iq,iomega)*(q_rlu(3))**2    
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
                   end if
                 end if
                 !
                 write(out_unit_W_fit,"(I5,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6)") &
&                           i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func),&
&                           aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)  
                 cycle
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
                   write(out_unit_W_fit,"(I5,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6)") &
&                           i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func),&
&                           aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)  
                   cycle
                 end if       
                 ! 
                 func = (func+f_coeff(5,1,1,1,iomega)*abs(q_rlu(1)) + f_coeff(6,1,1,1,iomega)*abs(q_rlu(2)) +      &
&                        f_coeff(7,1,1,1,iomega)*abs(q_rlu(3)))*exp(sign(1._DP,real(f_coeff(3,1,1,1,iomega)))      &
&                        *sqrt(f_coeff(2,1,1,1,iomega)**2*(q_rlu(1))**2 + f_coeff(3,1,1,1,iomega)**2*(q_rlu(2))**2 + &
&                        f_coeff(4,1,1,1,iomega)**2*(q_rlu(3))**2 ))     
                 !
                 func = func*iku_v_norm(q_sampl)**2
                 !func = func
                 epsm1_sampl = func*vslab/r1/(1._SP-vslab/r1*func)
                 W_sampl= vslab/r1*func*vslab/r1/(1._SP-vslab/r1*func)
                 
                 write(out_unit_W_fit,"(I5,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6)") &
&                           i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func),&
&                           aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)  
                 cycle
                end if
               end select
               !
               ! Semiconductors and Dirac cone, static case and finite frequency 
               !    
               if (iku_v_norm(q_sampl) < 1.e-5) then
                  !
                  epsm1_sampl = vslab*f_coeff(1,1,1,1,iomega)/(1.0-vslab*f_coeff(1,1,1,1,iomega))
                  W_sampl = f_coeff(1,1,1,1,iomega)/(1.0-vslab*f_coeff(1,1,1,1,iomega))*vslab**2/q0_def_norm**2 
                  if (iomega==1) then
                   select case (trim(rimw_type))
                    case ("Dirac")  
                     epsm1_sampl = vslab/q0_def_norm*f_coeff(1,1,1,1,iomega)/(1.0-vslab/q0_def_norm*f_coeff(1,1,1,1,iomega))
                     W_sampl = f_coeff(1,1,1,1,iomega)/(1.0-vslab/q0_def_norm*f_coeff(1,1,1,1,iomega))*vslab**2/q0_def_norm**3
                   end select
                  end if                  
                  !
                  if(RIM_id_epsm1_reference /= 0 .and. iomega == 1) then
                      W_sampl = W_sampl*em1_anis(i_dir)
                  end if
                  !
                  write(out_unit_W_fit,"(I5,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6)") &
&                           i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1,0.0,&
&                           0.0,real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
                  cycle
               end if
               !
               if (cut_is_slab) then
               !
               func =  f_coeff(1,1,1,1,iomega)*exp(sign(1._DP,real(f_coeff(2,1,1,1,iomega),DP))*sqrt(  &
&                      f_coeff(2,1,1,1,iomega)**2*(q_rlu(1))**2+f_coeff(3,1,1,1,iomega)**2*(q_rlu(2))**2))
               !
               else
               ! 
               func =  f_coeff(1,1,1,1,iomega)*exp(sign(1._DP,real(f_coeff(2,1,1,1,iomega),DP))*sqrt(f_coeff(2,1,1,1,iomega)**2 &
&                      *(q_rlu(1))**2 + f_coeff(3,1,1,1,iomega)**2*(q_rlu(2))**2 + f_coeff(4,1,1,1,iomega)**2*(q_rlu(3))**2))

               !
               end if
               !
               if(RIM_id_epsm1_reference /= 0 .and. iomega == 1) then
                 anis_fact = dot_product(em1_anis,(2._SP*pi*q_sampl/alat)**2)/iku_v_norm(q_sampl)**2
                 func = func*anis_fact
               end if
               !
               if (iomega==1) then
                  select case (trim(rimw_type))
                  !
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
&                          *f_coeff_loc(6)) +q_sampl(2)*(f_coeff_loc(3)+q_sampl(2)*f_coeff_loc(8) + 2_DP*q_sampl(3) &
&                          *f_coeff_loc(9))+q_sampl(3)*(f_coeff_loc(4)+q_sampl(3)*f_coeff_loc(10) + 2_DP*q_sampl(1) &
&                          *f_coeff_loc(7))
                  end select
                  !
               else
                 select case (trim(rimw_type))
                 !
                 case ("semiconductor","Dirac")
                    func = func*iku_v_norm(q_sampl)**2
                    !
                 case default
                    func = f_coeff_loc(1) +q_sampl(1)*(f_coeff_loc(2)+q_sampl(1)*f_coeff_loc(5) + 2_DP*q_sampl(2)   &
&                          *f_coeff_loc(6)) +q_sampl(2)*(f_coeff_loc(3)+q_sampl(2)*f_coeff_loc(8) + 2_DP*q_sampl(3) &
&                          *f_coeff_loc(9))+q_sampl(3)*(f_coeff_loc(4)+q_sampl(3)*f_coeff_loc(10) + 2_DP*q_sampl(1) &
&                          *f_coeff_loc(7))
                 end select
                 !
               end if
             end if
             !
             ! wings
             !
             if(ig1==1 .and. ig2 .ne. 1) then  
              !
              select case (trim(rimw_type))
              !
              case ("metal")
              !
              if (cut_is_slab) then
               if (iomega ==1) then
                func = real(f_coeff(2,1,ig2,iq,iomega))*abs(q_rlu(1)) + real(f_coeff(3,1,ig2,iq,iomega))*abs(q_rlu(2))   &
&                     +real(f_coeff(4,1,ig2,iq,iomega))*abs(q_rlu(3)) + aimag(f_coeff(2,1,ig2,iq,iomega))*q_rlu(1)       &                
&                     + aimag(f_coeff(3,1,ig2,iq,iomega))*q_rlu(2) + aimag(f_coeff(4,1,ig2,iq,iomega))*q_rlu(3)          &
&                     + f_coeff(5,1,ig2,iq,iomega)*(q_rlu(1))**2 + f_coeff(6,1,ig2,iq,iomega)*(q_rlu(2))**2              &
&                     + f_coeff(7,1,ig2,iq,iomega)*(q_rlu(3))**2 + f_coeff(8,1,ig2,iq,iomega)*sign(q_rlu(1)**2,q_rlu(1)) & 
&                     + f_coeff(9,1,ig2,iq,iomega)*sign(q_rlu(2)**2,q_rlu(2)) + f_coeff(10,1,ig2,iq,iomega)              &
&                     * sign(q_rlu(3)**2,q_rlu(3))
               else
                 if ((g_vec(ig2,idir(2))==g_vec(ig2,idir(3))) .and. (g_vec(ig2,idir(2))==0)) then
                func = ((f_coeff(2,1,ig2,iq,iomega)+f_coeff(5,1,ig2,iq,iomega)+ (f_coeff(2,1,ig2,iq,iomega)    & 
&                      -f_coeff(5,1,ig2,iq,iomega))*sign(1._SP,q_rlu(1)))*(q_rlu(1))**2   +                    & 
&                       (f_coeff(3,1,ig2,iq,iomega)+f_coeff(6,1,ig2,iq,iomega)+ (f_coeff(3,1,ig2,iq,iomega)    & 
&                      -f_coeff(6,1,ig2,iq,iomega))*sign(1._SP,q_rlu(2)))*(q_rlu(2))**2   +                    &  
&                       (f_coeff(4,1,ig2,iq,iomega)+f_coeff(7,1,ig2,iq,iomega)+ (f_coeff(4,1,ig2,iq,iomega)    & 
&                      -f_coeff(7,1,ig2,iq,iomega))*sign(1._SP,q_rlu(3)))*(q_rlu(3))**2   +                    &
&                       (f_coeff(8,1,ig2,iq,iomega)+f_coeff(11,1,ig2,iq,iomega)+ (f_coeff(8,1,ig2,iq,iomega)   & 
&                      -f_coeff(11,1,ig2,iq,iomega))*sign(1._SP,q_rlu(1)))*(q_rlu(1))**3   +                   & 
&                       (f_coeff(9,1,ig2,iq,iomega)+f_coeff(12,1,ig2,iq,iomega)+ (f_coeff(9,1,ig2,iq,iomega)   & 
&                      -f_coeff(12,1,ig2,iq,iomega))*sign(1._SP,q_rlu(2)))*(q_rlu(2))**3   +                   &  
&                       (f_coeff(10,1,ig2,iq,iomega)+f_coeff(13,1,ig2,iq,iomega)+ (f_coeff(10,1,ig2,iq,iomega) & 
&                      -f_coeff(13,1,ig2,iq,iomega))*sign(1._SP,q_rlu(3)))*(q_rlu(3))**3)/2.0                         
                  else 
                func = ((f_coeff(2,1,ig2,iq,iomega)+f_coeff(5,1,ig2,iq,iomega)+ (f_coeff(2,1,ig2,iq,iomega)    & 
&                      -f_coeff(5,1,ig2,iq,iomega))*sign(1._SP,q_rlu(1)))*(q_rlu(1))      +                    & 
&                       (f_coeff(3,1,ig2,iq,iomega)+f_coeff(6,1,ig2,iq,iomega)+ (f_coeff(3,1,ig2,iq,iomega)    & 
&                      -f_coeff(6,1,ig2,iq,iomega))*sign(1._SP,q_rlu(2)))*(q_rlu(2))      +                    &  
&                       (f_coeff(4,1,ig2,iq,iomega)+f_coeff(7,1,ig2,iq,iomega)+ (f_coeff(4,1,ig2,iq,iomega)    & 
&                      -f_coeff(7,1,ig2,iq,iomega))*sign(1._SP,q_rlu(3)))*(q_rlu(3))      +                    &
&                       (f_coeff(8,1,ig2,iq,iomega)+f_coeff(11,1,ig2,iq,iomega)+ (f_coeff(8,1,ig2,iq,iomega)   & 
&                      -f_coeff(11,1,ig2,iq,iomega))*sign(1._SP,q_rlu(1)))*(q_rlu(1))**2  +                    & 
&                       (f_coeff(9,1,ig2,iq,iomega)+f_coeff(12,1,ig2,iq,iomega)+ (f_coeff(9,1,ig2,iq,iomega)   & 
&                      -f_coeff(12,1,ig2,iq,iomega))*sign(1._SP,q_rlu(2)))*(q_rlu(2))**2  +                    &  
&                       (f_coeff(10,1,ig2,iq,iomega)+f_coeff(13,1,ig2,iq,iomega)+ (f_coeff(10,1,ig2,iq,iomega) & 
&                      -f_coeff(13,1,ig2,iq,iomega))*sign(1._SP,q_rlu(3)))*(q_rlu(3))**2)/2.0  
                  end if    
               end if                  
              end if
              !
              if (iku_v_norm(q_sampl) < q0_def_norm) then
               !
               if (cut_is_slab) then
                if (iomega ==1) then
                   ! System 2D, f func behaves as fcoeff*q
                   func = (real(f_coeff(2,1,ig2,iq,iomega))*abs(q_rlu(1)) + real(f_coeff(3,1,ig2,iq,iomega))*abs(q_rlu(2))  &
&                        + real(f_coeff(4,1,ig2,iq,iomega))*abs(q_rlu(3)) + aimag(f_coeff(2,1,ig2,iq,iomega))*q_rlu(1)      &                
&                        + aimag(f_coeff(3,1,ig2,iq,iomega))*q_rlu(2) + aimag(f_coeff(4,1,ig2,iq,iomega))*q_rlu(3))         &
&                         /iku_v_norm(q_sampl)
                   ! 
                   W_sampl = func/iku_v_norm(qpGc+q_sampl)**2*vslab**2/q0_def_norm
                   epsm1_sampl = func/iku_v_norm(qpGc+q_sampl)*vslab*sqrt(iku_v_norm(q_sampl)/q0_def_norm)
                   func=func*iku_v_norm(q_sampl)
                else
                 if ( (iomega .ne. 1) .and. (g_vec(ig2,idir(2))==g_vec(ig2,idir(3))) .and. (g_vec(ig2,idir(2))==0)) then
                   W_sampl = 0
                   epsm1_sampl = 0
                   func=0               
                 else
                   func = ((f_coeff(2,1,ig2,iq,iomega)-f_coeff(5,1,ig2,iq,iomega))*abs(q_rlu(1)) +  &
&                         +(f_coeff(3,1,ig2,iq,iomega)-f_coeff(6,1,ig2,iq,iomega))*abs(q_rlu(2)) +  &
&                         +(f_coeff(4,1,ig2,iq,iomega)-f_coeff(7,1,ig2,iq,iomega))*abs(q_rlu(3)))   &
                           /iku_v_norm(q_sampl)/2.0
                   W_sampl = func/iku_v_norm(qpGc+q_sampl)**2*vslab**2/q0_def_norm
                   epsm1_sampl = func/iku_v_norm(qpGc+q_sampl)*vslab*sqrt(iku_v_norm(q_sampl)/q0_def_norm)
                   func=func*iku_v_norm(q_sampl)  
                 end if
                end if
                !
               else 
                ! System 3D, the static f func behaves  as fcoeff*q^2
                ! Dinamically, f func has a linear term that however cancels out due to symmetry
                func = (q_sampl(1)*(q_sampl(1)*f_coeff_loc(5) + 2_SP*q_sampl(2)*f_coeff_loc(6)) &
&                       +q_sampl(2)*(q_sampl(2)*f_coeff_loc(8) + 2_SP*q_sampl(3)*f_coeff_loc(9)) &
&                       +q_sampl(3)*(q_sampl(3)*f_coeff_loc(10) + 2_SP*q_sampl(1)*f_coeff_loc(7)))/iku_v_norm(q_sampl)**2
                !
                W_sampl = func/iku_v_norm(qpGc+q_sampl)**2*vslab**2
                epsm1_sampl = func/iku_v_norm(qpGc+q_sampl)*vslab*iku_v_norm(q_sampl)
                func=func*iku_v_norm(q_sampl)**2
               end if
               !
               write(out_unit_W_fit,"(I5,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6)") &
&                           i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func),&
&                           aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
               cycle
              end if
              end select
             end if
           end if
          !
          epsm1_sampl = func*vslab/r1/(1._SP-vslab/r1*func)
          W_sampl= vslab/r1*func*vslab/r1/(1._SP-vslab/r1*func)
          if (ig1==ig2 .and. iomega==1) then
              select case (trim(rimw_type))
              !
              case ("metal")
               !
               epsm1_sampl = 1._SP/(1._SP-vslab/r1*func)
               W_sampl= vslab/r1/(1._SP-vslab/r1*func)
               !
              end select
           end if
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
           !
           !
           write(out_unit_W_fit,"(I5,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6,E15.6)") &
&                           i, q_out(1),q_out(2),q_out(3), q_out_norm, vslab/r1, real(func),&
&                           aimag(func),real(epsm1_sampl),aimag(epsm1_sampl),real(W_sampl),aimag(W_sampl)
         end do
         !
       end do
       !
       !Close output files
       !
       close(unit=out_unit_W_fit)
       close(unit=out_unit_W_num)
       !
     end do
   end do
 end do
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
 do i1=1,3 !i
   do i2=1,3 !j      
      do nn=11,19
         if (mod(nn-11,4)==0) then
           i3=(nn-11)/4+1
           f_func_loc(7+3*i1+i2) = f_func_loc(7+3*i1+i2) + f_func(nn)*symi(i3,i1)*symi(i3,i1)*symi(i3,i2)  
         else
           i4=mod(nn-11,3)+1
           i3=(nn-10-i4)/3+1
           f_func_loc(7+3*i1+i2) = f_func_loc(7+3*i1+i2) + f_func(nn)*(2.0*symi(i4,i1)*symi(i3,i1)*symi(i3,i2) &
&                                   + symi(i4,i2)*symi(i3,i1)*symi(i3,i1))
         end if
      end do
     do i3=0,5
        i4=(i3-mod(i3,2))/2+2+mod(i3,2)
        i5=i4+1-2*mod(i3,2)
        if (i4>3) i4=i4-3
        if (i5>3) i5=i5-3
        f_func_loc(7+3*i1+i2) = f_func_loc(7+3*i1+i2) + f_func(20)*symi((i3-mod(i3,2))/2+1,i1)*symi(i4,i1)*symi(i5,i2)  
     end do
   end do
 end do    
 !           
 do nn=11,19
    if (mod(nn-11,4)==0) then
      i3=(nn-11)/4+1
      f_func_loc(20) = f_func_loc(20) + f_func(nn)*symi(i3,1)*symi(i3,2)*symi(i3,3)  
    else
      i4=mod(nn-11,3)+1
      i3=(nn-10-i4)/3+1
      f_func_loc(20) = f_func_loc(20) + f_func(nn)*(symi(i3,1)*symi(i3,2)*symi(i4,3) + symi(i3,1)*symi(i4,2)*symi(i3,3) &
&                      + symi(i4,1)*symi(i3,1)*symi(i3,3))
    end if
 end do
 do i3=0,5
    i4=(i3-mod(i3,2))/2+2+mod(i3,2)
    i5=i4+1-2*mod(i3,2)
    if (i4>3) i4=i4-3
    if (i5>3) i5=i5-3   
    f_func_loc(20) = f_func_loc(20) + f_func(20)*symi((i3-mod(i3,2))/2+1,1)*symi(i4,2)*symi(i5,3)  
 end do
end subroutine








