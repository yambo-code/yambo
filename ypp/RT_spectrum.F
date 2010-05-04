!
! Copyright (C) 2000-2010 C. Attaccalite, A. Marini and the YAMBO team 
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
! Partialy inspired from Octopus v. 3.1.0
!
! Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
subroutine RT_spectrum(en,k)
 !
 use YPP
 use pars,           ONLY:SP,DP,pi
 use units,          ONLY:FS2AUT,HARTREE,SPEED_OF_LIGHT
 use electrons,      ONLY:levels
 use com,            ONLY:msg,of_open_close,warning
 use stderr,         ONLY:set_real_printed_length,real2ch
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp
 use parser_m,       ONLY:parser
 use timing,         ONLY:live_timing
 use IO_m,           ONLY:OP_RD,OP_RD_CL,io_control,NONE,DUMP
 use real_time,      ONLY:NE_steps,RT_step,j_current,i_time,Efield,  &
&                         max_rt_steps_disk,current_cache,A_vecpot,j_current_ext
 implicit none
 !
 type(levels)   :: en
 type(bz_samp) :: k
 !
 ! Work Space
 !
 integer  :: io_J,io_G,ID(2)
 integer  :: i_t,i_w,i_c,i1
 real(SP) :: const(3),real_j0(3)
 integer  :: i_t_start,i_t_end
 logical  :: Prt_Current,Prt_Einduced,Use_FFT,l_no_remove_DC,l_use_gauge_field
 real(SP)           :: W_RT_induced_step,W_step,Time_reached,W_reached
 complex(SP)        :: w_Aw,Integral
 real(SP),    allocatable :: damp_function(:)
 complex(DP),    allocatable :: current(:,:),current_ext_t(:,:),current_ext_w(:,:)
 complex(DP),    allocatable :: At_vecpot_vel(:,:),Aw_vecpot_vel(:,:)
 complex(DP), allocatable :: EELS(:,:),EPS(:,:)
 complex(DP),    allocatable :: E_induced(:,:)
 integer     :: fft_sign
 integer(8)  :: fftw_plan
 integer, external  :: io_SC_components
 !
 call msg("s",'Real-Time response')
 !=================================
 !
 ! CORE Logicals
 !---------------
 ! 
 call parser('PrtCurrent',Prt_Current)
 call parser('PrtEinduced',Prt_Einduced)
 call parser('UseFFT',Use_FFT)
 call parser('NoRmDC',l_no_remove_DC)
 call parser('UseGaugeField',l_use_gauge_field)
 ! 
 ! Check for saved current/G_lesser and vector potential
 !-----------------
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID(1)) ! Current response
 io_J=io_SC_components('J' ,en,ID(1))
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID(2)) 
 io_G=io_SC_components('G' ,en,ID(2))
 !
 if(io_J/=0) call warning(" Error while reading J database")
 if(io_G/=0) call warning(" Error while reading G lesser database")
 if(any((/io_G,io_J/)/=0)) return
 !
 ! Reduced Time range
 !====================
 !
 i_t_start=int(X_rt%time_range(1)/RT_step)
 i_t_end  =int(X_rt%time_range(2)/RT_step)
 !
 if(i_t_start<=0.0.or.i_t_start>NE_steps) i_t_start=1
 if(i_t_end<=0.0.or.i_t_end>NE_steps)     i_t_end  =NE_steps
 !
 W_RT_induced_step=2.*pi/(real(NE_steps,SP)*RT_step)  ! Frequency step in real-time dynamics
 !
 ! I/O of Current and Vector Potential
 !=====================================
 !
 allocate(current(NE_steps,3),current_ext_t(NE_steps,3),current_ext_w(NE_steps,3))
 call mem_est("Current",(/size(current)/),(/2*SP/))
 call mem_est("Current_ext_t",(/size(current_ext_t)/),(/2*SP/))
 call mem_est("Current_ext_w",(/size(current_ext_w)/),(/2*SP/))
 allocate(Aw_vecpot_vel(NE_steps,3),At_vecpot_vel(NE_steps,3))
 call mem_est("At_vecpot_vel",(/size(At_vecpot_vel)/),(/2*SP/))
 call mem_est("Aw_vecpot_vel",(/size(Aw_vecpot_vel)/),(/2*SP/))
 allocate(current_cache(18,max_rt_steps_disk))
 call mem_est("Current_Cache",(/size(current_cache)/),(/SP/))
 !
 current      =(0._SP,0._SP)
 current_ext_t=(0._SP,0._SP)
 current_ext_w=(0._SP,0._SP)
 Aw_vecpot_vel=(0._SP,0._SP)
 At_vecpot_vel=(0._SP,0._SP)
 !
 call live_timing('Current I/O:',NE_steps)
 do i_time=1,NE_steps
   !      
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/i_time+1/),ID=ID(1)) 
   io_J=io_SC_components('J' ,en,ID(1))
   !
   if(i_time>=i_t_start.and.i_time<=i_t_end) then
     current(i_time,:)       = j_current(:)
     current_ext_t(i_time,:) = j_current_ext(:)
     At_vecpot_vel(i_time,:) = A_vecpot%vecpot_vel(:)
   endif           
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 ! Print current and vector potential 
 !------------------------------------
 call OutPut_File("current")
 ! 
 ! DAMPING
 !=========
 !
 ! If there is a time-windows the damping function
 ! has be applied also the initial part otherwise 
 ! there will problems doing FFT for the non-zero starting point 
 !
 allocate(damp_function(NE_steps))
 call mem_est("Damp_Function",(/NE_steps/),(/SP/))
 !
 if(X_rt%damp_factor<2.*pi/(real(NE_steps,SP)*RT_step)) then
   X_rt%damp_factor=2.*pi/(real(NE_steps,SP)*RT_step)
   call warning('Damping factor redefined to be '//trim(real2ch(2.*pi/(real(NE_steps,SP)*RT_step)*HARTREE))//' eV')
 endif
 !
 if(trim(X_rt%damp)=="NONE") then
   damp_function(:) = 1.0
 elseif(trim(X_rt%damp)=="LORENTZIAN")  then
   forall(i_t=1:NE_steps) damp_function(i_t) = exp(-abs((i_t-1)*RT_step)*X_rt%damp_factor)
 elseif(trim(X_rt%damp)=="GAUSSIAN") then
   forall(i_t=1:NE_steps) damp_function(i_t) =  exp(-((i_t-1)*RT_step)**2*X_rt%damp_factor**2)
 else
   damp_function(:) = 1.0
   call warning(" Unknown DampMode. Switching to NONE")
 endif
 !
 ! Remove the dc component from current
 !--------------------------------------
 !
 if(.not.l_no_remove_dc) then
   !
   do i_c = 1, 3
     Integral = sum(At_vecpot_vel(:, i_c))
     At_vecpot_vel( :, i_c) = At_vecpot_vel(:,i_c) - Integral/real(NE_steps,SP)
   enddo
   !
   do i_c = 1, 3
     Integral = sum(current( :, i_c))
     current( :, i_c) = current(:,i_c) - Integral/real(NE_steps)
   enddo
   !
 endif
 ! 
 ! apply the damping
 !
 forall(i_t=1:NE_steps) current(i_t,:)      =current(i_t,:)*damp_function(i_t)
 forall(i_t=1:NE_steps) At_vecpot_vel(i_t,:)=At_vecpot_vel(i_t,:)*damp_function(i_t)
 !
 ! Induced Electric Field
 !------------------------
 !
 allocate(E_induced(NE_steps,3))
 call mem_est("E_induced",(/size(E_induced)/),(/2*SP/))
 !
 if(l_use_gauge_field) then
   E_induced=-At_vecpot_vel
 else
   ! 
   ! Integration with trapezoidal rule 
   !
   call live_timing('Integration:',NE_steps)
   do i_t=1, NE_steps
     E_induced(i_t,1)=-(2.d0*sum(current(:i_t,1))-current(1,1)-current(i_t,1))*RT_step/2.d0
     E_induced(i_t,2)=-(2.d0*sum(current(:i_t,2))-current(1,2)-current(i_t,2))*RT_step/2.d0
     E_induced(i_t,3)=-(2.d0*sum(current(:i_t,3))-current(1,3)-current(i_t,3))*RT_step/2.d0
    call live_timing(steps=1)
   enddo
   call live_timing()
   !
 endif
 !
 call OutPut_File("E_induced")
 !
 ! 1D FFT setup
 !--------------
 !
 if(Use_FFT) then
   !      
   call warning(" Energy steps and range redefined from real-time dynamics")
   !
   ! I want to print only the positive frequencies
   ! i_w > energy_steps/2 correspond to the negative ones
   !
   X_rt%energy_steps   =NE_steps/2
   X_rt%energy_range(1)=0.
   X_rt%energy_range(2)=W_RT_induced_step*NE_steps/2.
   !
   allocate(EELS(NE_steps,1:3))  
   allocate(EPS(NE_steps,1:3))  
   call mem_est("EELS EPS",(/size(EELS),size(EPS)/),(/2*SP,2*SP/))
   !
 else
   !
   allocate(EELS(X_rt%energy_steps,1:3))  
   allocate(EPS(X_rt%energy_steps,1:3))  
   call mem_est("EELS EPS",(/size(EELS),size(EPS)/),(/2*SP,2*SP/))
   !
 endif
 !
 ! Optical Porperties 
 !====================
 ! (it works only for constant vector potentials)
 !
 !
 EELS=(0._DP,0._DP)
 EPS =(0._DP,0._DP)
 !
 W_step=(X_rt%energy_range(2) - X_rt%energy_range(1))/X_rt%energy_steps
 !
 if(Use_FFT) then
   !
   fftw_plan=0
   fft_sign =-1
   !
   do i_c=1,3
     !
     EPS(:,i_c)          = current(:,i_c)
     Aw_vecpot_vel(:,i_c) = At_vecpot_vel(:,i_c)
     current_ext_w(:,i_c) = current_ext_t(:,i_c)
     !
#if defined _FFTW
    call fft_1d(current_ext_w(:,i_c),NE_steps,fft_sign,fftw_plan)
    call fft_1d(EPS(:,i_c),NE_steps,fft_sign,fftw_plan)
    call fft_1d(Aw_vecpot_vel(:,i_c),NE_steps,fft_sign,fftw_plan)
#else
    call fft_1d(current_ext_w(:,i_c),NE_steps,fft_sign)
    call fft_1d(EPS(:,i_c),NE_steps,fft_sign)
    call fft_1d(Aw_vecpot_vel(:,i_c),NE_steps,fft_sign)
#endif
     !
     ! due to the definition of FFT 
     !
     EPS(:,i_c)=conjg(EPS(:,i_c))*RT_step
     current_ext_w(:,i_c)=conjg(current_ext_w(:,i_c))*RT_step
     !
   enddo
   !
 else
   ! 
   ! Notice that in the integration should appear dt = RT_step but this cancel
   ! when one divides from Efield%amplitude that is the Fourier Transform of a delta function 
   !
   call live_timing('FFT:', X_rt%energy_steps)
   !
   do i_w = 1, X_rt%energy_steps
     W_reached = (i_w-1)*W_step+X_rt%energy_range(1)
     !
     do i_t = 1, NE_steps
       Time_reached = i_t*RT_step
       EPS(i_w,1:3)           = EPS(i_w,1:3)+current(i_t,1:3)*exp((0.,1.)*W_reached*Time_reached)*RT_step 
       Aw_vecpot_vel(i_w,1:3) = Aw_vecpot_vel(i_w,1:3)+At_vecpot_vel(i_t,1:3)*exp((0.,1.)*W_reached*Time_reached)*RT_step 
       current_ext_w(i_w,1:3) = current_ext_w(i_w,1:3)+current_ext_t(i_t,1:3)*exp((0.,1.)*W_reached*Time_reached)*RT_step 
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call live_timing()
   !
 endif
 !
 ! Remove residual constant term from the current
 !
 if(X_rt%energy_range(1)==0._SP) then
   !
   call msg('s', 'Remove residual constant term in the current')
   !
   i1=0
   real_j0(:)=0._SP
   !
   do i_w=1,X_rt%energy_steps
     W_reached = (i_w-1)*W_step+X_rt%energy_range(1)
     if(W_reached>=3._SP*X_rt%damp_factor.and.W_reached<=5._SP*X_rt%damp_factor) then
       i1=i1+1
       real_j0(:)=real_j0(:)+real(EPS(i_w,1:3))
     endif
   enddo
   !
   if(i1>0) real_j0(:)=real_j0(:)/real(i1)
   !
   const(:)=-(real(EPS(1,:))-real_j0(:))*X_rt%damp_factor
   !
 else
   call warning(" Cannot remove the residual constant term in the current set low limit energy range=0")
 endif
 !
 do i_w=1,X_rt%energy_steps
   W_reached = (i_w-1)*W_step+X_rt%energy_range(1)
   EPS(i_w,:)=EPS(i_w,:)-const(:)/((0._DP,1._DP)*W_reached-X_rt%damp_factor)
 enddo
 !
 EField(ref_efield)%amplitude=sqrt(Efield(ref_efield)%intensity*(8._SP*pi)/SPEED_OF_LIGHT)   
 !
 do i_w=1,X_rt%energy_steps
   !
   W_reached = (i_w-1)*W_step+X_rt%energy_range(1)
   !
   select case ( trim(Efield(ref_efield)%ef_name) )
     !
     case('DELTA')
       w_Aw=Efield(ref_efield)%amplitude
   end select
   !
   EPS(i_w,:) = EPS(i_w,:)/cmplx(X_rt%damp_factor,-W_reached)/w_Aw
   !
 enddo
 !
 EPS(:,:) = 1._DP+4._SP*pi*EPS(:,:)
 EELS(:,:)=-1._DP/EPS(:,:)  ! Minus sign to compare with yambo linear optics 
 !
 call OutPut_File("eps")
 call OutPut_File("eels")
 !
 deallocate(E_induced,EELS,current,damp_function,At_vecpot_vel,Aw_vecpot_vel, &
&           current_cache,current_ext_w,current_ext_t)
 call mem_est("Current_Cache")
 call mem_est("Current")
 call mem_est("Current_ext_w")
 call mem_est("Current_ext_t")
 call mem_est("Aw_vecpot_vel")
 call mem_est("At_vecpot_vel")
 call mem_est("EELS EPS")
 call mem_est("E_induced")
 call mem_est("Damp_Function")
 !
 contains
   !
   subroutine OutPut_File(what)
     !
     use SC,   ONLY:n_SC_descriptions,SC_description
     character(*) :: what
     !
     ! Work Space
     !
     real(SP)     :: output_data(10)
     integer      :: i_file
     integer      :: n_headings,i_d,i_s,steps_here
     character(lchlen),SAVE  :: file_name(4)
     character(schlen)       :: headings(10)
     !
     call set_real_printed_length(f_length=15,g_length=15)
     !
     if (trim(what)=="current".and.Prt_Current)  then
       i_file=1
       steps_here=NE_steps
       file_name(i_file)="YPP_current"
     else  if (trim(what)=="E_induced".and.Prt_Einduced)  then
       i_file=2
       steps_here=NE_steps
       file_name(i_file)="YPP_E_induced"
     else if (trim(what)=="eps") then
       i_file=3
       steps_here=X_rt%energy_steps
       file_name(i_file)="YPP_eps"
     else if (trim(what)=="eels")  then
       i_file=4
       steps_here=X_rt%energy_steps
       file_name(i_file)="YPP_eels"
     else
       return
     endif
     !
     call of_open_close(file_name(i_file),'ot')
     !   
     headings(1)="Time[fs]"
     !
     select case (trim(what))
       case("current")
         headings(2:4)=(/"A'_x","A'_y","A'_z"/)
         headings(5:7)=(/"j_x","j_y","j_z"/)
         headings(8:10)=(/"j_ext_x","j_ext_y","j_ext_z"/)
         n_headings=10
       case("E_induced")
         headings(2:4)=(/"E_x","E_y","E_z"/)
         n_headings=4
       case("eps")
         headings(1)  ='E/ev[1]'
         headings(2:3)=(/'eps_xx/Im[2]','eps_xx/Re[3]'/)
         headings(4:5)=(/'eps_yy/Im[4]','eps_yy/Re[5]'/)
         headings(6:7)=(/'eps_zz/Im[6]','eps_zz/Re[7]'/)
         n_headings=7
       case("eels")
         headings(1)  ='E/ev[1]'
         headings(2:3)=(/'eel_xx/Im[2]','eel_xx/Re[3]'/)
         headings(4:5)=(/'eel_yy/Im[4]','eel_yy/Re[5]'/)
         headings(6:7)=(/'eel_zz/Im[6]','eel_zz/Re[7]'/)
         n_headings=7
     end select
     !
     do i_d=1,n_SC_descriptions
       call msg('o '//trim(what),'#',trim(SC_description(i_d)),INDENT=0)
     enddo
     !
     call msg('o '//trim(what),'#',' ',INDENT=0)
     call msg('o '//trim(what),'#',headings(1:n_headings),INDENT=-2,USE_TABS=.TRUE.)
     call msg('o '//trim(what),'#',' ',INDENT=0)
     !
     do i_s=1,steps_here
       !
       output_data(1)=RT_step*i_s/FS2AUT
       !
       if (trim(what)=="current") output_data(2:n_headings)=&
&                (/real(At_vecpot_vel(i_s,1)),real(At_vecpot_vel(i_s,2)),real(At_vecpot_vel(i_s,3)), &
&                  real(current(i_s,1)),real(current(i_s,2)),real(current(i_s,3)), &
&                  real(current_ext_t(i_s,1)),real(current_ext_t(i_s,2)),real(current_ext_t(i_s,3))/)
       if (trim(what)=="E_induced") output_data(2:n_headings)=(/&
&                  real(E_induced(i_s,1),SP),real(E_induced(i_s,2),SP) , real(E_induced(i_s,3),SP)/)
       if (trim(what)=="eps") output_data(1:n_headings)=(/((i_s-1)*W_step+X_rt%energy_range(1))*HARTREE,&
&                  aimag(cmplx(EPS(i_s,1),kind=SP)),real(EPS(i_s,1),SP),&
&                  aimag(cmplx(EPS(i_s,2),kind=SP)),real(EPS(i_s,2),SP),&
&                  aimag(cmplx(EPS(i_s,3),kind=SP)),real(EPS(i_s,3),SP)/)
       if (trim(what)=="eels") output_data(1:n_headings)=(/((i_s-1)*W_step+X_rt%energy_range(1))*HARTREE,&
&                  aimag(cmplx(EELS(i_s,1),kind=SP)),real(EELS(i_s,1),SP),&
&                  aimag(cmplx(EELS(i_s,2),kind=SP)),real(EELS(i_s,2),SP),&
&                  aimag(cmplx(EELS(i_s,3),kind=SP)),real(EELS(i_s,3),SP)/)
       !
       call msg('o '//trim(what),'',output_data(1:n_headings),USE_TABS=.TRUE.)
     enddo
     !
     call of_open_close(file_name(i_file))
     !
     call set_real_printed_length()
     !
   end subroutine
   !
end subroutine
