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
subroutine RT_spectrum(en,k)
 !
 use YPP
 use pars,           ONLY:SP,DP,pi
 use units,          ONLY:FS2AUT,HARTREE,SPEED_OF_LIGHT
 use electrons,      ONLY:levels
 use com,            ONLY:msg,of_open_close,warning
 use stderr,         ONLY:set_real_printed_length
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp
 use parser_m,       ONLY:parser
 use timing,         ONLY:live_timing
 use IO_m,           ONLY:OP_RD,OP_RD_CL,io_control,NONE,DUMP
 use real_time,      ONLY:NE_steps,RT_step,j_current,i_time,Efield,  &
&                         max_rt_steps_disk,current_cache,n_ext_fields,&
&                         A_vecpot

 implicit none
 !
 type(levels)   :: en
 type(bz_samp) :: k
 !
 ! Work Space
 !
 integer  :: io_J,io_G,ID(2)
 integer  :: damp,i1,i2
 integer  :: niter,istart,iend,ishift
 logical  :: Prt_Current,Prt_Einduced,Use_FFT,l_no_remove_DC
 character(lchlen)  :: file_name(3),dump_ch
 character(schlen)  :: headings(7)
 real(SP)           :: values(8)
 real(SP) :: av,omega,tt,de,max_value
 real(SP),    allocatable :: damp_function(:)
 real(SP),    allocatable :: current(:,:)
 real(SP),    allocatable :: A_vecpot_vel(:,:)
 complex(DP), allocatable :: dielectric(:,:)
 real(DP),    allocatable :: E_induced(:,:)
 integer     :: fft_sign
 integer(8)  :: fftw_plan
 !
 ! Parameters
 !
 integer, parameter :: DAMP_NONE       = 0
 integer, parameter :: DAMP_LORENTZIAN = 1
 integer, parameter :: DAMP_GAUSSIAN   = 2
 !
 ! Work Space
 !
 integer, external  :: io_SC_components
 !
 call msg("s",'Real-Time response analizer')
 call parser('PrtCurrent',Prt_Current)
 call parser('PrtEinduced',Prt_EInduced)
 call parser('UseFFT',Use_FFT)
 call parser('NoRmDC',l_no_remove_DC)
 ! 
 ! Check damping mode
 !
 if(trim(X_rt%damp)=="NONE") then
   damp=DAMP_NONE
 elseif(trim(X_rt%damp)=="LORENTZIAN")  then
   damp=DAMP_LORENTZIAN
 elseif(trim(X_rt%damp)=="GAUSSIAN") then
   damp=DAMP_GAUSSIAN
 else
   damp=DAMP_NONE
   call warning(" Unknown DampMode. Switching to NONE")
 endif
 ! 
 ! Check for saved current and vector potential
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID(1)) ! Current response
 io_J=io_SC_components('J' ,en,ID(1))
 !
 if(io_J/=0) then
   call warning(" Error reading J database")
   return
 endif
 !
 ! Read last G_lesser to obtain Tot_steps
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID(2)) 
 io_G=io_SC_components('G' ,en,ID(2))
 !
 if(io_G/=0) then
   call warning(" Error reading G database")
   return
 endif
 !
 ! Fix Time limits
 !
 istart=int(X_rt%time_range(1)/RT_step)
 iend  =int(X_rt%time_range(2)/RT_step)
 !
 if(istart<=0.0.or.istart>NE_steps) istart=1
 if(iend<=0.0.or.iend>NE_steps)     iend  =NE_steps
 !
 niter=iend-istart+1
 !
 omega=2.*pi/(dble(niter)*RT_step)  ! Frequency step in real-time dynamics
 !
 ! Calculate damping function
 !
 allocate(damp_function(istart:iend))
 call mem_est("Damp_Function",(/niter/),(/SP/))
 !
 ! If there is a time-windows the damping function
 ! has be applied also the initial part otherwise 
 ! there will problems doing FFT for the non-zero starting point 
 !
 ishift=0
! if(istart/=1) ishift=int(real(niter)/2.0)
 !
 do i1=istart,iend
   i2=i1-istart-ishift
   select case(damp)
    case(DAMP_NONE)
      damp_function(i1) = 1.0
    case(DAMP_LORENTZIAN)
      damp_function(i1) = exp(-abs(i2*RT_step)*X_rt%damp_factor)
    case(DAMP_GAUSSIAN)
      damp_function(i1) = exp(-(i2*RT_step)**2*X_rt%damp_factor**2)
    end select
    !
 enddo
 !
 ! Read Current and Vector Potential
 !
 allocate(current(istart:iend,3))
 call mem_est("Current",(/size(current)/),(/SP/))
 allocate(A_vecpot_vel(3,istart:iend))
 call mem_est("A_vecpot_vel",(/size(A_vecpot_vel)/),(/SP/))
 allocate(current_cache(9,max_rt_steps_disk))
 call mem_est("Current_Cache",(/size(current_cache)/),(/SP/))
 !
 call live_timing('Reading current:',NE_steps)
 do i_time=1,NE_steps
   !      
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/i_time+1/),ID=ID(1)) 
   io_J=io_SC_components('J' ,en,ID(1))
   !
   if(i_time>=istart.and.i_time<=iend) then
     current(i_time,:)= j_current(:)
     A_vecpot_vel(:,i_time)     = real(A_vecpot%vecpot_vel(:))
   endif           
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 ! Print current and vector potential if required
 !
 call set_real_printed_length(f_length=15,g_length=15)
 !
 if(Prt_Current) then
   !
   file_name(1)="current"
   call of_open_close(file_name(1),'ot')
   headings(1)="Time[fs]"
   headings(2)="A'_x"
   headings(3)="A'_y"
   headings(4)="A'_z"
   headings(5)="j_x"
   headings(6)="j_y"
   headings(7)="j_z"
   call msg('o current','#',headings(1:7),INDENT=-2,USE_TABS=.TRUE.)
   do i1=istart,iend
     values(1:7)=(/RT_step*i1/FS2AUT,A_vecpot_vel(1,i1),A_vecpot_vel(2,i1),A_vecpot_vel(3,i1), &
&                 current(i1,1),current(i1,2),current(i1,3)/)
     call msg('o current','',values(1:7),USE_TABS=.TRUE.)
   enddo
   !
   call of_open_close(file_name(1))
   !
 endif
 !
 ! Remove the dc component from current
 !
 if(.not.l_no_remove_dc) then
   !
   do i1 = 1, 3
     av = sum(current( istart:iend, i1))
     current( istart:iend, i1) = current(istart:iend ,i1) - av/dble(niter)
   enddo
   !
 endif
 !
 ! Calculate the induced Electric Field
 !
 allocate(E_induced(istart:iend,3))
 call mem_est("E_induced",(/size(E_induced)/),(/SP/))
 !
 ! Integration with trapezoidal rule 
 !
 call live_timing('Integration:',iend-istart+1)
 do i1=istart, iend
   E_induced(i1,1)=-(2.d0*sum(current(istart:i1,1))-current(istart,1)-current(i1,1))*RT_step/2.d0
   E_induced(i1,2)=-(2.d0*sum(current(istart:i1,2))-current(istart,2)-current(i1,2))*RT_step/2.d0
   E_induced(i1,3)=-(2.d0*sum(current(istart:i1,3))-current(istart,3)-current(i1,3))*RT_step/2.d0
   call live_timing(steps=1)
 enddo
 call live_timing()
 !
 E_induced=E_induced/SPEED_OF_LIGHT
 !  
 ! Apply the damping function
 !
 E_induced(:,1)=E_induced(:,1)*damp_function(:)
 E_induced(:,2)=E_induced(:,2)*damp_function(:)
 E_induced(:,3)=E_induced(:,3)*damp_function(:)
 !
 if(Prt_Einduced) then
   !
   call set_real_printed_length(f_length=15,g_length=15)
   !
   file_name(2)="einduced"
   call of_open_close(file_name(2),'ot')
   headings(1)="Time[fs]"
   headings(2)="E_x"
   headings(3)="E_y"
   headings(4)="E_z"
   call msg('o einduced','#',headings(1:4),INDENT=-2,USE_TABS=.TRUE.)
   do i1=istart,iend
     values(1:4)=(/RT_step*real(i1)/FS2AUT, &
&    real(E_induced(i1,1),SP),real(E_induced(i1,2),SP) , real(E_induced(i1,3),SP)/)
     call msg('o einduced','',values(1:4),USE_TABS=.TRUE.)
   enddo
   !
   call set_real_printed_length()
   call of_open_close(file_name(2))
   !
 endif
 !
 if(Use_FFT) then
   !      
   call warning(" Energy steps and range set from real-time dynamics")
   !
   X_rt%energy_steps   =niter/2+1
   X_rt%energy_range(1)=0.
   X_rt%energy_range(2)=omega*niter/2.
   !
 endif
 !
 de=(X_rt%energy_range(2) - X_rt%energy_range(1))/X_rt%energy_steps
 !
 ! Calculate Dielectric constant
 ! it works only for constant vector potentials
 !
 allocate(dielectric(X_rt%energy_steps,1:3))  
 call mem_est("Dielectric",(/size(Dielectric)/),(/2*SP/))
 !
 dielectric=(0.,0.)
 !
 if(Use_FFT) then
  !
  fftw_plan=0
  fft_sign =-1
  !
  do i1=1,3
    !
#if defined _FFTW
   call fft_1d_real(E_induced(istart:iend,i1),dielectric(:,i1),niter,fft_sign,fftw_plan)
#else
   call fft_1d_real(E_induced(istart:iend,i1),dielectric(:,i1),niter,fft_sign)
#endif
    !
    ! due to the definition of FFT 
    !
    dielectric(:,i1)=conjg(dielectric(:,i1))
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
  do i1 = 1, X_rt%energy_steps
    omega = (i1-1)*de+X_rt%energy_range(1)
    !
    do i2 = istart, iend
      tt = (i2-istart+1)*RT_step
      dielectric(i1,1:3) = dielectric(i1,1:3) &
&                        + E_induced(i2,1:3)*exp((0.,1.)*omega*tt) 
    enddo
    !
    call live_timing(steps=1)
  enddo
  !
  call live_timing()
  !
 endif
 !
 if(X_rt%damp_factor<2.*pi/(dble(niter-ishift)*RT_step)) then
   write(dump_ch,*) "You should use a damping factor greater than :", &
&                    2.*pi/(dble(niter-ishift)*RT_step)*HARTREE 
   call warning(dump_ch)
 endif
 !
 select case(trim(Efield(ref_efield)%ef_name))
   case('DELTA')
     dielectric(:,:) = dielectric(:,:)/(-Efield(ref_efield)%amplitude)
   case('RESONANT')
     dielectric(:,:) = dielectric(:,:)/(-Efield(ref_efield)%amplitude)/(-2._SP)
   case('ANTIRESONANT')
     dielectric(:,:) = dielectric(:,:)/(-Efield(ref_efield)%amplitude)/2._SP
   case('EXPSTEP')
     do i1=1,X_rt%energy_steps
       omega = (i1-1)*de+X_rt%energy_range(1)
       dielectric(i1,:) = -dielectric(i1,:)*(Efield(ref_efield)%frequency+(0._SP,1._SP)*omega)
     enddo
     call warning(" Only epsilon(w = Efield%frequency) is correct ")
   case default
     call warning(" External Field "//trim(Efield(ref_efield)%ef_name)//" not implemented in ypp_rt ")
 end select
 !
 dielectric(:,:)=dielectric(:,:)+1._DP
 !
 ! Write dielectric constant 
 !
 file_name(3)='eps_q001_rt'
 call of_open_close(file_name(3),'ot')
 !
 omega=2.*pi/(dble(niter)*RT_step)  ! Frequency step in real-time dynamics
 !
 write(dump_ch,*) " Max Frequency Range: ",omega*HARTREE," - ",omega*dble((niter-ishift)/2.)*HARTREE," eV "
 call msg('o eps_q001_rt','#',dump_ch,INDENT=0,USE_TABS=.true.)
 call msg('o eps_q001_rt','#','',INDENT=0,USE_TABS=.true.)
 write(dump_ch,*) " Max Time  Range: ",0," - ",NE_steps/FS2AUT," fs "
 call msg('o eps_q001_rt','#',dump_ch,INDENT=0,USE_TABS=.true.)
 call msg('o eps_q001_rt','#','',INDENT=0,USE_TABS=.true.)
 !
 headings(1)  ='E/ev[1]'
 headings(2:3)=(/'e_x/Im[2]','e_x/Re[3]'/)
 headings(4:5)=(/'e_y/Im[4]','e_y/Re[5]'/)
 headings(6:7)=(/'e_z/Im[6]','e_z/Re[7]'/)
 !
 call msg('o eps_q001_rt','#',headings(1:7),INDENT=0,USE_TABS=.true.)
 !
 do i1=1,X_rt%energy_steps
   values(1)= ((i1-1)*de+X_rt%energy_range(1))*HARTREE
   !
   do i2=1,3
     values(2*i2)       = aimag(dielectric(i1,i2))
     values(2*i2+1)     =  real(dielectric(i1,i2))
   enddo
   !
   call msg('o eps_q001_rt','',values(1:7),INDENT=-2,USE_TABS=.TRUE.)
   !
 enddo
 !
 call of_open_close(file_name(3))
 ! 
 call set_real_printed_length()
 !
 deallocate(E_induced,dielectric,current,damp_function,A_vecpot_vel,current_cache)
 call mem_est("Current_Cache")
 call mem_est("Current")
 call mem_est("A_vecpot_vel")
 call mem_est("Dielectric")
 call mem_est("E_induced")
 call mem_est("Damp_Function")
 !
end subroutine
