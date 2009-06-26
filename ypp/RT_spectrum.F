!
! Copyright (C) 2000-2008 C. Attaccalite, A. Marini and the YAMBO team 
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
! Partialy copied from Octopus v. 3.1.0
!
subroutine RT_spectrum(en,k)
 !
 use YPP
 use electrons,      ONLY:levels
 use com,            ONLY:error
 use memory_m,       ONLY:mem_est
 use R_lattice,      ONLY:bz_samp
 use IO_m,           ONLY:OP_RD,OP_RD_CL,RD_CL_IF_END,io_control,NONE
 use real_time,      ONLY:Tot_steps,RT_step,j_current,A_norm,Ef_q0
 implicit none
 !
 type(levels)   :: en
 type(bz_samp) :: k
 !
 ! Work Space
 !
 integer  :: io_J,ID
 integer  :: damp,i1,i2
 integer  :: niter,istart,iend
 real(SP) :: dt,av
 real(SP), allocatable :: damp_function(:)
 real(SP), allocatable :: current(:,:)
 complex(SP), allocatable :: Avec(:,:)
 !
 ! Parameters
 !
 integer, parameter :: DAMP_NONE       = 0  
 integer, parameter :: DAMP_LORENTZIAN = 1
 integer, parameter :: DAMP_GAUSSIAN   = 2    
 integer, external  :: io_SC_components
 !
 ! 
 ! Check damping mode
 !
 if(trim(X_rt%damp)=="NONE") then
   damp=0
 elseif(trim(X_rt%damp)=="LORENTZIAN")  then
   damp=1
 elseif(trim(X_rt%damp)=="GAUSSIAN") then
   damp=2
 else
   call error(" Unknown DampMode chose ( NONE | LORENTZIAN | GAUSSIAN ) ")
 endif
 !
 ! Check for saved current and vector potential
 !
 call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID) ! Current response
 io_J=io_SC_components('J' ,en,ID)
 !
 ! Fix Time limits
 !
 istart=int(X_rt%time_range(1)/RT_step)
 iend  =int(X_rt%time_range(2)/RT_step)
 !
 if(istart<=0.0.or.istart>Tot_steps) istart=1
 if(iend<=0.0.or.iend>Tot_steps)     iend  =Tot_steps
 !
 niter=iend-istart+1
 !
 ! Calculate damping function
 !
 allocate(damp_function(istart:iend))
 call mem_est("Damp_Function",(/niter/),(/SP/))
 !
 do i1=istart,iend
   i2=i1-istart
   select case(damp)
    case(DAMP_NONE)
      damp_function(i1) = 1.0
    case(DAMP_LORENTZIAN)
      damp_function(i1)= exp(-i2*dt*X_rt%damp_factor)
    case(DAMP_GAUSSIAN)
      damp_function(i1)= exp(-(i2*dt)**2*X_rt%damp_factor**2)
    end select
 enddo
 !
 ! Read Current and Vector Potential
 !
 allocate(current(3,istart:iend),Avec(3,istart:iend))
 !
 do i1=1,Tot_steps
   !      
   call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/i1+1/),ID=ID) ! Current response
   io_J=io_SC_components('J' ,en,ID)
   if(i1>=istart.and.i1<=iend) then
     current(:,i1)=j_current(:)
     Avec(:,i1)   =Ef_q0(:)*A_norm
   endif
   !
 enddo
 !
 ! Remove the dc component
 !
 do i1 = 1, 3
  av = sum(current(i1, istart:iend))
  current(i1, istart:iend) = current(i1, istart:iend) - av/dble(niter)
 end do
 !
end subroutine
