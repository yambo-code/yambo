!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_polarization(E,k,FL_V_bands,FL_Chi_pol)
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE,cI
 use units,          ONLY:HA2FSm1,HA2EV
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use fields,         ONLY:Efield,Divide_by_Field
 use D_lattice,      ONLY:a,DL_vol
 use R_lattice,      ONLY:bz_samp,k_map,bz_map,nXkbz
 use vec_operate,    ONLY:get_id_perpendicular
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:NL_bands,NL_initial_P,NL_damping
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         FL_states,FL_Ho_plus_Sigma,FL_time_S_det,FL_Chi_order,&
&                         max_time_step,n_tot_time_steps
 use parallel_m,     ONLY:PAR_IND_Xk_bz,PAR_COM_Xk_ibz_INDEX
 use parallel_int,   ONLY:PP_redux_wait
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 type(bz_samp), intent(in)  :: k
 complex(SP),   intent(in)  :: FL_V_bands(NL_bands(2),n_tot_fl_modes,maxval(E%nbf),QP_nk,n_sp_pol)
 complex(DP),   intent(inout) :: FL_Chi_pol(3,FL_Chi_order+1)  ! In cartesian coordinates
 !
 ! Work Space
 !
 complex(DP)  :: FL_time_P_Berry_RED(3,n_tot_time_steps)  ! In reduced coordinates
 complex(DP)  :: FL_time_P_Berry_CART(3,n_tot_time_steps) ! In cartesian coordinates
 complex(DP)  :: FL_Pi_pol(3,n_tot_time_steps)
 integer      :: ik_bz,i_sp,i_t,i_order,i1_fl
 integer      :: id_in,id2,id3,Nperpend,i1,i2,i3,idx(3),ikbz,id
 complex(DP)  :: zeta
 real(DP)     :: imag_zeta
 real(SP)     :: time,w0_sm1
 character(4) :: filename
 integer,allocatable :: phase_correction(:,:)
 !
 ! Update Floquet Overlaps and determinant
 ! ===================================================================
 !
#if defined _TIMING
 call timing('FL Berry Pol NEQ',OPR='start')
#endif
 !
 FL_S_minus=cZERO
 FL_S_plus =cZERO
 FL_time_S_det =cZERO
 !
 do i_sp=1,n_sp_pol
   !
   do ik_bz=1,nXkbz
     !
     if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
     !
     call FL_overlaps(E,k,i_sp,ik_bz,FL_V_bands(:,:,:E%nbf(i_sp),:,i_sp), &
                                  &  FL_S_plus(:E%nbf(i_sp),:E%nbf(i_sp),:,:,ik_bz,i_sp),&
                                  &  FL_S_minus(:E%nbf(i_sp),:E%nbf(i_sp),:,:,ik_bz,i_sp),&
                                  &  FL_time_S_det(:,:,ik_bz,i_sp),.true.)
     !
   enddo
   !
   call PP_redux_wait(FL_S_plus(:,:,:,:,:,i_sp) , COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   call PP_redux_wait(FL_S_minus(:,:,:,:,:,i_sp), COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   call PP_redux_wait(FL_time_S_det(:,:,:,i_sp) , COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   !
 enddo
 !
 FL_time_P_Berry_RED  = cZERO
 FL_time_P_Berry_CART = cZERO
 !
 do id_in=1,3
   !
   call get_id_perpendicular(id_in,id2,id3)
   !
   Nperpend=k_map%max_kdir(id2)*k_map%max_kdir(id3)
   !
   allocate(phase_correction(k_map%max_kdir(id2),k_map%max_kdir(id3)))
   do i_t=1,n_tot_time_steps
     !
     do i2=1,k_map%max_kdir(id2)
       do i3=1,k_map%max_kdir(id3)
         !
         zeta=cONE
         !
         idx(id2)=i2
         idx(id3)=i3
         !
         ! Loop on the string
         !
         do i1=1,k_map%max_kdir(id_in)
           !
           idx(id_in)=i1
           ikbz=k_map%k_map_dir(idx(1),idx(2),idx(3))
           zeta=zeta*FL_time_S_det(i_t,id_in,ikbz,1)
           !
         enddo
         !
         ! Map phases between -pi and pi for all k_perpendicular
         ! Do we miss a factor 2?
         !
         imag_zeta=aimag(log(zeta))
         if (i_t == 1) phase_correction(i2,i3) = nint(imag_zeta/(1._DP*pi))
         imag_zeta=imag_zeta-1._DP*pi*phase_correction(i2,i3)
         !
         FL_time_P_Berry_RED(id_in,i_t)=FL_time_P_Berry_RED(id_in,i_t)+imag_zeta
         !
       enddo
     enddo
     !
     FL_time_P_Berry_RED(id_in,i_t)=-FL_time_P_Berry_RED(id_in,i_t)/real(Nperpend,SP)
     !
   enddo
   !
   deallocate(phase_correction)
   !
 enddo
 !
 !
 do id=1,3
   !
   do i_t=1,n_tot_time_steps
     !
     FL_time_P_Berry_CART(id,i_t)=sum(FL_time_P_Berry_RED(:,i_t)&
             &                   *a(:,id))/(DL_vol*2._SP*pi)*spin_occ&
             &                   -NL_initial_P(id)
     !
   enddo
   !
 enddo
!aim137 DEBUG <
!print*, ''
!print*, FL_time_P_Berry_RED(1,:)
!print*, FL_time_P_Berry_RED(2,:)
!print*, FL_time_P_Berry_RED(3,:)
!print*, 'id = 1, a= ',a(1,:)
!print*, 'id = 2, a= ',a(2,:)
!print*, 'id = 3, a= ',a(3,:)
!call error ('><><><><><><><><><><><><><>< testing code up to this point ><><><><><><><><><><><><><><')
!aim137 DEBUG >
 !
 !aim137 DEBUG <
 !print*, 'NL_initial_P:'
 !print*, real(NL_initial_P(:))
 !print*, 'FL_time_P_Berry_CART at t=0'
 !print*, real(FL_time_P_Berry_CART(:,1))
 !or for 1st order
 !print*, dot_product(real(FL_time_P_Berry_CART(:,1)),Efield(1)%versor(:))
 !aim137 DEBUG >
 !
 ! Damping
 ! aim137: there are no physical grounds for this damping
 ! aim137: hence, it is commented out
 !call FL_damping("LORENTZIAN",NL_damping,FL_time_P_Berry_CART)
 !
 !
 FL_Chi_pol = cZERO
 FL_Pi_pol = cZERO
 !
 do id=1,3
   !
   call FL_FT_polarization(FL_time_P_Berry_CART(id,:),FL_Pi_pol(id,:))
   !
 enddo
 !
 do i_order=0,FL_Chi_order
   !
   i1_fl = i_order + (max_time_step+1)
   !
   if (i_order == 1) then
     !
     FL_Chi_pol(1,i_order+1) = 4._SP*pi*dot_product(Efield(1)%versor(:),FL_Pi_pol(:,i1_fl)) &
                                           &      * Divide_by_Field(Efield(1),i_order)
     !
   else
     !
     FL_Chi_pol(:,i_order+1) = FL_Pi_pol(:,i1_fl) * Divide_by_Field(Efield(1),i_order)
     !
   endif
   !
 enddo
 !
#if defined _TIMING
 call timing('FL Berry Pol NEQ',OPR='stop')
#endif
 !
 !aim137 DEBUG <
 !write(filename,'(f4.2)') Efield(1)%frequency(1)*HA2EV
 !w0_sm1 = Efield(1)%frequency(1)*HA2FSm1
 !open(111,file='FL_time_Pol_at_w_'//trim(adjustl(filename))//'.dat')
 !write(111,*) 'Frequency[eV] = ',Efield(1)%frequency(1)*HA2EV
 !write(111,*) 't_step  time   Pol_x   Pol_y'
 !do i_t=1,n_tot_time_steps
 !time = 1/(w0_sm1) * (i_t-1)/n_tot_time_steps ! when i_t=1, time = 0
 !write(111,*) i_t, time, real(FL_time_P_Berry_CART(1,i_t)),real(FL_time_P_Berry_CART(2,i_t)),real(FL_time_P_Berry_CART(3,i_t))
 !enddo
 !close(111)
 !aim137 DEBUG >
end subroutine
!
!
subroutine FL_FT_polarization(FL_time_pol,FL_floquet_pol)
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE,cI
 use units,          ONLY:HA2FSm1
 use fields,         ONLY:Efield,EtoT
 use wrapper,        ONLY:M_by_V
 use interfaces,      ONLY:LINEAR_ALGEBRA_driver
 use linear_algebra,  ONLY:INV
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         FL_states,FL_Ho_plus_Sigma,FL_time_S_det,n_tot_time_steps,max_time_step
 !
 implicit none
 !
 complex(DP), intent(inout) :: FL_time_pol(n_tot_time_steps)
 complex(DP), intent(inout) :: FL_floquet_pol(n_tot_time_steps)
 !
 ! Work Space
 !
 complex(DP) :: FL_FT_Mat(n_tot_time_steps,n_tot_time_steps)
 complex(DP) :: tmp_P_t
 complex(SP) :: dummy_det
 integer     :: i_t,i1_fl,i1_shifted_fl
 real(SP)    :: time,w0_sm1
 !
 ! Build Matrix for Fourier Transform
 ! ===================================================================
 !
 FL_FT_Mat = cZERO
 FL_floquet_pol = cZERO
 !
 do i_t=1,n_tot_time_steps
   !
   !aim137 this is not needed
   !w0_sm1 = Efield(1)%frequency(1)*HA2FSm1
   !time = 1._SP/(w0_sm1) * (i_t-1._SP)/n_tot_time_steps ! when i_t=1, time = 0
   !print*, time, EtoT(E=Efield(1)%frequency(1))
   !
   do i1_fl=1,n_tot_time_steps
     !
     i1_shifted_fl = i1_fl -(max_time_step+1)
     !
     FL_FT_Mat(i_t,i1_fl) =  exp(-1._SP*cI*real(i1_shifted_fl,SP)*2._SP*pi*(i_t-1._SP)/n_tot_time_steps)
     !aim137 prelim - move the generation of the FT matrix to the module
     !
   enddo
   !
 enddo
 !
 ! Invert Matrix for Fourier Transform
 ! ===================================================================
 !
 !call SERIAL_inversion(n_tot_time_steps,FL_FT_Mat,dummy_det,.false.)
 call LINEAR_ALGEBRA_driver(INV,M=FL_FT_Mat)
 !
 ! Calculate FL_floquet_pol
 ! ===================================================================
 !
 !aim137 DEGUB - I change this for ypp's approach
 !call M_by_V('N',n_tot_time_steps,n_tot_time_steps,cONE,FL_FT_Mat,&
 !       &   n_tot_time_steps,FL_time_pol,1,cONE,FL_floquet_pol,1)
 !aim137 I imagine BETA should be cZERO - check
 !
 !aim137 DEBUG - ypp's approach
  do i1_fl=(max_time_step+1),n_tot_time_steps
    do i_t=1,n_tot_time_steps
    !
    FL_floquet_pol(i1_fl)=FL_floquet_pol(i1_fl)+&
            &             FL_FT_Mat(i1_fl,i_t)*FL_time_pol(i_t)
    !
    enddo
  enddo
 !
 !
!!aim137 DEBUG
!do i_t=1,n_tot_fl_modes
!  tmp_P_t = cZERO
!  do i1_fl=1,n_tot_fl_modes
!    i1_shifted_fl = i1_fl -(max_fl_mode+1)
!    tmp_P_t = tmp_P_t + exp(-1*cI*i1_shifted_fl*2*pi*(i_t-1)/n_tot_fl_modes)&
!                    & * FL_floquet_pol(i1_fl)
!  enddo
!  write(*,'(2(1f22.17))') real(FL_time_pol(i_t)),aimag(FL_time_pol(i_t))
!  write(*,'(2(1f22.17))') real(FL_time_pol(i_t) - tmp_P_t),aimag(FL_time_pol(i_t) - tmp_P_t)
!enddo
!!aim137 DEBUG - works ok
end subroutine FL_FT_polarization
!
subroutine FL_damping(damp_type,damp_factor,ft)
 !
 use pars,           ONLY:SP,DP
 use fields,         ONLY:Efield,EtoT
 use fl_optics,      ONLY:n_tot_time_steps
 !
 implicit none
 !
 real(SP),     intent(in)      :: damp_factor
 complex(DP),  intent(inout)   :: ft(3,n_tot_time_steps)
 character(*), intent(in)      :: damp_type
 !
 ! Work Space
 !
 integer  :: i_t
 real(SP) :: damp_function(n_tot_time_steps)
 real(SP) :: w0,T_period,t_samples(n_tot_time_steps)
 !
 w0 = Efield(1)%frequency(1) ! in Ha
 T_period = EtoT(E=w0)
 forall(i_t=1:n_tot_time_steps) t_samples(i_t)=T_period*(i_t-1._SP)/n_tot_time_steps
 !
 select case ( trim(damp_type) )
   case("LORENTZIAN")
     forall(i_t=1:n_tot_time_steps) damp_function(i_t) = exp(-abs(t_samples(i_t)*damp_factor))
   case("GAUSSIAN")
     forall(i_t=1:n_tot_time_steps) damp_function(i_t) = exp(-t_samples(i_t)**2*damp_factor**2)
   case("NONE")
     forall(i_t=1:n_tot_time_steps) damp_function(i_t) = 1._SP
   case default
     call error(" Unknown damping! ")
     return
 end select
 !
 forall(i_t=1:n_tot_time_steps)  ft(:,i_t)=ft(:,i_t)*damp_function(i_t)
 !
end subroutine FL_damping
