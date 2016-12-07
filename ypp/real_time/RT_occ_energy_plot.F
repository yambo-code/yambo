! 
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_occ_energy_plot(En,kpt,n_T_steps)
 ! 
 ! FIT the occupations with a Gauss–Newton algorithm
 !
 use units,          ONLY:HA2EV,HA2KEL,AUT2FS
 use LIVE_t,         ONLY:live_timing
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt,l_elphoton_scatt
 use pars,           ONLY:SP,schlen,rZERO
 use YPP_real_time,  ONLY:RT_time
 use RT_control,     ONLY:RT_apply
 use D_lattice,      ONLY:Bose_Temp
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,spin_occ,filled_tresh,n_spinor
 use vec_operate,    ONLY:sort
 use functions,      ONLY:Fermi_fnc,Fermi_fnc_derivative,Fermi_fnc_fit
 use com,            ONLY:msg,of_open_close,error
 use stderr,         ONLY:intc,real2ch
 use real_time,      ONLY:RT_carriers,l_RT_CCA_Kernel,RT_bands
 !
 implicit none
 !
#include <memory.h>
 !
 type(bz_samp) :: kpt
 type(levels)  :: En
 integer       :: n_T_steps
 !
 ! Work Space
 !
 integer, parameter    :: fit_dim=1000,N_max_files=16
 integer               :: n_groups=1,i_g,i_g_ep=0,i_g_ee=0,i_T,i_RT,ib,ik,i_f,i_fit
 integer               :: E_sort_index(RT_carriers%nstates)
 logical               :: l_ep_lifetimes
 real(SP)              :: RT_E_sorted(RT_carriers%nstates)
 real(SP),allocatable  :: spin_factors(:,:,:)
 !
 !...output (FIT)
 integer           ::n_titles
 character(10)     ::titles(max(n_T_steps+4,11)),ch_dummy,data_ch(n_T_steps+4,5,2),columns(n_T_steps+4)
 character(schlen) ::Ef_and_T_fname(2)
!,gamma_fname(8),gamma_ratio_fname(2),GRP_string
 real(SP)          ::o_data(20)
 !
 ! Local group Load&Fit type
 !---------------------------
 type Load_and_Fit
   !...main
   character(schlen) :: kind="none"
   real(SP), allocatable :: data(:,:,:)
   !...output
   integer           :: n_files=0
   character(schlen) :: file(6)
   character(schlen) :: running_file
   character(schlen) :: file_desc(6)
   !...FIT
   integer           :: n_fit_ok(2)=0
   logical           :: fittable=.FALSE.
   character(10)     :: FIT_title(2)
   real(SP), allocatable :: data_fit(:,:,:)
   real(SP), allocatable :: E_fit(:,:)
   real(SP), allocatable :: err_fit(:,:)
   real(SP), allocatable :: T(:,:)
   real(SP), allocatable :: Ef(:,:)
 end type Load_and_Fit
 !
 type(Load_and_Fit)    :: DATA_grp(5) ! e-e, e-p, e-gamma, occ, spin_occ
 !
 ! Energies
 !==========
 RT_E_sorted=RT_carriers%E_bare*HA2EV
 call sort(RT_E_sorted,indx=E_sort_index)
 filled_tresh=1.E-7
 !
 ! Allocate GROUPs!
 !==================
 !
 call Load_and_Fit_alloc(DATA_grp(1),1,"CARRIERS")
 !
 if (n_spinor==2) then
   call Load_and_Fit_alloc(DATA_grp(2),1,"spin-up")
   call Load_and_Fit_alloc(DATA_grp(3),1,"spin-dn")
   allocate(spin_factors(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_spinor))
   call electrons_spin_factors((/1,kpt%nibz/),RT_carriers%nb,spin_factors)
   n_groups=3
 endif
 !
 if (l_elel_scatt) then
   n_groups=n_groups+1
   call Load_and_Fit_alloc(DATA_grp(n_groups),2,"e-e")
   i_g_ee=n_groups
   n_groups=n_groups+1
   call Load_and_Fit_alloc(DATA_grp(n_groups),1,"e-e-ratio")
 endif
 if (l_elphoton_scatt) then
   n_groups=n_groups+1
   call Load_and_Fit_alloc(DATA_grp(n_groups),2,"e-photon")
 endif
 !
 l_ep_lifetimes = l_elph_scatt.and.l_RT_CCA_Kernel
 if (l_ep_lifetimes) then
   n_groups=n_groups+1
   call Load_and_Fit_alloc(DATA_grp(n_groups),2,"e-p")
   i_g_ep=n_groups
   n_groups=n_groups+1
   call Load_and_Fit_alloc(DATA_grp(n_groups),1,"e-p-ratio")
 endif
 !
 ! Load the GROUPs!
 !==================
 call live_timing('Data loading',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   ! Part 1: IO and store history
   !==============================
   call RT_apply(RT_bands,En,kpt,Time=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   ! Fill the large arrays to be used for the fitting procedure.
   !
   do i_RT=1,RT_carriers%nstates
     !
     ! Occupations
     !============
     DATA_grp(1)%data(i_RT,1,i_T)=RT_carriers%delta_f(i_RT)
     !
     ! Spin occupations
     !==================
     if (n_spinor==2) then
       ib=RT_carriers%table(i_RT,1)
       ik=RT_carriers%table(i_RT,3)
       DATA_grp(2)%data(i_RT,1,i_T)=RT_carriers%delta_f(i_RT)*spin_factors(ib,ik,1)
       DATA_grp(3)%data(i_RT,1,i_T)=RT_carriers%delta_f(i_RT)*spin_factors(ib,ik,2)
     endif
     !
     ! Lifetimes
     !===========
     !
     do i_g=1,n_groups
       select case (DATA_grp(i_g)%kind)
         case("e-e")
           DATA_grp(i_g)%data(i_RT,1,i_T)=RT_carriers%dGAMMA_E_E_Hole(i_RT)    +RT_carriers%GAMMA_bare(i_RT,1)
           DATA_grp(i_g)%data(i_RT,2,i_T)=RT_carriers%dGAMMA_E_E_Electron(i_RT)+RT_carriers%GAMMA_bare(i_RT,2)
         case("e-p")
           DATA_grp(i_g)%data(i_RT,1,i_T)=RT_carriers%dGAMMA_E_E_Hole(i_RT)    +RT_carriers%GAMMA_bare(i_RT,3)
           DATA_grp(i_g)%data(i_RT,2,i_T)=RT_carriers%dGAMMA_E_E_Electron(i_RT)+RT_carriers%GAMMA_bare(i_RT,4)
           DATA_grp(i_g)%data(i_RT,3,i_T)=RT_carriers%dGAMMA_E_P_Ho_abs(i_RT)  +RT_carriers%GAMMA_bare(i_RT,5)
           DATA_grp(i_g)%data(i_RT,4,i_T)=RT_carriers%dGAMMA_E_P_El_abs(i_RT)  +RT_carriers%GAMMA_bare(i_RT,6)
         case("e-p-ratio")
           call LIFE_ratio( DATA_grp(i_g_ep), DATA_grp(i_g) )
         case("e-e-ratio")
           call LIFE_ratio( DATA_grp(i_g_ee), DATA_grp(i_g) )
         case("e-photon")
           DATA_grp(i_g)%data(i_RT,1,i_T)=RT_carriers%dGAMMA_E_PHOT_Hole(i_RT)    +RT_carriers%GAMMA_bare(i_RT,7)
           DATA_grp(i_g)%data(i_RT,2,i_T)=RT_carriers%dGAMMA_E_PHOT_Electron(i_RT)+RT_carriers%GAMMA_bare(i_RT,8)
       end select
     enddo
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing( )
 !
 ! Fit the GROUPs!
 !=================
 call live_timing('Data fitting (Temperature and Fermi Level)',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   do i_g=1,n_groups
     !
     if (DATA_grp(i_g)%fittable) call FIT_me(DATA_grp(i_g))
     !
   enddo 
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing( )
 !
 ! ... report ...
 !
 do i_g=1,n_groups
   if (DATA_grp(i_g)%fittable) then
     call  msg('s',":: Fit success ("//trim(DATA_grp(i_g)%kind)//"):"//trim(intc(DATA_grp(i_g)%n_fit_ok(1)))//&
&                  "(h) -- "//trim(intc(DATA_grp(i_g)%n_fit_ok(2)))//"(e)")
   endif
 enddo
 !
 ! Re-ordering
 !=============
 !
 call live_timing('Data ordering',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   call REORDER_it(DATA_grp(i_g))
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing( )
 !
 ! Output(s)
 !==========
 !
 ! Fitted temperatures & Fermi Levels
 !------------------------------------
 Ef_and_T_fname(1)='YPP-RT_Temperatures'
 Ef_and_T_fname(2)='YPP-RT_E_Fermi'
 ! 
 do i_f=1,2
   if (i_f==1) ch_dummy='T'
   if (i_f==2) ch_dummy='Ef'
   call of_open_close(trim(Ef_and_T_fname(i_f)),'ot')
   call RT_write_descriptions(trim(Ef_and_T_fname(i_f)))
   n_titles =1
   titles(1)='Time [fs]'
   do i_g=1,n_groups
     if (.not.DATA_grp(i_g)%fittable) cycle
     titles(n_titles+1)=trim(ch_dummy)//trim(DATA_grp(i_g)%FIT_title(1))
     titles(n_titles+2)=trim(ch_dummy)//trim(DATA_grp(i_g)%FIT_title(2))
     n_titles=n_titles+2
   enddo
   call msg('o '//trim(Ef_and_T_fname(i_f)),'#')
   if (i_f==1) call msg('o '//trim(Ef_and_T_fname(i_f)),'# Fitted temperatures [K]')
   if (i_f==2) call msg('o '//trim(Ef_and_T_fname(i_f)),'# Fitted Fermi Levels [eV]')
   call msg('o '//trim(Ef_and_T_fname(i_f)),'#')
   call msg('o '//trim(Ef_and_T_fname(i_f)),'#',(/titles(1:n_titles)/),INDENT=0,USE_TABS=.true.)
   call msg('o '//trim(Ef_and_T_fname(i_f)),'#')
   do i_T=1,n_T_steps
     n_titles =1
     do i_g=1,n_groups
       if (.not.DATA_grp(i_g)%fittable) cycle
       o_data(1)                                = RT_time(i_T)*AUT2FS
       if (i_f==1) o_data(n_titles+1:n_titles+2)= DATA_grp(i_g)%T(:,i_T)*HA2KEL
       if (i_f==2) o_data(n_titles+1:n_titles+2)= DATA_grp(i_g)%Ef(:,i_T)*HA2EV
       n_titles=n_titles+2
     enddo
     call msg('o '//trim(Ef_and_T_fname(i_f)),'',(/o_data(1:n_titles)/),INDENT=-2,USE_TABS=.true.)
   enddo
   call of_open_close(trim(Ef_and_T_fname(i_f)))
 enddo
 !
 ! Distributions and fits
 !------------------------
 !
 !...columns
 do i_T=1,n_T_steps+1
   columns(i_T)="  #"//trim(intc(i_T))
 enddo
 !
 !... Titles
 titles(1)     =' E [eV]'
 data_ch(1,1,:)=' Time [fs]'
 data_ch(1,2,1)=' T(h) [K]'
 data_ch(1,2,2)=' T(e) [K]'
 data_ch(1,3,:)=' Ef   [eV]'
 data_ch(1,4,:)=' Avg error'
 data_ch(1,5,:)=' Max error'
 !
 do i_T=1,n_T_steps
   titles(i_T+1)=' @'//trim(real2ch( RT_time(i_T)*AUT2FS ))
   data_ch(i_T+1,1,:)= trim(real2ch( RT_time(i_T)*AUT2FS ))
   do i_fit=1,2
     data_ch(i_T+1,2,i_fit)=   trim(real2ch( DATA_grp(1)%T(i_fit,i_T)*HA2KEL ))
     data_ch(i_T+1,3,i_fit)=   trim(real2ch( DATA_grp(1)%Ef(i_fit,i_T)*HA2EV ))
     data_ch(i_T+1,4,i_fit)=   trim(real2ch( DATA_grp(1)%err_fit(i_fit,i_T) ))
     data_ch(i_T+1,5,i_fit)=   trim(real2ch( DATA_grp(1)%err_fit(i_fit+2,i_T) ))
   enddo
 enddo
 !
 do i_g=1,n_groups
   !
   if (trim(DATA_grp(i_g)%kind)=="none") cycle
   !
   call PLOT_the_GROUP_data( DATA_grp(i_g) )
   !
   if (DATA_grp(i_g)%fittable) then
     !
     !call PLOT_the_GROUP_fit( DATA_grp(i_g) )
     !
   endif
   !
 enddo
 !
!   do igamma=1,n_gamma
!     !
!     if (Bose_Temp==0._SP .and. igamma>2 .and. igamma<5) cycle 
!     !
!     output_fname(igamma+n3)=trim(gamma_fname(igamma))
!     if (n_T_steps>N_max_files) output_fname(igamma+n3)=trim(gamma_fname(igamma))//trim(GRP_string)
!     call of_open_close(trim(output_fname(igamma+n3)),'ot')
!     call RT_write_descriptions(trim(output_fname(igamma+n3)))
!     !
!     call msg('o '//trim(output_fname(igamma+n3)),'#')
!     call msg('o '//trim(output_fname(igamma+n3)),'# Lifetimes distribution [meV] at different times (Gamma@ time)')
!     call msg('o '//trim(output_fname(igamma+n3)),'#')
!     call msg('o '//trim(output_fname(igamma+n3)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
!     call msg('o '//trim(output_fname(igamma+n3)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
!     call msg('o '//trim(output_fname(igamma+n3)),'#')
!     !
!   enddo
!   !
!   ! FIT
!   !=====
!   !
!   do i_fit_kind=1,2
!     !
!     output_fname(1)='YPP-RT_occupations_FIT'
!     n_fits_done=1
!     if(n_spinor==2) then
!       output_fname(2)='YPP-RT_occupations_up_FIT'
!       output_fname(3)='YPP-RT_occupations_dw_FIT'
!       n_fits_done=n_fits_done+2
!     endif
!     do i_ratio=1,n_ratio
!       output_fname(n_fits_done+i_ratio)=trim(gamma_ratio_fname(i_ratio))//'_FIT'
!       n_fits_done=n_fits_done+1
!     enddo
!     !
!     do i_fit=1,n_fits_done
!       !
!       if(i_fit_kind==1) output_fname(i_fit)=trim(output_fname(i_fit))//'_holes'
!       if(i_fit_kind==2) output_fname(i_fit)=trim(output_fname(i_fit))//'_electrons'
!       if (n_T_steps>N_max_files) output_fname(i_fit)=trim(output_fname(i_fit))//trim(GRP_string)
!       !
!       call of_open_close(trim(output_fname(i_fit)),'ot')
!       call RT_write_descriptions(trim(output_fname(i_fit)))
!       !
!       call msg('o '//trim(output_fname(i_fit)),'#')
!       call msg('o '//trim(output_fname(i_fit)),'# Results of the FIT')
!       call msg('o '//trim(output_fname(i_fit)),'#')
!       do i1=1,5
!         call msg('o '//trim(output_fname(i_fit)),'#',(/data_ch(1,i1,i_fit_kind), &
!&                                            data_ch(n1+1:n2+1,i1,i_fit_kind)/), INDENT=-2,USE_TABS=.true.)
!       enddo
!       !     
!       call msg('o '//trim(output_fname(i_fit)),'#')
!       call msg('o '//trim(output_fname(i_fit)),'# Fit functions at different time (f@ time)')
!       call msg('o '//trim(output_fname(i_fit)),'#')
!       call msg('o '//trim(output_fname(i_fit)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
!       call msg('o '//trim(output_fname(i_fit)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
!       call msg('o '//trim(output_fname(i_fit)),'#')
!       !
!       do i1=1,fit_dim
!         if(i_fit==1)                               call msg('o '//trim(output_fname(i_fit)),'',&
!&                (/RT_E_fit(i1,i_fit_kind)*HA2EV,RT_occ_fit(i1,i_fit_kind,n1:n2)/),INDENT=-2,USE_TABS=.true.)
!         if((i_fit==2.or.i_fit==3).and.n_spinor==2) call msg('o '//trim(output_fname(i_fit)),'',&
!&                (/RT_E_fit(i1,i_fit_kind)*HA2EV,RT_occ_spin_fit(i1,i_fit_kind,i_fit-1,n1:n2)/),INDENT=-2,USE_TABS=.true.)
!         if((i_fit==2.or.i_fit==3).and.n_spinor==1) call msg('o '//trim(output_fname(i_fit)),'',&
!&                (/RT_E_fit(i1,i_fit_kind)*HA2EV,RT_gamma_ratio_fit(i1,i_fit_kind,i_fit-1,n1:n2)/),INDENT=-2,USE_TABS=.true.)
!         if (i_fit==4.or.i_fit==5)                  call msg('o '//trim(output_fname(i_fit)),'',&
!&                (/RT_E_fit(i1,i_fit_kind)*HA2EV,RT_gamma_ratio_fit(i1,i_fit_kind,i_fit-3,n1:n2)/),INDENT=-2,USE_TABS=.true.)
!       enddo
!       !
!       call of_open_close(trim(output_fname(i_fit)))
!       !
!     enddo
!     !
!   enddo
 !
 if (n_spinor==2) then
   YAMBO_DEALLOCATE_A(spin_factors)
 endif
 do i_g=1,n_groups 
   call local_free(DATA_grp(i_g))
 enddo
 !
 contains 
   !
   subroutine PLOT_the_GROUP_data(TYP)
     type(Load_and_Fit) :: TYP
     integer            :: range_(2),i_T_grp
     character(schlen)  :: GRP_string
     real(SP)           :: PRE_factor
     !
     PRE_factor=1._SP
     if (trim(TYP%kind)=="e-p".or.trim(TYP%kind)=="e-e") then
       PRE_factor=HA2EV*1000._SP
     else if (trim(TYP%kind)=="e-photon") then
       PRE_factor=HA2EV*1000._SP
     endif
     !
     do i_T_grp=1,n_T_steps,N_max_files
       !
       GRP_string="_"//trim(intc(i_T_grp/N_max_files+1))//"_of_"//trim(intc(n_T_steps/N_max_files+1))
       !
       range_=(/i_f, min(n_T_steps,i_f+N_max_files-1) /)
       !
       do i_f=1,TYP%n_files
         !
         TYP%running_file=TYP%file(i_f)
         if (n_T_steps>N_max_files) TYP%running_file=trim(TYP%file(i_f))//trim(GRP_string)
         !
         call of_open_close(trim(TYP%running_file),'ot')
         call RT_write_descriptions(trim(TYP%running_file))
         !
         call msg('o '//trim(TYP%running_file),'#')
         call msg('o '//trim(TYP%running_file),'# '//trim(TYP%file_desc(i_f)))
         call msg('o '//trim(TYP%running_file),'#')
         call msg('o '//trim(TYP%running_file),'#',(/columns(1:range_(2)-range_(1)+2)/),INDENT=-2,USE_TABS=.true.)
         call msg('o '//trim(TYP%running_file),'#',(/titles(1),titles(range_(1)+1:range_(2)+1)/),INDENT=-2,USE_TABS=.true.)
         call msg('o '//trim(TYP%running_file),'#')
         !
         do i_RT=1,RT_carriers%nstates
           !
           !ib    =RT_carriers%table(i_RT,1)
           !ik    =RT_carriers%table(i_RT,2)
           !ik_RT =RT_carriers%table(i_RT,3)
           !i_spin=RT_carriers%table(i_RT,4)
           !
           call msg('o '//trim(TYP%running_file),'',(/RT_E_sorted(i_RT),&
&                   TYP%data(i_RT,i_f,range_(1):range_(2))*PRE_factor/),INDENT=-2,USE_TABS=.true.)
           !
         enddo
         !
         call of_open_close(trim(TYP%running_file))
         !
       enddo
       !
     enddo
     !
   end subroutine
   !
   subroutine local_free(TYP)
     type(Load_and_Fit) :: TYP
     if (trim(TYP%kind)=="none") return
     YAMBO_DEALLOCATE_A(TYP%data_fit)
     YAMBO_DEALLOCATE_A(TYP%E_fit)
     YAMBO_DEALLOCATE_A(TYP%err_fit)
     YAMBO_DEALLOCATE_A(TYP%T)
     YAMBO_DEALLOCATE_A(TYP%Ef)
   end subroutine
   !
   subroutine Load_and_Fit_alloc(TYP,dim,what)
     !
     type(Load_and_Fit) :: TYP
     integer            :: dim
     character(*)       :: what
     !
     YAMBO_ALLOCATE(TYP%data(RT_carriers%nstates,dim,n_T_steps))
     !
     if (what=="CARRIERS".or.what=="spin-up".or.what=="spin-dn".or.what=="e-p-ratio".or.what=="e-e-ratio") then
       TYP%fittable=.TRUE.
       YAMBO_ALLOCATE(TYP%data_fit(fit_dim,dim,n_T_steps))
       YAMBO_ALLOCATE(TYP%E_fit(fit_dim,2))
       YAMBO_ALLOCATE(TYP%err_fit(4,n_T_steps))
       YAMBO_ALLOCATE(TYP%T(2,n_T_steps))
       YAMBO_ALLOCATE(TYP%Ef(2,n_T_steps))
       TYP%data_fit=rZERO
       TYP%E_fit  =rZERO
       TYP%err_fit=rZERO
       TYP%T      =rZERO
       TYP%Ef     =rZERO
     endif
     !
     TYP%kind=what
     !
     select case (what)
       case("CARRIERS")
         TYP%n_files=dim
         TYP%FIT_title=(/'(e)   ','(h)   '/)
         TYP%file(1)="YPP-RT_occupations_DATA"
         TYP%file_desc(1)="Occupations at different times"
       case("spin-up")
         TYP%n_files=dim
         TYP%FIT_title=(/'(e-up)','(h-up)'/)
         TYP%file(1)="YPP-RT_occupations_up_DATA"
         TYP%file_desc(1)="Occupations UP channel at different times"
       case("spin-dn")
         TYP%n_files=dim
         TYP%FIT_title=(/'(e-dn)','(h-dn)'/)
         TYP%file(1)="YPP-RT_occupations_dn_DATA"
         TYP%file_desc(1)="Occupations down channel at different times"
       case("e-p")
         TYP%n_files=dim
         TYP%file(1)='YPP-RT_EP_Hole_linewidths'
         TYP%file(2)='YPP-RT_EP_Elec_linewidths'
         TYP%file(3)='YPP-RT_EP_Hole_abs_linewidths'
         TYP%file(4)='YPP-RT_EP_Elec_abs_linewidths'
         TYP%file_desc(:)="Lifetimes distribution [meV] at different times"
       case("e-p-ratio")
         TYP%n_files=dim
         TYP%FIT_title=(/'(e-EP)','(h-EP)'/)
         TYP%file(1)='YPP-RT_EP_widths_ratio'
         TYP%file_desc(:)="Lifetimes ratio distribution [meV] at different times"
       case("e-e")
         TYP%n_files=dim
         TYP%file(1)='YPP-RT_EE_Hole_linewidths'
         TYP%file(2)='YPP-RT_EE_Elec_linewidths'
         TYP%file_desc(:)="Lifetimes distribution [meV] at different times"
       case("e-e-ratio")
         TYP%n_files=dim
         TYP%FIT_title=(/'(e-EE)','(h-EE)'/)
         TYP%file(1)='YPP-RT_EE_widths_ratio'
         TYP%file_desc(:)="Lifetimes ratio distribution [meV] at different times"
       case("e-photon")
         TYP%n_files=dim
         TYP%file(1)='YPP-RT_EPhoton_Hole_linewidths'
         TYP%file(2)='YPP-RT_EPhoton_Elec_linewidths'
         TYP%file_desc(:)="Lifetimes distribution [meV] at different times"
     end select
     !
   end subroutine
   !
   subroutine LIFE_ratio(TYP,TYP_ratio)
     !
     type(Load_and_Fit) :: TYP,TYP_ratio
     real(SP)           :: GAMMA_holes,GAMMA_elecs
     !
     do i_RT=1,RT_carriers%nstates
       !
       GAMMA_holes=TYP%data(i_RT,1,i_T)
       GAMMA_elecs=TYP%data(i_RT,2,i_T)
       if (trim(TYP%kind)=="e-p") then
         GAMMA_holes=GAMMA_holes+TYP%data(i_RT,3,i_T)
         GAMMA_elecs=GAMMA_elecs+TYP%data(i_RT,4,i_T)
       endif
       !
       if (RT_carriers%E_bare(i_RT)<=0._SP) TYP_ratio%data(i_RT,1,i_T)=-GAMMA_elecs/(GAMMA_elecs+GAMMA_holes)*spin_occ
       if (RT_carriers%E_bare(i_RT)> 0._SP) TYP_ratio%data(i_RT,2,i_T)= GAMMA_holes/(GAMMA_elecs+GAMMA_holes)*spin_occ
       !
     enddo
     !
   end subroutine
   !
   subroutine REORDER_it(TYP)
     type(Load_and_Fit) :: TYP
     real(SP)           :: TMP_vec(RT_carriers%nstates)
     integer            :: i_data
     if (trim(TYP%kind)=="none") return
     do i_data = 1, size(TYP%data,2)
       TMP_vec=TYP%data(E_sort_index(:),i_data,i_T)
       TYP%data(:,i_data,i_T)=TMP_vec
     enddo
   end subroutine
   !
   subroutine FIT_me(TYP)
     !
     type(Load_and_Fit) :: TYP
     integer            :: i_err
     !
     if (i_T>1) then
       TYP%T (:,i_T) = TYP%T (:,i_T-1)
       TYP%Ef(:,i_T) = TYP%Ef(:,i_T-1)
     endif
     !
     if (trim(TYP%kind)=="CARRIERS") then
       i_err=Fermi_fnc_fit(TYP%data(:,1,i_T),RT_carriers%nstates,&
&                          TYP%T(:,i_T),TYP%Ef(:,i_T),TYP%err_fit(:,i_T),&
&                          RT_occ_fit_out=TYP%data_fit(:,:,i_T),RT_E_fit_out=TYP%E_fit)
     else
       i_err=Fermi_fnc_fit(TYP%data(:,1,i_T),RT_carriers%nstates,&
&                          TYP%T(:,i_T),TYP%Ef(:,i_T),TYP%err_fit(:,i_T),&
&                          RT_occ_fit_out=TYP%data_fit(:,:,i_T))
     endif
     !
     if (i_err==1) then
       TYP%n_fit_ok(1)   =TYP%n_fit_ok(1)+1
       !TYP%T(2,i_T)      =rZERO
       !TYP%Ef(2,i_T)     =rZERO
     else if (i_err==2) then
       TYP%n_fit_ok(2)   =TYP%n_fit_ok(2)+1
       !TYP%T(1,i_T)      =rZERO
       !TYP%Ef(1,i_T)     =rZERO
     else if (i_err==3) then
       TYP%n_fit_ok(1:2) =TYP%n_fit_ok(1:2)+1
     endif
     !
   end subroutine
   !
end subroutine RT_occ_energy_plot
