! 
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
 use units,          ONLY:HA2EV,HA2KEL,FS2AUT
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use pars,           ONLY:SP,schlen
 use rt_ctl,         ONLY:OCC_T_range,OCC_deltaT,RT_apply
 use SC,             ONLY:SC_bands
 !use QP_m,           ONLY:QP_table,QP_n_states
 use D_lattice,      ONLY:Bose_Temp
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,spin_occ,filled_tresh
 use vec_operate,    ONLY:sort
 use functions,      ONLY:Fermi_fnc,Fermi_fnc_derivative,Fermi_fnc_fit
 use com,            ONLY:msg,of_open_close,error
 use stderr,         ONLY:intc,real2ch
 use real_time,      ONLY:RT_carriers,l_RT_CCA_Kernel
 !
 implicit none
 !
 type(bz_samp) :: kpt
 type(levels)  :: En
 integer       :: n_T_steps
 !
 ! Work Space
 !
 integer, parameter    :: max_iter=10000,fit_dim=1000,nmax_file_report=16
 real(SP)              :: RT_E_sorted(RT_carriers%nstates)
 real(SP)              :: RT_occ(RT_carriers%nstates,n_T_steps)
 real(SP)              :: RT_gamma(RT_carriers%nstates,8,n_T_steps)
 !
 real(SP)              :: RT_occ_fit(fit_dim,2,n_T_steps),RT_E_fit(fit_dim,2)
 real(SP)              :: RT_gamma_ratio(RT_carriers%nstates,2,n_T_steps)
 !
 real(SP)              :: io_Time
 real(SP)              :: TMP_vec(RT_carriers%nstates),data_fit(2,7)
 real(SP)              :: err_fit(4,n_T_steps),err_fit_gamma(4,n_T_steps)
 real(SP)              :: T_fit(2,n_T_steps),T_fit_gamma(4,n_T_steps)
 real(SP)              :: Ef_fit(2,n_T_steps),Ef_fit_gamma(4,n_T_steps)
 !
 logical  :: l_ELPH_lifetimes
 !
 integer  :: ifit,i_RT,ib,i_T,i1,i2,n1,n2,igamma,n_gamma
 integer  :: nfound(3),nfound_gamma(5)
 integer  :: E_sort_index(RT_carriers%nstates)
 !
 character(10)     ::titles(n_T_steps+4),data_ch(n_T_steps+4,5,2),columns(n_T_steps+4)
 character(schlen) ::output_fname(7),gamma_fname(7),GRP_string
 !
 l_ELPH_lifetimes=l_elph_scatt.and.l_RT_CCA_Kernel
 !
 RT_E_sorted=RT_carriers%E_bare
 call sort(RT_E_sorted,indx=E_sort_index)
 !
 RT_gamma=0._SP
 !
 T_fit=0._SP
 Ef_fit=0._SP
 nfound=0
 !
 T_fit_gamma=0._SP
 Ef_fit_gamma=0._SP
 nfound_gamma=0
 !
 filled_tresh=1.E-7
 !
 do i_T=1,n_T_steps
   !
   ! Part 1: IO and store history
   !==============================
   io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   !
   call RT_apply(SC_bands,En,kpt,Time=io_Time,keep_RT_carriers=.TRUE.)
   !
   ! Fill the large arrays to be used for the fitting procedure.
   !
   do i_RT=1,RT_carriers%nstates
     !
     ! Occupations
     !============
     RT_occ(i_RT,i_T)=RT_carriers%delta_f(i_RT)
     !
     ! Lifetimes
     !===========
     n_gamma=0
     if (l_ELPH_lifetimes) then
       n_gamma=4
       RT_gamma(i_RT,1,i_T)=RT_carriers%dGAMMA_E_P_Hole(i_RT)    +RT_carriers%GAMMA_bare(i_RT,3)
       RT_gamma(i_RT,2,i_T)=RT_carriers%dGAMMA_E_P_Electron(i_RT)+RT_carriers%GAMMA_bare(i_RT,4)
       RT_gamma(i_RT,3,i_T)=RT_carriers%dGAMMA_E_P_Ho_abs(i_RT)  +RT_carriers%GAMMA_bare(i_RT,5)
       RT_gamma(i_RT,4,i_T)=RT_carriers%dGAMMA_E_P_El_abs(i_RT)  +RT_carriers%GAMMA_bare(i_RT,6)
     endif
     !
     if (l_elel_scatt) then
       n_gamma=n_gamma+2
       RT_gamma(i_RT,n_gamma-1,i_T)=RT_carriers%dGAMMA_E_E_Hole(i_RT)    +RT_carriers%GAMMA_bare(i_RT,1)
       RT_gamma(i_RT,n_gamma  ,i_T)=RT_carriers%dGAMMA_E_E_Electron(i_RT)+RT_carriers%GAMMA_bare(i_RT,2)
     endif
     !
   enddo
   !
   ! Part 2: Fit occ and life ratio
   !================================
   !
   if(i_T>1) then
     T_fit(:,i_T) =T_fit(:,i_T-1)
     Ef_fit(:,i_T)=Ef_fit(:,i_T-1)
     T_fit_gamma(:,i_T) =T_fit_gamma(:,i_T-1)
     Ef_fit_gamma(:,i_T)=Ef_fit_gamma(:,i_T-1)
   endif
   !
   ! Occupations FIT
   !
   nfound(3)=Fermi_fnc_fit(RT_occ(:,i_T),RT_carriers%nstates,T_fit(:,i_T),Ef_fit(:,i_T),err_fit(:,i_T), &
&                            RT_occ_fit_out=RT_occ_fit(:,:,i_T),RT_E_fit_out=RT_E_fit)
   !
   if(nfound(3)==1) nfound(1)  =nfound(1)+1
   if(nfound(3)==2) nfound(2)  =nfound(2)+1
   if(nfound(3)==3) nfound(1:2)=nfound(1:2)+1
   !
   if(n_gamma<2) cycle
   !
   ! Lifetimes FIT
   !
   if(n_gamma==2) RT_gamma_ratio(:,1,i_T)=RT_gamma(:,1,i_T)/( RT_gamma(:,1,i_T)+RT_gamma(:,2,i_T) )
   if(n_gamma> 2) RT_gamma_ratio(:,1,i_T)=( RT_gamma(:,1,i_T)+RT_gamma(:,3,i_T)+RT_gamma(:,5,i_T) ) / &
&                                       ( RT_gamma(:,1,i_T)+RT_gamma(:,3,i_T)+RT_gamma(:,5,i_T)+    &
&                                         RT_gamma(:,2,i_T)+RT_gamma(:,4,i_T)+RT_gamma(:,6,i_T) ) 
   !
   RT_gamma_ratio=RT_gamma_ratio*spin_occ
   !
   nfound_gamma(5)=Fermi_fnc_fit(RT_gamma_ratio(:,i_T,1),RT_carriers%nstates,&
&                                 T_fit_gamma(1:2,i_T),Ef_fit_gamma(1:2,i_T),err_fit_gamma(:,i_T))
   !
   if(nfound_gamma(5)==1) nfound_gamma(1)  =nfound_gamma(1)+1
   if(nfound_gamma(5)==2) nfound_gamma(2)  =nfound_gamma(2)+1
   if(nfound_gamma(5)==3) nfound_gamma(1:2)=nfound_gamma(1:2)+1
   !
   if(n_gamma<=6) cycle
   !
   RT_gamma_ratio(:,2,i_T)=RT_gamma(:,7,i_T)/( RT_gamma(:,5,i_T)+RT_gamma(:,8,i_T) )
   !
   nfound_gamma(5)=Fermi_fnc_fit(RT_gamma_ratio(:,2,i_T),RT_carriers%nstates,&
&                                  T_fit_gamma(3:4,i_T),Ef_fit_gamma(3:4,i_T),err_fit_gamma(:,i_T))
   !
   if(nfound_gamma(5)==1) nfound_gamma(3)  =nfound_gamma(3)+1
   if(nfound_gamma(5)==2) nfound_gamma(4)  =nfound_gamma(4)+1
   if(nfound_gamma(5)==3) nfound_gamma(3:4)=nfound_gamma(3:4)+1
   !
 enddo
 !
 !
 do i_T=1,n_T_steps
   !
   !RT_occ(:,i_T)=RT_occ(E_sort_index(:),i_T)
   TMP_vec=RT_occ(E_sort_index(:),i_T)
   RT_occ(:,i_T)=TMP_vec
   do igamma=1,n_gamma
     TMP_vec=RT_gamma(E_sort_index(:),igamma,i_T)
     RT_gamma(:,igamma,i_T)=TMP_vec
   enddo
   !
 enddo
 !
 call msg('s',":: Holes fitting Temperatures found " &
&          //trim(intc(nfound(1)))//"/"//trim(intc(n_T_steps)))
 call msg('s',":: Elec. fitting Temperatures found " &
&          //trim(intc(nfound(2)))//"/"//trim(intc(n_T_steps)))
 !
 if(n_gamma>1) then
   call msg('s',":: Hole lifetimes(1) fitting Temperatures found " &
&          //trim(intc(nfound_gamma(1)))//"/"//trim(intc(n_T_steps)))
   call msg('s',":: Elec lifetimes(1) fitting Temperatures found " &
&          //trim(intc(nfound_gamma(2)))//"/"//trim(intc(n_T_steps)))
 endif
 !
 if(n_gamma>4) then
 call msg('s',":: Hole lifetimes(2) fitting Temperatures found " &
&          //trim(intc(nfound_gamma(3)))//"/"//trim(intc(n_T_steps)))
 call msg('s',":: Elec lifetimes(2) fitting Temperatures found " &
&          //trim(intc(nfound_gamma(4)))//"/"//trim(intc(n_T_steps)))
 endif
 !
 titles(1)='Time [fs]'
 titles(2:3)=(/'OCC el ','OCC hol'/)
 n1=3   
 !
 ! Only El-El
 if(n_gamma==2) then
   gamma_fname(1)='YPP-RT_EE_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EE_Elec_linewidths'
   titles(4:5)=(/'EE  el ','EE  hol'/)   
   n1=5
 ! Only El-Ph
 else if (n_gamma==4) then
   gamma_fname(1)='YPP-RT_EP_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EP_Elec_linewidths'
   gamma_fname(3)='YPP-RT_EP_Hole_abs_linewidths'
   gamma_fname(4)='YPP-RT_EP_Elec_abs_linewidths'
   titles(4:5)=(/'EP  el ','EP  hol'/)   
   n1=5
 ! Both
 else if (n_gamma==6) then
   gamma_fname(1)='YPP-RT_EP_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EP_Elec_linewidths'
   gamma_fname(3)='YPP-RT_EP_Hole_abs_linewidths'
   gamma_fname(4)='YPP-RT_EP_Elec_abs_linewidths'
   gamma_fname(5)='YPP-RT_EE_Hole_linewidths'
   gamma_fname(6)='YPP-RT_EE_Elec_linewidths'
   titles(4:7)=(/'EP  el ','EP  hol','EE  el ','EE  hol'/)   
   n1=7
 endif
 !
 output_fname(1)='YPP-RT_Temperatures'
 output_fname(2)='YPP-RT_E_Fermi'
 !
 call of_open_close(trim(output_fname(1)),'ot')
 call of_open_close(trim(output_fname(2)),'ot')
 !
 call msg('o '//trim(output_fname(1)),'#')
 call msg('o '//trim(output_fname(1)),'# Fitted temperatures [K]')
 call msg('o '//trim(output_fname(1)),'#')
 call msg('o '//trim(output_fname(1)),'#',(/titles(1:n1)/),INDENT=0,USE_TABS=.true.)
 call msg('o '//trim(output_fname(1)),'#')
 !
 call msg('o '//trim(output_fname(2)),'#')
 call msg('o '//trim(output_fname(2)),'# Fitted Fermi levels [eV]')
 call msg('o '//trim(output_fname(2)),'#')
 call msg('o '//trim(output_fname(2)),'#',(/titles(1:n1)/),INDENT=0,USE_TABS=.true.)
 call msg('o '//trim(output_fname(2)),'#')
 !
 do i_T=1,n_T_steps
   !
   io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   !
   data_fit(1,:)= io_Time/FS2AUT
   !
   do i1=1,4
     if(i1<=2) then
       data_fit(1,i1+1)= T_fit(i1,i_T)*HA2KEL
       data_fit(2,i1+1)= Ef_fit(i1,i_T)*HA2EV
     endif
     if(n_gamma<2) cycle 
     data_fit(1,i1+3)= T_fit_gamma(i1,i_T)*HA2KEL
     data_fit(2,i1+3)= Ef_fit_gamma(i1,i_T)*HA2EV
   enddo
   !
   ! Write temperatures and fermi levels to file
   !
   call msg('o '//trim(output_fname(1)),'',(/data_fit(1,1:n1)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname(2)),'',(/data_fit(2,1:n1)/),INDENT=-2,USE_TABS=.true.)
   !
 enddo
 !
 call of_open_close(trim(output_fname(1)))
 call of_open_close(trim(output_fname(2)))
 !
 do i_T=1,n_T_steps+1
   columns(i_T)="  #"//trim(intc(i_T))
 enddo
 !
 ! Prepare titles
 !
 titles(1)     =' E [eV]'
 data_ch(1,1,:)=' Time [fs]'
 data_ch(1,2,1)=' T(h) [K]'
 data_ch(1,2,2)=' T(e) [K]'
 data_ch(1,3,:)=' Ef   [eV]'
 data_ch(1,4,:)=' Avg error'
 data_ch(1,5,:)=' Max error'
 !
 do i_T=1,n_T_steps
   io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   titles(i_T+1)=' f@'//trim(real2ch( io_Time/FS2AUT ))
   data_ch(i_T+1,1,:)= trim(real2ch( io_Time/FS2AUT ))
   do ifit=1,2
     data_ch(i_T+1,2,ifit)=   trim(real2ch( T_fit(ifit,i_T)*HA2KEL ))
     data_ch(i_T+1,3,ifit)=   trim(real2ch( Ef_fit(ifit,i_T)*HA2EV ))
     data_ch(i_T+1,4,ifit)=   trim(real2ch( err_fit(ifit,i_T) ))
     data_ch(i_T+1,5,ifit)=   trim(real2ch( err_fit(ifit+2,i_T) ))
   enddo
 enddo
 !
 do i2=1,n_T_steps,nmax_file_report
   !
   n1=i2
   n2=min(n_T_steps,i2+15)
   !
   GRP_string="_"//trim(intc(i2/nmax_file_report+1))//"_of_"//trim(intc(n_T_steps/nmax_file_report+1))
   !
   output_fname(1)='YPP-RT_occupation_distribution_DATA'
   !
   if (n_T_steps>nmax_file_report) output_fname(1)=trim(output_fname(1))//trim(GRP_string)
   call of_open_close(trim(output_fname(1)),'ot')
   !
   call msg('o '//trim(output_fname(1)),'#')
   call msg('o '//trim(output_fname(1)),'# Occupations distribution at different times (f@ time)')
   call msg('o '//trim(output_fname(1)),'#')
   call msg('o '//trim(output_fname(1)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname(1)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname(1)),'#')
   !
   do igamma=1,n_gamma
     !
     if (Bose_Temp==0._SP .and. igamma>2 .and. igamma<5) cycle 
     !
     output_fname(igamma+1)=trim(gamma_fname(igamma))
     if (n_T_steps>nmax_file_report) output_fname(igamma+1)=trim(gamma_fname(igamma))//trim(GRP_string)
     call of_open_close(trim(output_fname(igamma+1)),'ot')
     !
     call msg('o '//trim(output_fname(igamma+1)),'#')
     call msg('o '//trim(output_fname(igamma+1)),'# Lifetimes distribution at different times (f@ time)')
     call msg('o '//trim(output_fname(igamma+1)),'#')
     call msg('o '//trim(output_fname(igamma+1)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(igamma+1)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(igamma+1)),'#')
   enddo
   !
   do i_RT=1,RT_carriers%nstates
     !
     !ib    =RT_carriers%table(i_RT,1)
     !ik    =RT_carriers%table(i_RT,2)
     !ik_RT =RT_carriers%table(i_RT,3)
     !i_spin=RT_carriers%table(i_RT,4)
     !
     call msg('o '//trim(output_fname(1)),'',(/RT_E_sorted(i_RT)*HA2EV,RT_occ(i_RT,n1:n2)/),INDENT=-2,USE_TABS=.true.)
     !
     do igamma=1,n_gamma
       if (Bose_Temp==0._SP .and. igamma>2 .and. igamma<5) cycle
       call msg('o '//trim(output_fname(igamma+1)),'',(/RT_E_sorted(i_RT)*HA2EV,RT_gamma(i_RT,igamma,n1:n2)/),&
&                                                       INDENT=-2,USE_TABS=.true.)
     enddo
     !
   enddo
   !
   call of_open_close(trim(output_fname(1)))
   do igamma=1,n_gamma
     if (Bose_Temp==0._SP .and. igamma>2 .and. igamma<5) cycle
     call of_open_close(trim(output_fname(igamma+1)))
   enddo
   !
   do ifit=1,2
     !
     output_fname(1)='YPP-RT_occupation_distribution_FIT'
     if(ifit==1) output_fname(1)=trim(output_fname(1))//'_hole'
     if(ifit==2) output_fname(1)=trim(output_fname(1))//'_electrons'
     if (n_T_steps>nmax_file_report) output_fname(1)=trim(output_fname(1))//trim(GRP_string)
     !
     call of_open_close(trim(output_fname(1)),'ot')
     !
     call msg('o '//trim(output_fname(1)),'#')
     call msg('o '//trim(output_fname(1)),'# Results of the FIT')
     call msg('o '//trim(output_fname(1)),'#')
     do i1=1,5
       call msg('o '//trim(output_fname(1)),'#',(/data_ch(1,i1,ifit), &
&                                            data_ch(n1+1:n2+1,i1,ifit)/), INDENT=-2,USE_TABS=.true.)
     enddo
     !     
     call msg('o '//trim(output_fname(1)),'#')
     call msg('o '//trim(output_fname(1)),'# Fit functions at different time (f@ time)')
     call msg('o '//trim(output_fname(1)),'#')
     call msg('o '//trim(output_fname(1)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(1)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(1)),'#')
     !
     do i1=1,fit_dim
       call msg('o '//trim(output_fname(1)),'',(/RT_E_fit(i1,ifit)*HA2EV,&
&                                                RT_occ_fit(i1,ifit,n1:n2)/),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     call of_open_close(trim(output_fname(1)))
     !
   enddo
 enddo
 !
end subroutine RT_occ_energy_plot
