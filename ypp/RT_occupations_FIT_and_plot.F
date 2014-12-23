! 
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine RT_occupations_FIT_and_plot(RT_E,RT_occ,RT_gamma,ngamma,n_T_steps)
 ! 
 ! FIT the occupations with a Gauss–Newton algorithm
 !
 use units,          ONLY:HA2EV,HA2KEL,FS2AUT
 use pars,           ONLY:SP,schlen
 use rt_ctl,         ONLY:OCC_T_range,OCC_deltaT
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_table,QP_n_states
 use D_lattice,      ONLY:Bose_Temp
 use electrons,      ONLY:levels,spin,spin_occ,filled_tresh
 use vec_operate,    ONLY:sort
 use functions,      ONLY:Fermi_fnc,Fermi_fnc_derivative,Fermi_fnc_fit
 use com,            ONLY:msg,of_open_close,error
 use stderr,         ONLY:intc,real2ch
 use real_time,      ONLY:RT_nk,RT_ibz_coo,RT_carriers
 !
 implicit none
 !
 integer               :: n_T_steps,ngamma
 real(SP)              :: RT_E(RT_carriers%nstates)
 real(SP)              :: RT_occ(RT_carriers%nstates,n_T_steps)
 real(SP)              :: RT_gamma(RT_carriers%nstates,n_T_steps,8)
 !
 integer, parameter    :: max_iter=10000,fit_dim=1000,nmax_file_report=16
 real(SP)              :: RT_occ_fit(fit_dim,n_T_steps,2),RT_E_fit(fit_dim,2)
 real(SP)              :: RT_gamma_ratio(RT_carriers%nstates,n_T_steps,2)
 !
 ! Work Space
 !
 real(SP)              :: io_Time
 real(SP)              :: TMP_vec(RT_carriers%nstates),data_fit(7,2)
 real(SP)              :: err_fit(n_T_steps,4),err_fit_gamma(n_T_steps,4)
 real(SP)              :: T_fit(n_T_steps,2),T_fit_gamma(n_T_steps,4)
 real(SP)              :: Ef_fit(n_T_steps,2),Ef_fit_gamma(n_T_steps,4)
 ! 
 integer  :: ifit,i_s,i_RT,ik,ik_rim,ib,i1,i_T,i2,n1,n2,i_spin,igamma
 integer  :: nfound(3),nfound_gamma(5)
 integer  :: E_sort_index(RT_carriers%nstates)
 !
 character(10)     ::titles(n_T_steps+4),data_ch(n_T_steps+4,5,2),columns(n_T_steps+4)
 character(schlen) ::output_fname(max(2,ngamma+1)),gamma_fname(ngamma),GRP_string
 !
 T_fit=0._SP
 Ef_fit=0._SP
 nfound=0
 !
 T_fit_gamma=0._SP
 Ef_fit_gamma=0._SP
 nfound_gamma=0
 !
 call sort(RT_E,indx=E_sort_index)
 do i1=1,n_T_steps
   TMP_vec=RT_occ(E_sort_index(:),i1)
   RT_occ(:,i1)=TMP_vec
   do i2=1,ngamma
     TMP_vec=RT_gamma(E_sort_index(:),i1,i2)
     RT_gamma(:,i1,i2)=TMP_vec
   enddo
 enddo
 !
 filled_tresh=1.E-7
 !
 do i1=1,n_T_steps
   !
   if(i1>1) then
     T_fit(i1,:) =T_fit(i1-1,:)
     Ef_fit(i1,:)=Ef_fit(i1-1,:)
     T_fit_gamma(i1,:) =T_fit_gamma(i1-1,:)
     Ef_fit_gamma(i1,:)=Ef_fit_gamma(i1-1,:)
   endif
   !
   ! Occupations FIT
   !
   nfound(3)=Fermi_fnc_fit(RT_E,RT_occ(:,i1),RT_carriers%nstates,T_fit(i1,:),Ef_fit(i1,:),err_fit(i1,:),&
&                            RT_occ_fit_out=RT_occ_fit(:,i1,:),RT_E_fit_out=RT_E_fit)
   !
   if(nfound(3)==1) nfound(1)  =nfound(1)+1
   if(nfound(3)==2) nfound(2)  =nfound(2)+1
   if(nfound(3)==3) nfound(1:2)=nfound(1:2)+1
   !
   if(ngamma<2) cycle
   !
   ! Lifetimes FIT
   !
   if(ngamma==2) RT_gamma_ratio(:,i1,1)=RT_gamma(:,i1,1)/( RT_gamma(:,i1,1)+RT_gamma(:,i1,2) )
   if(ngamma> 2) RT_gamma_ratio(:,i1,1)=( RT_gamma(:,i1,1)+RT_gamma(:,i1,3)+RT_gamma(:,i1,5) ) / &
&                                       ( RT_gamma(:,i1,1)+RT_gamma(:,i1,3)+RT_gamma(:,i1,5)+    &
&                                         RT_gamma(:,i1,2)+RT_gamma(:,i1,4)+RT_gamma(:,i1,6) ) 
   !
   RT_gamma_ratio=RT_gamma_ratio*spin_occ
   !
   nfound_gamma(5)=Fermi_fnc_fit(RT_E,RT_gamma_ratio(:,i1,1),RT_carriers%nstates,T_fit_gamma(i1,1:2),Ef_fit_gamma(i1,1:2),&
&                                err_fit_gamma(i1,:))
   !
   if(nfound_gamma(5)==1) nfound_gamma(1)  =nfound_gamma(1)+1
   if(nfound_gamma(5)==2) nfound_gamma(2)  =nfound_gamma(2)+1
   if(nfound_gamma(5)==3) nfound_gamma(1:2)=nfound_gamma(1:2)+1
   !
   if(ngamma<=6) cycle
   !
   RT_gamma_ratio(:,i1,2)=RT_gamma(:,i1,7)/( RT_gamma(:,i1,5)+RT_gamma(:,i1,8) )
   !
   nfound_gamma(5)=Fermi_fnc_fit(RT_E,RT_gamma_ratio(:,i1,2),RT_carriers%nstates,T_fit_gamma(i1,3:4),Ef_fit_gamma(i1,3:4),&
&                          err_fit_gamma(i1,:))
   !
   if(nfound_gamma(5)==1) nfound_gamma(3)  =nfound_gamma(3)+1
   if(nfound_gamma(5)==2) nfound_gamma(4)  =nfound_gamma(4)+1
   if(nfound_gamma(5)==3) nfound_gamma(3:4)=nfound_gamma(3:4)+1
   !      
 enddo
 !
 call msg('s',":: Holes fitting Temperatures found " &
&          //trim(intc(nfound(1)))//"/"//trim(intc(n_T_steps)))
 call msg('s',":: Elec. fitting Temperatures found " &
&          //trim(intc(nfound(2)))//"/"//trim(intc(n_T_steps)))
 !
 if(ngamma>1) then
   call msg('s',":: Hole lifetimes(1) fitting Temperatures found " &
&          //trim(intc(nfound_gamma(1)))//"/"//trim(intc(n_T_steps)))
   call msg('s',":: Elec lifetimes(1) fitting Temperatures found " &
&          //trim(intc(nfound_gamma(2)))//"/"//trim(intc(n_T_steps)))
 endif
 !
 if(ngamma>4) then
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
 if(ngamma==2) then
   gamma_fname(1)='YPP-RT_EE_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EE_Elec_linewidths'
   titles(4:5)=(/'EE  el ','EE  hol'/)   
   n1=5
 ! Only El-Ph
 else if (ngamma==6) then
   gamma_fname(1)='YPP-RT_EP_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EP_Elec_linewidths'
   gamma_fname(3)='YPP-RT_EP_Hole_st_em_linewidths'
   gamma_fname(4)='YPP-RT_EP_Elec_st_em_linewidths'
   gamma_fname(5)='YPP-RT_EP_Hole_abs_linewidths'
   gamma_fname(6)='YPP-RT_EP_Elec_abs_linewidths'
   titles(4:5)=(/'EP  el ','EP  hol'/)   
   n1=5
 ! Both
 else if (ngamma==8) then
   gamma_fname(1)='YPP-RT_EP_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EP_Elec_linewidths'
   gamma_fname(3)='YPP-RT_EP_Hole_st_em_linewidths'
   gamma_fname(4)='YPP-RT_EP_Elec_st_em_linewidths'
   gamma_fname(5)='YPP-RT_EP_Hole_abs_linewidths'
   gamma_fname(6)='YPP-RT_EP_Elec_abs_linewidths'
   gamma_fname(7)='YPP-RT_EE_Hole_linewidths'
   gamma_fname(8)='YPP-RT_EE_Elec_linewidths'
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
       data_fit(i1+1,1)= T_fit(i_T,i1)*HA2KEL
       data_fit(i1+1,2)= Ef_fit(i_T,i1)*HA2EV
     endif
     if(ngamma<2) cycle 
     data_fit(i1+3,1)= T_fit_gamma(i_T,i1)*HA2KEL
     data_fit(i1+3,2)= Ef_fit_gamma(i_T,i1)*HA2EV
   enddo
   !
   ! Write temperatures and fermi levels to file
   !
   call msg('o '//trim(output_fname(1)),'',(/data_fit(1:n1,1)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname(2)),'',(/data_fit(1:n1,2)/),INDENT=-2,USE_TABS=.true.)
   !
 enddo
 !
 call of_open_close(trim(output_fname(1)))
 call of_open_close(trim(output_fname(2)))
 !
 do i1=1,n_T_steps+1
   columns(i1)="  #"//trim(intc(i1))
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
     data_ch(i_T+1,2,ifit)=   trim(real2ch( T_fit(i_T,ifit)*HA2KEL ))
     data_ch(i_T+1,3,ifit)=   trim(real2ch( Ef_fit(i_T,ifit)*HA2EV ))
     data_ch(i_T+1,4,ifit)=   trim(real2ch( err_fit(i_T,ifit) ))
     data_ch(i_T+1,5,ifit)=   trim(real2ch( err_fit(i_T,ifit+2) ))
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
   do igamma=1,ngamma
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
   i_RT=0
   do i_s=1,QP_n_states
     !
     ib    =QP_table(i_s,1)
     ik    =QP_table(i_s,3)
     i_spin=spin(QP_table(i_s,:))
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       if (ib<SC_bands(1)) cycle
       if (ib>SC_bands(2)) cycle
       if (ik_rim>RT_nk) cycle
       !
       i_RT=i_RT+1
       !
       call msg('o '//trim(output_fname(1)),'',(/RT_E(i_RT)*HA2EV,RT_occ(i_RT,n1:n2)/),INDENT=-2,USE_TABS=.true.)
       !
       do igamma=1,ngamma
         if (Bose_Temp==0._SP .and. igamma>2 .and. igamma<5) cycle
         call msg('o '//trim(output_fname(igamma+1)),'',(/RT_E(i_RT)*HA2EV,RT_gamma(i_RT,n1:n2,igamma)/),&
&                                                         INDENT=-2,USE_TABS=.true.)
       enddo
       !
     enddo
     !
   enddo
   !
   call of_open_close(trim(output_fname(1)))
   do igamma=1,ngamma
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
&                                                RT_occ_fit(i1,n1:n2,ifit)/),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     call of_open_close(trim(output_fname(1)))
     !
   enddo
 enddo
 !
end subroutine RT_occupations_FIT_and_plot
