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
 use units,          ONLY:HA2EV,HA2KEL,AUT2FS
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use pars,           ONLY:SP,schlen
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
 type(bz_samp) :: kpt
 type(levels)  :: En
 integer       :: n_T_steps
 !
 ! Work Space
 !
 integer, parameter    :: max_iter=10000,fit_dim=1000,nmax_file_report=16
 real(SP)              :: RT_E_sorted(RT_carriers%nstates)
 real(SP)              :: RT_occ(RT_carriers%nstates,n_T_steps),RT_spin_occ(RT_carriers%nstates,2,n_T_steps)
 real(SP)              :: RT_gamma(RT_carriers%nstates,6,n_T_steps),RT_gamma_ratio(RT_carriers%nstates,2,n_T_steps)
 !
 real(SP)              :: RT_occ_fit(fit_dim,2,n_T_steps),RT_occ_spin_fit(fit_dim,2,2,n_T_steps),RT_E_fit(fit_dim,2)
 real(SP)              :: RT_gamma_ratio_fit(fit_dim,2,2,n_T_steps)
 !
 real(SP)              :: TMP_vec(RT_carriers%nstates),data_fit(2,11),GAMMA_holes,GAMMA_elecs
 real(SP)              :: err_fit(4,n_T_steps),err_fit_gamma(4,n_T_steps),err_fit_spin(4,n_T_steps)
 real(SP)              :: T_fit(2,n_T_steps),T_fit_gamma(4,n_T_steps),T_fit_spin(4,n_T_steps)
 real(SP)              :: Ef_fit(2,n_T_steps),Ef_fit_gamma(4,n_T_steps),Ef_fit_spin(4,n_T_steps)
 !
 real(SP),allocatable  :: spin_factors(:,:,:)
 !
 logical  :: l_ELPH_lifetimes
 !
 integer  :: i_fit,i_fit_kind,i_RT,ib,ik,i_spinor,i_T,i1,i2,n1,n2,n3,igamma,n_gamma,i_ratio,n_ratio,n_fits_done
 integer  :: nfound(3),nfound_gamma(5),nfound_spin(5)
 integer  :: E_sort_index(RT_carriers%nstates)
 !
 character(10)     ::titles(max(n_T_steps+4,11)),data_ch(n_T_steps+4,5,2),columns(n_T_steps+4),tmp_titles(12)
 character(schlen) ::output_fname(11),gamma_fname(7),gamma_ratio_fname(2),GRP_string
 !
 l_ELPH_lifetimes=l_elph_scatt.and.l_RT_CCA_Kernel
 !
 RT_E_sorted=RT_carriers%E_bare*HA2EV
 call sort(RT_E_sorted,indx=E_sort_index)
 !
 RT_gamma=0._SP
 !
 T_fit=0._SP
 Ef_fit=0._SP
 nfound=0
 !
 T_fit_spin=0._SP
 Ef_fit_spin=0._SP
 nfound_spin=0
 !
 T_fit_gamma=0._SP
 Ef_fit_gamma=0._SP
 nfound_gamma=0
 !
 filled_tresh=1.E-7
 !
 if (n_spinor==2) then
   allocate(spin_factors(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_spinor))
   call electrons_spin_factors(kpt,(/1,kpt%nibz/),RT_carriers%nb,spin_factors)
 endif 
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
     RT_occ(i_RT,i_T)=RT_carriers%delta_f(i_RT)
     !
     ! Spin occupations
     !==================
     if(n_spinor==2) then
       ib=RT_carriers%table(i_RT,1)
       ik=RT_carriers%table(i_RT,3)
       RT_spin_occ(i_RT,1,i_T)=RT_carriers%delta_f(i_RT)*spin_factors(ib,ik,1)
       RT_spin_occ(i_RT,2,i_T)=RT_carriers%delta_f(i_RT)*spin_factors(ib,ik,2)
     endif
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
   if(i_T>1) then
     T_fit(:,i_T) =T_fit(:,i_T-1)
     Ef_fit(:,i_T)=Ef_fit(:,i_T-1)
     T_fit_spin(:,i_T) =T_fit_spin(:,i_T-1)
     Ef_fit_spin(:,i_T)=Ef_fit_spin(:,i_T-1)
     T_fit_gamma(:,i_T) =T_fit_gamma(:,i_T-1)
     Ef_fit_gamma(:,i_T)=Ef_fit_gamma(:,i_T-1)
   endif
   !
   ! Occupations FIT
   !
   nfound(3)=Fermi_fnc_fit(RT_occ(:,i_T),RT_carriers%nstates,T_fit(:,i_T),Ef_fit(:,i_T),err_fit(:,i_T),&
&                            RT_occ_fit_out=RT_occ_fit(:,:,i_T),RT_E_fit_out=RT_E_fit)
   !
   if(nfound(3)==1) nfound(1)  =nfound(1)+1
   if(nfound(3)==2) nfound(2)  =nfound(2)+1
   if(nfound(3)==3) nfound(1:2)=nfound(1:2)+1
   !
   if(n_spinor==2) then
     !
     ! Spin occupations FIT
     !
     nfound_spin(5)=Fermi_fnc_fit(RT_spin_occ(:,1,i_T),RT_carriers%nstates,&
&                                 T_fit_spin(1:2,i_T),Ef_fit_spin(1:2,i_T),err_fit_spin(:,i_T), &
&                                 RT_occ_fit_out=RT_occ_spin_fit(:,:,1,i_T))
     !
     if(nfound_spin(5)==1) nfound_spin(1)  =nfound_spin(1)+1
     if(nfound_spin(5)==2) nfound_spin(2)  =nfound_spin(2)+1
     if(nfound_spin(5)==3) nfound_spin(1:2)=nfound_spin(1:2)+1
     !
     nfound_spin(5)=Fermi_fnc_fit(RT_spin_occ(:,2,i_T),RT_carriers%nstates,&
&                                 T_fit_spin(3:4,i_T),Ef_fit_spin(3:4,i_T),err_fit_spin(:,i_T), &
&                                 RT_occ_fit_out=RT_occ_spin_fit(:,:,2,i_T))
     !
     if(nfound_spin(5)==1) nfound_spin(3)  =nfound_spin(3)+1
     if(nfound_spin(5)==2) nfound_spin(4)  =nfound_spin(4)+1
     if(nfound_spin(5)==3) nfound_spin(3:4)=nfound_spin(3:4)+1
     !
   endif
   !
   n_ratio=0
   !
   if(n_gamma<2) cycle
   n_ratio=1
   !
   ! Lifetimes ratio FIT
   !
   do i_RT=1,RT_carriers%nstates
     !
     if(n_gamma==2) then
       GAMMA_holes=RT_gamma(i_RT,1,i_T)
       GAMMA_elecs=RT_gamma(i_RT,2,i_T)
     else if(n_gamma> 2) then
       GAMMA_holes=RT_gamma(i_RT,1,i_T)+RT_gamma(i_RT,3,i_T)
       GAMMA_elecs=RT_gamma(i_RT,2,i_T)+RT_gamma(i_RT,4,i_T)
     endif
     !
     if(RT_carriers%E_bare(i_RT)<=0._SP) RT_gamma_ratio(i_RT,1,i_T)=-GAMMA_elecs/(GAMMA_elecs+GAMMA_holes)*spin_occ
     if(RT_carriers%E_bare(i_RT)> 0._SP) RT_gamma_ratio(i_RT,1,i_T)= GAMMA_holes/(GAMMA_elecs+GAMMA_holes)*spin_occ
     !
   enddo
   !
   nfound_gamma(5)=Fermi_fnc_fit(RT_gamma_ratio(:,1,i_T),RT_carriers%nstates,&
&                                 T_fit_gamma(1:2,i_T),Ef_fit_gamma(1:2,i_T),err_fit_gamma(:,i_T), &
&                                 RT_occ_fit_out=RT_gamma_ratio_fit(:,:,1,i_T))
   !
   if(nfound_gamma(5)==1) nfound_gamma(1)  =nfound_gamma(1)+1
   if(nfound_gamma(5)==2) nfound_gamma(2)  =nfound_gamma(2)+1
   if(nfound_gamma(5)==3) nfound_gamma(1:2)=nfound_gamma(1:2)+1
   !
   if(n_gamma<=6) cycle
   n_ratio=2
   !
   do i_RT=1,RT_carriers%nstates
     !
     GAMMA_holes=RT_gamma(i_RT,5,i_T)
     GAMMA_elecs=RT_gamma(i_RT,6,i_T)
     !
     if(RT_carriers%E_bare(i_RT)<=0._SP) RT_gamma_ratio(i_RT,2,i_T)=-GAMMA_elecs/(GAMMA_elecs+GAMMA_holes)*spin_occ
     if(RT_carriers%E_bare(i_RT)> 0._SP) RT_gamma_ratio(i_RT,2,i_T)= GAMMA_holes/(GAMMA_elecs+GAMMA_holes)*spin_occ
     !
   enddo
   !
   nfound_gamma(5)=Fermi_fnc_fit(RT_gamma_ratio(:,2,i_T),RT_carriers%nstates,&
&                                  T_fit_gamma(3:4,i_T),Ef_fit_gamma(3:4,i_T),err_fit_gamma(:,i_T), &
&                                 RT_occ_fit_out=RT_gamma_ratio_fit(:,:,2,i_T))
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
   TMP_vec=RT_occ(E_sort_index(:),i_T)
   RT_occ(:,i_T)=TMP_vec
   do i_spinor=1,2*(n_spinor-1)
     TMP_vec=RT_spin_occ(E_sort_index(:),i_spinor,i_T)
     RT_spin_occ(:,i_spinor,i_T)=TMP_vec
   enddo
   do igamma=1,n_gamma
     TMP_vec=RT_gamma(E_sort_index(:),igamma,i_T)
     RT_gamma(:,igamma,i_T)=TMP_vec
   enddo
   do i_ratio=1,n_ratio
     TMP_vec=RT_gamma_ratio(E_sort_index(:),i_ratio,i_T)
     RT_gamma_ratio(:,i_ratio,i_T)=TMP_vec
   enddo
   !
 enddo
 !
 RT_gamma=RT_gamma*HA2EV*1000._SP
 !
 !
 ! Write to output fitted temperatures
 !====================================
 !
 call msg('s',":: Holes fitting Temperatures found " &
&          //trim(intc(nfound(1)))//"/"//trim(intc(n_T_steps)))
 call msg('s',":: Elec. fitting Temperatures found " &
&          //trim(intc(nfound(2)))//"/"//trim(intc(n_T_steps)))
 !
 if(n_spinor>1) then
   call msg('s',":: Holes spin up fitting Temperatures found " &
&            //trim(intc(nfound_spin(1)))//"/"//trim(intc(n_T_steps)))
   call msg('s',":: Elec. spin up fitting Temperatures found " &
&            //trim(intc(nfound_spin(2)))//"/"//trim(intc(n_T_steps)))
   call msg('s',":: Holes spin dw fitting Temperatures found " &
&            //trim(intc(nfound_spin(3)))//"/"//trim(intc(n_T_steps)))
   call msg('s',":: Elec. spin dw fitting Temperatures found " &
&            //trim(intc(nfound_spin(4)))//"/"//trim(intc(n_T_steps)))
 endif
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
 tmp_titles(2:3)=(/'(e)   ','(h)   '/)
 n1=3
 if(n_spinor==2) then
   tmp_titles(4:7)=(/'(e-up)','(h-up)','(e-dw)','(h-dw)'/)
   n1=n1+4
 endif
 !
 ! Only El-El
 if(n_gamma==2) then
   gamma_fname(1)='YPP-RT_EE_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EE_Elec_linewidths'
   gamma_ratio_fname(1)='YPP-RT_EE_widths_ratio'
   tmp_titles(n1+1:n1+2)=(/'(e-EE)','(h-EE)'/)   
   n1=n1+2
   n_ratio=1
 ! Only El-Ph
 else if (n_gamma==4) then
   gamma_fname(1)='YPP-RT_EP_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EP_Elec_linewidths'
   gamma_fname(3)='YPP-RT_EP_Hole_abs_linewidths'
   gamma_fname(4)='YPP-RT_EP_Elec_abs_linewidths'
   gamma_ratio_fname(1)='YPP-RT_EP_widths_ratio'
   tmp_titles(n1+1:n1+2)=(/'(e-EP)','(h-EP)'/)   
   n1=n1+2
 ! Both
 else if (n_gamma==6) then
   gamma_fname(1)='YPP-RT_EP_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EP_Elec_linewidths'
   gamma_fname(3)='YPP-RT_EP_Hole_abs_linewidths'
   gamma_fname(4)='YPP-RT_EP_Elec_abs_linewidths'
   gamma_fname(5)='YPP-RT_EE_Hole_linewidths'
   gamma_fname(6)='YPP-RT_EE_Elec_linewidths'
   gamma_ratio_fname(1)='YPP-RT_EP_widths_ratio'
   gamma_ratio_fname(2)='YPP-RT_EE_widths_ratio'
   tmp_titles(n1+1:n1+4)=(/'(e-EP)','(h-EP)','(e-EE)','(h-EE)'/)   
   n1=n1+4
 endif
 !
 output_fname(1)='YPP-RT_Temperatures'
 output_fname(2)='YPP-RT_E_Fermi'
 !
 call of_open_close(trim(output_fname(1)),'ot')
 call RT_write_descriptions(trim(output_fname(1)))
 call of_open_close(trim(output_fname(2)),'ot')
 call RT_write_descriptions(trim(output_fname(2)))
 !
 do i1=2,n1
   titles(i1)='T'//trim(tmp_titles(i1))
 enddo
 call msg('o '//trim(output_fname(1)),'#')
 call msg('o '//trim(output_fname(1)),'# Fitted temperatures [K]')
 call msg('o '//trim(output_fname(1)),'#')
 call msg('o '//trim(output_fname(1)),'#',(/titles(1:n1)/),INDENT=0,USE_TABS=.true.)
 call msg('o '//trim(output_fname(1)),'#')
 !
 do i1=2,n1
   titles(11)='Ef'//trim(tmp_titles(i1))
 enddo
 call msg('o '//trim(output_fname(2)),'#')
 call msg('o '//trim(output_fname(2)),'# Fitted Fermi levels [eV]')
 call msg('o '//trim(output_fname(2)),'#')
 call msg('o '//trim(output_fname(2)),'#',(/titles(1:n1)/),INDENT=0,USE_TABS=.true.)
 call msg('o '//trim(output_fname(2)),'#')
 !
 do i_T=1,n_T_steps
   !
   data_fit(1:2,:)= RT_time(i_T)*AUT2FS
   !
   do i1=1,2
     data_fit(1,i1+1)= T_fit(i1,i_T)*HA2KEL
     data_fit(2,i1+1)= Ef_fit(i1,i_T)*HA2EV
   enddo
   !
   n1=3
   if(n_spinor==2) then
     do i1=1,4
       data_fit(1,i1+n1)= T_fit_spin(i1,i_T)*HA2KEL
       data_fit(2,i1+n1)= Ef_fit_spin(i1,i_T)*HA2EV
     enddo
     n1=n1+4
   endif
   !
   if(n_gamma>=2) then
     do i1=1,2
       data_fit(1,i1+n1)= T_fit_gamma(i1,i_T)*HA2KEL
       data_fit(2,i1+n1)= Ef_fit_gamma(i1,i_T)*HA2EV
     enddo
     n1=n1+2
   endif
   !
   if(n_gamma==6) then
     do i1=1,2
       data_fit(1,i1+n1)= T_fit_gamma(i1,i_T)*HA2KEL
       data_fit(2,i1+n1)= Ef_fit_gamma(i1,i_T)*HA2EV
     enddo
     n1=n1+2
   endif
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
 ! Write to output distributions and fits
 !========================================
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
   titles(i_T+1)=' @'//trim(real2ch( RT_time(i_T)*AUT2FS ))
   data_ch(i_T+1,1,:)= trim(real2ch( RT_time(i_T)*AUT2FS ))
   do i_fit=1,2
     data_ch(i_T+1,2,i_fit)=   trim(real2ch( T_fit(i_fit,i_T)*HA2KEL ))
     data_ch(i_T+1,3,i_fit)=   trim(real2ch( Ef_fit(i_fit,i_T)*HA2EV ))
     data_ch(i_T+1,4,i_fit)=   trim(real2ch( err_fit(i_fit,i_T) ))
     data_ch(i_T+1,5,i_fit)=   trim(real2ch( err_fit(i_fit+2,i_T) ))
   enddo
 enddo
 !
 do i2=1,n_T_steps,nmax_file_report
   !
   ! Titles
   !========
   !
   n1=i2
   n2=min(n_T_steps,i2+15)
   !
   GRP_string="_"//trim(intc(i2/nmax_file_report+1))//"_of_"//trim(intc(n_T_steps/nmax_file_report+1))
   !
   output_fname(1)='YPP-RT_occupations_DATA'
   !
   if (n_T_steps>nmax_file_report) output_fname(1)=trim(output_fname(1))//trim(GRP_string)
   call of_open_close(trim(output_fname(1)),'ot')
   call RT_write_descriptions(trim(output_fname(1)))
   !
   call msg('o '//trim(output_fname(1)),'#')
   call msg('o '//trim(output_fname(1)),'# Occupations distribution at different times (f@ time)')
   call msg('o '//trim(output_fname(1)),'#')
   call msg('o '//trim(output_fname(1)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname(1)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname(1)),'#')
   n3=1
   !
   do i_spinor=1,2*(n_spinor-1)
     !
     if(i_spinor==1) output_fname(i_spinor+1)='YPP-RT_occupations_up_DATA'
     if(i_spinor==2) output_fname(i_spinor+1)='YPP-RT_occupations_dw_DATA'
     !
     if (n_T_steps>nmax_file_report) output_fname(i_spinor+1)=trim(output_fname(i_spinor+1))//trim(GRP_string)
     call of_open_close(trim(output_fname(i_spinor+1)),'ot')
     call RT_write_descriptions(trim(output_fname(i_spinor+1)))
     !
     call msg('o '//trim(output_fname(i_spinor+1)),'#')
     if(i_spinor==1) call msg('o '//trim(output_fname(i_spinor+1)),&
&                             '# Spin up occupations distribution at different times (f@ time)')
     if(i_spinor==2) call msg('o '//trim(output_fname(i_spinor+1)),&
&                             '# Spin dw occupations distribution at different times (f@ time)')
     call msg('o '//trim(output_fname(i_spinor+1)),'#')
     call msg('o '//trim(output_fname(i_spinor+1)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(i_spinor+1)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(i_spinor+1)),'#')
     !
     n3=i_spinor+1
     !
   enddo
   !
   do igamma=1,n_gamma
     !
     if (Bose_Temp==0._SP .and. igamma>2 .and. igamma<5) cycle 
     !
     output_fname(igamma+n3)=trim(gamma_fname(igamma))
     if (n_T_steps>nmax_file_report) output_fname(igamma+n3)=trim(gamma_fname(igamma))//trim(GRP_string)
     call of_open_close(trim(output_fname(igamma+n3)),'ot')
     call RT_write_descriptions(trim(output_fname(igamma+n3)))
     !
     call msg('o '//trim(output_fname(igamma+n3)),'#')
     call msg('o '//trim(output_fname(igamma+n3)),'# Lifetimes distribution [meV] at different times (Gamma@ time)')
     call msg('o '//trim(output_fname(igamma+n3)),'#')
     call msg('o '//trim(output_fname(igamma+n3)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(igamma+n3)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(igamma+n3)),'#')
     !
   enddo
   !
   do i_ratio=1,n_ratio
     !
     output_fname(n_gamma+n3+i_ratio)=trim(gamma_ratio_fname(i_ratio))//"_DATA"
     if (n_T_steps>nmax_file_report) output_fname(n_gamma+n3+i_ratio)=trim(gamma_ratio_fname(i_ratio))//trim(GRP_string)
     call of_open_close(trim(output_fname(igamma+n3)),'ot')
     call RT_write_descriptions(trim(output_fname(igamma+n3)))
     !
     call msg('o '//trim(output_fname(igamma+n3)),'#')
     call msg('o '//trim(output_fname(igamma+n3)),'# Lifetimes ratio distribution at different times (Gamma_ratio@ time)')
     call msg('o '//trim(output_fname(igamma+n3)),'#')
     call msg('o '//trim(output_fname(igamma+n3)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(igamma+n3)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(igamma+n3)),'#')
     !
   enddo
   !
   ! Data
   !======
   !
   do i_RT=1,RT_carriers%nstates
     !
     !ib    =RT_carriers%table(i_RT,1)
     !ik    =RT_carriers%table(i_RT,2)
     !ik_RT =RT_carriers%table(i_RT,3)
     !i_spin=RT_carriers%table(i_RT,4)
     !
     call msg('o '//trim(output_fname(1)),'',(/RT_E_sorted(i_RT),RT_occ(i_RT,n1:n2)/),INDENT=-2,USE_TABS=.true.)
     !
     do i_spinor=1,2*(n_spinor-1)
       call msg('o '//trim(output_fname(i_spinor+1)),'',(/RT_E_sorted(i_RT),RT_spin_occ(i_RT,i_spinor,n1:n2)/),&
&                                                       INDENT=-2,USE_TABS=.true.)
     enddo
     !
     do igamma=1,n_gamma
       if (Bose_Temp==0._SP .and. igamma>2 .and. igamma<5) cycle
       call msg('o '//trim(output_fname(igamma+n3)),'',(/RT_E_sorted(i_RT),RT_gamma(i_RT,igamma,n1:n2)/),&
&                                                       INDENT=-2,USE_TABS=.true.)
     enddo
     !
     do i_ratio=1,n_ratio
       call msg('o '//trim(output_fname(n_gamma+n3+i_ratio)),'',(/RT_E_sorted(i_RT),RT_gamma_ratio(i_RT,i_ratio,n1:n2)/),&
&                                                       INDENT=-2,USE_TABS=.true.)
     enddo
     !
   enddo
   !
   call of_open_close(trim(output_fname(1)))
   do i_spinor=1,2*(n_spinor-1)
     call of_open_close(trim(output_fname(i_spinor+1)))
   enddo
   do igamma=1,n_gamma
     if (Bose_Temp==0._SP .and. igamma>2 .and. igamma<5) cycle
     call of_open_close(trim(output_fname(igamma+n3)))
   enddo
   do i_ratio=1,n_ratio
     call of_open_close(trim(output_fname(n_gamma+n3+i_ratio)))
   enddo
   !
   ! FIT
   !=====
   !
   do i_fit_kind=1,2
     !
     output_fname(1)='YPP-RT_occupations_FIT'
     n_fits_done=1
     if(n_spinor==2) then
       output_fname(2)='YPP-RT_occupations_up_FIT'
       output_fname(3)='YPP-RT_occupations_dw_FIT'
       n_fits_done=n_fits_done+2
     endif
     do i_ratio=1,n_ratio
       output_fname(n_fits_done+i_ratio)=trim(gamma_ratio_fname(i_ratio))//'_FIT'
       n_fits_done=n_fits_done+1
     enddo
     !
     do i_fit=1,n_fits_done
       !
       if(i_fit_kind==1) output_fname(i_fit)=trim(output_fname(i_fit))//'_holes'
       if(i_fit_kind==2) output_fname(i_fit)=trim(output_fname(i_fit))//'_electrons'
       if (n_T_steps>nmax_file_report) output_fname(i_fit)=trim(output_fname(i_fit))//trim(GRP_string)
       !
       call of_open_close(trim(output_fname(i_fit)),'ot')
       call RT_write_descriptions(trim(output_fname(i_fit)))
       !
       call msg('o '//trim(output_fname(i_fit)),'#')
       call msg('o '//trim(output_fname(i_fit)),'# Results of the FIT')
       call msg('o '//trim(output_fname(i_fit)),'#')
       do i1=1,5
         call msg('o '//trim(output_fname(i_fit)),'#',(/data_ch(1,i1,i_fit_kind), &
&                                            data_ch(n1+1:n2+1,i1,i_fit_kind)/), INDENT=-2,USE_TABS=.true.)
       enddo
       !     
       call msg('o '//trim(output_fname(i_fit)),'#')
       call msg('o '//trim(output_fname(i_fit)),'# Fit functions at different time (f@ time)')
       call msg('o '//trim(output_fname(i_fit)),'#')
       call msg('o '//trim(output_fname(i_fit)),'#',(/columns(1:n2-n1+2)/),INDENT=-2,USE_TABS=.true.)
       call msg('o '//trim(output_fname(i_fit)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
       call msg('o '//trim(output_fname(i_fit)),'#')
       !
       do i1=1,fit_dim
         if(i_fit==1)                               call msg('o '//trim(output_fname(i_fit)),'',&
&                (/RT_E_fit(i1,i_fit_kind)*HA2EV,RT_occ_fit(i1,i_fit_kind,n1:n2)/),INDENT=-2,USE_TABS=.true.)
         if((i_fit==2.or.i_fit==3).and.n_spinor==2) call msg('o '//trim(output_fname(i_fit)),'',&
&                (/RT_E_fit(i1,i_fit_kind)*HA2EV,RT_occ_spin_fit(i1,i_fit_kind,i_fit-1,n1:n2)/),INDENT=-2,USE_TABS=.true.)
         if((i_fit==2.or.i_fit==3).and.n_spinor==1) call msg('o '//trim(output_fname(i_fit)),'',&
&                (/RT_E_fit(i1,i_fit_kind)*HA2EV,RT_gamma_ratio_fit(i1,i_fit_kind,i_fit-1,n1:n2)/),INDENT=-2,USE_TABS=.true.)
         if (i_fit==4.or.i_fit==5)                  call msg('o '//trim(output_fname(i_fit)),'',&
&                (/RT_E_fit(i1,i_fit_kind)*HA2EV,RT_gamma_ratio_fit(i1,i_fit_kind,i_fit-3,n1:n2)/),INDENT=-2,USE_TABS=.true.)
       enddo
       !
       call of_open_close(trim(output_fname(i_fit)))
       !
     enddo
     !
   enddo
 enddo
 !
 if (n_spinor==2) deallocate(spin_factors)
 !
end subroutine RT_occ_energy_plot
