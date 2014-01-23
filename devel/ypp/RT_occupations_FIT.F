! 
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team
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
subroutine RT_occupations_FIT(RT_E,RT_occ,RT_gamma,ngamma,n_T_steps)
 ! 
 ! FIT the occupations with a Gauss–Newton algorithm
 !
 use units,          ONLY:HA2EV,HA2KEL,FS2AUT
 use pars,           ONLY:SP,lchlen,schlen
 use YPP,            ONLY:OCC_T_range,OCC_deltaT
 use SC,             ONLY:SC_bands,RT_nk,RT_nstates,RT_ibz_coo
 use QP_m,           ONLY:QP_table,QP_n_states
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin
 use vec_operate,    ONLY:sort
 use functions,      ONLY:Fermi_fnc,Fermi_fnc_derivative, &
&                         Fermi_fnc_fit,Fermi_fnc_T_derivative
 use com,            ONLY:msg,of_open_close,error
 use stderr,         ONLY:intc,real2ch
 !
 implicit none
 !
 integer               :: n_T_steps,ngamma
 real(SP)              :: RT_E(RT_nstates)
 real(SP)              :: RT_occ(RT_nstates,n_T_steps)
 real(SP)              :: RT_gamma(RT_nstates,n_T_steps,ngamma)
 !
 integer, parameter    :: max_iter=10000,fit_dim=1000,nmax_file_report=16
 real(SP)              :: RT_occ_fit(fit_dim,n_T_steps,2),RT_E_fit(fit_dim,2)
 !
 ! Work Space
 !
 real(SP)              :: io_Time
 real(SP)              :: TMP_vec(RT_nstates)
 real(SP)              :: err_fit(n_T_steps,4)
 real(SP)              :: T_fit(n_T_steps,2)
 real(SP)              :: Ef_fit(n_T_steps,2)
 ! 
 integer  :: ifit,i_s,i_RT,ik,ik_rim,ib,i1,i_T,i2,n1,n2,i_fp,i_spin,igamma
 integer  :: nfound(3)
 integer  :: E_sort_index(RT_nstates)
 !
 character(10)     ::titles(n_T_steps+1),data_ch(n_T_steps+1,5,2),columns(n_T_steps+1)
 character(schlen) ::output_fname(ngamma+1),gamma_fname(ngamma),GRP_string
 !
 T_fit=0._SP
 Ef_fit=0._SP
 nfound=0
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
 do i1=1,n_T_steps
   !
   if(i1>1) then
     T_fit(i1,:) =T_fit(i1-1,:)
     Ef_fit(i1,:)=Ef_fit(i1-1,:)
   endif
   !
   nfound(3)=Fermi_fnc_fit(RT_E,RT_occ(:,i1),RT_nstates,T_fit(i1,:),Ef_fit(i1,:),err_fit(i1,:),&
&                            RT_occ_fit_out=RT_occ_fit(:,i1,:),RT_E_fit_out=RT_E_fit)
   !
   if(nfound(3)==1) nfound(1)  =nfound(1)+1
   if(nfound(3)==2) nfound(2)  =nfound(2)+1
   if(nfound(3)==3) nfound(1:2)=nfound(1:2)+1
   !      
 enddo
 !
 call msg('s',":: Holes fitting Temperatures found " &
&          //trim(intc(nfound(1)))//"/"//trim(intc(n_T_steps)))
 call msg('s',":: Elec. fitting Temperatures found " &
&          //trim(intc(nfound(2)))//"/"//trim(intc(n_T_steps)))
 !
 do i1=1,n_T_steps
   columns(i1)="  #"//trim(intc(i1+1))
 enddo
 titles(1)='  E [eV]  '
 data_ch(1,1,:)=' t    [fs]'
 data_ch(1,2,1)=' T(h) [K]'
 data_ch(1,2,2)=' T(e) [K]'
 data_ch(1,3,:)=' Ef   [eV]'
 data_ch(1,4,:)=' Avg error'
 data_ch(1,5,:)=' Max error'
 !
 ! Only El-El
 if(ngamma==2) then
   gamma_fname(1)='YPP-RT_EE_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EE_Elec_linewidths'
 ! Only El-Ph
 else if (ngamma==4) then
   gamma_fname(1)='YPP-RT_EP_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EP_Elec_linewidths'
   gamma_fname(3)='YPP-RT_EP_Hole_abs_linewidths'
   gamma_fname(4)='YPP-RT_EP_Elec_abs_linewidths'
 ! Both
 else if (ngamma==6) then
   gamma_fname(1)='YPP-RT_EP_Hole_linewidths'
   gamma_fname(2)='YPP-RT_EP_Elec_linewidths'
   gamma_fname(3)='YPP-RT_EP_Hole_abs_linewidths'
   gamma_fname(4)='YPP-RT_EP_Elec_abs_linewidths'
   gamma_fname(5)='YPP-RT_EE_Hole_linewidths'
   gamma_fname(6)='YPP-RT_EE_Elec_linewidths'
 endif
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
   call msg('o '//trim(output_fname(1)),'#',(/'  #1',columns(1:n2-n1+1)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname(1)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname(1)),'#')
   !
   do igamma=1,ngamma
     !
     output_fname(igamma+1)=trim(gamma_fname(igamma))
     if (n_T_steps>nmax_file_report) output_fname(igamma+1)=trim(gamma_fname(igamma))//trim(GRP_string)
     call of_open_close(trim(output_fname(igamma+1)),'ot')
     !
     call msg('o '//trim(output_fname(igamma+1)),'#')
     call msg('o '//trim(output_fname(igamma+1)),'# Lifetimes distribution at different times (f@ time)')
     call msg('o '//trim(output_fname(igamma+1)),'#')
     call msg('o '//trim(output_fname(igamma+1)),'#',(/'  #1',columns(1:n2-n1+1)/),INDENT=-2,USE_TABS=.true.)
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
         call msg('o '//trim(output_fname(igamma+1)),'',(/RT_E(i_RT)*HA2EV,&
&                 RT_gamma(i_RT,n1:n2,igamma)/),INDENT=-2,USE_TABS=.true.)
       enddo
       !
     enddo
     !
   enddo
   !
   call of_open_close(trim(output_fname(1)))
   do igamma=1,ngamma
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
     call msg('o '//trim(output_fname(1)),'#',(/'  #1',columns(1:n2-n1+1)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(1)),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname(1)),'#')
     !
     do i1=1,fit_dim
       call msg('o '//trim(output_fname(1)),'',(/RT_E_fit(i1,ifit)*HA2EV,&
  &               RT_occ_fit(i1,n1:n2,ifit)/),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     call of_open_close(trim(output_fname(1)))
     !
   enddo
 enddo
 !
end subroutine RT_occupations_FIT
