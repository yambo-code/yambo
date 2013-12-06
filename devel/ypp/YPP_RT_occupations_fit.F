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
subroutine YPP_RT_occupations_FIT(RT_E,RT_occ,n_T_steps)
 ! 
 ! FIT the occupations with a Gauss–Newton algorithm
 !
 use units,          ONLY:HA2EV,HA2KEL,FS2AUT
 use pars,           ONLY:SP,lchlen
 use YPP,            ONLY:output_fname,OCC_T_range,OCC_deltaT
 use SC,             ONLY:SC_bands,n_SC_descriptions,SC_description, &
&                         RT_nk,RT_nstates,RT_ibz_coo
 use QP_m,           ONLY:QP_table,QP_n_states
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin
 use vec_operate,    ONLY:sort
 use functions,      ONLY:Fermi_fnc,Fermi_fnc_derivative,Fermi_fnc_T_derivative
 use com,            ONLY:msg,of_open_close,error
 use stderr,         ONLY:intc,real2ch
 !
 implicit none
 !
 integer               :: n_T_steps
 real(SP)              :: RT_E(RT_nstates)
 real(SP)              :: RT_occ(RT_nstates,n_T_steps)
 !
 integer, parameter    :: max_iter=10000,fit_dim=1000,nmax_file_report=16
 real(SP)              :: RT_occ_fit(fit_dim,n_T_steps,2),RT_E_fit(fit_dim,2)
 !
 ! Work Space
 !
 real(SP)              :: io_Time
 real(SP)              :: RT_occ_tmp(RT_nstates)
 real(SP)              :: err_fit(n_T_steps,2)
 real(SP)              :: T_fit(n_T_steps,2)
 real(SP)              :: Ef_fit(n_T_steps,2)
 ! 
 integer  :: ifit,i_s,i_RT,ik,ik_rim,ib,i1,i_T,i2,n1,n2,i_fp,i_spin
 integer  :: nfound(3)
 integer  :: E_sort_index(RT_nstates)
 !
 character(10)     ::titles(n_T_steps+1),data_ch(n_T_steps+1,4,2)
 !
 integer, external :: Fermi_fnc_fit
 !
 T_fit=0._SP
 Ef_fit=0._SP
 nfound=0
 !
 call sort(RT_E,indx=E_sort_index)
 do i1=1,n_T_steps
   RT_occ_tmp(:)=RT_occ(E_sort_index(:),i1)
   RT_occ(:,i1)=RT_occ_tmp(:)
 enddo
 !
 do i1=1,n_T_steps
   !
   if(i1>1) then
     T_fit(i1,:) =T_fit(i1-1,:)
     Ef_fit(i1,:)=Ef_fit(i1-1,:)
   endif
   !
   nfound(3)=Fermi_fnc_fit(RT_E,RT_occ(:,i1),RT_E_fit,RT_occ_fit(:,i1,:),T_fit(i1,:),Ef_fit(i1,:),err_fit(i1,:),.false.)
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
 titles(1)='   [eV]   '
 data_ch(1,1,:)=' t    [fs]'
 data_ch(1,2,1)=' T(h) [K]'
 data_ch(1,2,2)=' T(e) [K]'
 data_ch(1,3,:)=' Ef   [eV]'
 data_ch(1,4,:)=' error'
 !
 do i_T=1,n_T_steps
   io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   titles(i_T+1)=' f@'//trim(real2ch( io_Time/FS2AUT ))
   data_ch(i_T+1,1,:)= trim(real2ch( io_Time/FS2AUT ))
   do ifit=1,2
     data_ch(i_T+1,2,ifit)=   trim(real2ch( T_fit(i_T,ifit)*HA2KEL ))
     data_ch(i_T+1,3,ifit)=   trim(real2ch( Ef_fit(i_T,ifit)*HA2EV ))
     data_ch(i_T+1,4,ifit)=   trim(real2ch( err_fit(i_T,ifit) ))
   enddo
 enddo
 !
 do i2=1,n_T_steps,nmax_file_report
   n1=i2
   n2=min(n_T_steps,i2+15)
   !
   output_fname='YPP-RT_occupation_distribution_DATA'
   if(n_T_steps>nmax_file_report) output_fname=trim(output_fname)//'_grp'//trim(intc(i2/nmax_file_report+1))
   !
   call of_open_close(trim(output_fname),'ot')
   !
   do i_fp=1,n_SC_descriptions
     call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
   enddo
   !
   call msg('o '//trim(output_fname),'#')
   call msg('o '//trim(output_fname),'# Occupations distribution at different times (f@ time)')
   call msg('o '//trim(output_fname),'#')
   call msg('o '//trim(output_fname),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
   call msg('o '//trim(output_fname),'#')
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
       call msg('o '//trim(output_fname),'',(/RT_E(i_RT)*HA2EV,&
    &            RT_occ(i_RT,n1:n2)/),INDENT=-2,USE_TABS=.true.)
       !
     enddo
     !
   enddo
   !
   call of_open_close(trim(output_fname))
   !
   do ifit=1,2
     !
     output_fname='YPP-RT_occupation_distribution_FIT'
     if(ifit==1) output_fname=trim(output_fname)//'_hole'
     if(ifit==2) output_fname=trim(output_fname)//'_electrons'
     if(n_T_steps>nmax_file_report) &
&      output_fname=trim(output_fname)//'_grp'//trim(intc(i2/nmax_file_report+1))
     !
     call of_open_close(trim(output_fname),'ot')
     !
     do i_fp=1,n_SC_descriptions
       call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
     enddo
     !
     call msg('o '//trim(output_fname),'#')
     call msg('o '//trim(output_fname),'# Results of the FIT')
     do i1=1,4
       call msg('o '//trim(output_fname),'#',(/data_ch(1,i1,ifit), &
&                                            data_ch(n1+1:n2+1,i1,ifit)/), INDENT=-2,USE_TABS=.true.)
     enddo
     !     
     call msg('o '//trim(output_fname),'#')
     call msg('o '//trim(output_fname),'# Fit functions at different time (f@ time)')
     call msg('o '//trim(output_fname),'#')
     call msg('o '//trim(output_fname),'#',(/titles(1),titles(n1+1:n2+1)/),INDENT=-2,USE_TABS=.true.)
     call msg('o '//trim(output_fname),'#')
     !
     do i1=1,fit_dim
       call msg('o '//trim(output_fname),'',(/RT_E_fit(i1,ifit)*HA2EV,&
  &               RT_occ_fit(i1,n1:n2,ifit)/),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     call of_open_close(trim(output_fname))
     !
   enddo
 enddo
 !
end subroutine YPP_RT_occupations_fit
