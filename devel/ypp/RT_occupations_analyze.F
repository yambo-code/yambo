! 
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_occupations_analyze(k,E)
 !
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use units,          ONLY:FS2AUT,SP,HA2EV
 use pars,           ONLY:schlen
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc,set_real_printed_length
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,spin_occ
 use QP_m,           ONLY:QP_table,QP_n_states
 use YPP,            ONLY:output_fname,OCC_T_range,OCC_deltaT,l_RT_occ_FIT,l_RT_occ_Plot
 use com,            ONLY:msg,of_open_close,error
 use IO_m,           ONLY:NONE
 use QP_CTL_m,       ONLY:QP_apply
 use rt_ctl,         ONLY:RT_OCC_LIFE_and_REF_IO
 use real_time,      ONLY:E_P_Hole_lifetime,E_P_Electron_lifetime,&
&                         E_P_Ho_abs_lifetime,E_P_El_abs_lifetime, & 
&                         l_RT_CCA_Kernel,E_E_Hole_lifetime,E_E_Electron_lifetime,&
&                         RIM_ibz_coo,RIM_bz_coo,l_RT_uses_E_RIM,RT_delta0_occupations,&
&                         RT_E_occupations_reference,REF_lifetime,&
&                         RT_nk,RT_nstates,RT_ibz_coo,RT_Nk_ibz,RT_all2ibz
 use SC,             ONLY:SC_bands
 !
 implicit none
 !
 type(bz_samp) :: k
 type(levels)  :: E
 !
 ! Work Space
 !
 integer               :: ik,ib,i_spin,i_s,i_RT,i_T,n_T_steps,n1
 integer               :: ik_rim,ik_E_rim,ik_RT_rim
 real(SP)              :: io_Time,data_to_plot(8)
 real(SP), allocatable :: RT_occupations_sum(:,:),RT_occ(:,:),RT_delta0_occ(:,:),RT_gamma(:,:,:),RT_E(:)
 character(schlen)     :: titles(8),specs(8,2)
 logical               :: l_ELPH_lifetimes,l_group_occ
 integer,     external :: RT_occ_check_and_load_ref
 !
 call section('*','Real-Time occupations')
 !
 n_T_steps=RT_occ_check_and_load_ref(E,k)
 !
 ! RIM support of carrier dynamics
 !=================================
 call RT_check_RIM(k,E)
 !
 call QP_apply(SC_bands,E,k,'G',msg_fmt='s')
 !
 call parser('OCCgroup',l_group_occ)
 !
 RT_nstates=RT_nk*(SC_bands(2)-SC_bands(1)+1)
 allocate(RT_E(RT_nstates))
 !
 ! Dump RIM energies in E
 !
 i_RT=0
 do i_s=1,QP_n_states
   !
   ib    =QP_table(i_s,1)
   ik    =QP_table(i_s,3)
   i_spin=spin(QP_table(i_s,:))
   !
   if (ib<SC_bands(1)) cycle
   if (ib>SC_bands(2)) cycle
   !
   do ik_rim=1,RIM_ibz_coo(ik,2)-RIM_ibz_coo(ik,1)+1
     ik_RT_rim=RT_ibz_coo(ik,1)+ik_rim-1
     ik_E_rim =RIM_ibz_coo(ik,1)+ik_rim-1
     if (ik_RT_rim>RT_nk) cycle
     i_RT=i_RT+1
     if(     l_RT_uses_E_RIM) RT_E(i_RT)=E%E_RIM(ib,ik_E_rim,1)
     if(.not.l_RT_uses_E_RIM) RT_E(i_RT)=E%E(ib,ik_E_rim,1)
   enddo
   !
 enddo
 !
 l_ELPH_lifetimes=l_elph_scatt.and.l_RT_CCA_Kernel
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 ! SET titles
 ! 
 titles(1)='T[fs]'
 titles(2)='Occ.'
 !
 if (l_elel_scatt.and.l_ELPH_lifetimes) then
   titles(3:6)=(/'Hole[meV]','El. [meV]','Hole[meV]','El. [meV]'/)
   titles(7:8)=(/'Hole[meV]','El. [meV]'/)
 else if (l_elel_scatt.and..not.l_ELPH_lifetimes) then
   titles(3:4)=(/'Hole[meV]','El. [meV]'/)
 else if (.not.l_elel_scatt.and.l_ELPH_lifetimes) then
   titles(3:6)=(/'Hole[meV]','El. [meV]','Hole[meV]','El. [meV]'/)
 endif
 !
 specs(1:2,1)=(/'         ','         '/)
 specs(3:6,1)=(/'|--------','------El-','Ph-------','--------|'/)
 specs(7:8,1)=(/'|-----El-','El------|'/)
 !
 specs(1:2,2)=(/'         ','         '/)
 specs(3:6,2)=(/'(spontan ','emission)','   (absor','ption)   '/)
 specs(7:8,2)=(/'         ','         '/)
 !
 ! F(it) for each (ik,ib)
 !
 if(l_RT_occ_Plot) then
   allocate(RT_occupations_sum(3,n_T_steps)) 
   RT_occupations_sum=0._SP
 endif
 !
 allocate(RT_occ(RT_nstates,n_T_steps),RT_delta0_occ(RT_nstates,n_T_steps),RT_gamma(RT_nstates,n_T_steps,6))
 RT_occ  =0._SP
 RT_gamma=0._SP
 !
 do i_T=1,n_T_steps
   !
   io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   !
   call RT_OCC_LIFE_and_REF_IO(E,'G',k=k,Time=io_Time)
   !
   i_RT=0
   !
   do i_s=1,QP_n_states
     !
     ! n   =QP_table(i_s,1)
     ! k   =QP_table(i_s,3)
     ! sp  =QP_table(i_s,4)
     !
     ib    =QP_table(i_s,1)
     ik    =QP_table(i_s,3)
     i_spin=spin(QP_table(i_s,:))
     !
     if (ib<SC_bands(1)) cycle
     if (ib>SC_bands(2)) cycle
     !
     if(l_RT_occ_Plot) then
       output_fname='YPP-RT_occupations_k'//trim(intc(ik))//'_b'//trim(intc(ib))
       if (i_T==1) call open_file()
     endif
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       if (l_RT_occ_plot.and. .not.l_group_occ .and. ik_rim>RT_ibz_coo(ik,1)) cycle
       !
       i_RT=i_RT+1
       !
       if (.not.l_group_occ .or. (ik_rim==RT_ibz_coo(ik,1).and.l_group_occ)) data_to_plot(2:)=0._SP
       !
       ! Lifetimes
       !===========
       n1=1
       if (l_ELPH_lifetimes) then
         n1=7
         RT_gamma(i_RT,i_T,1)=E_P_Hole_lifetime(ib,ik_rim)+REF_lifetime(ib,ik_rim,3)
         RT_gamma(i_RT,i_T,2)=E_P_Electron_lifetime(ib,ik_rim)+REF_lifetime(ib,ik_rim,4)
         RT_gamma(i_RT,i_T,3)=E_P_Ho_abs_lifetime(ib,ik_rim)+REF_lifetime(ib,ik_rim,5)
         RT_gamma(i_RT,i_T,4)=E_P_El_abs_lifetime(ib,ik_rim)+REF_lifetime(ib,ik_rim,6)
       endif
       !
       if (l_elel_scatt) then
         n1=3
         if(l_ELPH_lifetimes) n1=7
         RT_gamma(i_RT,i_T,n1-2)=E_E_Hole_lifetime(ib,ik_rim)+REF_lifetime(ib,ik_rim,1)
         RT_gamma(i_RT,i_T,n1-1)=E_E_Electron_lifetime(ib,ik_rim)+REF_lifetime(ib,ik_rim,2)
       endif
       !
       RT_gamma(i_RT,i_T,:)=RT_gamma(i_RT,i_T,:)*HA2EV*1000.
       RT_delta0_occ(i_RT,i_T)=RT_delta0_occupations(ib,ik_rim)
       RT_occ(i_RT,i_T)=RT_E_occupations_reference(ib,ik)+RT_delta0_occ(i_RT,i_T)
       !
       if (.not.l_RT_occ_plot) cycle
       !
       data_to_plot(3:8)=data_to_plot(3:8)+RT_gamma(i_RT,i_T,1:6)
       !
       if(ik_rim==RT_ibz_coo(ik,2).and.l_group_occ) data_to_plot(3:)=data_to_plot(3:)/real(RT_NK_ibz(ik),SP)
       !
       ! Occupations
       !=============
       RT_occupations_sum(3,i_T)=RT_occupations_sum(3,i_T)+RT_delta0_occ(i_RT,i_T)*k%weights(ik)/real(RT_Nk_ibz(ik),SP)
       !
       if (ib==SC_bands(1).and.SC_bands(1)>1) RT_occupations_sum(3,i_T)=RT_occupations_sum(3,i_T)+ &
&                                             spin_occ*k%weights(ik)*real(SC_bands(1)-1)/real(RT_Nk_ibz(ik),SP)
       !
       data_to_plot(1)=io_TIME/FS2AUT
       data_to_plot(2)=data_to_plot(2)+ RT_delta0_occupations(ib,ik_rim) *k%weights(ik)
       !
       if(.not.l_group_occ .or. (ik_rim==RT_ibz_coo(ik,2).and.l_group_occ) ) then
         data_to_plot(2)=data_to_plot(2)/real(RT_Nk_ibz(ik),SP)
         if (ib<=E%nbf) RT_occupations_sum(1,i_T)=RT_occupations_sum(1,i_T)+data_to_plot(2)
         if (ib> E%nbf) RT_occupations_sum(2,i_T)=RT_occupations_sum(2,i_T)+data_to_plot(2)
       endif
       !
       if(.not.l_group_occ .or. (ik_rim==RT_ibz_coo(ik,2).and.l_group_occ) ) then
         call msg('o '//trim(output_fname),'',data_to_plot(:n1+1),INDENT=-2,USE_TABS=.true.)
         if (i_T==n_T_steps) call of_open_close(trim(output_fname))
       endif
       !
     enddo
   enddo
 enddo
 !
 if (l_RT_occ_plot) then
   !
   ! Sum_{ib,ik} f_{ik,ib}(it)
   !
   output_fname='YPP-RT_occupations_ALL'
   call of_open_close(trim(output_fname),'ot')
   titles(1)='T[fs]'
   titles(2)='Elec.'
   titles(3)='Holes'
   titles(4)='Tot. Occ. [OMEGA^-1]'
   call msg('o '//trim(output_fname),'#')
   call msg('o '//trim(output_fname),'#',titles(:4),INDENT=0,USE_TABS=.TRUE.)
   call msg('o '//trim(output_fname),'#')
   !
   do i_T=1,n_T_steps
     io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
     data_to_plot(:4)=(/io_TIME/FS2AUT,RT_occupations_sum(:,i_T)/)  !/(DL_vol*BO2ANG**3*1.E-24)/)
     call msg('o '//trim(output_fname),'',data_to_plot(:4),INDENT=-2,USE_TABS=.true.)
   enddo
   call of_open_close(trim(output_fname))
 endif
 !
 ! f(eps_{ik,ib}), gamma(eps_{ik,ib})
 !
 if(l_RT_occ_Fit) call RT_occupations_FIT_and_plot(RT_E,RT_occ,RT_gamma,n1-1,n_T_steps)
 !
 if (allocated(RT_E))               deallocate(RT_E)
 if (allocated(RT_occ))             deallocate(RT_occ)
 if (allocated(RT_delta0_occ))      deallocate(RT_delta0_occ)
 if (allocated(RT_gamma))           deallocate(RT_gamma)
 if (allocated(RT_occupations_sum)) deallocate(RT_occupations_sum)
 !
 if(.not.l_RT_uses_E_rim) deallocate(RIM_ibz_coo,RIM_bz_coo,RT_ibz_coo,RT_Nk_ibz)
 !
 call set_real_printed_length()
 !
 contains
   !
   subroutine open_file()
     !
     call of_open_close(trim(output_fname),'ot')
     !
     call msg('o '//trim(output_fname),'#')
     !
     ! Titles
     !========
     if (l_elel_scatt.and.l_ELPH_lifetimes) then
       call msg('o '//trim(output_fname),'#',specs(:,1),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),'#',specs(:,1),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),'#',titles(:8),INDENT=0,USE_TABS=.TRUE.)
     else if (l_elel_scatt.and..not.l_ELPH_lifetimes) then
       call msg('o '//trim(output_fname),'#',titles(:4),INDENT=0,USE_TABS=.TRUE.)
     else if (.not.l_elel_scatt.and.l_ELPH_lifetimes) then
       call msg('o '//trim(output_fname),'#',specs(:6,2),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),'#',titles(:6),INDENT=0,USE_TABS=.TRUE.)
     else if (.not.l_elel_scatt.and..not.l_ELPH_lifetimes) then
       call msg('o '//trim(output_fname),'#',titles(:2),INDENT=0,USE_TABS=.TRUE.)
     endif
     call msg('o '//trim(output_fname),'#')
     !
   end subroutine open_file
   !
end subroutine RT_occupations_analyze
