! 
! Copyright (C) 2000-2014 A. Marini and the YAMBO team
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
subroutine RT_occupations_analyze(k,E)
 !
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use units,          ONLY:FS2AUT,SP,HA2EV
 use pars,           ONLY:schlen
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc,set_real_printed_length
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:nel,levels,BZ_RIM_nbands,spin,spin_occ,n_sp_pol
 use QP_m,           ONLY:QP_state,QP_table,QP_n_states
 use YPP,            ONLY:output_fname,OCC_T_range,OCC_deltaT,l_RT_Occ_FIT,l_RT_occ_Plot
 use com,            ONLY:msg,of_open_close,warning,error
 use IO_m,           ONLY:NONE
 use real_time,      ONLY:OCCUPATIONS_IO_and_interpolate,E_P_Hole_lifetime,&
&                         E_P_Electron_lifetime,E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         l_RT_CCA_Kernel,E_E_Hole_lifetime,E_E_Electron_lifetime,&
&                         RIM_ibz_coo,RIM_bz_coo,l_RT_uses_E_RIM,NE_time,RT_E_occupations
 use SC,             ONLY:SC_bands,&
&                         RT_nk,RT_nstates,RT_ibz_coo,RT_Nk_ibz,RT_all2ibz
 !
 implicit none
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer               :: ik,ib,i_spin,i_s,i_RT,i_T,n_T_steps,i_fp,n1,n_skip_time
 integer               :: ik_rim,ik_E_rim,ik_RT_rim
 real(SP)              :: io_Time,data_to_plot(10),Plot_deltaT
 real(SP), allocatable :: RT_occupations_sum(:,:),RT_occ(:,:),RT_gamma(:,:,:),RT_E(:)
 character(schlen)     :: titles(8),specs(8,2)
 logical               :: l_ELPH_lifetimes,l_group_occ
 integer,     external :: RT_occupations_check,RT_k_grid
 !
 call section('*','Real-Time occupations')
 !
 io_TIME=0.
 call OCCUPATIONS_IO_and_interpolate(E,'G',k=k,Time=io_TIME,imposed_COM=NONE)
 if (.not.allocated(RT_E_occupations)) call error('OCCUPATIONS database not found')
 !
 call parser('OCCgroup',l_group_occ)
 !
 ! RIM support of carrier dynamics: maps RIM energies in a new array with same ordering of occupations
 !=================================
 !
 if (RT_nk/=k%nibz) then
   !
   l_RT_uses_E_RIM=.true.
   !
   if (RT_k_grid(E,k)/=RT_nk) call error('Wrong RT k-grid, likely your ndb.E_RIM is changed')
   !
 else
   !
   l_RT_uses_E_rim=.false.
   !
   allocate(RIM_ibz_coo(k%nibz,2))
   allocate(RIM_bz_coo(k%nbz,2))
   allocate(RT_ibz_coo(k%nibz,2))
   allocate(RT_Nk_ibz(k%nibz))
   !
   RT_Nk_ibz=1
   allocate(RT_all2ibz(k%nibz))
   do ik=1,k%nibz
     RIM_ibz_coo(ik,:)=ik
     RT_ibz_coo(ik,:)=ik
     RT_all2ibz(ik)=ik
   enddo
   !
 endif
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
 n_T_steps=RT_occupations_check(E,k)
 !
 if (n_T_steps<0) return
 if (NE_time<OCC_T_range(2)) then
   OCC_T_range(2)=NE_time
   n_T_steps=nint(OCC_T_range(2)/OCC_deltaT)+1
 endif
 !
 call msg('s','Time range [fs]:',OCC_T_range/FS2AUT)
 call msg('s','Time step  [fs]:',OCC_deltaT/FS2AUT)
 call msg('s','Time steps     :',n_T_steps)
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
   titles(3:4)=(/'Hole[meV]','El. [meV]'/)
   titles(5:6)=(/'Habs[meV]','Eabs[meV]'/)
   titles(7:8)=(/'Hole[meV]','El. [meV]'/)
 else if (l_elel_scatt.and..not.l_ELPH_lifetimes) then
   titles(3:4 )=(/'Hole[meV]','El. [meV]'/)
 else if (.not.l_elel_scatt.and.l_ELPH_lifetimes) then
   titles(3:4)=(/'Hole[meV]','El. [meV]'/)
   titles(5:6)=(/'Habs[meV]','Eabs[meV]'/)
 endif
 !
 specs(1: 2,1)=(/'         ','         '/)
 specs(3: 6,1)=(/'|--------','--El-Ph--','---------','--------|'/)
 specs(7: 8,1)=(/'|-----El-','El------|'/)
 !
 specs(1: 2,2)=(/'         ','         '/)
 specs(3: 6,2)=(/'(   emis ','sion    )','   (absor','ption)   '/)
 specs(7: 8,2)=(/'         ','         '/)
 !
 ! F(it) for each (ik,ib)
 !
 if(l_RT_occ_Plot) then
   allocate(RT_occupations_sum(3,n_T_steps)) 
   RT_occupations_sum=0._SP
 endif
 !
 allocate(RT_occ(RT_nstates,n_T_steps),RT_gamma(RT_nstates,n_T_steps,6))
 RT_occ=0._SP
 RT_gamma=0._SP
 !
 do i_T=1,n_T_steps
   !
   io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   !
   call OCCUPATIONS_IO_and_interpolate(E,'G',k=k,Time=io_Time)
   !
   call RT_occupations_repair(E)
   !
   i_RT=0
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
     if(l_group_occ) then
       output_fname='YPP-RT_occupations_k'//trim(intc(ik))//'_b'//trim(intc(ib))
       if(i_T==1) call open_file()
     endif
     !
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       !
       if (ik_rim>RT_nk) cycle
       !
       i_RT=i_RT+1
       !
       if(.not.l_group_occ .or. ik_rim==RT_ibz_coo(ik,1).and.l_group_occ) data_to_plot(2:)=0._SP
       !
       ! Lifetimes
       !===========
       n1=1
       if (l_ELPH_lifetimes) then
         n1=5
         RT_gamma(i_RT,i_T,1)=E_P_Hole_lifetime(ib,ik_rim)
         RT_gamma(i_RT,i_T,2)=E_P_Electron_lifetime(ib,ik_rim)
         RT_gamma(i_RT,i_T,3)=E_P_Ho_abs_lifetime(ib,ik_rim)
         RT_gamma(i_RT,i_T,4)=E_P_El_abs_lifetime(ib,ik_rim)
       endif
       !
       if (l_elel_scatt) then
         n1=3
         if (l_ELPH_lifetimes) n1=7
         RT_gamma(i_RT,i_T,n1-2)=E_E_Hole_lifetime(ib,ik_rim)
         RT_gamma(i_RT,i_T,n1-1)=E_E_Electron_lifetime(ib,ik_rim)
       endif
       !
       RT_gamma(i_RT,i_T,:)=RT_gamma(i_RT,i_T,:)*HA2EV*1000.
       RT_occ(i_RT,i_T)=RT_E_occupations(ib,ik_rim)
       !
       data_to_plot(3:8)=data_to_plot(3:8)+RT_gamma(i_RT,i_T,1:6)
       !
       if(ik_rim==RT_ibz_coo(ik,2).and.l_group_occ) &
&        data_to_plot(3:)=data_to_plot(3:)/real(RT_NK_ibz(ik),SP)
       !
       if (.not.l_RT_occ_Plot) cycle
       !
       if(.not.l_group_occ) then
         output_fname='YPP-RT_occupations_k'//trim(intc(ik_rim))//'_b'//trim(intc(ib))
         if (i_T==1) call open_file() 
       endif
       !
       ! Occupations
       !=============
       RT_occupations_sum(3,i_T)=RT_occupations_sum(3,i_T)+&
&        RT_E_occupations(ib,ik_rim)*k%weights(ik)/real(RT_NK_ibz(ik),SP)
       !
       if (ib==SC_bands(1).and.SC_bands(1)>1) &
&         RT_occupations_sum(3,i_T)=RT_occupations_sum(3,i_T)+ &
&           spin_occ*k%weights(ik)*real(SC_bands(1)-1)/real(RT_NK_ibz(ik),SP)
       !
       data_to_plot(1)=io_TIME/FS2AUT
       !
       if (ib<=E%nbf) data_to_plot(2)=data_to_plot(2)+(RT_E_occupations(ib,ik_rim)-spin_occ)*k%weights(ik)
       if (ib> E%nbf) data_to_plot(2)=data_to_plot(2)+ RT_E_occupations(ib,ik_rim)          *k%weights(ik)
       !
       if(.not.l_group_occ .or. ik_rim==RT_ibz_coo(ik,2).and.l_group_occ) then
         data_to_plot(2)=data_to_plot(2)/real(RT_NK_ibz(ik),SP)
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
 if(l_RT_occ_plot) then
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
 if(l_RT_occ_Fit) call RT_occupations_FIT(RT_E,RT_occ,RT_gamma,n1-1,n_T_steps)
 !
 if (allocated(RT_E))               deallocate(RT_E)
 if (allocated(RT_occ))             deallocate(RT_occ)
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
       call msg('o '//trim(output_fname),'#',specs(: ,1),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),'#',specs(: ,1),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),'#',titles(: 8),INDENT=0,USE_TABS=.TRUE.)
     else if (l_elel_scatt.and..not.l_ELPH_lifetimes) then
       call msg('o '//trim(output_fname),'#',titles(: 4),INDENT=0,USE_TABS=.TRUE.)
     else if (.not.l_elel_scatt.and.l_ELPH_lifetimes) then
       call msg('o '//trim(output_fname),'#',specs(:6,2),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),'#',titles(: 6),INDENT=0,USE_TABS=.TRUE.)
     else if (.not.l_elel_scatt.and..not.l_ELPH_lifetimes) then
       call msg('o '//trim(output_fname),'#',titles(: 2),INDENT=0,USE_TABS=.TRUE.)
     endif
     call msg('o '//trim(output_fname),'#')
     !
   end subroutine open_file
   !
end subroutine RT_occupations_analyze
