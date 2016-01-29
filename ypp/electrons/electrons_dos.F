! 
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine electrons_dos(Xk,Xen)
 !
 use units,          ONLY:HA2EV,FS2AUT
 use pars,           ONLY:SP,schlen
 use stderr,         ONLY:intc
 use wave_func,      ONLY:WF
 use FFT_m,          ONLY:fft_size
 use R_lattice,      ONLY:bz_samp,nkibz,nkbz
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:levels,spin,n_sp_pol,n_spinor,n_spin,spin_occ, &
&                         BZ_DbGd_nkpt,BZ_DbGd_table,BZ_DbGd_nbands
 use YPP,            ONLY:output_fname,dos_broadening,dos_bands,&
&                         dos_E_range,dos_E_steps
 use com,            ONLY:msg,of_open_close
 use functions,      ONLY:Fermi_fnc_derivative
 use QP_CTL_m,       ONLY:QP_apply
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:PARALLEL_index,WF_load,el_density_and_current
 use parallel_m,     ONLY:PP_indexes,myid,PP_redux_wait
 use parser_m,       ONLY:parser
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 !
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 ! 
 real(SP)              :: el_dos(dos_E_steps,n_spin),dos_E,delta_E,f_occ,dos_norm,&
&                         el_dos_max,WF_fac(dos_bands(1):dos_bands(2),nkibz,n_spinor)
 integer               :: i_E,ik,ik_bz,ib,i_wf,i_spin,i_spinor,ik_rand,i1
 character(schlen)     :: titles(4)
 logical               :: l_DbGd,l_prt_occ_only,l_separate_eh
 type(PP_indexes)      :: px
 !
 call section('*','Electronic DOS')
 !
 call parser('PrtOccOnly',l_prt_occ_only)
 !
 ! E Double Grid
 !===============
 !
 l_DbGd=.FALSE.
 if (BZ_DbGd_nbands>0) then
   l_DbGd=.TRUE.
   call OCCUPATIONS_Extend(Xen,Xen)
 endif
 !
 if(n_spinor==2) then
   !
   call live_timing('DOS-spinor-factor',nkibz*(dos_bands(2)-dos_bands(1)+1),SERIAL=.true.)
   do ik=1,nkibz
     call WF_load(WF,0,1,dos_bands,(/ik,ik/),title='-Projected DOS',impose_free_and_alloc=.TRUE.)
     do ib=dos_bands(1),dos_bands(2)
       do i_spinor=1,n_spinor
         i_wf=WF%index(ib,ik,i_spinor)
         WF_fac(ib,ik,i_spinor)=real(Vstar_dot_V_omp( fft_size, WF%c(:, i_wf ), WF%c(:,i_wf) ),SP)
       enddo
       call live_timing(steps=1)
     enddo
     call WF_free()
   enddo
   call live_timing()
 endif
 !
 if (l_DbGd) dos_bands(2)=min(dos_bands(2),BZ_DbGd_nbands)
 !
 el_dos_max=0.
 !
 call QP_state_table_setup(Xen)
 call QP_apply(dos_bands,Xen,Xk,'G',msg_fmt='s')
 !
 ! Output files headers
 !
 output_fname='el_dos'
 call of_open_close(trim(output_fname),'ot')
 if (n_spin==1) then
   titles(1)='E[eV]'
   titles(2)='DOS'
   call msg('o dos','#',titles(:2),INDENT=0,USE_TABS=.true.)    
 else
   titles(1)='E[eV]'
   titles(2)='DOS [up]'
   titles(3)='DOS [dn]'
   titles(4)='DOS [up+dn]'
   call msg('o dos','#',titles(:4),INDENT=0,USE_TABS=.true.)    
 endif
 call msg('o dos',"#")
 !
 if (dos_E_range(1)>dos_E_range(2)) then
   dos_E_range(1)=minval(Xen%E(dos_bands(1):dos_bands(2),:,:))-5.*dos_broadening
   dos_E_range(2)=maxval(Xen%E(dos_bands(1):dos_bands(2),:,:))+5.*dos_broadening
 endif
 delta_E=(dos_E_range(2)-dos_E_range(1))/dos_E_steps
 !
 dos_norm=1./DL_vol
 if (l_DbGd) dos_norm=dos_norm/real(sum(BZ_DbGd_nkpt))
 !
 ! Parallel setup and live_timing
 !================================
 !
 call PARALLEL_index(px,(/dos_E_steps/))
 call live_timing('DOS',px%n_of_elements(myid+1))
 !
 el_dos(:,:)=0.
 !
 do i_E=1,dos_E_steps
   !
   if (.not.px%element_1D(i_E)) cycle
   !
   dos_E=dos_E_range(1)+i_E*delta_E
   !
   do ib=dos_bands(1),dos_bands(2)
     do i_spin=1,n_sp_pol
       !
       if (l_DbGd) then
         do ik_bz=1,nkbz
           ik=Xk%sstar(ik_bz,1)
           do i1=1,BZ_DbGd_nkpt(ik_bz)
             ik_rand=BZ_DbGd_table(ik_bz,i1,1)
             f_occ=spin_occ
             if(l_prt_occ_only) f_occ=Xen%f_DbGd(ib,ik_rand,i_spin)
             el_dos(i_E,i_spin)=el_dos(i_E,i_spin)+f_occ*&
&                      Fermi_fnc_derivative(Xen%E_DbGd(ib,ik_rand,i_spin)-dos_E,dos_broadening)*dos_norm
           enddo
         enddo
       else
         do ik=1,nkibz
           f_occ=spin_occ
           if(l_prt_occ_only) f_occ=Xen%f(ib,ik,i_spin)
           if(n_spinor==2) then
             do i_spinor=1,n_spinor
               el_dos(i_E,i_spinor)=el_dos(i_E,i_spinor)+f_occ*WF_fac(ib,ik,i_spinor)*dos_norm*&
&                        Fermi_fnc_derivative(Xen%E(ib,ik,i_spin)-dos_E,dos_broadening)*Xk%weights(ik)
             enddo
           else
             el_dos(i_E,i_spin)=el_dos(i_E,i_spin)+f_occ*Xk%weights(ik)*dos_norm*&
&                      Fermi_fnc_derivative(Xen%E(ib,ik,i_spin)-dos_E,dos_broadening)
           endif
         enddo
       endif
       !
     enddo
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call PP_redux_wait(el_dos)
 !
 el_dos_max=max(el_dos_max,maxval(abs(el_dos)))
 !
 ! Output file
 !
 l_separate_eh=.true.
 !
 do i_E=1,dos_E_steps
   dos_E=dos_E_range(1)+i_E*delta_E
   !
   if (n_spin==1) call msg('o dos','',(/dos_E*HA2EV,el_dos(i_E,1)/),INDENT=-2,USE_TABS=.true.)
   if (n_spin==2) call msg('o dos','',(/dos_E*HA2EV,el_dos(i_E,:),  &
&                                           el_dos(i_E,1)+el_dos(i_E,2)/),INDENT=-2,USE_TABS=.true.)
 enddo
 !
 call live_timing(steps=1)
 !
 !
 call of_open_close(trim(output_fname))
 !
 call live_timing()
 !
end subroutine