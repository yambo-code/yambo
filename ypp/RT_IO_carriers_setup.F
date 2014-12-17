! 
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine RT_IO_carriers_setup(E,k,what,N_times)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt
 use R_lattice,      ONLY:bz_samp
 use memory_m,       ONLY:mem_est
 use units,          ONLY:FS2AUT
 use electrons,      ONLY:levels,BZ_RIM_tot_Nkpts
 use rt_ctl,         ONLY:RT_IO_JPO_steps,OCC_deltaT,OCC_T_range
 use real_time,      ONLY:RT_step,l_RT_CCA_Kernel,&
&                         E_P_Hole_lifetime,E_P_Electron_lifetime,& 
&                         E_P_Ho_abs_lifetime,E_P_El_abs_lifetime, &
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,&
&                         NE_time,RT_nk,RIM_ibz_coo,RIM_bz_coo,l_RT_uses_E_RIM,&
&                         RT_ibz_coo,RT_all2ibz,RT_Nk_ibz,RT_H_occupations_reference,&
&                         RT_delta0_occupations,RT_E_occupations_reference,REF_lifetime,&
&                         G_lesser_reference
 use SC,             ONLY:SC_bands
 use com,            ONLY:msg,error
 !
 implicit none
 !
 type(bz_samp)      :: k
 type(levels)       :: E
 character(*)       :: what
 integer            :: N_times
 !
 ! Work Space
 !
 type(bz_samp)     :: q
 integer           :: ik,Nk
 integer, external :: RT_k_grid
 !
 if (index(what,'OCCUPATIONS')>0) then
   !
   allocate(G_lesser_reference(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk))
   call mem_est("Gf_l_ref",(/size(G_lesser_reference)/),(/2*SP/))
   allocate(RT_delta0_occupations(SC_bands(1):SC_bands(2),RT_nk))
   call mem_est("RT_delta0_occupations",(/size(RT_delta0_occupations)/),(/SP/))
   allocate(RT_E_occupations_reference(SC_bands(1):SC_bands(2),RT_nk))
   allocate(RT_H_occupations_reference(SC_bands(1):SC_bands(2),RT_nk))
   call mem_est("RT_E_occ_ref RT_H_occ_ref",&
&               (/size(RT_E_occupations_reference),size(RT_H_occupations_reference)/),(/SP,SP/))
   !
 endif
 !
 if (index(what,'LIFETIMES')>0.and.(l_elel_scatt.or.l_elph_scatt)) then
   !
   allocate(REF_lifetime(SC_bands(1):SC_bands(2),RT_nk,6))
   call mem_est("REF_lifetime",(/size(REF_lifetime)/),(/SP/))
   !
   if ((l_elph_scatt.and.l_RT_CCA_Kernel).and..not.allocated(E_P_Hole_lifetime)) then
     allocate(E_P_Hole_lifetime(SC_bands(1):SC_bands(2),RT_nk))
     allocate(E_P_Electron_lifetime(SC_bands(1):SC_bands(2),RT_nk))
     allocate(E_P_Ho_abs_lifetime(SC_bands(1):SC_bands(2),RT_nk))
     allocate(E_P_El_abs_lifetime(SC_bands(1):SC_bands(2),RT_nk))
   endif
   !
   if (l_elel_scatt.and..not.allocated(E_E_Hole_lifetime)) then
     allocate(E_E_Hole_lifetime(SC_bands(1):SC_bands(2),RT_nk))
     allocate(E_E_Electron_lifetime(SC_bands(1):SC_bands(2),RT_nk))
   endif
   !
 endif
 !
 if (index(what,'TIME_RANGE')>0) then
   !
   if (OCC_deltaT<RT_IO_JPO_steps*RT_step) then
     ! 
     OCC_deltaT=RT_IO_JPO_steps*RT_step
     ! 
   else
     !
     OCC_deltaT=nint(OCC_deltaT/RT_IO_JPO_steps/RT_step)*RT_IO_JPO_steps*RT_step
     !
   endif
   !
   N_times=nint( (OCC_T_range(2)-OCC_T_range(1))/OCC_deltaT )+1
   !
   if (N_times<0) return
   !
   if (NE_time<OCC_T_range(2)) then
     OCC_T_range(2)=NE_time
     if(OCC_deltaT>NE_time) OCC_deltaT=NE_time
     N_times=nint(OCC_T_range(2)/OCC_deltaT)+1
   endif
   !
 endif
 !
 if (index(what,'DOUBLE_GRID')>0) then
   !
   if(BZ_RIM_tot_Nkpts==0) then
     !
     RT_Nk=k%nibz
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
   else
     !
     Nk=RT_k_grid(E,k,q)
     !
     l_RT_uses_E_RIM=.true.
     !
     ! Shouldn't this check be done by variables_SC when reading BZ_RIM_tot_Nkpts ???
     !
     if(Nk==0) call error(' No ndb.RIM_E_and_k file found while reading from an SC DB with RIM points')
     !
     if(Nk/=RT_nk) call error(' Wrong RT k-grid, likely your ndb.RIM_E_and_k is changed')
     !
     allocate(k%pt_RIM(Nk,3))
     do ik=1,k%nibz
       k%pt_RIM(RT_ibz_coo(ik,1):RT_ibz_coo(ik,2),:)=k%ptbz_RIM(RIM_ibz_coo(ik,1):RIM_ibz_coo(ik,2),:)
     enddo
     !
   endif
   !
 endif
 !
 if (index(what,'REPORT')>0) then
   call msg('s','Time range [fs]:',OCC_T_range/FS2AUT)
   call msg('s','Time step  [fs]:',OCC_deltaT/FS2AUT)
   call msg('s','Time steps     :',N_times)
 endif
 !
end subroutine RT_IO_carriers_setup
