!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine RT_Rabi_frequency_ALL_and_print(En,i_field)
 !
 ! To be fixed in parallel
 ! To be fixed for degenerate states 
 !
 use pars,           ONLY:SP,cZERO,cI
 use units,          ONLY:HA2EV,PS2AUT
 use R_lattice,      ONLY:nkibz
 use electrons,      ONLY:n_sp_pol,levels
 use X_m,            ONLY:global_gauge
 use fields,         ONLY:Efield,field_frequency
 use real_time,      ONLY:RT_bands,RT_nbands
 use dipoles,        ONLY:DIP_iR,DIP_v
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_Xk_ibz_index
 use vec_operate,    ONLY:degeneration_finder
 use OUTPUT,         ONLY:OUTPUT_driver
 !
 implicit none
 !
 type(levels), intent(in)  :: En
 integer,      intent(in)  :: i_field
 !
 ! Workspace
 !
 real(SP)    :: Rabi_frequency
 integer     :: i_fr,i_sp_pol,ik,ik_mem,ib1,ib2,i_grp1,i_grp2,i_sp_pol_ref,ik_ref,ib1_ref,ib2_ref, &
 &              first_el(RT_nbands),n_of_el(RT_nbands),n_deg_grp,n_deg_states(2)
 logical     :: vel_gauge,len_gauge
 real(SP)    :: effective_frequency,E_trans,E_dist,delta_E
 complex(SP) :: POL_or_Jtot(3),field_term
 !
 len_gauge=trim(global_gauge)=="length"
 vel_gauge=trim(global_gauge)=="velocity"
 !
 call OUTPUT_driver('rabi_frequencies',action="append")
 !
 effective_frequency=0._SP
 do i_fr=1,Efield(i_field)%n_frequencies
   effective_frequency=effective_frequency+field_frequency(Efield(i_field),i_fr)
 enddo
 effective_frequency=effective_frequency/real(Efield(i_field)%n_frequencies,SP)
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,nkibz
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_Xk_ibz_index(ik)
     call degeneration_finder(En%E(RT_bands(1):RT_bands(2),ik,i_sp_pol),RT_nbands,first_el,n_of_el,n_deg_grp, &
     &                        0.0001_SP/HA2EV,Include_single_values=.true.)
     do i_grp1=1,n_deg_grp
       ib1=first_el(i_grp1)+RT_bands(1)-1
       do i_grp2=i_grp1+1,n_deg_grp
         ib2=first_el(i_grp2)+RT_bands(1)-1
         delta_E=En%E(ib2,ik,i_sp_pol)-En%E(ib1,ik,i_sp_pol)
         if (delta_E<1.E-5_SP/HA2EV) cycle
         POL_or_Jtot=cZERO
         do ib1=first_el(i_grp1)+RT_bands(1)-1,first_el(i_grp1)+n_of_el(i_grp1)-1+RT_bands(1)-1
           do ib2=first_el(i_grp2)+RT_bands(1)-1,first_el(i_grp2)+n_of_el(i_grp2)-1+RT_bands(1)-1
             if (len_gauge) POL_or_Jtot(1:3)=POL_or_Jtot(1:3)+cI*DIP_iR(1:3,ib2,ib1,ik_mem,i_sp_pol)
             if (vel_gauge) POL_or_Jtot(1:3)=POL_or_Jtot(1:3)-DIP_v(1:3,ib2,ib1,ik_mem,i_sp_pol)
           enddo
         enddo
         field_term=sum(EField(i_field)%versor(1:3)*POL_or_Jtot(1:3) )*Efield(i_field)%amplitude !&
         E_dist=effective_frequency-delta_E
         E_trans=delta_E
         Rabi_frequency=abs(field_term)*real(PS2AUT,SP)
         i_sp_pol_ref=i_sp_pol
         ik_ref=ik
         ib1_ref=first_el(i_grp1)+RT_bands(1)-1
         ib2_ref=first_el(i_grp2)+RT_bands(1)-1
         n_deg_states=(/n_of_el(i_grp1),n_of_el(i_grp2)/)
         !
         call OUTPUT_driver("rabi",TITLES=(/" dE ","det."/),    R_VALUES=(/E_trans,E_dist/),UNIT="eV")
         call OUTPUT_driver("rabi",TITLES=(/"ik ","ib1","ib2"/),I_VALUES=(/ik_ref,ib1_ref,ib2_ref/))
         call OUTPUT_driver("rabi",TITLES=(/"n_v","n_c"/),      I_VALUES=n_deg_states)
         call OUTPUT_driver("rabi",TITLES=(/"w"/),              R_VALUES=(/Rabi_frequency/),UNIT="THz")
         call OUTPUT_driver("rabi",action="write")
         !
       enddo
     enddo
   enddo
 enddo
 !
 call OUTPUT_driver('rabi_frequencies',action="close")
 !
end subroutine RT_Rabi_frequency_ALL_and_print
