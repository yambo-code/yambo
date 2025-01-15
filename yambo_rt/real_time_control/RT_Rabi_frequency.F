!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
function RT_Rabi_frequency(En,i_field,l_print_rabi)
 !
 ! To be fixed in parallel
 ! To be fixed for degenerate states 
 !
 use pars,           ONLY:SP,rZERO,cZERO,cI
 use units,          ONLY:HA2EV,PS2AUT
 use R_lattice,      ONLY:nkibz
 use electrons,      ONLY:n_sp_pol,levels
 use X_m,            ONLY:global_gauge
 use fields,         ONLY:Efield,field_frequency
 use real_time,      ONLY:RT_bands,RT_nbands
 use dipoles,        ONLY:DIP_iR,DIP_v
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_Xk_ibz_index
 use com,            ONLY:msg,of_open_close
 use vec_operate,    ONLY:degeneration_finder
 !
 implicit none
 !
 real(SP)            :: RT_Rabi_frequency
 type(levels), intent(in)  :: En
 integer,      intent(in)  :: i_field
 logical,      intent(in)  :: l_print_rabi
 !
 ! Workspace
 !
 integer     :: i_fr,i_sp_pol,ik,ik_mem,ib1,ib2,i_grp1,i_grp2,i_sp_pol_ref,ik_ref,ib1_ref,ib2_ref, &
&               first_el(RT_nbands),n_of_el(RT_nbands),n_deg_grp,n_deg_states(2)
 logical     :: vel_gauge,len_gauge
 real(SP)    :: effective_frequency,E_trans,E_dist,delta_E
 complex(SP) :: POL_or_Jtot(3),field_term
 !
 len_gauge=trim(global_gauge)=="length"
 vel_gauge=trim(global_gauge)=="velocity"
 !
 effective_frequency=0._SP
 do i_fr=1,Efield(i_field)%n_frequencies
   effective_frequency=effective_frequency+field_frequency(Efield(i_field),i_fr)
 enddo
 effective_frequency=effective_frequency/real(Efield(i_field)%n_frequencies,SP)
 !
 RT_Rabi_frequency=rZERO
 E_dist=0.1_SP
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,nkibz
     !if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !ik_mem=PAR_Xk_ibz_index(ik)
     do ib1=RT_bands(1),RT_bands(2)
       do ib2=ib1+1,RT_bands(2)
         delta_E=En%E(ib2,ik,i_sp_pol)-En%E(ib1,ik,i_sp_pol)
         if (delta_E<1.E-5_SP) cycle
         if (abs(delta_E-effective_frequency)>abs(E_dist)) cycle
         E_dist=effective_frequency-delta_E
         E_trans=delta_E
       enddo
     enddo
   enddo
 enddo
 !
 if (E_dist==0.1_SP) then
   call warning(" Rabi_term: No transition found within 0.1 Hartree of the field frequency")
   return
 endif
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
         if (abs(delta_E-effective_frequency)>abs(E_trans-effective_frequency)) cycle
         POL_or_Jtot=cZERO
         do ib1=first_el(i_grp1)+RT_bands(1)-1,first_el(i_grp1)+n_of_el(i_grp1)-1+RT_bands(1)-1
           do ib2=first_el(i_grp2)+RT_bands(1)-1,first_el(i_grp2)+n_of_el(i_grp2)-1+RT_bands(1)-1
             if (len_gauge) POL_or_Jtot(1:3)=POL_or_Jtot(1:3)+cI*DIP_iR(1:3,ib2,ib1,ik_mem,i_sp_pol)
             if (vel_gauge) POL_or_Jtot(1:3)=POL_or_Jtot(1:3)-DIP_v(1:3,ib2,ib1,ik_mem,i_sp_pol)
           enddo
         enddo
         field_term=sum(EField(i_field)%versor(1:3)*POL_or_Jtot(1:3) )*Efield(i_field)%amplitude 
         if (RT_Rabi_frequency>abs(field_term)) cycle
         RT_Rabi_frequency=abs(field_term)
         i_sp_pol_ref=i_sp_pol
         ik_ref=ik
         ib1_ref=first_el(i_grp1)+RT_bands(1)-1
         ib2_ref=first_el(i_grp2)+RT_bands(1)-1
         n_deg_states=(/n_of_el(i_grp1),n_of_el(i_grp2)/)
       enddo
     enddo
   enddo
 enddo
 !
 RT_Rabi_frequency=RT_Rabi_frequency*real(PS2AUT,SP) !*2._SP
 !
 if (l_print_rabi) then
   call of_open_close("rabi_frequencies","ot")
   call msg("o rabi","#" )
   call msg("o rabi","#"," Main Rabi term",INDENT=0)
   call msg("o rabi","# transition energy and detuning ",(/E_trans,E_dist/)*HA2EV,after_msg="[eV]",INDENT=0)
   call msg("o rabi","# transition indexes             ",(/ik_ref,ib1_ref,ib2_ref/),after_msg="(ik,iv,ic)",INDENT=0)
   call msg("o rabi","# degenerate states              ", n_deg_states,after_msg="(n_v,n_c)",INDENT=0)
   call msg("o rabi","# coupling frequency             ",RT_Rabi_frequency,after_msg="[THz]",INDENT=0)
   call msg("o rabi","#" )
   call of_open_close("rabi_frequencies")
 endif
 !
end function RT_Rabi_frequency
