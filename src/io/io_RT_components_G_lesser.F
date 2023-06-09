!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine io_RT_components_G_lesser(ID,T_size)
 !
 use pars,           ONLY:SP,schlen
 use drivers,        ONLY:l_real_time
 use R_lattice,      ONLY:nkibz
 use stderr,         ONLY:intc
 use IO_int,         ONLY:def_variable_bulk,io_variable_bulk
 use IO_m,           ONLY:variable_exist,write_is_on,read_is_on,io_sec,io_unit,netcdf_call,nf90_sync
 use hamiltonian,    ONLY:H_rotation
 use RT_control,     ONLY:Gless_RT_IO_t,NE_i_start_time,RT_do_IO
 use real_time,      ONLY:dG_lesser,G_MEM_steps,RT_bands,RT_nbands,RT_nk,l_RT_rotate_DM,&
&                         NE_i_time,RT_dyn_step,RT_step,i_MEM_now,i_MEM_prev,i_MEM_old, &
&                         H_EQ,l_RT_RWA,RTibz,Ho_plus_sigma
 use netcdf,         ONLY:NF90_UNLIMITED
#if defined _PAR_IO
 use parallel_m,     ONLY:PAR_G_k_range
#endif
 !
 implicit none
 !
 integer, intent(in)  :: ID,T_size
 !
 logical           :: l_H_rot,l_H_sig,l_H_EQ
 integer           :: ib,ik,ikibz,i_sp_pol,i_mem,i_mem_sorted,G_IO_steps,VAR_SZ(5),IPOS(5),&
&                     PAR_G_k_range_ibz(2),k_range(2),ip,TMP_VAR_SZ
 character(schlen) :: dim_names(5),dim_name_ibz,G_var_name,Sigma_var_name,H_var_name,ROT_var_name
 complex(SP), allocatable ::Delta_Sigma(:,:,:,:)
 !
 ! Setup
 !=======
 !
 ! Variables SIZE and strings...
 !-------------------------------
 VAR_SZ(1:4)=(/2,RT_nbands,RT_nbands,1/)
 dim_names(1:4)  =[character(schlen) :: 'complex', 'RT_nbands','RT_nbands','k_fragment']
 dim_name_ibz ='k_fragment'
 IPOS(1:4)=1
#if defined _PAR_IO
 VAR_SZ(4)    = RT_nk
 dim_names(4) ='RT_nk'
 IPOS(4)      =PAR_G_k_range(1)
 dim_name_ibz ='nkibz'
#endif
 VAR_SZ(5)    = G_MEM_steps
 dim_names(5) ='G_MEM_steps'
 if (RT_do_IO("G_lesser_K_section")) then
   VAR_SZ(5)    = NF90_UNLIMITED
   dim_names(5) ='G_IO_time_step'
 endif
 ! 
 ! Sizes...
 !----------
 i_sp_pol=1
 ik = maxval(io_sec(ID,:))-2
 k_range      =(/ik,ik/)
 G_IO_steps   =1
#if defined _PAR_IO
 k_range      =PAR_G_k_range
 G_IO_steps   =Gless_RT_IO_t%INTERVAL_steps/nint(RT_dyn_step/RT_step)
#endif
 ! 
 ! Variables...
 !--------------
 G_var_name="dG_lesser"
 Sigma_var_name="Delta_Sigma"
 ROT_var_name="H_rotation"
 H_var_name="H_equilibrium"
#if !defined _PAR_IO
 G_var_name="dG_lesser_K"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol))
 ikibz=RTibz%k_map(ik)
 Sigma_var_name="Delta_Sigma_K"//trim(intc(ikibz))//"_SP_POL"//trim(intc(i_sp_pol))
 H_var_name="H_equilibrium_K"//trim(intc(ikibz))//"_SP_POL"//trim(intc(i_sp_pol))
 ROT_var_name="H_rotation_K"//trim(intc(ikibz))//"_SP_POL"//trim(intc(i_sp_pol))
#endif
 ! 
 ! H_sig/H_rot...
 !----------------
 l_H_sig= variable_exist(ID,trim(Sigma_var_name)) .and. allocated(Ho_plus_sigma)
 if ( NE_i_time==NE_i_start_time .and. write_is_on(ID)) l_H_sig= G_IO_steps==1 .and. RT_do_IO("G_lesser_K_section")
 !
 l_H_EQ= variable_exist(ID,trim(H_var_name)) .and. allocated(H_EQ)
 if ( NE_i_time==NE_i_start_time .and. write_is_on(ID)) l_H_EQ=  l_H_sig
 !
 l_H_rot=l_RT_rotate_DM .and. (variable_exist(ID,trim(ROT_var_name)).or.write_is_on(ID))
 ! 
 ! Variable Definitions...
 !-------------------------
#if defined _PAR_IO 
 if( NE_i_time==NE_i_start_time.or..not.l_real_time) then
#endif
   call def_variable_bulk(ID,trim(G_var_name),4,VAR_SZ,SP,dim_names=dim_names,par_io_kind='collective',silent=.true.)
   if (l_H_sig) then
     TMP_VAR_SZ=1
#if defined _PAR_IO 
     TMP_VAR_SZ=nkibz
#endif
     call def_variable_bulk(ID,trim(Sigma_var_name),5,(/VAR_SZ(:3),TMP_VAR_SZ,VAR_SZ(5)/),SP,&
&                           dim_names=(/dim_names(:3),dim_name_ibz,dim_names(5)/),par_io_kind='collective',silent=.true.)   
   endif
   if (l_H_rot) call def_variable_bulk(ID,trim(ROT_var_name),6,VAR_SZ(1:4),SP,&
&                                      dim_names=dim_names(1:4),par_io_kind='collective',silent=.true.)
   if (l_H_EQ) call def_variable_bulk(ID,trim(H_var_name),7,VAR_SZ(1:4),SP,&
&                                     dim_names=dim_names(1:4),par_io_kind='collective',silent=.true.)
#if defined _PAR_IO 
 endif
#endif
 !
 ! Variable I/O...
 !-----------------
 if (l_real_time) then
   do i_mem_sorted=G_MEM_steps-T_size+1,G_MEM_steps
     if (RT_do_IO("G_lesser_K_section"))         IPOS(5)=Gless_RT_IO_t%N+(i_mem_sorted-G_MEM_steps)
     if (RT_do_IO("G_lesser_RESTART_K_section")) IPOS(5)=         T_size+(i_mem_sorted-G_MEM_steps)
     if (i_mem_sorted==G_MEM_steps  ) i_mem=i_MEM_now
     if (i_mem_sorted==G_MEM_steps-1) i_mem=i_MEM_prev
     if (i_mem_sorted==G_MEM_steps-2) i_mem=i_MEM_old
     call io_variable_bulk(ID,4,C4=dG_lesser(:,:,k_range(1):k_range(2),i_mem:i_mem),IPOS=IPOS)
   enddo
 else
   IPOS(5)=Gless_RT_IO_t%N
   call io_variable_bulk(ID,4,C4=dG_lesser(:,:,k_range(1):k_range(2),1:1),IPOS=IPOS)
 endif
 !
 if (l_H_sig) then
   PAR_G_k_range_ibz(1)=RTibz%k_map(k_range(1))
   PAR_G_k_range_ibz(2)=RTibz%k_map(k_range(2))
   allocate(Delta_Sigma(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range_ibz(1):PAR_G_k_range_ibz(2),1))
   if(write_is_on(ID)) then
     do ikibz=PAR_G_k_range_ibz(1),PAR_G_k_range_ibz(2)
       ik=RTibz%k_range(ikibz,1)
       if(     l_RT_RWA) Delta_Sigma(:,:,ikibz,1:1)=Ho_plus_sigma(:,:,ik,1:1)
       if(.not.l_RT_RWA) Delta_Sigma(:,:,ikibz,1:1)=Ho_plus_sigma(:,:,ik,1:1)-H_EQ(:,:,ik,1:1)
     enddo
   endif
#if defined _PAR_IO
   IPOS(4)=PAR_G_k_range_ibz(1)
#endif
   call io_variable_bulk(ID,5,C4=Delta_Sigma,IPOS=IPOS)
#if defined _PAR_IO
   IPOS(4)=PAR_G_k_range(1)
#endif
   if(read_is_on(ID)) then
     do ip=PAR_G_k_range_ibz(1),PAR_G_k_range_ibz(2)
       ikibz=RTibz%k_map(ip)
       Ho_plus_sigma(:,:,ip,1:1)=Delta_Sigma(:,:,ikibz,1:1)
     enddo
   endif
   deallocate(Delta_Sigma)
 endif
 !
 if (l_H_rot) call io_variable_bulk(ID,6,C3=H_rotation(:,:,k_range(1):k_range(2),1),IPOS=IPOS(1:4))
 !
 if (l_H_EQ) call io_variable_bulk(ID,7,C3=H_EQ(:,:,k_range(1):k_range(2),1),IPOS=IPOS(1:4))
 !
 ! In case of frequent I/O this gives a significant slowdown of the simulation
 if (G_IO_steps>=10) call netcdf_call(nf90_sync(io_unit(ID)),ID)
 !
end subroutine io_RT_components_G_lesser
