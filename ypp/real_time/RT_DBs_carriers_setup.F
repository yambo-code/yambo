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
subroutine RT_DBs_carriers_setup(en,kpt,qpt)
 !
 use pars,          ONLY:SP,schlen
 use units,         ONLY:HA2KEL
 use electrons,     ONLY:levels
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_WR_CL,REP
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:Bose_Temp
 use parallel_m,    ONLY:PAR_G_k_range
 use RT_control,    ONLY:CARR_RT_IO_t,RT_carriers_to_RT_E
 use real_time,     ONLY:RT_carriers,NE_i_time,l_elel_scatt,l_elph_scatt,&
&                        l_elphoton_scatt,RT_potential
 use YPP_real_time, ONLY:l_RTpump_energy,l_RTpump_Fermi
 !
 implicit none
 !
 type(levels), intent(inout) :: en
 type(bz_samp),intent(in)    :: kpt,qpt
 !
 ! Work Space
 !
 type(levels)      :: E_RT
 !
 integer           :: io_RT_err,ID
 integer, external :: io_RT_components
 logical           :: SUCCESS
 character(schlen) :: sec_ch
 !
 logical, allocatable :: do_kpt(:)
 !
 if (l_RTpump_energy) sec_ch="Energy selection"
 if (l_RTpump_Fermi)  sec_ch="Fermi function"
 !
 call section('*','Manual carriers excitation via '//trim(sec_ch))
 !
 ! Force a tiny temperature to allocate EP_abs components
 !
 Bose_Temp=1/HA2KEL
 !
 NE_i_time   =1
 PAR_G_k_range=(/1,kpt%nibz/)
 !
 l_elel_scatt    =.true.
 l_elph_scatt    =.true.
 l_elphoton_scatt=.true.
 RT_potential    ="HARTREE+GS_xc"
 !
 call RT_k_grid(en,kpt,qpt)
 !
 call RT_alloc(en,"carriers")
 !
 call RT_carriers_object(RT_carriers,en,kpt,ID,"allocate")
 !
 allocate(do_kpt(RT_carriers%nk))
 !
 call RT_carriers_to_RT_E(en,E_RT,"Eo")
 !
 call RT_select_k_space(E_RT,kpt,do_kpt)
 !
 if(l_RTpump_energy) call RT_manual_excitation(E_RT,kpt,do_kpt,SUCCESS)
 !
 if(l_RTpump_Fermi)  call RT_Fermi_excitation(E_RT,kpt,do_kpt,SUCCESS)
 !
 deallocate(do_kpt)
 !
 if (.not.SUCCESS) then
   call warning("No level to be manually pumped found")
   return
 endif
 !
 CARR_RT_IO_t%N=1
 allocate(CARR_RT_IO_t%Time(1))
 CARR_RT_IO_t%Time(CARR_RT_IO_t%N)=0._SP
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1,2,3/),COM=REP,ID=ID)
 io_RT_err=io_RT_components("carriers",ID)
 !
 call RT_carriers_object(RT_carriers,en,kpt,ID,"free")
 !
 call RT_free("carriers")
 !
end subroutine RT_DBs_carriers_setup
!
!
subroutine RT_select_k_space(E_RT,kpt,do_kpt)
 !
 use com,            ONLY:msg
 use pars,           ONLY:SP
 use stderr,         ONLY:intc
 use units,          ONLY:HA2EV
 use vec_operate,    ONLY:c2a
 use electrons,      ONLY:levels
 use YPPm,           ONLY:coo_in,BANDS_path,CIRCUIT_k,CIRCUIT_k_label
 use YPP_interfaces, ONLY:k_special
 use R_lattice,      ONLY:bz_samp,k_the_nearest
 use real_time,      ONLY:RT_carriers
 use YPP_real_time,  ONLY:Eh_pumped_pair_BZ_width,Eh_pumped_pair_BZ_regions,&
&                         Eh_pumped_pair_n_BZ_regions
 !
 implicit none
 !
 type(levels),  intent(in)  :: E_RT
 type(bz_samp), intent(in)  :: kpt
 logical,       intent(out) :: do_kpt(RT_carriers%nk)
 !
 integer      :: i_region,i_k_near,i_err
 real(SP)     :: local_k(3),k_dist(RT_carriers%nk)
 !
 do_kpt=.true.
 if(Eh_pumped_pair_n_BZ_regions==0) return
 !
 do_kpt=.false.
 !
 ! Use special points to select the region
 !
 call k_special( )
 !
 if (len_trim(BANDS_path)>0) call k_circuit_driver( BANDS_path, 0 )
 !
 do i_region=1,Eh_pumped_pair_n_BZ_regions
   local_k=Eh_pumped_pair_BZ_regions(i_region,:)
   !
   if (trim(coo_in)=="cc")  call c2a(v_in=local_k,mode="kc2i")
   if (trim(coo_in)=="rlu") call c2a(v_in=local_k,mode="ka2i")
   !
   i_k_near=k_the_nearest(local_k,RT_carriers%kpt,RT_carriers%nk,k_dist,ROTATE_k_ref=.TRUE.,i_err=i_err)
   !
   if (k_dist(i_k_near)<Eh_pumped_pair_BZ_width.or.Eh_pumped_pair_BZ_width<0) then
     do_kpt(i_k_near)=.TRUE. 
     call NEAR_k_report(i_k_near,local_k," ")
   endif
 enddo
 !
 do i_region=1,CIRCUIT_k%nbz
   !
   i_k_near=k_the_nearest(CIRCUIT_k%pt(i_region,:),RT_carriers%kpt,RT_carriers%nk,k_dist,ROTATE_k_ref=.TRUE.,i_err=i_err)
   !
   if (k_dist(i_k_near)<Eh_pumped_pair_BZ_width.or.Eh_pumped_pair_BZ_width<0) then
     do_kpt(i_k_near)=.TRUE. 
     call NEAR_k_report(i_k_near,CIRCUIT_k%pt(i_region,:),CIRCUIT_k_label(i_region))
   endif
 enddo
 !
 contains
   !
   subroutine NEAR_k_report( ik, u_k, label )
     use pars,           ONLY:schlen
     integer           :: ik,i_c,i_v
     real(SP)          :: u_k(3)
     character(*)      :: label
     character(schlen) :: title
     call msg('s','User    defined',u_k,after_msg="[iku]"//" "//trim(label))
     call msg('s','Nearest k-point',RT_carriers%kpt(ik,:),after_msg="[iku][ik="//trim(intc(ik))//"]")
     call msg('s','Distance       ',k_dist(ik),after_msg="[iku]")
     do i_c=E_RT%nbf(1)+1,E_RT%nbf(1)+2
       do i_v=E_RT%nbf(1)-1,E_RT%nbf(1)
         title="Gap CBm+"//trim(intc(i_c-E_RT%nbf(1)))//" VBM-"//trim(intc(E_RT%nbf(1)-i_v))
         call msg('s',trim(title),(E_RT%E(i_c,ik,1)-E_RT%E(i_v,ik,1))*HA2EV,after_msg="[ev]")
       enddo 
     enddo
   end subroutine NEAR_k_report
   !
end subroutine RT_select_k_space
