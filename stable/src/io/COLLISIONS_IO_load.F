!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine COLLISIONS_IO_load(CHECK_EXISTENCE_ONLY)
 !
 ! Load all collisions contribution from any kind of self-energy 
 ! exchange, COHSEX etc.. and store them together in QP_rho(i_qp)
 !
 use pars,            ONLY:IP,SP,LP,schlen
 use drivers,         ONLY:l_sc_hf,l_sc_cohsex,l_sc_exx,l_collisions_IO,l_sc_srpa,l_carrier_dynamics 
 use com,             ONLY:warning,msg
 use global_XC,       ONLY:SC_HF,SC_COHSEX
 use QP_m,            ONLY:QP_n_states
 use memory_m,        ONLY:mem_sum,mem_est
 use collisions_IO,   ONLY:COLLISIONS_v,COLLISIONS_table,V_kind,collisions_IO_free,&
&                          COLLISIONS_path,io_COLLISIONS,COLLISIONS_range,collisions_RANGES_setup,n_collisions
 use stderr,          ONLY:intc
 use IO_m,            ONLY:OP_RD_CL,io_control,VERIFY,REP
 use parallel_m,      ONLY:PP_indexes,PP_indexes_reset,myid
 use interfaces,      ONLY:PARALLEL_index
 use collisions_CACHE,ONLY:COLLISIONS_cache_size,last_QP_mem,cache_memory,CACHE
#if defined _RT
 use real_time,       ONLY:RT_qp_px
#endif
 !
 implicit none
 !
 logical            :: CHECK_EXISTENCE_ONLY
 !
 ! Parameters
 !
 integer, parameter :: max_V_kinds=10
 !
 ! When COLLISIONS_cache_size<0 automatically 
 ! it fills up to 1.7 Gb processor with COLLISIONS_cache_size states
 !
 real(SP),parameter :: mem_proc   =.7 
 !
 ! Work Space
 !
 integer            :: i1,i2,i3,ic,i_qp,ID,n_tot_rho_states,io_err(max_V_kinds),V_kinds(max_V_kinds),n_V_kinds,&
&                      max_COLLISION_cache,iv,QP_n_states_loaded
 real(SP)           :: mem_before_cache
 type(PP_indexes)   :: px
 character(schlen)  :: dump_ch 
 complex(SP), allocatable :: COLLISIONS_disk(:,:,:)
 !
 last_QP_mem =0
 n_V_kinds   =0
 cache_memory=0
 io_err      =0
 !
 if(l_sc_hf.or.l_sc_cohsex.or.l_sc_exx) then
   n_V_kinds          =n_V_kinds+1
   V_kinds(n_V_kinds) =SC_HF
 endif
 !
 if(l_sc_cohsex.or.l_sc_srpa) then
   n_V_kinds          =n_V_kinds+1
   V_kinds(n_V_kinds) =SC_COHSEX
 endif
 !
 if (n_V_kinds==0) return
 !
 ! Check only if the DB exist and are readable
 !
 if (CHECK_EXISTENCE_ONLY) then
   !
   do iv=1,n_V_kinds
     !
     V_kind=V_kinds(iv)
     !
     ! In the carrier dynamics only the HF part is not needed
     !
     if (l_carrier_dynamics.and.V_kind/=SC_COHSEX) cycle
     !
     call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
     io_err(iv)=io_COLLISIONS(0,ID)
     !
   enddo
   !
   l_collisions_IO=all(io_err==0)
   !
   if (.not.l_collisions_IO) COLLISIONS_path=" "
   !
   return
   !
 endif
 !           
 call msg('sr','[COLLISIONS] Loading ...')
 !
 if (COLLISIONS_cache_size==0..or..not.l_collisions_IO) return
 !
#if defined _RT
 call PP_indexes_reset(RT_qp_px)
 call PARALLEL_index(RT_qp_px,(/QP_n_states/))
 QP_n_states_loaded=RT_qp_px%n_of_elements(myid+1)
#else
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/QP_n_states/))
 QP_n_states_loaded=px%n_of_elements(myid+1)
#endif
 !
 mem_before_cache=mem_sum
 !
 if(COLLISIONS_cache_size<=0.) COLLISIONS_cache_size=mem_proc-mem_before_cache
 !
 if(COLLISIONS_cache_size<=0.) then
   call warning("Automatic collisions Cache failed ") 
   COLLISIONS_cache_size=0.
   return
 endif
 !
 ! Array ranges setup
 !
 call collisions_RANGES_setup()
 !
 max_COLLISION_cache=sum(COLLISIONS_range(:,2)-COLLISIONS_range(:,1)+1)
 !
 allocate(COLLISIONS_disk(COLLISIONS_range(1,1):COLLISIONS_range(1,2),&
&                         COLLISIONS_range(2,1):COLLISIONS_range(2,2),&
&                         COLLISIONS_range(3,1):COLLISIONS_range(3,2)))
 call mem_est("COLLISIONS_disk",(/size(COLLISIONS_disk)/),(/2*SP/))
 !
 i_qp=0
 !
 allocate(CACHE(QP_n_states))
 !
 do while(cache_memory<COLLISIONS_cache_size.and.i_qp<QP_n_states)  
   !
   i_qp=i_qp+1
   !
#if defined _RT
   if(.not.RT_qp_px%element_1D(i_qp)) cycle  
#else
   if(.not.px%element_1D(i_qp)) cycle  
#endif
   !
   if (.not.l_carrier_dynamics) then
     !
     allocate(CACHE(i_qp)%COLLISIONS_table(COLLISIONS_range(1,1):COLLISIONS_range(1,2),&
&                                          COLLISIONS_range(2,1):COLLISIONS_range(2,2),&
&                                          COLLISIONS_range(3,1):COLLISIONS_range(3,2)))
     !
     CACHE(i_qp)%COLLISIONS_table(:,:,:)=0
     !
   endif
   COLLISIONS_disk=(0.,0.)
   !
   do iv=1,n_V_kinds
     !
     V_kind=V_kinds(iv)
     !
     ! In the carrier dynamics only the HF part is not needed
     !
     if (l_carrier_dynamics.and.V_kind/=SC_COHSEX) cycle
     !
     call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),ID=ID)
     io_err(iv)=io_COLLISIONS(i_qp,ID)
     !
     ic=0
     !
     do i1=COLLISIONS_range(1,1),COLLISIONS_range(1,2)
       do i2=COLLISIONS_range(2,1),COLLISIONS_range(2,2)
         do i3=COLLISIONS_range(3,1),COLLISIONS_range(3,2)
           !
           if (l_carrier_dynamics) then
             ic=ic+1
             COLLISIONS_disk(i1,i2,i3)=COLLISIONS_disk(i1,i2,i3)+COLLISIONS_v(ic)
           else
             if(COLLISIONS_table(i1,i2,i3)==1) then
               ic=ic+1
               CACHE(i_qp)%COLLISIONS_table(i1,i2,i3)=1
               COLLISIONS_disk(i1,i2,i3)=COLLISIONS_disk(i1,i2,i3)+COLLISIONS_v(ic)
             endif
           endif
           !
         enddo
       enddo
     enddo
     !
   enddo
   !
   n_tot_rho_states=sum(int(CACHE(i_qp)%COLLISIONS_table(:,:,:),IP))
   !
   if (l_carrier_dynamics) then
     allocate(CACHE(i_qp)%COLLISIONS_v3(COLLISIONS_range(1,1):COLLISIONS_range(1,2),&
&                                       COLLISIONS_range(2,1):COLLISIONS_range(2,2),&
&                                       COLLISIONS_range(3,1):COLLISIONS_range(3,2)))
   else
     allocate(CACHE(i_qp)%COLLISIONS_v(n_tot_rho_states))
   endif
   !
   ic=0
   !
   do i1=COLLISIONS_range(1,1),COLLISIONS_range(1,2)
     do i2=COLLISIONS_range(2,1),COLLISIONS_range(2,2)
       do i3=COLLISIONS_range(3,1),COLLISIONS_range(3,2)
         if (l_carrier_dynamics) then
           CACHE(i_qp)%COLLISIONS_v3(i1,i2,i3)=COLLISIONS_disk(i1,i2,i3)
         else
           if(CACHE(i_qp)%COLLISIONS_table(i1,i2,i3)==1) then
             ic=ic+1
             CACHE(i_qp)%COLLISIONS_v(ic)=COLLISIONS_disk(i1,i2,i3)
           endif
         endif
       enddo
     enddo
   enddo
   !
   cache_memory=mem_sum-mem_before_cache
   !
 end do
 !
 last_QP_mem=i_qp
 !
 if (l_carrier_dynamics) then
   call mem_est("QP_COLLISIONS",(/size(CACHE(1)%COLLISIONS_v3)*QP_n_states_loaded/),(/2*SP/))
 else
   call mem_est("QP_COLLISIONS_table",(/size(CACHE(1)%COLLISIONS_table)*QP_n_states_loaded/),(/LP/))
   call mem_est("QP_COLLISIONS",(/size(CACHE(1)%COLLISIONS_v)*QP_n_states_loaded/),(/2*SP/))
 endif
 !
 deallocate(COLLISIONS_disk)
 call mem_est("COLLISIONS_disk")
 !
 ! If all QP are in memory I don't need anymore collisions_IO
 !
 if(last_QP_mem==QP_n_states) call collisions_IO_free()
 !
#if !defined _RT
 call PP_indexes_reset(px)
#endif
 !
 write (dump_ch,'(a,f10.2,a)') '[COLLISIONS] QP in memory :',real(last_QP_mem)/real(QP_n_states)*100.0,'%'
 call msg('s',dump_ch)
 write (dump_ch,'(a,f10.5)')   '[COLLISIONS] Collisions Cache use [Gb]:',cache_memory
 call msg('s',dump_ch)
 !
end subroutine COLLISIONS_IO_load
