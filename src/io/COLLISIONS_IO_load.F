!
! Copyright (C) 2000-2012 A. Marini, C. Attaccalite and the YAMBO team 
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
 use drivers,         ONLY:l_sc_hf,l_sc_cohsex,l_sc_exx,l_collisions_IO,l_sc_srpa
 use com,             ONLY:warning,msg
 use global_XC,       ONLY:SC_HF,SC_COHSEX
 use QP_m,            ONLY:QP_n_states,QP_nb,QP_nk
 use SC,              ONLY:SC_bands
 use memory_m,        ONLY:mem_sum,mem_est
 use collisions_IO,   ONLY:COLLISIONS_v,COLLISIONS_table,V_kind,collisions_IO_free,&
&                          COLLISIONS_path,io_COLLISIONS
 use stderr,          ONLY:intc
 use IO_m,            ONLY:OP_RD_CL,io_control,VERIFY,REP,io_file
 use par_proc_m,      ONLY:pp_indexes,pp_indexes_reset
 use par_indexes_m,   ONLY:par_indexes
 use collisions_CACHE,ONLY:COLLISIONS_cache_size,last_QP_mem,cache_memory,CACHE
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
 integer            :: i1,ic,i_qp,ib,ibp,ik,ID,n_tot_rho_states,io_err(max_V_kinds),V_kinds(max_V_kinds),n_V_kinds,&
&                      max_COLLISION_cache
 real(SP)           :: mem_before_cache
 type(pp_indexes)   :: px
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
   do i1=1,n_V_kinds
     !
     V_kind=V_kinds(i1)
     !
     call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
     io_err(i1)=io_COLLISIONS(0,ID)
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
 if (COLLISIONS_cache_size==0..or..not.l_collisions_IO) return
 !
 call pp_indexes_reset(px)
 !
 call par_indexes(px,(/QP_n_states/))
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
 max_COLLISION_cache=QP_nb**2*QP_nk
 !
 allocate(COLLISIONS_disk(SC_bands(1):QP_nb,SC_bands(1):QP_nb,QP_nk))
 call mem_est("COLLISIONS_disk",(/QP_nb-SC_bands(1)+1,QP_nb-SC_bands(1)+1,QP_nk/),(/2*SP/))
 !
 i_qp=0
 !
 allocate(CACHE(QP_n_states))
 !
 do while(cache_memory<COLLISIONS_cache_size.and.i_qp<QP_n_states)  
   !
   i_qp=i_qp+1
   !
   if(.not.px%element_1D(i_qp)) cycle  
   !
   allocate(CACHE(i_qp)%COLLISIONS_table(SC_bands(1):QP_nb,SC_bands(1):QP_nb,QP_nk))
   call mem_est(" QP_COLLISIONS_table_"//trim(intc(i_qp)),(/LP/))
   !
   CACHE(i_qp)%COLLISIONS_table(:,:,:)=0
   COLLISIONS_disk=(0.,0.)
   !
   do i1=1,n_V_kinds
     !
     V_kind=V_kinds(i1)
     !
     call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),ID=ID)
     io_err(1)=io_COLLISIONS(i_qp,ID)
     !
     ic=0
     !
     do ib=SC_bands(1),QP_nb
       do ibp=SC_bands(1),QP_nb
         do ik=1,QP_nk
           !
           if(COLLISIONS_table(ib,ibp,ik)==1) then
             ic=ic+1
             CACHE(i_qp)%COLLISIONS_table(ib,ibp,ik)=1
             COLLISIONS_disk(ib,ibp,ik)=COLLISIONS_disk(ib,ibp,ik)+COLLISIONS_v(ic)
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
   allocate(CACHE(i_qp)%COLLISIONS_v(n_tot_rho_states))
   call mem_est("QP_COLLISIONS_v"//trim(intc(i_qp)),(/2*SP/))
   !
   ic=0
   !
   do ib=SC_bands(1),QP_nb
     do ibp=SC_bands(1),QP_nb
       do ik=1,QP_nk
         if(CACHE(i_qp)%COLLISIONS_table(ib,ibp,ik)==1) then
           ic=ic+1
           CACHE(i_qp)%COLLISIONS_v(ic)=COLLISIONS_disk(ib,ibp,ik)
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
 deallocate(COLLISIONS_disk)
 call mem_est("COLLISIONS_disk")
 !
 ! If all QP are in memory I don't need anymore collisions_IO
 !
 if(last_QP_mem==QP_n_states) call collisions_IO_free()
 !
 call pp_indexes_reset(px)
 !
 write (dump_ch,'(a,f10.2,a)') '[COLLISIONS] QP in memory :',real(last_QP_mem)/real(QP_n_states)*100.0,'%'
 call msg('s',dump_ch)
 write (dump_ch,'(a,f10.5)')   '[COLLISIONS] Collisions Cache use [Gb]:',cache_memory
 call msg('s',dump_ch)
 !
end subroutine COLLISIONS_IO_load
