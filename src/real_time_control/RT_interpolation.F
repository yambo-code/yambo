!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_interpolation(bands,E,K,what,Time,l_f,l_E,l_W)
 !
 use pars,           ONLY:schlen,SP
 use stderr,         ONLY:string_split,real2ch,intc
 use memory_m,       ONLY:mem_est
 use parser_m,       ONLY:parser
 use electrons,      ONLY:levels,n_sp_pol,BZ_DbGd_tot_nkpts,BZ_DbGd_kpts,BZ_DbGd_nbands
 use R_lattice,      ONLY:bz_samp
 use IO_m,           ONLY:OP_RD_CL,NONE,DUMP,io_control
 use units,          ONLY:FS2AUT,PS2AUT,HA2EV
 use real_time,      ONLY:RT_carriers
 use com,            ONLY:warning,msg
 !
 implicit none
 !
 integer,intent(in)    :: bands(2)
 type(levels),  intent(inout) :: E
 type(bz_samp), intent(in)    :: K
 character(3),  intent(in)    :: what ! X,K,G
 real(SP),      intent(in)    :: Time
 logical,       intent(in)    :: l_f,l_E,l_W
 !
 ! Work space
 !
 integer               :: i_RT,nb(2),ik
 real(SP)              :: df_Max,dE_Max,N_carriers(2)
 real(SP), allocatable :: V_in_tmp(:)
 integer,parameter     :: n_pieces=10
 integer, external     :: io_RT_components
 logical               :: l_DbGd
 !
 l_DbGd=BZ_DbGd_tot_nkpts>0
 !
 nb(1)=max(bands(1),RT_carriers%nb(1))
 nb(2)=min(bands(2),RT_carriers%nb(2))
 !
 N_carriers=0._SP
 !
 if (l_f.and..not.associated(E%df)) then
   !
   ! These are *always* the equilibrium occupations
   !
   if(.not.associated(E%fo)) then
     allocate(E%fo(E%nb,E%nk,n_sp_pol))
     call mem_est("E-df E-fo",(/size(E%fo),size(E%df)/),(/SP,SP/))
     E%fo=E%f
   endif
   !
   allocate(E%df(E%nb,E%nk,n_sp_pol))
   call mem_est("E-df",(/size(E%df)/),(/SP,SP/))
   E%df=0._SP
   call Interpolate_and_transfer("df",RT_carriers%delta_f,K%pt,E%nk)
   E%f=E%fo+E%df
   !
   if (l_DbGd) then
     allocate(E%df_DbGd(E%nb,BZ_DbGd_tot_nkpts,n_sp_pol))
     call mem_est("E-df_DbGd",(/size(E%df_DbGd)/),(/SP,SP/))
     E%df_DbGd=0._SP
     call Interpolate_and_transfer("df",RT_carriers%delta_f,BZ_DbGd_kpts,BZ_DbGd_tot_nkpts)
   endif
   !
 endif
 !
 if (l_E) then
   !
   ! ** WARNING ** (DS: 2015/10/21)
   ! Eo may already contain the DFT energies in case QP corrections have been applyed
   ! In this case the equilibrium QP energies are not stored anywhere
   !
   if(.not.associated(E%Eo)) then
     allocate(E%Eo(E%nb,E%nk,n_sp_pol))
     call mem_est("E-Eo",(/size(E%Eo)/),(/SP/))
     E%Eo=E%E
   endif
   !
   call Interpolate_and_transfer("dE",RT_carriers%delta_E,K%pt,E%nk)
   !
   if (l_DbGd) call Interpolate_and_transfer("dE",RT_carriers%delta_E,BZ_DbGd_kpts,BZ_DbGd_tot_nkpts)
   !
 endif
 !
 if (l_W) then
   !
   ! To be implemented
   return
   !
 endif
 !
 call msg('nr',   'Updated Carrier components')
 call msg('rn',   '==========================')
 call msg('r',    '[RT] Reference Time           [fs]: ',Time/FS2AUT)
 call msg('r',    '[RT] Bands range                  : ', (/nb(1),nb(2)/)) 
 !
 if (l_f) then
   call msg('r',  '[RT] Maximum occupations change   : ',df_Max)
   call msg('r',  '[RT] N of carriers                : ',N_carriers(1))
   if (l_DbGd) call msg('r',  '[RT]                 (Double Grid): ',N_carriers(2))
 endif
 !
 if (l_E) then
   call msg('r',  '[RT] Max energy levels change [ev]: ',dE_Max*HA2EV)
 endif
 !
 call msg('rn',  '[RT] Full and metallic bands       : ', (/E%nbf,E%nbm/)) 
 !
 call REPORT_OCCUPATIONS(E)
 !
 contains
  !
  subroutine Interpolate_and_transfer(OBJ,V_in,K_in,NK)
    !
    character(*) :: OBJ
    integer      :: NK
    real(SP)     :: V_in(RT_carriers%nstates),K_in(NK,3)
    !
    ! ws
    !
    integer      :: i_carr,nb_max
    real(SP)     :: K_weights(NK),V_out(RT_carriers%nb(1):RT_carriers%nb(2),NK,n_sp_pol)
    !
    if (NK/=E%nk) K_weights=1._SP/real(BZ_DbGd_tot_nkpts,SP)
    if (NK==E%nk) K_weights(:)=K%weights(:)
    !
    call Nearest_kpt_interpolation(RT_carriers%nk,NK,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                  RT_carriers%kpt,K_in,RT_carriers%k_weight,K_weights,V_in,V_out,what,.TRUE.)
    !
    if (NK==E%nk) nb_max=    nb(2)
    if (NK/=E%nk) nb_max=min(nb(2),BZ_DbGd_nbands)
    !
    if (OBJ=="df") then
      !
      ! Metallic and Insulating bands update
      !
      E%nbf=nb(1)-1
      E%nbm=nb(2)
      !
      if(NK==E%nk) then
        E%df( nb(1):nb(2),:,:)=V_out(nb(1):nb(2),:,:)
        df_Max=maxval(abs(V_out(nb(1):nb(2),:,:)))
        i_carr=1
      else if (NK/=E%nk) then
        E%df_DbGd( nb(1):nb_max,:,:)=V_out(nb(1):nb_max,:,:)
        i_carr=2
      endif
      !
      do ik=1,NK
        N_carriers(i_carr)=N_carriers(i_carr)+sum(abs(V_out(nb(1):nb_max,ik,:)))*K_weights(ik)/2._SP
      enddo
      !
    else if (OBJ=="dE") then
      !
      if(NK==E%nk) then
        E%E( nb(1):nb(2),:,:)=E%E(nb(1):nb(2),:,:)+V_out(nb(1):nb(2),:,:)
        dE_Max=maxval(abs(V_out(nb(1):nb(2),:,:)))
      else if (NK/=E%nk) then
        E%E_DbGd( nb(1):nb_max,:,:)=E%E_DbGd(nb(1):nb_max,:,:)+V_out(nb(1):nb_max,:,:)
      endif
      !
    endif
    !
  end subroutine Interpolate_and_transfer
  !
end subroutine