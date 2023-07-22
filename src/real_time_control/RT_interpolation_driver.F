!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine RT_interpolation_driver(bands,E,K,what,Time,l_f,l_E,l_W,l_B,NN,VERB)
 !
 use pars,           ONLY:SP,schlen
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use units,          ONLY:HA2EV,AUT2FS
 use drivers,        ONLY:l_real_time
 use real_time,      ONLY:RT_carriers,l_TRabs_YPP
 use com,            ONLY:msg
 use interpolate,    ONLY:l_integral_respect,NN_n_of_nearest
 !
#include<memory.h>
 !
 integer,       intent(in)    :: bands(2),NN
 type(levels),  intent(inout) :: E
 type(bz_samp), intent(inout) :: K
 character(3),  intent(in)    :: what ! X,K,G
 real(SP),      intent(in)    :: Time
 logical,       intent(in)    :: l_f,l_E,l_W,l_B,VERB
 !
 ! Work space
 !
 integer               :: nb(2)
 real(SP)              :: df_Max,dE_Max,N_carriers(2)
 integer,parameter     :: n_pieces=10
 character(schlen)     :: title
 logical               :: l_DbGd
 !
 ! Setup
 !-------
 l_DbGd=K%FGibz%N>0     .and. .not.l_TRabs_YPP
 l_integral_respect=.not.l_real_time.and..not.l_TRabs_YPP
 NN_n_of_nearest=NN
 !
 nb(1)=max(bands(1),RT_carriers%nb(1))
 nb(2)=min(bands(2),RT_carriers%nb(2))
 !
 N_carriers=0._SP
 !
 if (l_f.and..not.allocated(E%df)) then
   !
   ! These are *always* the equilibrium occupations
   !
   if(.not.allocated(E%fo)) then
     YAMBO_ALLOC(E%fo,(E%nb,E%nk,n_sp_pol))
     E%fo=E%f
   endif
   !
   YAMBO_ALLOC(E%df,(E%nb,E%nk,n_sp_pol))
   E%df=0._SP
   if (l_DbGd.and..not.allocated(E%FG%df)) then
     if (.not.allocated(E%FG%df)) then
       YAMBO_ALLOC(E%FG%df,(E%FG%nb,E%FG%nk,n_sp_pol))
     endif
     E%FG%df=0._SP
   endif
   !
   call RT_interpolation_doit_and_transfer("df",K,E,nb,l_B,N_carriers,df_Max,dE_Max)
   !
   E%f=E%fo+E%df
   if (l_DbGd) E%FG%f=E%FG%f+E%FG%df
   !
 endif
 !
 if (l_E) then
   !
   ! ** WARNING ** (DS: 2015/10/21)
   ! Eo may already contain the DFT energies in case QP corrections have been applyed
   ! In this case the equilibrium QP energies are not stored anywhere
   !
   if(.not.allocated(E%Eo)) then
     YAMBO_ALLOC(E%Eo,(E%nb,E%nk,n_sp_pol))
     E%Eo=E%E
   endif
   !
   call RT_interpolation_doit_and_transfer("dE",K,E,nb,l_B,N_carriers,df_Max,dE_Max)
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
 if (.not.VERB) return
 !
 title=" "
 if (l_f) title="f"
 if (l_E) title=trim(title)//"E"
 if (l_W) title=trim(title)//"W"
 title=trim(title)//"@"//trim(what)
 !
 call msg('nr',   'Updated Carrier components')
 call msg('rn',   '==========================')
 call msg('r',    '['//trim(title)//'] Reference Time                ',Time*AUT2FS,"[fs]")
 call msg('r',    '['//trim(title)//'] Bands range                   ', (/nb(1),nb(2)/)) 
 !
 if (l_f) then
   call msg('r',  '['//trim(title)//'] Maximum occupations change    ',df_Max)
   call msg('r',  '['//trim(title)//'] N of carriers                 ',N_carriers(1))
   if (l_DbGd) call msg('r',  '['//trim(title)//']                 (Double Grid) ',N_carriers(2))
 endif
 !
 if (l_E) then
   call msg('r',  '['//trim(title)//'] Max energy levels change      ',dE_Max*HA2EV,"[eV]")
 endif
 !
 call msg('rn',   '['//trim(title)//'] Full and metallic bands       ', (/E%nbf(1),E%nbm(1)/)) 
 !
 if (.not.l_real_time) call REPORT_Occupations(trim(title),E)
 !
end subroutine RT_interpolation_driver
