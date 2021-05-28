!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine PH_of(q)
 !
 use pars,           ONLY:SP,schlen,pi
 use OUTPUT_simple,  ONLY:OUTPUT_add_column
 use RT_lifetimes,   ONLY:RT_PE_life
 use PHEL,           ONLY:PH_Q,PH_SF_W,PHEL_qp
 use ELPH,           ONLY:PH_freqs_sq,elph_branches,PHEL_desc,ELPH_desc,&
 &                        l_GKKP_expanded_DB_exists,l_GKKP_DB_exists
 use R_lattice,      ONLY:bz_samp
 use interfaces,     ONLY:DESC_write
 use QP_m,           ONLY:l_extended_output,QP_solver,QP_Sc_steps
 use stderr,         ONLY:intc
 !
 implicit none
 !
 type(bz_samp),     intent(in) :: q
 !
 ! Work Space
 !
 integer          :: iq,il,iw,iw_mem
 real(SP)         :: ph_freq
 character(schlen):: Gfile
 ! SF's
 real(SP)         :: KK_ref
 complex(SP)      :: D(QP_Sc_steps),W
 !
 if (trim(QP_solver)=="n") then
   call OUTPUT_add_column("ph",action="reset")
   call OUTPUT_add_column("ph",action="open")
   call DESC_write('o ph','#',PHEL_desc)
   call DESC_write('o ph','#',ELPH_desc)
   do iq=PH_Q(1),PH_Q(2)
     do il=elph_branches(1),elph_branches(2)
       call OUTPUT_add_column("ph",TITLES=(/"Q-point","Branch "/),I_VALUES=(/iq,il/))
       if (l_GKKP_expanded_DB_exists) ph_freq=abs(sqrt(PH_freqs_sq(q%k_table(iq,1),il)))
       if (l_GKKP_DB_exists         ) ph_freq=abs(sqrt(PH_freqs_sq(iq,il)))
       call OUTPUT_add_column("ph",TITLES=(/"Energy"/),R_VALUES=(/ph_freq/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"Width(emit)"/),R_VALUES=(/-RT_PE_life%emit(il,iq)/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"Width(abs)"/),R_VALUES=(/-RT_PE_life%abs(il,iq)/),UNIT="meV")
       call OUTPUT_add_column("ph",TITLES=(/"Width(eq)"/),R_VALUES=(/-aimag(PHEL_qp%PI(il,iq,1))/),UNIT="meV")
       !
       if (l_extended_output) then
         call OUTPUT_add_column("ph",TITLES=(/"Width(interb)"/),R_VALUES=(/-PHEL_qp%interband(il,iq)/),UNIT="meV")
         call OUTPUT_add_column("ph",TITLES=(/"Width(intrab)"/),R_VALUES=(/-PHEL_qp%intraband(il,iq)/),UNIT="meV")
         call OUTPUT_add_column("ph",TITLES=(/"Width(approx)"/),R_VALUES=(/-PHEL_qp%approx(il,iq)/),UNIT="meV")
       endif
       !
       call OUTPUT_add_column("ph",ORDER=(/"Q-point","Branch ","Energy "/))
       call OUTPUT_add_column("ph",ORDER=(/"Width(emit)  ","Width(abs)   ","Width(eq)    "/))
       if (l_extended_output) then
         call OUTPUT_add_column("ph",ORDER=(/"Width(interb)","Width(intrab)","Width(approx)"/))
       endif
       call OUTPUT_add_column("ph",action="write")
     enddo
   enddo
   call OUTPUT_add_column("ph",action="close")
 else
   do iq=PH_Q(1),PH_Q(2)
     do il=elph_branches(1),elph_branches(2)
       if (l_GKKP_expanded_DB_exists) ph_freq=abs(sqrt(PH_freqs_sq(q%k_table(iq,1),il)))
       if (l_GKKP_DB_exists         ) ph_freq=abs(sqrt(PH_freqs_sq(iq,il)))
       Gfile="G_ph_Q"//trim(intc(iq))//"_Branch_"//trim(intc(il))
       call OUTPUT_add_column(trim(Gfile),action="reset")
       call OUTPUT_add_column(trim(Gfile),action="open")
       call DESC_write('o '//trim(Gfile),'#',PHEL_desc)
       call DESC_write('o '//trim(Gfile),'#',ELPH_desc)
       !
       ! K_ref
       do iw=1,QP_Sc_steps-1
         if ( (real(PH_SF_W%p(iw))<0..and.real(PH_SF_W%p(iw+1))>0.).or.abs(real(PH_SF_W%p(iw)))==0.) then
           KK_ref=real(real(PHEL_qp%PI(il,iq,iw)))
         endif
       enddo
       !
       ! D
       do iw=1,QP_Sc_steps
         W=PH_SF_W%p(iw)
         D(iw)=D_o(ph_freq,W)/(1._SP-D_o(ph_freq,W)*(PHEL_qp%PI(il,iq,iw)-KK_ref))
       enddo
       !
       do iw=1,QP_Sc_steps
         iw_mem=(iq-1)*PH_Q(2)+iw
         call OUTPUT_add_column(trim(Gfile),TITLES=(/"Energy"/),R_VALUES=(/real(PH_SF_W%p(iw))/),UNIT="meV")
         call OUTPUT_add_column(trim(Gfile),TITLES=(/"-Im[D]/pi","Re[D]    "/),R_VALUES=(/-1./pi*aimag(D(iw)),real(D(iw))/))
         call OUTPUT_add_column(trim(Gfile),TITLES=(/"-Im[\Pi]/pi","Re[\Pi]    "/),&
&                               R_VALUES=(/-1./pi*aimag(PHEL_qp%PI(il,iq,iw)),real(PHEL_qp%PI(il,iq,iw)-KK_ref)/),UNIT="meV")
         call OUTPUT_add_column(trim(Gfile),action="write")
       enddo
       !
       call OUTPUT_add_column(trim(Gfile),action="close")
     enddo
   enddo
 endif
 !
 contains
   !
   function D_o(omega_o,freq)
     real(SP)    :: omega_o
     complex(SP) :: freq,D_o
     D_o=-1._SP/2._SP*(1._SP/(freq+omega_o)-1._SP/(freq-omega_o))
   end function
end subroutine
