!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine OCCUPATIONS_Extend_Double_Grid(what,E,k)
 !
 use pars,           ONLY:SP,DP
 use parser_m,       ONLY:parser
 use electrons,      ONLY:levels,spin_occ,n_sp_pol,nel,n_bands
 use R_lattice,      ONLY:bz_samp,bz_fine_grid
 use D_lattice,      ONLY:Tel
 use functions,      ONLY:Fermi_fnc
 use units,          ONLY:HA2EV
 use com,            ONLY:msg
 !
 implicit none
 ! 
 character(*)        :: what
 type(levels)        :: E
 type(bz_samp)       :: k
 !
 ! Work Space
 !
 ! Ef loop
 integer ,parameter :: n_try=20
 real(SP),parameter :: dE=0.2/HA2EV
 integer            :: i_try,ierr
 real(SP)           :: delta_Nel(n_try),Ef(n_try)
 !
 ! re-alignment
 logical              :: l_user_bands_shift
 integer              :: is,ib,nb_shift,ik
 real(SP)             :: Emean(n_bands,n_sp_pol),Emean_fg(n_bands,n_sp_pol),E_shift,E_diff(2)
 real(SP),allocatable :: Edummy(:,:,:)
 !
 if (E%FG%nb==0.or..not.allocated(E%FG%E)) return
 !
 !E%FG%E(:,:,:)=E%FG%E(:,:,:)-E%E_Fermi
 !do is=1,n_sp_pol
 !  do ik=1,E%FG%nk
 !    do ib=1,E%FG%nb
 !      E%FG%f(ib,ik,is) =spin_occ*Fermi_fnc(E%FG%E(ib,ik,is),Tel)
 !    enddo
 !  enddo 
 !enddo
 !return
 !
 call parser('FGbandshift',l_user_bands_shift)
 !
 if (l_user_bands_shift) then
   !
   call parser('FGbandshift',nb_shift)
   call warning('[FineGd@'//what//'] User defined bands shift')
   call msg('r','[FineGd@'//what//'] #of bands to be shifted       ',nb_shift)
   !
   allocate(Edummy(E%FG%nb+nb_shift,E%FG%nk,n_sp_pol))
   !
   E_diff=(/100.,-100./)
   do is=1,n_sp_pol
     !
     do ib=1,n_bands
       Emean(ib,is)   =sum(E%E(ib,:,is)+E%E_fermi)/real(k%nibz)
       if (ib<=E%FG%Nb) Emean_fg(ib,is)=sum(E%FG%E(ib,:,is))/real(E%FG%nk)
     enddo
     !
     if (nb_shift>0) then
       !
       E_shift=Emean_fg(1,is)-Emean(nb_shift+1,is)
       !
       do ib=E%FG%nb,1,-1
         Edummy(ib+nb_shift,:,:)=E%FG%E(ib,:,:)+E_shift
       enddo
       do ib=1,nb_shift
         Edummy(ib,1:E%FG%nk,is)=E%E(ib,1,is)+E_shift+E%E_fermi
       enddo
       !
       deallocate(E%FG%E,E%FG%f)
       E%FG%nb=E%FG%nb+nb_shift
       allocate(E%FG%E(E%FG%nb+nb_shift,E%FG%nk,n_sp_pol))
       allocate(E%FG%f(E%FG%nb+nb_shift,E%FG%nk,n_sp_pol))
       E%FG%E=Edummy
       deallocate(Edummy)
       !
     else
       !
       call error("Negative FGbandshift still not implemented")
       !
     endif
     do ib=1,E%FG%nb
       Emean_fg(ib,is)=sum(E%FG%E(ib,:,is))/real(E%FG%nk)
       if (abs(Emean_fg(ib,is)-Emean(ib,is))<E_diff(1)) E_diff(1)=abs(Emean_fg(ib,is)-Emean(ib,is))
       if (abs(Emean_fg(ib,is)-Emean(ib,is))>E_diff(2)) E_diff(2)=abs(Emean_fg(ib,is)-Emean(ib,is))
     enddo
   enddo
   !
   call msg('r','[FineGd@'//what//'] Min/MAX Mean energy difference', E_diff*HA2EV,"[eV]")
   !
 endif
 !
 ! ..init
 i_try=1
 Ef(1)=E%E_Fermi
 if (k%FGbz%N>0 ) delta_Nel(1)=gimme_Nel(k%FGbz,"BZ") 
 if (k%FGibz%N>0) delta_Nel(1)=gimme_Nel(k%FGibz,"IBZ") 
 call Ef_update(ierr)
 !
 ! ..loops
 LOOP: do i_try=2,n_try-1
   !
   if (ierr<0) then
     if (k%FGbz%N>0 ) delta_Nel(i_try)=gimme_Nel(k%FGbz,"BZ") 
     if (k%FGibz%N>0) delta_Nel(i_try)=gimme_Nel(k%FGibz,"IBZ") 
     call Ef_update(ierr)
   endif
   !
   if (ierr>0) then
     if (k%FGbz%N>0 ) delta_Nel(i_try)=gimme_Nel(k%FGbz,"BZ") 
     if (k%FGibz%N>0) delta_Nel(i_try)=gimme_Nel(k%FGibz,"IBZ") 
     E%FG%E(:,:,:)=E%FG%E(:,:,:)-Ef(i_try)
     exit LOOP
   endif
   !
 enddo LOOP
 !
 if (abs(delta_Nel(1))>1.E-5) then
   call msg('r','[FineGd@'//what//'] (default) integrated electrons',delta_Nel(1)+nel)
   call msg('r','[FineGd@'//what//'] Femi Loop search iterations   ',i_try)
   call msg('r','[FineGd@'//what//'] (new) Fermi level             ',Ef(i_try)*HA2EV,"[eV]")
   call msg('r','[FineGd@'//what//'] (new) integrated electrons    ',delta_Nel(i_try)+nel)
   call warning('[FineGd@'//what//'] Fermi level not aligned with from built-in levels')
 endif
 !
 contains
   !
   subroutine Ef_update(ierr)
     !
     integer :: ierr
     !
     if (abs(delta_Nel(i_try))<1.E-5) then
       ierr=1
       return
     endif
     if (i_try>1) then
       if (delta_Nel(i_try)*delta_Nel(i_try-1)<0.) then
         ierr=1
         if (abs(delta_Nel(i_try))>abs(delta_Nel(i_try-1))) then
           Ef(i_try)=Ef(i_try-1) 
         endif
         return
       endif
       if (abs(delta_Nel(i_try)-delta_Nel(i_try-1))<1.E-5) then
         Ef(i_try)=Ef(i_try-1) 
         ierr=1
         return
       endif
     endif
     if (delta_Nel(i_try)>0.) Ef(i_try+1)=Ef(i_try)-dE
     if (delta_Nel(i_try)<0.) Ef(i_try+1)=Ef(i_try)+dE
     ierr=-1
     !
   end subroutine
   !
   function gimme_Nel(FG,mode)
     type(bz_fine_grid) :: FG
     character(*)       :: mode
     real(DP)           :: Nel_accumulated
     integer :: ik_fg,iE_fg,ikbz,ikibz
     real(SP):: En,gimme_Nel,f
     Nel_accumulated=0._DP
     do ik_fg=1,FG%N
       iE_fg=FG%E_map(ik_fg)
       ikbz =FG%k_map(ik_fg)
       if (mode=="IBZ") ikibz=ikbz
       if (mode=="BZ" ) ikibz=k%sstar(ikbz,1)
       do is=1,n_sp_pol
         do ib=1,min(E%FG%nb,E%nb)
           En=E%E(ib,ikibz,is)
           if (iE_fg>0) En=E%FG%E(ib,iE_fg,is)
           f=spin_occ*Fermi_fnc(En-Ef(i_try),Tel)
           Nel_accumulated=Nel_accumulated+FG%weights(ik_fg)*f
           if (iE_fg>0) E%FG%f(ib,iE_fg,is)=f
         enddo
       enddo
     enddo
     gimme_Nel=real(Nel_accumulated,SP)-Nel
   end function
   !
end subroutine
