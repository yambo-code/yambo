!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine OCCUPATIONS_Extend_Double_Grid(what,E,k)
 !
 use pars,           ONLY:SP,DP
 use parser_m,       ONLY:parser
 use electrons,      ONLY:levels,spin_occ,n_sp_pol,nel,n_bands
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:T_elecs,T_holes
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
 integer            :: i_try,ierr,i_ok
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
 call E_Fermi_to_Nel( )
 if (abs(delta_Nel(1))<1.E-5) then
   call Occupations_update( 1 )
   return
 endif
 !
 ! ..loops
 LOOP: do i_try=2,n_try-1
   !
   call check_and_propose_a_new_Fermi_level(ierr)
   if (ierr<0) call E_Fermi_to_Nel( )
   if (ierr>0) then
     call Occupations_update( i_ok )
     exit LOOP
   endif
   !
 enddo LOOP
 !
 if (i_ok==1 ) return
 if (i_ok==-1) call error("Impossible to align the Double-Grid")
 !
 if (abs(delta_Nel(1))>1.E-5) then
   call msg('r','[FineGd@'//what//'] (default) integrated electrons',delta_Nel(1)+nel)
   call msg('r','[FineGd@'//what//'] Femi Loop search iterations   ',i_ok)
   call msg('r','[FineGd@'//what//'] (new) Fermi level             ',Ef(i_ok)*HA2EV,"[eV]")
   call msg('r','[FineGd@'//what//'] (new) integrated electrons    ',delta_Nel(i_ok)+nel)
   call warning('[FineGd@'//what//'] Fermi level not aligned with from built-in levels')
 endif
 !
 contains
   !
   subroutine check_and_propose_a_new_Fermi_level(ierr)
     !
     integer :: ierr
     !
     if (abs(delta_Nel(i_try-1))<1.E-5) then
       i_ok=i_try-1
       ierr=1
       return
     endif
     if (i_try>=3) then
       if (delta_Nel(i_try-2)*delta_Nel(i_try-1)<0.) then
         ierr=1
         Ef(i_try)=Ef(i_try-1) 
         i_ok=i_try-1
         if (abs(delta_Nel(i_try-1))>abs(delta_Nel(i_try-2))) then
           Ef(i_try)=Ef(i_try-2) 
           i_ok=i_try-2
         endif
         return
       endif
       if (abs(delta_Nel(i_try-1)-delta_Nel(i_try-2))<1.E-5) then
         Ef(i_try)=Ef(i_try-2) 
         i_ok=i_try-2
         ierr=1
         return
       endif
     endif
     if (delta_Nel(i_try-1)>0.) Ef(i_try)=Ef(i_try-1)-dE
     if (delta_Nel(i_try-1)<0.) Ef(i_try)=Ef(i_try-1)+dE
     i_ok=-1
     ierr=-1
     !
   end subroutine
   !
   subroutine E_Fermi_to_Nel( )
     real(DP):: Nel_accumulated
     integer :: ik_fg,iE_fg,ikibz,Nk
     real(SP):: f,k_weight
     Nel_accumulated=0._DP
     do ik_fg=1,k%FGbare%N
       iE_fg   =k%FGbare%E_map(ik_fg)
       ikibz   =k%FGbare%k_map(ik_fg)
       k_weight=k%FGbare%weights(ik_fg)
       do is=1,n_sp_pol
         do ib=1,min(E%FG%nb,E%nb)
           if (iE_fg>0) then
             if(E%FG%E(ib,iE_fg,is)-Ef(i_try)<=0._SP) f=spin_occ*Fermi_fnc(E%FG%E(ib,iE_fg,is)-Ef(i_try),T_holes)
             if(E%FG%E(ib,iE_fg,is)-Ef(i_try)> 0._SP) f=spin_occ*Fermi_fnc(E%FG%E(ib,iE_fg,is)-Ef(i_try),T_elecs)
           else
             f=spin_occ*E%f(ib,ikibz,is)
           endif
           Nel_accumulated=Nel_accumulated+k_weight*f
         enddo
       enddo
     enddo
     delta_Nel(i_try)=real(Nel_accumulated,SP)-Nel
   end subroutine
   !
   subroutine Occupations_update( i_loop )
     integer :: ik_fg,i_loop
     E%FG%E(:,:,:)=E%FG%E(:,:,:)-Ef(i_loop)
     E%FG%E_Fermi=Ef(i_loop)
     do ik_fg=1,E%FG%Nk
       do is=1,n_sp_pol
         do ib=1,E%FG%nb
           if(E%FG%E(ib,ik_fg,is)<=0._SP) E%FG%f(ib,ik_fg,is)=spin_occ*Fermi_fnc(E%FG%E(ib,ik_fg,is),T_holes)
           if(E%FG%E(ib,ik_fg,is)> 0._SP) E%FG%f(ib,ik_fg,is)=spin_occ*Fermi_fnc(E%FG%E(ib,ik_fg,is),T_elecs)
         enddo
       enddo
     enddo
   end subroutine
   !
end subroutine
