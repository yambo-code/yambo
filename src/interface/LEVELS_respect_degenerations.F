!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine LEVELS_respect_degenerations(E,Dip,X)
 !
 use electrons,     ONLY:levels
 use stderr,        ONLY:intc
 use vec_operate,   ONLY:degeneration_finder
 use BS,            ONLY:BS_bands
 use X_m,           ONLY:X_t
 use DIPOLES,       ONLY:DIPOLE_t
 use QP_m,          ONLY:QP_n_G_bands
 use parser_m,      ONLY:parser
 !
 implicit none
 !
 type(levels)   ::E
 type(DIPOLE_t) ::Dip
 type(X_t)      ::X(5) 
 !
 ! Work Space
 !
 integer      :: B_user(2),B_fixed(2),i_f
 logical      :: l_USER_fields(16),DegFix
 character(2) :: X_str
 !
 ! By defaulft I assume that the degeneration is lifted
 ! but if the flag SkipDegFix is activated this operation is not done
 !
 call parser('DegFix',DegFix)
 !
 ! Check for USER definitions 
 !---------------------------
 call LEVELS_mirror("USER_fields",E,X,Dip,l_USER_fields)
 !
 ! Find the energy respecting range and fix it
 !---------------------------------------------
 do i_f=1,11
   !
   if (.not.l_USER_fields(i_f)) cycle
   !
   if(i_f< 6) then
     call levels_check(E,X(i_f)%ib)
     if (i_f==1) X_str="Xx"
     if (i_f==2) X_str="Xs"
     if (i_f==3) X_str="Xd"
     if (i_f==4) X_str="Xp"
     if (i_f==5) X_str="Xm"
     call fix_and_message(X_str,DegFix,X(i_f)%ib)
   endif
   if (i_f==6) then
     call levels_check(E,Dip%ib)
     call fix_and_message("Dipoles",DegFix,Dip%ib)
   endif
   if (i_f==7) then
     call levels_check(E,BS_bands)
     call fix_and_message("BS",DegFix,BS_bands)
   endif
   !
 enddo
 !
 ! MIRROR
 !--------
 call LEVELS_mirror("MIRROR_bands",E,X,Dip,l_USER_fields)
 !
 contains
  !
  subroutine levels_check(Ein,Bin)
   !
   use R_lattice,     ONLY:nkibz
   use electrons,     ONLY:levels,n_bands
   use units,         ONLY:HA2EV
   !
   type(levels) ::Ein
   integer      ::Bin(2)
   !
   ! Work Space
   integer :: ik,first_el(n_bands),n_of_el(n_bands),n_deg_grp,i_g,i_c
   !
   B_user =Bin
   B_fixed=Bin
   !
   do ik=1,nkibz
     !
     call degeneration_finder(E%E(:n_bands,ik,1),n_bands,first_el,n_of_el,n_deg_grp,0.0001/HA2EV)
     !
     do i_g=1,n_deg_grp
       do i_c=1,2
         if (B_fixed(i_c)>=first_el(i_g).and.B_fixed(i_c)<=first_el(i_g)+n_of_el(i_g)-1) then
           if (i_c==1) B_fixed(i_c)=first_el(i_g)
           if (i_c==2) B_fixed(i_c)=first_el(i_g)+n_of_el(i_g)-1
         endif
       enddo
     enddo
   enddo 
   !
  end subroutine
  !
  subroutine fix_and_message(what,IMPOSE,B_final)
   character(*) ::what
   logical      ::IMPOSE
   integer      ::B_final(2)
   !
   if (all((/B_fixed==B_user/))) return
   !
   call warning("["//what//"] User bands "//&
&               trim(intc(B_user(1)))//"-"//trim(intc(B_user(2)))//" break level degeneracy")
   !
   if (.not.IMPOSE) return
   !
   B_final=B_fixed
   call warning("["//what//"] Bands redefined to be "//trim(intc(B_final(1)))//"-"//trim(intc(B_final(2))))
   !
  end subroutine
 !
end subroutine
