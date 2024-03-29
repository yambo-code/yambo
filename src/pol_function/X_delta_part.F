!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine X_delta_part(NG,NW,X,Gamp)
 !
 ! The goal of this routine is to construct the "delta-like" part
 ! of the inverse dielectric matrix.
 ! This function is needed in the self-energy construction of systems
 ! without spatial inversion simmetry.
 !
 ! Now if the Lehmann representation of eps^-1(q,G,Gp;w) is, in general,
 ! complex
 !
 !  eps^-1(q,G,Gp;w)= \sum_I R_i(q,G,Gp) F_i(q,G,Gp;w)
 !
 ! I would like to rewrite eps^-1 as
 !
 !  eps^-1(q,G,Gp;w)=
 !   -1/pi \int dw' epsd^-1(q,G,Gp;w) [1/(w-w'+i delta)-1/(w+w'-idelta)]
 !
 ! For real R_i(q,G,Gp) (when both inversion and time-reversal) simply
 !
 !  epsd^-1(q,G,Gp;w)= Im [eps^-1(q,G,Gp;w)]   for w>0
 !  epsd^-1(q,G,Gp;w)= 0                       for w>0
 !
 ! Without inversion it is always true that (simply as conseguence of
 ! their definition)
 !
 !  R_i(q,G,Gp) = [ R_i(q,Gp,G) ]^*
 !
 ! Thus I can write
 !
 !  epsd^-1(q,G,Gp;w)=
 !    1/2 Im[eps^-1(G,Gp)+eps^-1(Gp,G)]-i Re[eps^-1(G,Gp)-eps^-1(Gp,G)]    w>0
 !                   =  0                                                  w<0
 !
 use pars,          ONLY:SP
 use D_lattice,     ONLY:i_space_inv
 implicit none
 !
 integer      :: NG,NW
 complex(SP)  :: Gamp(NG,NG)
 complex(SP)  :: X(NG,NG,NW)
 !
 ! Work Space
 !
 integer    :: i1,i2,i3
 complex(SP):: Xt(NG,NG)
 !
 if (i_space_inv==1) then
   forall(i1=1:NG,i2=1:NG,i3=1:NW) &
&        X(i1,i2,i3)=aimag(X(i1,i2,i3))*Gamp(i1,i2)
 else
   do i3=1,NW
     forall(i1=1:NG,i2=1:NG) Xt(i1,i2)  =X(i1,i2,i3)*Gamp(i1,i2)
     forall(i1=1:NG,i2=1:NG) X(i1,i2,i3)=&
&                                    0.5*aimag(Xt(i1,i2)+Xt(i2,i1))-&
&                               (0.,0.5)* real(Xt(i1,i2)-Xt(i2,i1))
   enddo
 endif
 !
end subroutine
