!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine X_delta_part(X,Xw,Gamp)
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
 use X_m,           ONLY:X_t,X_mat
 use frequency,     ONLY:w_samp
 implicit none
 type(X_t)    :: X
 type(w_samp) :: Xw
 complex(SP)  :: Gamp(X%ng,X%ng)
 !
 ! Work Space
 !
 integer    :: i1,i2,i3
 complex(SP):: Xt(X%ng,X%ng)
 !
 !
#if defined PJ_SC
 !
 ! In the calculation for the staticall screened m.e. of W
 ! the extraction of the delta_like part must be skipped
 !
 forall(i1=1:X%ng) X_mat(i1,i1,1)=X_mat(i1,i1,1)+1.
 forall(i1=1:X%ng,i2=1:X%ng) X_mat(i1,i2,1)=X_mat(i1,i2,1)*Gamp(i1,i2)
 return
#endif
 !
 if (i_space_inv==1) then
   forall(i1=1:X%ng,i2=1:X%ng,i3=1:Xw%n(1)) &
&        X_mat(i1,i2,i3)=aimag(X_mat(i1,i2,i3))*Gamp(i1,i2)
 else
   do i3=1,Xw%n(1)
     forall(i1=1:X%ng,i2=1:X%ng) Xt(i1,i2)=X_mat(i1,i2,i3)*Gamp(i1,i2)
     forall(i1=1:X%ng,i2=1:X%ng) X_mat(i1,i2,i3)=&
&                                     0.5*aimag(Xt(i1,i2)+Xt(i2,i1))-&
&                                (0.,0.5)* real(Xt(i1,i2)-Xt(i2,i1))
   enddo
 endif
 !
end subroutine
