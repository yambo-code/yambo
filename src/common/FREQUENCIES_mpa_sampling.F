!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): DALV AF
!
!
!---------------------------------------------------------------
! Pair sampling of a positive unidimentional (frequency) domain 
! as a function of a parameter (the scale)
! 
!                     to be used in the MPA interpolation*
!
! *DA. Leon et al, PRB 104, 115157 (2021)
!
! Notes:
!
!   1) Homogeneous (homo)
!   2) Linear Partition Pair Sampling (lPPS)
!   3) Quadratic Partition Pair Sampling (qPPS)
!   4) Cubic Partition Pair Sampling (cPPS)
!   5) ...
!
! *The samplings do not depend on the sampled function 
!---------------------------------------------------------------

 subroutine FREQUENCIES_mpa_sampling(np, w1, w2, d, ps, gs, w) 
   use pars,     ONLY:SP,DP,cI
   implicit none
   !
   integer,      intent(in)  :: np       ! number of desired frequency pairs/poles
   complex(SP),  intent(in)  :: w1,w2    ! segment [w1,w2]
   real(SP),     intent(in)  :: d(2)     ! shifts
   character(2), intent(in)  :: ps       ! flavour of sampling: 1line, 2lines, ...
   character(2), intent(in)  :: gs       ! type of grid: (ho)mo, (lP)PS, (qP)PS, (cP)PS, ...
   complex(SP),  intent(out) :: w(2*np)
   !
   integer     :: i,ex
   integer(SP) :: lp,r,c,p
   real(SP)    :: aux
   complex(SP) :: ws
   real(SP), parameter :: log2=0.693147180560_SP
   !
   ws = w2-w1
   select case(np)
     case(1)
       w = (/w1+epsilon(1._SP), w2/)
     case(2)
       select case(ps)
         case('2l') !2lines
           w = (/real(w1,SP)+d(1)*cI, real(w2,SP)+d(2)*cI, w1, w2/)
         case('1l') !1line
           select case(gs)
             case ('ho') !homo
               w = (/w1, w1+0.33_SP*ws, w1+0.66_SP*ws, w2/)
             case ('lP') !lhPPS
               w = (/w1, w1+0.25_SP*ws, w1+0.5_SP*ws, w2/)
             case ('qP') !QPPS
               w = (/w1, w1+0.0625_SP*ws, w1+0.25_SP*ws, w2/)
             case ('cP') !CPPS
               w = (/w1, w1+0.015625_SP*ws, w1+0.125_SP*ws, w2/)
             case default
               ! error 
           end select
         case default
         ! error 
       end select
     case (3)
       select case(ps)
         case('2l') !2lines
           select case(gs)
             case ('ho') !homo
               w=(/real(w1,SP)+d(1)*cI,real(w1+0.5_SP*ws)+d(2)*cI,real(w2,SP)+d(2)*cI,w1,w1+0.5_SP*ws,w2/)
             case ('lP') !lPPS
               w=(/real(w1,SP)+d(1)*cI,real(w1+0.5_SP*ws)+d(2)*cI,real(w2,SP)+d(2)*cI,w1,w1+0.5_SP*ws,w2/)
             case ('qP') !qPPS
               w=(/real(w1,SP)+d(1)*cI,real(w1+0.25_SP*ws)+d(2)*cI,real(w2,SP)+d(2)*cI,w1,w1+0.25_SP*ws,w2/)
             case ('cP') !cPPS
               w=(/real(w1,SP)+d(1)*cI,real(w1+0.125_SP*ws)+d(2)*cI,real(w2,SP)+d(2)*cI,w1,w1+0.125_SP*ws,w2/)
             case default
               ! error 
           end select
         case('1l') !1line
           select case(gs)
             case ('ho') !homo
               w=(/w1,w1+0.2_SP*ws,w1+0.4_SP*ws,w1+0.6_SP*ws,w1+0.8_SP*ws,w2/)
             case ('lP') !lPPS
               w=(/w1,w1+0.125_SP*ws,w1+0.25_SP*ws,w1+0.5_SP*ws,w1+0.75_SP*ws,w2/)
             case ('qP') !qPPS
               w=(/w1,w1+0.015625_SP*ws,w1+0.0625_SP*ws,w1+0.25_SP*ws,w1+0.5625_SP*ws,w2/)
             case ('cP') !cPPS
               w=(/w1,w1+0.00195313_SP*ws,w1+0.015625_SP*ws,w1+0.125_SP*ws,w1+0.421875_SP*ws,w2/)
             case default
               ! error 
           end select
       end select
     case default 
       select case(ps)
         case('2l') !2lines
           select case(gs)
             case('ho') !homo
               w(np+1) = w1
               w(1)=real(w1,SP)+d(1)*cI
               do i=2, np 
                 w(np+i)=w1+ws*(i-1)/(np-1) 
                 w(i)=real(w(np+i),SP)+d(2)*cI
               enddo
             case('lP') !lPPS
               w(np+1) = w1
               w(1)=real(w1,SP)+d(1)*cI
               w(2*np) = w2
               w(np)=real(w2,SP)+d(2)*cI
               lp=log(-1._SP+np)/log(2._SP)
               r=mod(np-1,2**lp)
               if (r>0) then
                 do i=2,2*r
                   w(np+i)=w1+ws*(i-1)/2._SP**(lp+1)
                   w(i)=real(w(np+i),SP)+d(2)*cI 
                 enddo
                 !
                 do i=2*r+1,np-1
                   w(np+i)=w1+ws*(i-1-r)/2._SP**(lp)
                   w(i)=real(w(np+i),SP)+d(2)*cI 
                 enddo
               else
                 w(np+2)=w1+ws/2._SP**(lp+1)
                 w(2)=real(w(np+2),SP)+d(2)*cI 
                 do i=2*r+3,np-1
                   w(np+i)=w1+ws*(i-2-r)/2._SP**(lp)
                   w(i)=real(w(np+i),SP)+d(2)*cI 
                 enddo
               endif  
               !
             case default !qPPS or cPPS
               ex=2 !gs='qP'
               if(gs=='cP') ex=3
               !
               w(np+1) = w1
               w(1)=real(w1,SP)+d(1)*cI
               w(2*np) = w2
               w(np)=real(w2,SP)+d(2)*cI
               lp=log(-1._SP+np)/log(2._SP)
               r=mod(np-1,2**lp)
               if (r>0) then
                 do i=2,2*r
                   w(np+i)=w1+ws*( (i-1)/2._SP**(lp+1) )**ex
                   w(i)=real(w(np+i),SP)+d(2)*cI 
                 enddo
                 !
                 do i=2*r+1,np-1
                   w(np+i)=w1+ws*( (i-1-r)/2._SP**(lp) )**ex
                   w(i)=real(w(np+i),SP)+d(2)*cI 
                 enddo
               else
                 w(np+2)=w1+ws/( 2._SP**(lp+1) )**ex
                 w(2)=real(w(np+2),SP)+d(2)*cI 
                 do i=2*r+3,np-1
                   w(np+i)=w1+ws*( (i-2-r)/2._SP**(lp) )**ex
                   w(i)=real(w(np+i),SP)+d(2)*cI 
                 enddo
               endif  
               !
           end select
         case('1l') !1line
           select case(gs)
             case('ho') !homo
               w(1) = w1
               do i=2, 2*np 
                 w(i)=w1+ws*(i-1)/(2*np-1) 
               enddo
             case('lP') !lPPS
               w(1) = w1
               w(2*np) = w2
               lp=log(-1._SP+2._SP*np)/log(2._SP)
               r=mod(2*np-1,2**lp)
               if (r>0) then
                 do i=2,2*r
                   w(i)=w1+ws*(i-1)/2._SP**(lp+1)
                 enddo
                 !
                 do i=2*r+1,2*np-1
                   w(i)=w1+ws*(i-1-r)/2._SP**(lp)
                 enddo
               else
                 w(2)=w1+ws/2._SP**(lp+1)
                 do i=2*r+3,2*np-1
                   w(i)=w1+ws*(i-2-r)/2._SP**(lp)
                 enddo
               endif 
               !
             case default !qPPS or cPPS
               ex=2 !gs='qP'
               if(gs=='cP') ex=3
               !
               w(1) = w1
               w(2*np) = w2
               lp=log(-1._SP+2._SP*np)/log(2._SP)
               r=mod(2*np-1,2**lp)
               if (r>0) then
                 do i=2,2*r
                   w(i)=w1+ws*( (i-1)/2._SP**(lp+1) )**ex
                 enddo
                 !
                 do i=2*r+1,2*np-1
                   w(i)=w1+ws*( (i-1-r)/2._SP**(lp) )**ex
                 enddo
               else
                 w(2)=w1+ws/2._SP**(lp+1)
                 do i=2*r+3,2*np-1
                   w(i)=w1+ws*( (i-2-r)/2._SP**(lp) )**ex
                 enddo
               endif 
               !
           end select
       end select
   end select 
   !
end subroutine FREQUENCIES_mpa_sampling

