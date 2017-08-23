!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
!
! Copyright (C) 1992-2005, Lucia Reining, Valerio Olevano,
!   Francesco Sottile, Stefan Albrecht, Giovanni Onida,
!                    Fabien Bruneval
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
subroutine Dipole_kb_Ylm(Ylm,dYlm,Ygrad,lang,k)
 !
 ! Ylm(il,im) := Spherical harmonic 
 ! dYlm([1,2],il,im) := Derivative of the Spherical harmonic [1=theta,2=phi]
 !
 !  il=1,2,3
 !  il=1  im=1
 !  il=2  im=1,2,3
 !  il=3  im=1,2,3,4,5
 !
 ! Ygrad(1,i) = \partial(theta)/\partial(k_i) i=1,3
 ! Ygrad(2,i) = \partial(phi)/\partial(k_i) i=1,3
 !
 use pars, ONLY:SP,DP,pi,cI,cZERO
 use com,  ONLY:warning
 !
 implicit none
 integer     :: lang
 complex(SP) :: Ylm(lang,2*(lang-1)+1),dYlm(2,lang,2*(lang-1)+1)
 real(SP)    :: k(3),Ygrad(2,3)
 ! 
 ! Work Space
 !
 real(SP)    :: costh,sinth,cosphi,sinphi,ch,kmod,kxymod,local_err
 complex(SP) :: eiphi
 !
 if (lang>4) call warning('Ylm with l>3 not coded. Projectors set to zero.')
 !
 Ylm(:,:)   =cZERO
 dYlm(:,:,:)=cZERO
 Ygrad(:,:) =0._SP
 !
 local_err=1.E-6
 if(SP==DP) local_err=1.E-12
 !
 kmod=sqrt(dot_product(k,k))
 if (kmod<local_err) return
 !
 ! This is a way to handle the z axis
 ! Since phi is not defined there we use an axis which is 
 ! almost z, just shifte by local_err
 kxymod=sqrt(k(1)**2+k(2)**2)
 if(kxymod<local_err) then
   k(1)=local_err
   k(2)=0._SP
   kxymod=local_err
 endif
 !
 costh= k(3) /kmod
 sinth=kxymod/kmod
 !
 cosphi=k(1)/kxymod
 sinphi=k(2)/kxymod
 eiphi=cmplx(cosphi,sinphi)
 !
 Ygrad(1,1)= k(3)*cosphi/kmod**2 
 Ygrad(1,2)= k(3)*sinphi/kmod**2 
 Ygrad(1,3)=  -kxymod   /kmod**2
 !
 Ygrad(2,1)=-sinphi /kxymod
 Ygrad(2,2)= cosphi /kxymod
 Ygrad(2,3)= 0._SP
 !
 Ylm(lindx(0),mindx(0,0))=1._SP/sqrt(4._SP*pi)
 dYlm(:,lindx(0),mindx(0,0))=cZERO
 if (lang.eq.1) return
 !
 ch=sqrt(3._SP/(4._SP*pi))
 Ylm(   lindx(1),mindx(1,0))= ch*costh
 dYlm(1,lindx(1),mindx(1,0))=-ch*sinth
 dYlm(2,lindx(1),mindx(1,0))= cZERO
 ch=sqrt(3._SP/(8._SP*pi))
 Ylm(   lindx(1),mindx(1,1))=-ch*sinth*eiphi
 dYlm(1,lindx(1),mindx(1,1))=-ch*costh*eiphi
 dYlm(2,lindx(1),mindx(1,1))=-ch*sinth*eiphi*cI
 ! M < 0
 Ylm(   lindx(1),mindx(1,-1))=-conjg(Ylm(   lindx(1),mindx(1,1)))
 dYlm(:,lindx(1),mindx(1,-1))=-conjg(dYlm(:,lindx(1),mindx(1,1)))
 if (lang.eq.2) return
 !
 ch=sqrt(5._SP/(16._SP*pi))
 Ylm(   lindx(2),mindx(2,0))= ch*(3._SP*costh**2-1._SP)
 dYlm(1,lindx(2),mindx(2,0))=-ch*costh*sinth*6._SP
 dYlm(2,lindx(2),mindx(2,0))= cZERO
 ch=sqrt(15._SP/(8._SP*pi))
 Ylm(   lindx(2),mindx(2,1))=-ch*sinth*costh        *eiphi
 dYlm(1,lindx(2),mindx(2,1))=-ch*(costh**2-sinth**2)*eiphi
 dYlm(2,lindx(2),mindx(2,1))=-ch*sinth*costh        *eiphi*cI
 ch=sqrt(15._SP/(32._SP*pi))
 Ylm(   lindx(2),mindx(2,2))= ch*      sinth**2   *eiphi**2
 dYlm(1,lindx(2),mindx(2,2))= ch*2._SP*costh*sinth*eiphi**2
 dYlm(2,lindx(2),mindx(2,2))= ch*2._SP*sinth**2   *eiphi**2*cI
 ! M < 0
 Ylm(   lindx(2),mindx(2,-1))=-conjg(Ylm(   lindx(2),mindx(2,1)))
 dYlm(:,lindx(2),mindx(2,-1))=-conjg(dYlm(:,lindx(2),mindx(2,1)))
 Ylm(   lindx(2),mindx(2,-2))= conjg(Ylm(   lindx(2),mindx(2,2)))
 dYlm(:,lindx(2),mindx(2,-2))= conjg(dYlm(:,lindx(2),mindx(2,2)))
 if (lang.eq.3) return
 !
 ch=sqrt(  7._SP/(16._SP*pi))
 Ylm(   lindx(3),mindx(3,0))= ch*(5._SP*costh**3-3._SP*costh)
 dYlm(1,lindx(3),mindx(3,0))=-ch*sinth*(15._SP*costh**2-3._SP)
 dYlm(2,lindx(3),mindx(3,0))= cZERO
 ch=sqrt( 21._SP/(64._SP*pi))
 Ylm(   lindx(3),mindx(3,1))=-ch*sinth*( 5._SP*costh**2 -1._SP      )*eiphi
 dYlm(1,lindx(3),mindx(3,1))=-ch      *(15._SP*costh**3-11._SP*costh)*eiphi
 dYlm(2,lindx(3),mindx(3,1))=-ch*sinth*( 5._SP*costh**2 -1._SP      )*eiphi*cI
 ch=sqrt(105._SP/(32._SP*pi))
 Ylm(   lindx(3),mindx(3,2))= ch*sinth**2 * costh               *eiphi**2
 dYlm(1,lindx(3),mindx(3,2))= ch*(2._SP*sinth*costh**2-sinth**3)*eiphi**2
 dYlm(2,lindx(3),mindx(3,2))= ch* 2._SP*sinth**2*costh          *eiphi**2*cI
 ch=sqrt( 35._SP/(64._SP*pi))
 Ylm(   lindx(3),mindx(3,3))=-ch*sinth**3            *eiphi**3
 dYlm(1,lindx(3),mindx(3,3))=-ch*3._SP*sinth**2*costh*eiphi**3
 dYlm(2,lindx(3),mindx(3,3))=-ch*3._SP*sinth**3      *eiphi**3*cI
 ! M < 0
 Ylm(   lindx(3),mindx(3,-1))=-conjg(Ylm(   lindx(3),mindx(3,1)))
 dYlm(:,lindx(3),mindx(3,-1))=-conjg(dYlm(:,lindx(3),mindx(3,1)))
 Ylm(   lindx(3),mindx(3,-2))= conjg(Ylm(   lindx(3),mindx(3,2)))
 dYlm(:,lindx(3),mindx(3,-2))= conjg(dYlm(:,lindx(3),mindx(3,2)))
 Ylm(   lindx(3),mindx(3,-3))=-conjg(Ylm(   lindx(3),mindx(3,3)))
 dYlm(:,lindx(3),mindx(3,-3))=-conjg(dYlm(:,lindx(3),mindx(3,3)))
 if (lang.eq.4) return
 !
 contains
   !
   integer function lindx(i_lang)
     integer i_lang
     lindx = i_lang+1
   end function
   !
   integer function mindx(i_lang,i_mang)
     integer i_lang,i_mang
     mindx=i_mang+1
     if (i_lang.ne.0) mindx=mindx+i_lang
   end function
   !
end subroutine
