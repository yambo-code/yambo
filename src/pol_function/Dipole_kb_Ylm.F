!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 use pars, ONLY:SP,pi
 implicit none
 integer     :: lang
 complex(SP) :: Ylm(lang,2*lang+1),dYlm(2,lang,2*lang+1)
 real(SP)    :: k(3),Ygrad(2,3)
 ! 
 ! Work Space
 !
 integer il,im
 real(SP)    :: costh,sinth,cosphi,sinphi,ch,kmod,kxymod
 complex(SP) :: eiphi
 !
 Ylm(:,:)=(0.,0.)
 dYlm(:,:,:)=(0.,0.)
 Ygrad(:,:)=0.
 !
 kmod=sqrt(dot_product(k,k))
 kxymod=sqrt(k(1)**2.+k(2)**2.)
 costh=k(3)/kmod
 sinth=sqrt(k(1)**2.+k(2)**2.)/kmod
 cosphi=1.
 sinphi=0.
 if (kxymod.gt.1.E-5) then
  cosphi=k(1)/sqrt(k(1)**2.+k(2)**2.)
  sinphi=k(2)/sqrt(k(1)**2.+k(2)**2.)
 endif
 eiphi=cmplx(cosphi,sinphi)
 !
 if (abs(sinth).gt.1.E-5) then
  Ygrad(1,1:2)=k(1:2)*k(3)/(kmod**3.*sinth)
  Ygrad(1,3)=((k(3)/kmod)**2.-1.)/(kmod*sinth)
 endif
 if (kxymod.gt.1.E-5) then
  if (abs(cosphi).gt.1.E-5) Ygrad(2,1)=-k(1)*k(2)/(kxymod**3.*cosphi)
  if (abs(sinphi).gt.1.E-5) Ygrad(2,2)=k(1)*k(2)/(kxymod**3.*sinphi)
  Ygrad(2,3)=0.
 endif
 !
 Ylm(lindx(0),mindx(0,0))=1./sqrt(4.*pi)
 dYlm(:,lindx(0),mindx(0,0))=(0.,0.)
 if (lang.eq.1) return
 !
 ch=sqrt(3./(4.*pi))
 Ylm(lindx(1),mindx(1,0))=ch*costh
 dYlm(1,lindx(1),mindx(1,0))=-ch*sinth
 dYlm(2,lindx(1),mindx(1,0))=(0.,0.)
 ch=sqrt(3./(8.*pi))
 Ylm(lindx(1),mindx(1,1))=-ch*sinth*eiphi
 dYlm(1,lindx(1),mindx(1,1))=-ch*costh*eiphi
 dYlm(2,lindx(1),mindx(1,1))=-(0.,1.)*ch*sinth*eiphi
 ! M < 0
 Ylm(lindx(1),mindx(1,-1))=-conjg(Ylm(lindx(1),mindx(1,1)))
 dYlm(:,lindx(1),mindx(1,-1))=-conjg(dYlm(:,lindx(1),mindx(1,1)))
 if (lang.eq.2) return
 !
 ch=sqrt(5./(16.*pi))
 Ylm(lindx(2),mindx(2,0))=ch*(3.*costh**2.-1)
 dYlm(1,lindx(2),mindx(2,0))=-6.*ch*costh*sinth
 dYlm(2,lindx(2),mindx(2,0))=(0.,0.)
 ch=sqrt(15./(8.*pi))
 Ylm(lindx(2),mindx(2,1))=-ch*sinth*costh*eiphi
 dYlm(1,lindx(2),mindx(2,1))=-ch*(costh**2.-sinth**2.)*eiphi
 dYlm(2,lindx(2),mindx(2,1))=-(0.,1.)*ch*sinth*costh*eiphi
 ch=sqrt(15./(32.*pi))
 Ylm(lindx(2),mindx(2,2))=ch*sinth**2.*eiphi**2.
 dYlm(1,lindx(2),mindx(2,2))=ch*2.*costh*sinth*eiphi**2.
 dYlm(2,lindx(2),mindx(2,2))=(0.,2.)*ch*sinth**2.*eiphi**2.
 ! M < 0
 Ylm(lindx(2),mindx(2,-2))=conjg(Ylm(lindx(2),mindx(2,2)))
 dYlm(1,lindx(2),mindx(2,-2))=conjg(dYlm(1,lindx(2),mindx(2,2)))
 dYlm(2,lindx(2),mindx(2,-2))=-conjg(dYlm(2,lindx(2),mindx(2,2)))
 Ylm(lindx(2),mindx(2,-1))=-conjg(Ylm(lindx(2),mindx(2,1)))
 dYlm(:,lindx(2),mindx(2,-1))=-conjg(dYlm(:,lindx(2),mindx(2,1)))
 if (lang.eq.3) return
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
