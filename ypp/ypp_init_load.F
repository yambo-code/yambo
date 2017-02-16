!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine ypp_init_load(defs)
 !
 use it_m,          ONLY:it,initdefs,E_unit,G_unit
 use wave_func,     ONLY:wf_ng
 use YPP
 implicit none
 type(initdefs)::defs
 !
 ! RunLevels
 !
 call it('r',defs,'kpt',     '[R] K-grid analysis') 
 call it('r',defs,'excp',    '[R] Excitonic Properties')
 call it('r',defs,'exc_wf',  '[R] Excitonic Wavefunction')
 call it('r',defs,'freehole','[R] Free hole position')
 call it('r',defs,'exc_amp', '[R] Excitonic Amplitudes')
 call it('r',defs,'plot',         '[R] Plot')
 call it('r',defs,'density',      '[R] Density')
 call it('r',defs,'wavefunction', '[R] Wavefunction')
 !
 !
 ! Kpt analysis
 !
 call it(defs,'cooIn','Points coordinates (in) cc/rlu/iku',coo_in)
 call it(defs,'cooOut','Points coordinates (out) cc/rlu/iku',coo_out)
 call it(defs,'GWKpts','GW K-point grid',Kgw_1)
 call it(defs,'Qpts',  'Q-point grid',   Kgw_1)
 call it(defs,'KShift1','K-grid shift vector[1] ',qlong(1,:))
 call it(defs,'KShift2','K-grid shift vector[2] ',qlong(2,:))
 call it(defs,'KShift3','K-grid shift vector[3] ',qlong(3,:))
 !
 ! EXC WF
 !
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit)
 call it(defs,'Degen_Step', 'Maximum energy separation of two degenerate states',&
&                           deg_energy,E_unit)
 call it(defs,'States','Index of the BS state(s)',state_ctl)
 call it(defs,'Cells', 'Number of cell repetitions (even or 1)',ncell)
 call it(defs,'Hole', '[cc] Hole position in unit cell',r_hole)
 call it(defs,'WFMult', 'Multiplication factor to the excitonic wavefunction',WF_multiplier)
 !
 ! PLOT
 !
 ! p_dir: plot cut in the a1,a2,a3 basis
 ! 
 ! p_format: (g)nuplot/(x)crysden
 !
 call it(defs,'Format', 'Output format [(g)nuplot/(x)crysden]',p_format)
 call it(defs,'Direction', '[rlu] [1/2/3] for 1d or [12/13/23] for 2d [123] for 3D',p_dir)
 !
 !
 !
 !
end subroutine
