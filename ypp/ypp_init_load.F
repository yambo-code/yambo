!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
 use it_m,          ONLY:it,initdefs,E_unit,G_unit,V_general,&
&                        Time_unit,Bfield_unit,Efield_unit,V_real_time
 use wave_func,     ONLY:wf_ng
 use YPP
 implicit none
 type(initdefs)::defs
 !
 ! RunLevels
 !
 call it('r',defs,'bzgrids',   '[R] BZ Grid') 
 call it('r',defs,'K_grid',    '[R] K-grid analysis') 
 call it('r',defs,'Q_grid',    '[R] Q-grid analysis') 
 call it('r',defs,'dos',       '[R] DOS')
 !
 !
 call it('r',defs,'High_Symm', '[R] High_Symmetry points and paths') 
 call it('r',defs,'excitons',  '[R] Excitons')
 call it('r',defs,'electrons', '[R] Electrons (and holes)')
 call it('r',defs,'freehole',  '[R] Free hole position')
 call it('r',defs,'amplitude', '[R] Amplitude')
 call it('r',defs,'plot',         '[R] Plot')
 call it('r',defs,'density',      '[R] Density')
 call it('r',defs,'wavefunction', '[R] Wavefunction')
 call QP_ctl_load(defs,3)
 !
 !
 call it('r',defs,'bzrim',   '[R] BZ energy RIM analyzer')
 call it('r',defs,'qpdb',    '[R] Quasi-particle database manager')
 !
 !
 ! DOS
 !
 call it(defs,'DOS_broad', 'Broadening of the DOS',dos_broadening,E_unit) 
 call it(defs,'DOS_bands', 'DOS bands',dos_bands) 
 !
 ! BZ grids
 !
 call it(defs,'cooIn','Points coordinates (in) cc/rlu/iku/alat',coo_in)
 call it(defs,'cooOut','Points coordinates (out) cc/rlu/iku/alat',coo_out)
 call it(defs,'GWKpts','GW K-point grid',Kgw_1)
 call it(defs,'Qpts',  'Q-point grid',   Kgw_1)
 call it(defs,'KShift1','K-grid shift vector[1] ',qlong(1,:))
 call it(defs,'KShift2','K-grid shift vector[2] ',qlong(2,:))
 call it(defs,'KShift3','K-grid shift vector[3] ',qlong(3,:))
 !
 !
 call it('f',defs,'NoWeights',    ' Do not print K-points weight') 
 call it('f',defs,'ForceUserPts', ' Do not check the correcteness of the user points') 
 !
 ! EXC WF
 !
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit)
 call it(defs,'Degen_Step', 'Maximum energy separation of two degenerate states',&
&                           deg_energy,E_unit)
 call it(defs,'States','Index of the BS state(s)',state_ctl)
 call it(defs,'Cells', 'Number of cell repetitions (even or 1)',ncell)
 call it(defs,'Hole', '[cc] Hole position in unit cell',r_hole)
 call it(defs,'WFMult', 'Multiplication factor to the excitonic wavefunction',WF_multiplier,verb_level=V_general)
 !
 !
 ! PLOT
 !
 ! p_dir: plot cut in the a1,a2,a3 basis
 ! 
 ! p_format: (c)ube/(g)nuplot/(x)crysden
 !
 call it(defs,'Format', 'Output format [(c)ube/(g)nuplot/(x)crysden]',p_format)
 call it(defs,'Direction', '[rlu] [1/2/3] for 1d or [12/13/23] for 2d [123] for 3D',p_dir)
 !
 !
 !
 !BZ E RIM
 !
 call it(defs,'BZ_RIM_Nk','BZ Random Integration points',BZ_RIM_nkpts)
 call it(defs,'BZ_RIM_path','Path to the SAVE folder with the RIM DB1',BZ_RIM_path)
 !
end subroutine
