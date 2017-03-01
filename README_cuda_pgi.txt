
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
with -D_CUDA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(eval mpif90 -O2 -fast -Munroll -Mnoframe -Mdalign -Mbackslash  -Mnomain   -Mcuda=cuda8.0 -Mcudalib=cufft,cublas -I /home/z320/YAMBO/yambo-devel/include  -o yambo driver.o getopt.o yambo_driver.o -L/home/z320/YAMBO/yambo-devel/lib  -lbse -lacfdt -lqp -lpol_function -ltddft -lsetup -lqp_control -lbz_ops -lcoulomb -lwf_and_fft -lstop_and_restart -linterface -lio -lcommon -lcommunicate -lxc_functionals -lparser -lparallel -llinear_algebra -lmatrices -lmodules -lexternal_c    -L/usr/local/Cluster-Apps/intel/mkl/11.3.3.210/compilers_and_libraries_2016.3.210/linux/mkl/lib/intel64 -lmkl_intel_lp64  -lmkl_core -lmkl_pgi_thread  -L/usr/local/Cluster-Apps/intel/mkl/11.3.3.210/compilers_and_libraries_2016.3.210/linux/mkl/lib/intel64 -lmkl_intel_lp64  -lmkl_core -lmkl_pgi_thread -lnetcdff -lnetcdf -lxcf90 -lxc -lslatec -llocal -L/usr/local/Cluster-Apps/intel/mkl/11.3.3.210/compilers_and_libraries_2016.3.210/linux/mkl/lib/intel64  -lfftw3xf_intel -lm  ) > /dev/null
nvlink warning : Skipping incompatible '/home/z320/YAMBO/yambo-devel/lib/libmatrices.a' when searching for -lmatrices
nvlink error   : Undefined reference to '_fft_m_16' in '/home/z320/YAMBO/yambo-devel/lib/libwf_and_fft.a:WF_apply_symm.o'
nvlink error   : Undefined reference to '_d_lattice_16' in '/home/z320/YAMBO/yambo-devel/lib/libwf_and_fft.a:WF_apply_symm.o'
pgacclnk: child process exit status 2: /usr/local/Cluster-Apps/pgi/linux86-64/16.10/bin/pgnvd
make[1]: *** [yambo] Error 2
make[1]: Leaving directory `/home/z320/YAMBO/yambo-devel/driver'
make: *** [yambo] Error 2


nm src/wf_and_fft/WF_apply_symm.o | grep -i fft
                 U _fft_m_0_
                 U _fft_m_16_
                 U _fft_m_2_
                 U fft_m_
[z320@login-gfx2 yambo-devel]$ nm src/modules/mod_FFT.o | grep -i fft
0000000000000000 A ..Dm_fft_m
0000000000000031 t __fft_m_modx_END
000000000000018c C _fft_m_0_
00000000000000c8 C _fft_m_16_
0000000000000008 C _fft_m_2_
0000000000000000 T fft_m_
0000000000000010 T fft_m_modx_


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
without -D_CUDA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

linking works

[z320@login-gfx2 yambo-devel]$ nm src/modules/mod_FFT.o  | grep -i fft
0000000000000000 A ..Dm_fft_m
0000000000000031 t __fft_m_modx_END
0000000000000180 C _fft_m_0_
0000000000000008 C _fft_m_2_
0000000000000000 T fft_m_
0000000000000010 T fft_m_modx_
[z320@login-gfx2 yambo-devel]$ nm src/wf_and_fft/WF_apply_symm.o | grep -i fft
                 U _fft_m_0_
                 U _fft_m_2_
                 U fft_m_
[z320@login-gfx2 yambo-devel]$ 

