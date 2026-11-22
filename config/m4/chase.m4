
AC_DEFUN([AC_CHASE_SETUP],[

AC_ARG_ENABLE(chase,   AS_HELP_STRING([--enable-chase],[Use ChASE eigensolver. Default is no]))
AC_ARG_WITH(chase_libdir,AS_HELP_STRING([--with-chase-libdir=<path>],[Path to the ChASE lib directory],[32]))
AC_ARG_WITH(chase_includedir,AS_HELP_STRING([--with-chase-includedir=<path>],[Path to the ChASE include directory],[32]))
AC_ARG_WITH(chase_libs,AS_HELP_STRING([--with-chase-libs=<libs>],[Complete ChASE link line (overrides chase.pc)],[32]))

compile_chase="no"
internal_chase="no"
external_chase="yes"
AC_SUBST(compile_chase)
AC_SUBST(internal_chase)
AC_SUBST(external_chase)

if test "$enable_chase" = "yes"
then
	if test x"$with_chase_libdir" != "x"
	then
		echo "LIBDIR = ${with_chase_libdir}"
	else
		with_chase_libdir="${ChASEROOT}/build/lib64"
	fi
	
	if test x"$with_chase_includedir" != "x"
	then
		echo "INCLUDEDIR = ${with_chase_includedir}"
	else
		with_chase_includedir="${ChASEROOT}/build/include"
	fi

	AC_SUBST([CHASE_LIBS_R],["$with_chase_libdir"])
	AC_SUBST([CHASE_INCS_R],["$with_chase_includedir"])

        #if test "$have_chase" = no
        #then
        #        echo "Downloading ChASE..."
        #        git clone https://github.com/ChASE-library/ChASE.git
        #        cd ChASE
        #        ChASEROOT=$(pwd)
        #        mkdir build
        #        cd build
        #        cmake .. -DCMAKE_INSTALL_PREFIX=${ChASEROOT} #We assume CMake exists
        #        make install
        #        cd ../..
        #fi

        AC_CHECK_FILES("${CHASE_LIBS_R}/libchase_c.a"    ,[],[AC_ERROR([lchase_c not found   ])])
        AC_CHECK_FILES("${CHASE_LIBS_R}/libchase_f.a"    ,[],[AC_ERROR([lchase_f not found   ])])
        #AC_CHECK_FILES("${CHASE_LIBS_R}/libchase_cuda_kernels.a" ,[],[AC_ERROR([lchase_cuda not found])])
        AC_CHECK_FILES("${CHASE_LIBS_R}/libchase_cuda_kernels.a" ,[have_chase_cuda=yes],[have_chase_cuda=no])
        #
        # The ChASE backend is fixed when ChASE is built: a GPU ChASE
        # always solves on the GPU. Follow the Yambo GPU setting.
        have_chase_gpu="$have_chase_cuda"
        if test -f "${CHASE_LIBS_R}/libchase_hip_kernels.a" ; then have_chase_gpu="yes" ; fi
        CHASE_info="(CPU build)"
        if test "$have_chase_gpu" = "yes" ; then CHASE_info="(GPU build)" ; fi
        if test "$GPU_SUPPORT" = "no_gpu" && test "$have_chase_gpu" = "yes" ; then
          CHASE_info="(GPU build: WARNING, ChASE runs on the GPU although Yambo is CPU-only)"
          AC_MSG_WARN([Yambo is configured without GPU support, but the ChASE in ${CHASE_LIBS_R} is a GPU build: ChASE will run on the GPU.
For a CPU-only run, point --with-chase-libdir/--with-chase-includedir to a CPU build of ChASE.])
        fi
        if test "$GPU_SUPPORT" != "no_gpu" && test "$have_chase_gpu" = "no" ; then
          CHASE_info="(CPU build: WARNING, ChASE runs on the CPU although Yambo uses the GPU)"
          AC_MSG_WARN([Yambo is configured with GPU support ($GPU_SUPPORT), but the ChASE in ${CHASE_LIBS_R} is a CPU build: ChASE will run on the CPU.])
        fi

        #AC_SUBST([CHASE_INCS],["${CHASE_INCS} -I${CHASE_INCS_R}"])
        CHASE_INCS="$CHASE_INCS -I${CHASE_INCS_R}"
        #
        if test "$have_chase_cuda" = "yes" ; then
          #AC_SUBST([CHASE_LIBS],["${CHASE_LIBS} -L${CHASE_LIBS_R} -lchase_c -lchase_f -lchase_cuda_kernels "])
          CHASE_LIBS="${CHASE_LIBS} -L${CHASE_LIBS_R} -lchase_c -lchase_f -lchase_cuda_kernels"
        else
          #AC_SUBST([CHASE_LIBS],["${CHASE_LIBS} -L${CHASE_LIBS_R} -lchase_c -lchase_f "])
          CHASE_LIBS="${CHASE_LIBS} -L${CHASE_LIBS_R} -lchase_c -lchase_f"
        fi
        #AC_SUBST([CHASE_LIBS],["${CHASE_LIBS} -lstdc++ "])
        CHASE_LIBS="${CHASE_LIBS} -lstdc++"
        #
        # Prefer the complete static link line exported by ChASE (chase.pc),
        # or a user supplied one. Older chase.pc files do not list the ChASE
        # libraries at all; keep the line above for those.
        AC_PATH_PROGS(PKG_CONFIG,[pkg-config pkgconf])
        chase_pc_path="${CHASE_LIBS_R}/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
        if test x"$with_chase_libs" != "x" ; then
          CHASE_LIBS="$with_chase_libs"
        elif test x"$PKG_CONFIG" != "x" && \
             PKG_CONFIG_PATH="$chase_pc_path" $PKG_CONFIG --libs chase 2>/dev/null | grep -q -e '-lchase_c' ; then
          CHASE_LIBS=`PKG_CONFIG_PATH="$chase_pc_path" $PKG_CONFIG --static --libs chase`
        fi
	
        ##AC_SUBST([CHASE_LIBS],["${CHASE_LIBS}  "])
        #-lcudart -lcublas -lcusolver -lcudart -lcurand -lnccl
        #LIBS="${LIBS} ${CHASE_LIBS} "
        #INCS="${INCS} ${CHASE_INCS} "

        #Assuming a CUDA compiler...
        #LIBS+="-lcudart -lcublas -lcusolver -lcudart -lcurand -lnccl -lstdc++ "

        def_chase="-D_CHASE"
else
        def_chase=""
fi

AC_SUBST(CHASE_LIBS)
AC_SUBST(CHASE_INCS)
AC_SUBST(def_chase)
AC_SUBST(CHASE_info)

dnl Process Makefile.in to create Makefile

])
