module load openjdk
module load gcc/8.3.1

export CXX="g++" 
export GCAM_HOME=${HOME}/work/GCAM 
export GCAMLIB_HOME=${GCAM_HOME}/gcam-libs 
export BOOST_INCLUDE=${GCAMLIB_HOME}/boost-lib/stage/include
export BOOST_LIB=${GCAMLIB_HOME}/boost-lib/stage/lib 
export BOOSTROOT=${GCAMLIB_HOME}/boost-lib/
export BOOST_NUMERIC_BINDINGS=${GCAMLIB_HOME}/boost-numeric-bindings
export XERCES_INCLUDE=${GCAMLIB_HOME}/xercesc/include
export XERCES_LIB=${GCAMLIB_HOME}/xercesc/lib
export JAVA_INCLUDE=${JAVA_HOME}/java/include 
export JAVA_LIB=${JAVA_HOME}/java/jre/lib/amd64/server
#export BASEX_LIB=${GCAMLIB_HOME}/basex/BaseX-8.6.7.jar
export JARS_LIB=${GCAMLIB_HOME}/jars/*

#export USE_GCAM_PARALLEL=1
#export TBBROOT=/opt/aci/sw/tbb/2017_gcc-5.3.1
#export TBB_INCDIR=${TBBROOT}/include
#export TBB_LIBDIR=${TBBROOT}/build/linux_intel64_gcc_cc5.3.1_libc2.12_kernel2.6.32_release
export USE_LAPACK=0
export USE_MKL=0
#export MKL_ROOT=${HOME}/intel/mkl
#export MKL_INCLUDE=${MKLROOT}/include
#export MKL_LIB=${MKLROOT}/lib/intel64
#export MKL_CFLAGS='-fopenmp -I${MKL_INCLUDE}'
#export MKL_LDFLAGS='-fopenmp -L${MKL_LIB} -lmkl_intel_lp64 -lmkl_core -lmkl_gnu_thread -ldl -lpthread -lm'
#export MKL_RPATH='-Wl,rpath,${MKL_LIB}'
#export LAPACKINC=/opt/aci/sw/lapack/3.6.0_gcc-5.3.1/usr/include
#export LAPACKLIB=/opt/aci/sw/lapack/3.6.0_gcc-5.3.1/usr/lib64
#export BLASLIB=/opt/aci/sw/blas/3.6.0_gcc-5.3.1/usr/lib64

export CLASSPATH=.:$JARS_LIB:XMLDBDriver.jar
export LD_LIBRARY_PATH=${XERCES_LIB}:${BASEX_LIB}:$LD_LIBRARY_PATH
export PATH=${PATH}:${JAVA_HOME}

#export MKL_NUM_THREADS=1
#export MKL_DOMAIN_NUM_THREADS="BLAS=$nproc"
