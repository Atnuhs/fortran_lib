
# 共通設定 ----------------------------------------------------
# FC = gfortran
FC = ifort

FASTOPTION = -O2 -xHost
DEBUGOPTION =

# for gfortran
ifeq ($(FC),gfortran)
	DEBUGOPTION += \
	-Wall -pedantic -fbounds-check -O \
	-Wuninitialized -ffpe-trap=invalid,zero,overflow
endif

# for ifort
ifeq ($(FC),ifort)
	DEBUGOPTION += \
	-check all -warn all -std -gen_interfaces \
	-fpe0 -ftrapuv -traceback
endif

# md.out
MD_TARGET = md.out
MDDEBUG_TARGET = md_debug.out
MD_OBJECTS = const.o md_input.o md.o

# tcd.out
TCD_TARGET = my_tcd.out
TCDDEBUG_TARGET = my_tcd_debug.out
TCD_OBJECTS = const.o fft.o md_input.o ana_func.o my_tcd.o

# msd.out
MSD_TARGET = my_msd.out
MSDDEBUG_TARGET = my_msd_debug.out
MSD_OBJECTS = const.o fft.o md_input.o ana_func.o my_msd.o



# 実行方法 ----------------------------------------------------

.SUFFIXES: .f90
%.o: %.f90
	$(FC) -c $<

# tcd.out
${TCD_TARGET}: ${TCD_OBJECTS}
	${FC} -o $@ ${TCD_OBJECTS} ${FASTOPTION}
${TCDDEBUG_TARGET}: ${TCD_OBJECTS}
	${FC} -o $@ ${TCD_OBJECTS} ${DEBUGOPTION}

# msd.out
${MSD_TARGET}: ${MSD_OBJECTS}
	${FC} -o $@ ${MSD_OBJECTS} ${FASTOPTION}
${MSDDEBUG_TARGET}: ${MSD_OBJECTS}
	${FC} -o $@ ${MSD_OBJECTS} ${DEBUGOPTION}

# md.out
${MD_TARGET}: ${MD_OBJECTS}
	${FC} -o $@ ${MD_OBJECTS} ${FASTOPTION}
${MDDEBUG_TARGET}: ${MD_OBJECTS}
	${FC} -o $@ ${MD_OBJECTS} ${DEBUGOPTION}


# clean
.PHONY: clean

clean:
	${RM} *.mod *.o *.out
