# ===========================================
# md.f90 my_tcd.f90 my_msd.f90 のビルド手順
# mantainer:: Shunta Abe
# ===========================================

# ----------------------
# 変数/共通設定
# ----------------------
vpath %.f90 ../src

# モジュールのファイルリスト。（依存がある場合は依存先を左へ）
SRCS := const.f90 fft.f90 md_input.f90 ana_func.f90 random.f90
OBJS := ${SRCS:f90=o}
DEPS := ${OBJS:o=d}

# program ~ end programを持つファイルリスト
# 追加する場合はビルド手順も既存のものに倣って書く。
# make all にも書き足す。
MD := md.f90
MSD := my_msd.f90
TCD := my_tcd.f90
ALL := $(MD) $(MSD) $(TCD)

# ----------------------
# コンパイラ切り替え
# ----------------------
FC = gfortran
# FC = ifort
FASTOPTION = -O3
DEBUGOPTION = -g
PROFOPTION = -O0 -pg
ifeq ($(FC),gfortran)
	DEBUGOPTION += \
	-Wall -pedantic -fbounds-check -O \
	-Wuninitialized -ffpe-trap=invalid,zero,overflow
else ifeq ($(FC),ifort)
	DEBUGOPTION += \
	-check all -warn all -std -gen_interfaces \
	-fpe0 -ftrapuv -traceback
else
$(error "FC" must be "gfortran" or "ifort")
endif

# ------------------------
# ビルド debug/fast切り替え
# ------------------------
# buildtype = debug
# buildtype = prof
buildtype = fast
FFLAGS = 
ifeq ($(buildtype),debug)
	FFLAGS += $(DEBUGOPTION)
else ifeq ($(buildtype),fast)
	FFLAGS += $(FASTOPTION)
else ifeq ($(buildtype),prof)
	FFLAGS += $(PROFOPTION)
else
$(error "buildtype" must be "debug" or "fast")
endif

# ------------------
# ビルド実行方法
# ------------------
.PHONY: clean all
define COMPILE_MAIN
${1:f90=out}: $(OBJS) ${1:f90=o}
	$(FC) $(FFLAGS) -o ${1:f90=out} $(OBJS) ${1:f90=o}
endef

all: $(foreach t,$(ALL),$(t:f90=out))

$(eval $(call COMPILE_MAIN,$(MD)))
$(eval $(call COMPILE_MAIN,$(MSD)))
$(eval $(call COMPILE_MAIN,$(TCD)))


clean:
	${RM} *.mod *.o *.d *.out



-include $(DEPS)

%.o: %.f90
	$(FC) $(FFLAGS) -cpp -MD -c -o $@ $<
