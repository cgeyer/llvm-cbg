##===- lib/Target/cbg/Makefile ---------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

LEVEL = ../../..
LIBRARYNAME = LLVMcbgCodeGen
TARGET = cbg

# Make sure that tblgen is run, first thing.
BUILT_SOURCES = cbgGenRegisterInfo.h.inc cbgGenRegisterNames.inc \
                cbgGenRegisterInfo.inc cbgGenInstrNames.inc \
                cbgGenInstrInfo.inc cbgGenAsmWriter.inc \
                cbgGenDAGISel.inc cbgGenSubtarget.inc cbgGenCallingConv.inc

DIRS = TargetInfo

include $(LEVEL)/Makefile.common

