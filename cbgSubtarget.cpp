//===- cbgSubtarget.cpp - CBG Subtarget Information -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the CBG specific subclass of TargetSubtarget.
//
//===----------------------------------------------------------------------===//

#include "cbgSubtarget.h"
#include "cbgGenSubtarget.inc"
using namespace llvm;

cbgSubtarget::cbgSubtarget(const std::string &TT, const std::string &FS) :
    HasSelCC(false),
    HasMovCC(false),
    HasPredBlocksCC(false),
    HasPredBlocksReg(false),
    HasPredInstrCC(false),
    HasPredInstrReg(false),
    HasHWLoop(false),
    HasHWLoops(false),
    HasVLIWIfElse(false),
    HasHWLoopOpt(false) {
  
  // Determine default and user specified characteristics
  const char *CPU = "v8";

  // Parse features string.
  ParseSubtargetFeatures(FS, CPU);
}
