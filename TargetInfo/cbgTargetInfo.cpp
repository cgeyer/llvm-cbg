//===-- cbgTargetInfo.cpp - cbg Target Implementation -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "cbg.h"
#include "llvm/Module.h"
#include "llvm/Target/TargetRegistry.h"
using namespace llvm;

Target llvm::ThecbgTarget;

extern "C" void LLVMInitializecbgTargetInfo() {
  RegisterTarget<Triple::cbg> X(ThecbgTarget, "cbg", "CBG");
}
