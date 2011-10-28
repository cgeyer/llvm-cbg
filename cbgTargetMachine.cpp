//===-- cbgTargetMachine.cpp - Define TargetMachine for cbg -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "cbg.h"
#include "cbgMCAsmInfo.h"
#include "cbgTargetMachine.h"
#include "llvm/PassManager.h"
#include "llvm/Target/TargetRegistry.h"
using namespace llvm;

extern "C" void LLVMInitializecbgTarget() {
  // Register the target.
  RegisterTargetMachine<cbgV8TargetMachine> X(ThecbgTarget);

  RegisterAsmInfo<cbgELFMCAsmInfo> A(ThecbgTarget);
}

/// cbgTargetMachine ctor - Create an ILP32 architecture model
///
cbgTargetMachine::cbgTargetMachine(const Target &T, const std::string &TT, 
                                       const std::string &FS)
  : LLVMTargetMachine(T, TT),
    Subtarget(TT, FS),
    DataLayout(Subtarget.getDataLayout()),
    TLInfo(*this), TSInfo(*this), InstrInfo(Subtarget),
    FrameLowering(Subtarget) {
}

bool cbgTargetMachine::addInstSelector(PassManagerBase &PM,
                                         CodeGenOpt::Level OptLevel) {
  PM.add(createcbgISelDag(*this));
  return false;
}

/// addPreEmitPass - This pass may be implemented by targets that want to run
/// passes immediately before machine code is emitted.  This should return
/// true if -print-machineinstrs should print out the code after the passes.
bool cbgTargetMachine::addPreEmitPass(PassManagerBase &PM,
                                        CodeGenOpt::Level OptLevel){
  PM.add(createcbgFPMoverPass(*this));
  PM.add(createcbgDelaySlotFillerPass(*this));
  return true;
}

cbgV8TargetMachine::cbgV8TargetMachine(const Target &T,
                                           const std::string &TT, 
                                           const std::string &FS)
  : cbgTargetMachine(T, TT, FS) {
}
