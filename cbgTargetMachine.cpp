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
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Target/TargetRegistry.h"

#if 0
#include "llvm/Support/Debug.h"
#define __DEBUG
#endif

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

// for debugging purpose
/*bool cbgTargetMachine::addPreISel(PassManagerBase &PM,
                                    CodeGenOpt::Level OptLevel) {
#ifdef __DEBUG
  dbgs() << __func__ << "\n";
#endif

  return false;
}*/

/*bool cbgTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           formatted_raw_ostream &Out,
                                           CodeGenFileType FileType,
                                           CodeGenOpt::Level OptLevel,
                                           bool DisableVerify) {
#ifdef __DEBUG
  dbgs() << __func__ << "\n";
#endif
  // disable verification pass if we handle costume instructions
  if (Subtarget.hasHWLoop() || Subtarget.hasHWLoops()) {
    DisableVerify = true;
  }
  return LLVMTargetMachine::addPassesToEmitFile(PM, Out,
      FileType, OptLevel, DisableVerify);
}*/


bool cbgTargetMachine::addInstSelector(PassManagerBase &PM,
                                         CodeGenOpt::Level OptLevel) {
#ifdef __DEBUG
  dbgs() << __func__ << "\n";
#endif
  PM.add(createcbgISelDag(*this));
  return false;
}

/// addPreEmitPass - This pass may be implemented by targets that want to run
/// passes immediately before machine code is emitted.  This should return
/// true if -print-machineinstrs should print out the code after the passes.
bool cbgTargetMachine::addPreEmitPass(PassManagerBase &PM,
                                        CodeGenOpt::Level OptLevel){
#ifdef __DEBUG
  dbgs() << __func__ << "\n";
#endif

  if (Subtarget.hasHWLoop()) {
    PM.add(createcbgHWLoopPass(*this, 1));
  } else if (Subtarget.hasHWLoops()) {
    PM.add(createcbgHWLoopPass(*this, 2));
  }
  PM.add(createcbgFPMoverPass(*this));
  PM.add(createcbgDelaySlotFillerPass(*this));
  return true;
}

cbgV8TargetMachine::cbgV8TargetMachine(const Target &T,
                                           const std::string &TT, 
                                           const std::string &FS)
  : cbgTargetMachine(T, TT, FS) {
}
