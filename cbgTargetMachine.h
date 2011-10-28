//===-- cbgTargetMachine.h - Define TargetMachine for cbg ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the cbg specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef CBGTARGETMACHINE_H
#define CBGTARGETMACHINE_H

#include "cbgInstrInfo.h"
#include "cbgISelLowering.h"
#include "cbgFrameLowering.h"
#include "cbgSelectionDAGInfo.h"
#include "cbgSubtarget.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetFrameLowering.h"

namespace llvm {

class cbgTargetMachine : public LLVMTargetMachine {
  cbgSubtarget Subtarget;
  const TargetData DataLayout;       // Calculates type size & alignment
  cbgTargetLowering TLInfo;
  cbgSelectionDAGInfo TSInfo;
  cbgInstrInfo InstrInfo;
  cbgFrameLowering FrameLowering;
public:
  cbgTargetMachine(const Target &T, const std::string &TT,
                     const std::string &FS, bool is64bit);

  virtual const cbgInstrInfo *getInstrInfo() const { return &InstrInfo; }
  virtual const TargetFrameLowering  *getFrameLowering() const {
    return &FrameLowering;
  }
  virtual const cbgSubtarget   *getSubtargetImpl() const{ return &Subtarget; }
  virtual const cbgRegisterInfo *getRegisterInfo() const {
    return &InstrInfo.getRegisterInfo();
  }
  virtual const cbgTargetLowering* getTargetLowering() const {
    return &TLInfo;
  }
  virtual const cbgSelectionDAGInfo* getSelectionDAGInfo() const {
    return &TSInfo;
  }
  virtual const TargetData       *getTargetData() const { return &DataLayout; }

  // Pass Pipeline Configuration
  virtual bool addInstSelector(PassManagerBase &PM, CodeGenOpt::Level OptLevel);
  virtual bool addPreEmitPass(PassManagerBase &PM, CodeGenOpt::Level OptLevel);
};

/// SparcV8TargetMachine - Sparc 32-bit target machine
///
class cbgV8TargetMachine : public cbgTargetMachine {
public:
  cbgV8TargetMachine(const Target &T, const std::string &TT,
                       const std::string &FS);
};

} // end namespace llvm

#endif
