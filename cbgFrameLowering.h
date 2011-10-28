//===- cbgFrameLowering.h - Define frame lowering for cbg --*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#ifndef CBG_FRAMEINFO_H
#define CBG_FRAMEINFO_H

#include "cbg.h"
#include "cbgSubtarget.h"
#include "llvm/Target/TargetFrameLowering.h"

namespace llvm {
  class cbgSubtarget;

class cbgFrameLowering : public TargetFrameLowering {
  const cbgSubtarget &STI;
public:
  explicit cbgFrameLowering(const cbgSubtarget &sti)
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, 8, 0), STI(sti) {
  }

  /// emitProlog/emitEpilog - These methods insert prolog and epilog code into
  /// the function.
  void emitPrologue(MachineFunction &MF) const;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const;

  bool hasFP(const MachineFunction &MF) const { return false; }
};

} // End llvm namespace

#endif
