//===- cbgRegisterInfo.h - cbg Register Information Impl ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the cbg implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef CBGREGISTERINFO_H
#define CBGREGISTERINFO_H

#include "llvm/Target/TargetRegisterInfo.h"
#include "cbgGenRegisterInfo.h.inc"

namespace llvm {

class cbgSubtarget;
class TargetInstrInfo;
class Type;

struct cbgRegisterInfo : public cbgGenRegisterInfo {
  cbgSubtarget &Subtarget;
  const TargetInstrInfo &TII;

  cbgRegisterInfo(cbgSubtarget &st, const TargetInstrInfo &tii);

  /// Code Generation virtual methods...
  const unsigned *getCalleeSavedRegs(const MachineFunction *MF = 0) const;

  BitVector getReservedRegs(const MachineFunction &MF) const;

  void eliminateCallFramePseudoInstr(MachineFunction &MF,
                                     MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I) const;

  void eliminateFrameIndex(MachineBasicBlock::iterator II,
                           int SPAdj, RegScavenger *RS = NULL) const;

  void processFunctionBeforeFrameFinalized(MachineFunction &MF) const;

  // Debug information queries.
  unsigned getRARegister() const;
  unsigned getFrameRegister(const MachineFunction &MF) const;

  // Exception handling queries.
  unsigned getEHExceptionRegister() const;
  unsigned getEHHandlerRegister() const;

  int getDwarfRegNum(unsigned RegNum, bool isEH) const;
};

} // end namespace llvm

#endif
