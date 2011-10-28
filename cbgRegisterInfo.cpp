//===- cbgRegisterInfo.cpp - CBG Register Information -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the CBG implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "cbg.h"
#include "cbgRegisterInfo.h"
#include "cbgSubtarget.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineLocation.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Type.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
using namespace llvm;

cbgRegisterInfo::cbgRegisterInfo(cbgSubtarget &st,
                                     const TargetInstrInfo &tii)
  : cbgGenRegisterInfo(CBG::ADJCALLSTACKDOWN, CBG::ADJCALLSTACKUP),
    Subtarget(st), TII(tii) {
}

const unsigned* cbgRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF)
                                                                         const {
  static const unsigned CalleeSavedRegs[] = { 0 };
  return CalleeSavedRegs;
}

BitVector cbgRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  Reserved.set(CBG::G2);
  Reserved.set(CBG::G3);
  Reserved.set(CBG::G4);
  Reserved.set(CBG::O6);
  Reserved.set(CBG::I6);
  Reserved.set(CBG::I7);
  Reserved.set(CBG::G0);
  Reserved.set(CBG::G5);
  Reserved.set(CBG::G6);
  Reserved.set(CBG::G7);
  return Reserved;
}

void cbgRegisterInfo::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  MachineInstr &MI = *I;
  DebugLoc dl = MI.getDebugLoc();
  int Size = MI.getOperand(0).getImm();
  if (MI.getOpcode() == CBG::ADJCALLSTACKDOWN)
    Size = -Size;
  if (Size)
    BuildMI(MBB, I, dl, TII.get(CBG::ADDri), CBG::O6).addReg(CBG::O6).addImm(Size);
  MBB.erase(I);
}

void
cbgRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                       int SPAdj, RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  unsigned i = 0;
  MachineInstr &MI = *II;
  DebugLoc dl = MI.getDebugLoc();
  while (!MI.getOperand(i).isFI()) {
    ++i;
    assert(i < MI.getNumOperands() && "Instr doesn't have FrameIndex operand!");
  }

  int FrameIndex = MI.getOperand(i).getIndex();

  // Addressable stack objects are accessed using neg. offsets from %fp
  MachineFunction &MF = *MI.getParent()->getParent();
  int Offset = MF.getFrameInfo()->getObjectOffset(FrameIndex) +
               MI.getOperand(i+1).getImm();

  // Replace frame index with a frame pointer reference.
  if (Offset >= -4096 && Offset <= 4095) {
    // If the offset is small enough to fit in the immediate field, directly
    // encode it.
    MI.getOperand(i).ChangeToRegister(CBG::I6, false);
    MI.getOperand(i+1).ChangeToImmediate(Offset);
  } else {
    // Otherwise, emit a G1 = SETHI %hi(offset).  FIXME: it would be better to 
    // scavenge a register here instead of reserving G1 all of the time.
    unsigned OffHi = (unsigned)Offset >> 10U;
    BuildMI(*MI.getParent(), II, dl, TII.get(CBG::SETHIi), CBG::G1).addImm(OffHi);
    // Emit G1 = G1 + I6
    BuildMI(*MI.getParent(), II, dl, TII.get(CBG::ADDrr), CBG::G1).addReg(CBG::G1)
      .addReg(CBG::I6);
    // Insert: G1+%lo(offset) into the user.
    MI.getOperand(i).ChangeToRegister(CBG::G1, false);
    MI.getOperand(i+1).ChangeToImmediate(Offset & ((1 << 10)-1));
  }
}

void cbgRegisterInfo::
processFunctionBeforeFrameFinalized(MachineFunction &MF) const {}

unsigned cbgRegisterInfo::getRARegister() const {
  return CBG::I7;
}

unsigned cbgRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return CBG::I6;
}

unsigned cbgRegisterInfo::getEHExceptionRegister() const {
  llvm_unreachable("What is the exception register");
  return 0;
}

unsigned cbgRegisterInfo::getEHHandlerRegister() const {
  llvm_unreachable("What is the exception handler register");
  return 0;
}

int cbgRegisterInfo::getDwarfRegNum(unsigned RegNum, bool isEH) const {
  return cbgRegisterInfo::getDwarfRegNumFull(RegNum, 0);
}

#include "cbgGenRegisterInfo.inc"

