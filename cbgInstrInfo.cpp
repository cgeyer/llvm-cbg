//===- cbgInstrInfo.cpp - cbg Instruction Information -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the cbg implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "cbgInstrInfo.h"
#include "cbgSubtarget.h"
#include "cbg.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "cbgGenInstrInfo.inc"
#include "cbgMachineFunctionInfo.h"
using namespace llvm;

cbgInstrInfo::cbgInstrInfo(cbgSubtarget &ST)
  : TargetInstrInfoImpl(cbgInsts, array_lengthof(cbgInsts)),
    RI(ST, *this), Subtarget(ST) {
}

/// isLoadFromStackSlot - If the specified machine instruction is a direct
/// load from a stack slot, return the virtual or physical register number of
/// the destination along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than loading from the stack slot.
unsigned cbgInstrInfo::isLoadFromStackSlot(const MachineInstr *MI,
                                             int &FrameIndex) const {
  if (MI->getOpcode() == CBG::LDri ||
      MI->getOpcode() == CBG::LDFri ||
      MI->getOpcode() == CBG::LDDFri) {
    if (MI->getOperand(1).isFI() && MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      FrameIndex = MI->getOperand(1).getIndex();
      return MI->getOperand(0).getReg();
    }
  }
  return 0;
}

/// isStoreToStackSlot - If the specified machine instruction is a direct
/// store to a stack slot, return the virtual or physical register number of
/// the source reg along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than storing to the stack slot.
unsigned cbgInstrInfo::isStoreToStackSlot(const MachineInstr *MI,
                                            int &FrameIndex) const {
  if (MI->getOpcode() == CBG::STri ||
      MI->getOpcode() == CBG::STFri ||
      MI->getOpcode() == CBG::STDFri) {
    if (MI->getOperand(0).isFI() && MI->getOperand(1).isImm() &&
        MI->getOperand(1).getImm() == 0) {
      FrameIndex = MI->getOperand(0).getIndex();
      return MI->getOperand(2).getReg();
    }
  }
  return 0;
}

static bool IsIntegerCC(unsigned CC)
{
  return  (CC <= CBGCC::ICC_VC);
}



CBGCC::CondCodes CBG::getOppositeBranchCondition(CBGCC::CondCodes CC)
{
  switch(CC) {
  default: llvm_unreachable("Unknown condition code");
  case CBGCC::ICC_NE:   return CBGCC::ICC_E;
  case CBGCC::ICC_E:    return CBGCC::ICC_NE;
  case CBGCC::ICC_G:    return CBGCC::ICC_LE;
  case CBGCC::ICC_LE:   return CBGCC::ICC_G;
  case CBGCC::ICC_GE:   return CBGCC::ICC_L;
  case CBGCC::ICC_L:    return CBGCC::ICC_GE;
  case CBGCC::ICC_GU:   return CBGCC::ICC_LEU;
  case CBGCC::ICC_LEU:  return CBGCC::ICC_GU;
  case CBGCC::ICC_CC:   return CBGCC::ICC_CS;
  case CBGCC::ICC_CS:   return CBGCC::ICC_CC;
  case CBGCC::ICC_POS:  return CBGCC::ICC_NEG;
  case CBGCC::ICC_NEG:  return CBGCC::ICC_POS;
  case CBGCC::ICC_VC:   return CBGCC::ICC_VS;
  case CBGCC::ICC_VS:   return CBGCC::ICC_VC;

  case CBGCC::FCC_U:    return CBGCC::FCC_O;
  case CBGCC::FCC_O:    return CBGCC::FCC_U;
  case CBGCC::FCC_G:    return CBGCC::FCC_LE;
  case CBGCC::FCC_LE:   return CBGCC::FCC_G;
  case CBGCC::FCC_UG:   return CBGCC::FCC_ULE;
  case CBGCC::FCC_ULE:  return CBGCC::FCC_UG;
  case CBGCC::FCC_L:    return CBGCC::FCC_GE;
  case CBGCC::FCC_GE:   return CBGCC::FCC_L;
  case CBGCC::FCC_UL:   return CBGCC::FCC_UGE;
  case CBGCC::FCC_UGE:  return CBGCC::FCC_UL;
  case CBGCC::FCC_LG:   return CBGCC::FCC_UE;
  case CBGCC::FCC_UE:   return CBGCC::FCC_LG;
  case CBGCC::FCC_NE:   return CBGCC::FCC_E;
  case CBGCC::FCC_E:    return CBGCC::FCC_NE;
  }
}



bool cbgInstrInfo::AnalyzeBranch(MachineBasicBlock &MBB,
                                   MachineBasicBlock *&TBB,
                                   MachineBasicBlock *&FBB,
                                   SmallVectorImpl<MachineOperand> &Cond,
                                   bool AllowModify) const
{

  MachineBasicBlock::iterator I = MBB.end();
  MachineBasicBlock::iterator UnCondBrIter = MBB.end();
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugValue())
      continue;

    //When we see a non-terminator, we are done
    if (!isUnpredicatedTerminator(I))
      break;

    //Terminator is not a branch
    if (!I->getDesc().isBranch())
      return true;

    //Handle Unconditional branches
    if (I->getOpcode() == CBG::BA) {
      UnCondBrIter = I;

      if (!AllowModify) {
        TBB = I->getOperand(0).getMBB();
        continue;
      }

      while (llvm::next(I) != MBB.end())
        llvm::next(I)->eraseFromParent();

      Cond.clear();
      FBB = 0;

      if (MBB.isLayoutSuccessor(I->getOperand(0).getMBB())) {
        TBB = 0;
        I->eraseFromParent();
        I = MBB.end();
        UnCondBrIter = MBB.end();
        continue;
      }

      TBB = I->getOperand(0).getMBB();
      continue;
    }

    unsigned Opcode = I->getOpcode();
    if (Opcode != CBG::BCOND && Opcode != CBG::FBCOND)
      return true; //Unknown Opcode

    CBGCC::CondCodes BranchCode = (CBGCC::CondCodes)I->getOperand(1).getImm();

    if (Cond.empty()) {
      MachineBasicBlock *TargetBB = I->getOperand(0).getMBB();
      if (AllowModify && UnCondBrIter != MBB.end() &&
          MBB.isLayoutSuccessor(TargetBB)) {

        //Transform the code
        //
        //    brCC L1
        //    ba L2
        // L1:
        //    ..
        // L2:
        //
        // into
        //
        //   brnCC L2
        // L1:
        //   ...
        // L2:
        //
        BranchCode = CBG::getOppositeBranchCondition(BranchCode);
        MachineBasicBlock::iterator OldInst = I;
        BuildMI(MBB, UnCondBrIter, MBB.findDebugLoc(I), get(Opcode))
          .addMBB(UnCondBrIter->getOperand(0).getMBB()).addImm(BranchCode);
        BuildMI(MBB, UnCondBrIter, MBB.findDebugLoc(I), get(CBG::BA))
          .addMBB(TargetBB);
        MBB.addSuccessor(TargetBB);
        OldInst->eraseFromParent();
        UnCondBrIter->eraseFromParent();

        UnCondBrIter = MBB.end();
        I = MBB.end();
        continue;
      }
      FBB = TBB;
      TBB = I->getOperand(0).getMBB();
      Cond.push_back(MachineOperand::CreateImm(BranchCode));
      continue;
    }
    //FIXME: Handle subsequent conditional branches
    //For now, we can't handle multiple conditional branches
    return true;
  }
  return false;
}

unsigned
cbgInstrInfo::InsertBranch(MachineBasicBlock &MBB,MachineBasicBlock *TBB,
                             MachineBasicBlock *FBB,
                             const SmallVectorImpl<MachineOperand> &Cond,
                             DebugLoc DL) const {
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 1 || Cond.size() == 0) &&
         "cbg branch conditions should have one component!");

  if (Cond.empty()) {
    assert(!FBB && "Unconditional branch with multiple successors!");
    BuildMI(&MBB, DL, get(CBG::BA)).addMBB(TBB);
    return 1;
  }

  //Conditional branch
  unsigned CC = Cond[0].getImm();

  if (IsIntegerCC(CC))
    BuildMI(&MBB, DL, get(CBG::BCOND)).addMBB(TBB).addImm(CC);
  else
    BuildMI(&MBB, DL, get(CBG::FBCOND)).addMBB(TBB).addImm(CC);
  if (!FBB)
    return 1;

  BuildMI(&MBB, DL, get(CBG::BA)).addMBB(FBB);
  return 2;
}

unsigned cbgInstrInfo::RemoveBranch(MachineBasicBlock &MBB) const
{
  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugValue())
      continue;

    if (I->getOpcode() != CBG::BA
        && I->getOpcode() != CBG::BCOND
        && I->getOpcode() != CBG::FBCOND)
      break; // Not a branch

    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }
  return Count;
}

void cbgInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator I, DebugLoc DL,
                                 unsigned DestReg, unsigned SrcReg,
                                 bool KillSrc) const {
  if (CBG::IntRegsRegClass.contains(DestReg, SrcReg))
    BuildMI(MBB, I, DL, get(CBG::ORrr), DestReg).addReg(CBG::G0)
      .addReg(SrcReg, getKillRegState(KillSrc));
  else if (CBG::FPRegsRegClass.contains(DestReg, SrcReg))
    BuildMI(MBB, I, DL, get(CBG::FMOVS), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
  /*else if (CBG::DFPRegsRegClass.contains(DestReg, SrcReg))
    BuildMI(MBB, I, DL, get(Subtarget.isV9() ? CBG::FMOVD : CBG::FpMOVD), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));*/
  else
    llvm_unreachable("Impossible reg-to-reg copy");
}

void cbgInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                    unsigned SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  // On the order of operands here: think "[FrameIdx + 0] = SrcReg".
  if (RC == CBG::IntRegsRegisterClass)
    BuildMI(MBB, I, DL, get(CBG::STri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg, getKillRegState(isKill));
  else if (RC == CBG::FPRegsRegisterClass)
    BuildMI(MBB, I, DL, get(CBG::STFri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg,  getKillRegState(isKill));
  else if (RC == CBG::DFPRegsRegisterClass)
    BuildMI(MBB, I, DL, get(CBG::STDFri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg,  getKillRegState(isKill));
  else
    llvm_unreachable("Can't store this register to stack slot");
}

void cbgInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                     unsigned DestReg, int FI,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  if (RC == CBG::IntRegsRegisterClass)
    BuildMI(MBB, I, DL, get(CBG::LDri), DestReg).addFrameIndex(FI).addImm(0);
  else if (RC == CBG::FPRegsRegisterClass)
    BuildMI(MBB, I, DL, get(CBG::LDFri), DestReg).addFrameIndex(FI).addImm(0);
  else if (RC == CBG::DFPRegsRegisterClass)
    BuildMI(MBB, I, DL, get(CBG::LDDFri), DestReg).addFrameIndex(FI).addImm(0);
  else
    llvm_unreachable("Can't load this register from stack slot");
}

unsigned cbgInstrInfo::getGlobalBaseReg(MachineFunction *MF) const
{
  cbgMachineFunctionInfo *cbgFI = MF->getInfo<cbgMachineFunctionInfo>();
  unsigned GlobalBaseReg = cbgFI->getGlobalBaseReg();
  if (GlobalBaseReg != 0)
    return GlobalBaseReg;

  // Insert the set of GlobalBaseReg into the first MBB of the function
  MachineBasicBlock &FirstMBB = MF->front();
  MachineBasicBlock::iterator MBBI = FirstMBB.begin();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();

  GlobalBaseReg = RegInfo.createVirtualRegister(&CBG::IntRegsRegClass);


  DebugLoc dl;

  BuildMI(FirstMBB, MBBI, dl, get(CBG::GETPCX), GlobalBaseReg);
  cbgFI->setGlobalBaseReg(GlobalBaseReg);
  return GlobalBaseReg;
}
