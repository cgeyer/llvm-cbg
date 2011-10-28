//===-- FPMover.cpp - Sparc double-precision floating point move fixer ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Expand FpMOVD/FpABSD/FpNEGD instructions into their single-precision pieces.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "fpmover"
#include "cbg.h"
#include "cbgSubtarget.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

STATISTIC(NumFpDs , "Number of instructions translated");
STATISTIC(NoopFpDs, "Number of noop instructions removed");

namespace {
  struct FPMover : public MachineFunctionPass {
    /// Target machine description which we query for reg. names, data
    /// layout, etc.
    ///
    TargetMachine &TM;
    
    static char ID;
    explicit FPMover(TargetMachine &tm) 
      : MachineFunctionPass(ID), TM(tm) { }

    virtual const char *getPassName() const {
      return "Sparc Double-FP Move Fixer";
    }

    bool runOnMachineBasicBlock(MachineBasicBlock &MBB);
    bool runOnMachineFunction(MachineFunction &F);
  };
  char FPMover::ID = 0;
} // end of anonymous namespace

/// createSparcFPMoverPass - Returns a pass that turns FpMOVD
/// instructions into FMOVS instructions
///
FunctionPass *llvm::createcbgFPMoverPass(TargetMachine &tm) {
  return new FPMover(tm);
}

/// getDoubleRegPair - Given a DFP register, return the even and odd FP
/// registers that correspond to it.
static void getDoubleRegPair(unsigned DoubleReg, unsigned &EvenReg,
                             unsigned &OddReg) {
  static const unsigned EvenHalvesOfPairs[] = {
    CBG::F0, CBG::F2, CBG::F4, CBG::F6, CBG::F8, CBG::F10, CBG::F12, CBG::F14,
    CBG::F16, CBG::F18, CBG::F20, CBG::F22, CBG::F24, CBG::F26, CBG::F28, CBG::F30
  };
  static const unsigned OddHalvesOfPairs[] = {
    CBG::F1, CBG::F3, CBG::F5, CBG::F7, CBG::F9, CBG::F11, CBG::F13, CBG::F15,
    CBG::F17, CBG::F19, CBG::F21, CBG::F23, CBG::F25, CBG::F27, CBG::F29, CBG::F31
  };
  static const unsigned DoubleRegsInOrder[] = {
    CBG::D0, CBG::D1, CBG::D2, CBG::D3, CBG::D4, CBG::D5, CBG::D6, CBG::D7, CBG::D8,
    CBG::D9, CBG::D10, CBG::D11, CBG::D12, CBG::D13, CBG::D14, CBG::D15
  };
  for (unsigned i = 0; i < sizeof(DoubleRegsInOrder)/sizeof(unsigned); ++i)
    if (DoubleRegsInOrder[i] == DoubleReg) {
      EvenReg = EvenHalvesOfPairs[i];
      OddReg = OddHalvesOfPairs[i];
      return;
    }
  llvm_unreachable("Can't find reg");
}

/// runOnMachineBasicBlock - Fixup FpMOVD instructions in this MBB.
///
bool FPMover::runOnMachineBasicBlock(MachineBasicBlock &MBB) {
  bool Changed = false;
  for (MachineBasicBlock::iterator I = MBB.begin(); I != MBB.end(); ) {
    MachineInstr *MI = I++;
    DebugLoc dl = MI->getDebugLoc();
    if (MI->getOpcode() == CBG::FpMOVD || MI->getOpcode() == CBG::FpABSD ||
        MI->getOpcode() == CBG::FpNEGD) {
      Changed = true;
      unsigned DestDReg = MI->getOperand(0).getReg();
      unsigned SrcDReg  = MI->getOperand(1).getReg();
      if (DestDReg == SrcDReg && MI->getOpcode() == CBG::FpMOVD) {
        MBB.erase(MI);   // Eliminate the noop copy.
        ++NoopFpDs;
        continue;
      }
      
      unsigned EvenSrcReg = 0, OddSrcReg = 0, EvenDestReg = 0, OddDestReg = 0;
      getDoubleRegPair(DestDReg, EvenDestReg, OddDestReg);
      getDoubleRegPair(SrcDReg, EvenSrcReg, OddSrcReg);

      const TargetInstrInfo *TII = TM.getInstrInfo();
      if (MI->getOpcode() == CBG::FpMOVD)
        MI->setDesc(TII->get(CBG::FMOVS));
      else if (MI->getOpcode() == CBG::FpNEGD)
        MI->setDesc(TII->get(CBG::FNEGS));
      else if (MI->getOpcode() == CBG::FpABSD)
        MI->setDesc(TII->get(CBG::FABSS));
      else
        llvm_unreachable("Unknown opcode!");
        
      MI->getOperand(0).setReg(EvenDestReg);
      MI->getOperand(1).setReg(EvenSrcReg);
      DEBUG(errs() << "FPMover: the modified instr is: " << *MI);
      // Insert copy for the other half of the double.
      if (DestDReg != SrcDReg) {
        MI = BuildMI(MBB, I, dl, TM.getInstrInfo()->get(CBG::FMOVS), OddDestReg)
          .addReg(OddSrcReg);
        DEBUG(errs() << "FPMover: the inserted instr is: " << *MI);
      }
      ++NumFpDs;
    }
  }
  return Changed;
}

bool FPMover::runOnMachineFunction(MachineFunction &F) {
  // If the target has V9 instructions, the fp-mover pseudos will never be
  // emitted.  Avoid a scan of the instructions to improve compile time.
  /*if (TM.getSubtarget<cbgSubtarget>().isV9())
    return false;*/
  
  bool Changed = false;
  for (MachineFunction::iterator FI = F.begin(), FE = F.end();
       FI != FE; ++FI)
    Changed |= runOnMachineBasicBlock(*FI);
  return Changed;
}
