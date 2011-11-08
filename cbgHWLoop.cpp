/*
 * cbgHWLoop.cpp
 *
 *  Created on: Nov 1, 2011
 *      Author: cbg
 */

#include "cbg.h"
#include "cbgInstrInfo.h"
#include "PredecessorList.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Constants.h"
#include "llvm/Support/Debug.h"

#include <vector>
#include <list>

#include <iostream>

#if 0
  #define DD_PRINT(func) {\
    std::cerr << func << std::endl; \
  }
#else
  #define DD_PRINT(func)
#endif

using namespace llvm;

namespace {
  class HWLoopPass : public MachineFunctionPass {

  public:
//    typedef std::list<MachineBasicBlock*> MBB_list;
    typedef PredecessorList::MBB_list MBB_list;
    static char ID;

  protected:
    TargetMachine &TM;
    unsigned LoopDepth;
    unsigned LoopCount;

    virtual MBB_list getPossibleLoops(MachineBasicBlock &MBB) const;
    virtual void insertSingleHWLoop(MachineBasicBlock &MBB);

  public:

    explicit HWLoopPass(TargetMachine &tm, unsigned loopDepth) : MachineFunctionPass(ID), TM(tm),
                                                                 LoopDepth(loopDepth),
                                                                 LoopCount(0) {}

    /*virtual bool doInitialization(Loop* loop, LPPassManager &LPM) { DD_PRINT(__func__); return false; }
    virtual bool runOnLoop(Loop* loop, LPPassManager &LPM);
    virtual bool doFinalization() { DD_PRINT(__func__); return false; }*/

    bool runOnMachineBasicBlock(MachineBasicBlock &MBB);
    bool runOnMachineFunction(MachineFunction &F);

    virtual const char *getPassName() const {
      return "CBG HWloop pass";
    }

  };

  char HWLoopPass::ID = 0;

}

static bool machineBasicBlockInSet(MachineBasicBlock* const &MBB, HWLoopPass::MBB_list &mbb_set) {
  HWLoopPass::MBB_list::iterator mbb_iterator;
  for (mbb_iterator = mbb_set.begin(); mbb_iterator != mbb_set.end(); ++mbb_iterator) {
    if ((*mbb_iterator)->getNumber() == MBB->getNumber()) {
      return true;
    }
  }
  return false;
}

HWLoopPass::MBB_list HWLoopPass::getPossibleLoops(MachineBasicBlock &MBB) const {

  MBB_list possible_loops;
  MBB_list all_successors;
  MachineBasicBlock::succ_iterator s_iter;
  MachineBasicBlock::succ_iterator s_end_iter = MBB.succ_end();
  PredecessorList all_predecessors;
  bool endLoop;

  // return empty vector if BB has no successor
  if (MBB.succ_empty()) {
//    std::cerr << "Branch BB#"<< MBB.getNumber() << " has no successors." << std::endl;
    return possible_loops;
  }

  // insert current basic block as "root element" into predecessor
  all_predecessors.insertRoot(&MBB);

  // init successor set
  for (s_iter = MBB.succ_begin(); s_iter != s_end_iter; ++s_iter) {
//    std::cerr << "Adding BB#"<< (*s_iter)->getNumber() << " to successor list..." << std::endl;
    all_successors.push_back(*s_iter);
    all_predecessors.insertSuccessor(*s_iter, &MBB);
    if (((*s_iter)->getNumber() == MBB.getNumber()) && (!machineBasicBlockInSet(&MBB, possible_loops))) {
      possible_loops.push_back(&MBB);
      // we have found a small basic block loop => ignore bigger ones
      return possible_loops;
    }
  }

//  std::cerr << "Done with initialization!" << std::endl;

  // add all successors to successor set
  for (MBB_list::iterator suc_it = all_successors.begin();
      suc_it != all_successors.end();
      ++suc_it) {

    endLoop = false;

    s_end_iter = (*suc_it)->succ_end();
    for (s_iter = (*suc_it)->succ_begin(); s_iter != s_end_iter; ++s_iter) {

      // in any case, add predecessor to set
      all_predecessors.insertSuccessor(*s_iter, const_cast<const MachineBasicBlock*&>(*suc_it));
//      std::cerr << "Set BB#" << (*suc_it)->getNumber() << " to predecessor of BB#" << (*s_iter)->getNumber() << std::endl;
      // if successor of successor set is current BB, we have found a loop
      if ((*s_iter)->getNumber() == MBB.getNumber()) {
        possible_loops = all_predecessors.getPredecessors(const_cast<const MachineBasicBlock*&>(*suc_it));
        endLoop = true;
        break;
      } else {
        // otherwise we have to add it to the successor
        if (!machineBasicBlockInSet(*s_iter, all_successors)) {
//          std::cerr << "Adding BB#"<< (*s_iter)->getNumber() << " to successor list..." << std::endl;
          all_successors.push_back(*s_iter);
        }
      }

    }

    // we have found a loop, so exit current for loop
    if (endLoop) {
      break;
    }

  }

//  std::cerr << "Done with successor analysis!" << std::endl;

  return possible_loops;

}

/*
static void printPossibleLoops(HWLoopPass::MBB_list &mbb_set) {
  HWLoopPass::MBB_list::iterator mbb_iter = mbb_set.begin();
  HWLoopPass::MBB_list::iterator mbb_end_iter = mbb_set.end();
  if (mbb_set.size() != 0) {
    std::cerr << "Found Loops in set: " << std::endl << "\t";
    while (mbb_iter != mbb_end_iter) {
      std::cerr << "BB#" << (*mbb_iter)->getNumber() << " -> ";
      ++mbb_iter;
    }
    std::cerr << "BB#" << (*(mbb_set.begin()))->getNumber() << std::endl;
  }
}
*/

static void updatePredecessors(MachineBasicBlock* newMBB, MachineBasicBlock* MBB) {

  std::vector<MachineBasicBlock*> predecessors;
  MachineBasicBlock::pred_iterator pred_iter = MBB->pred_begin();
  while (pred_iter != MBB->pred_end()) {
    // add all predecessors except own basic block to set
    if (*pred_iter != MBB) {
      predecessors.push_back(*pred_iter);
    }
    ++pred_iter;
  }

  pred_iter = predecessors.begin();

  while (pred_iter != predecessors.end()) {
    (*pred_iter)->removeSuccessor(MBB);
    (*pred_iter)->addSuccessor(newMBB);
    // update branch targets to old basic block
    for (MachineBasicBlock::iterator instr_it = (*pred_iter)->begin();
         instr_it != (*pred_iter)->end();
         ++instr_it) {
      if ((instr_it->getOpcode() == CBG::BA) || (instr_it->getOpcode() == CBG::BCOND)) {
        if (instr_it->getOperand(0).getMBB() == MBB) {
          instr_it->getOperand(0).setMBB(newMBB);
        }
      }
    }
    ++pred_iter;
  }


}

void HWLoopPass::insertSingleHWLoop(MachineBasicBlock &MBB) {

  MachineFunction *F = MBB.getParent();
  const BasicBlock *LLVM_BB = MBB.getBasicBlock();

  MachineBasicBlock* newMBB = F->CreateMachineBasicBlock(LLVM_BB);

  MachineFunction::iterator func_it = F->begin();

  while ((*func_it).getNumber() != MBB.getNumber()) {
    ++func_it;
  }
  ++func_it;

  F->insert(func_it, newMBB);

  newMBB->moveBefore(&MBB);

  MachineBasicBlock::iterator mbb_iter = newMBB->begin();
  MachineInstrBuilder mi =
      BuildMI(*newMBB, mbb_iter, newMBB->findDebugLoc(mbb_iter), TM.getInstrInfo()->get(CBG::HWLOOP));

  mi.addMBB(&MBB).addMBB(MBB.getNextNode()).addImm(20);

  /*mi = BuildMI(*newMBB, mbb_iter, newMBB->findDebugLoc(mbb_iter), TM.getInstrInfo()->get(CBG::BA));
  mi.addMBB(newMBB);*/

  /*for (func_it = F->begin(); func_it != F->end(); ++func_it) {
    (*func_it).ReplaceUsesOfBlockWith(&MBB, newMBB);
  }*/

  updatePredecessors(newMBB, &MBB);
  newMBB->addSuccessor(&MBB);
//  MBB.getNextNode()->setHasAddressTaken();

//  MBB.getNextNode()->setHasAddressTaken();

}

bool HWLoopPass::runOnMachineFunction(MachineFunction &F) {
  bool Changed = false;
  MBB_list possible_loops;
  for (MachineFunction::iterator FI = F.begin(), FE = F.end();
      FI != FE; ++FI) {
//    std::cerr << "Analyzing BB#" << (*FI).getNumber() << std::endl;
    possible_loops = getPossibleLoops(*FI);
//    std::cerr << "Printing out loop..." << std::endl;
//    printPossibleLoops(possible_loops);
    if (possible_loops.size() == 1) {
      insertSingleHWLoop(*FI);
    }
    // Changed |= runOnMachineBasicBlock(*FI);
  }
  return Changed;
}

bool HWLoopPass::runOnMachineBasicBlock(MachineBasicBlock &MBB) {
  return false;
}

FunctionPass* llvm::createcbgHWLoopPass(TargetMachine &tm, unsigned loopDepth) {
  DD_PRINT(__func__);
  return new HWLoopPass(tm, loopDepth);
}
