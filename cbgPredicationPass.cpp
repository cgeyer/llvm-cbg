/**
 * @file cbgPredicationPass.cpp
 *
 * @date 2011-11-14
 * @author Clemens Bernhard Geyer
 */

#include "cbg.h"
#include "cbgInstrInfo.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"

#include <vector>
#include <list>

#include <iostream>

using namespace llvm;

namespace {
  /**
   * @brief Abstract basic class for all predication passes.
   *        Provides Methods which are required in all predication passes.
   */
  class PredicationPass : public MachineFunctionPass {

  public:

    typedef enum {
      IFELSEBRANCH,
      IFBRANCH,
      NOBRANCH
    } branchType;

    static char ID;

  protected:
    TargetMachine &TM;

    static int getConditionCode(MachineBasicBlock &MBB);

    static branchType getBranchType(MachineBasicBlock &TBB, MachineBasicBlock &FBB);

    virtual void removeBranches(MachineBasicBlock &MBB);
    virtual void mergeBlocks(MachineBasicBlock &Pre, MachineBasicBlock &Post);

    struct BranchMBB {
      MachineBasicBlock* TBB;
      MachineBasicBlock* FBB;
      branchType         btype;
    };


  public:

    typedef std::list<BranchMBB> BranchMBB_list;
    explicit PredicationPass(TargetMachine &tm) : MachineFunctionPass(ID), TM(tm) {}
    virtual bool runOnMachineFunction(MachineFunction &F) = 0;
    virtual const char *getPassName() const {
      return "CBG predication pass";
    }

  };

  char PredicationPass::ID = 0;

  /**
   * @brief Pass to insert predicated blocks based on condition
   *        codes of the status register.
   */
  class PredicatedBlocksCCPass : public PredicationPass {
  protected:
    virtual bool insertPredicatedBlock(MachineBasicBlock &TBB, MachineBasicBlock &FBB, branchType btype);
  public:
    explicit PredicatedBlocksCCPass(TargetMachine &tm) : PredicationPass(tm) {}
    virtual bool runOnMachineFunction(MachineFunction &F);
  };

  /**
   * @brief Abstract basic class which provides methods for all
   *        predication passes using predication registers.
   */
  class PredicatedRegPass : public PredicationPass {
  public:
    typedef std::list<unsigned> RegList;
  protected:
    virtual unsigned getNextFreePredRegister(MachineBasicBlock &MBB);
    virtual unsigned getNextFreePredRegister(MachineBasicBlock &TBB, MachineBasicBlock &FBB);
    virtual bool isPredRegInList(RegList &rList, unsigned RegNo);
    virtual RegList getUsedPredRegisters(MachineBasicBlock &MBB);
    virtual RegList getUsedPredRegisters(MachineBasicBlock &TBB, MachineBasicBlock &FBB);
    virtual RegList cutPredRegLists(RegList &rList1, RegList &rList2);
    virtual RegList removePredRegClears(MachineBasicBlock &MBB);
  public:
    explicit PredicatedRegPass(TargetMachine &tm) : PredicationPass(tm) {}
    virtual bool runOnMachineFunction(MachineFunction &F) = 0;
  };

  /**
   * @brief Pass which implements predicated blocks based on predication registers.
   */
  class PredicatedBlocksRegPass : public PredicatedRegPass {
  protected:
    virtual void removePredEnds(MachineBasicBlock &MBB);
    virtual bool insertPredicatedBlock(MachineBasicBlock &TBB, MachineBasicBlock &FBB, branchType btype);
  public:
    explicit PredicatedBlocksRegPass(TargetMachine &tm) : PredicatedRegPass(tm) {}
    virtual bool runOnMachineFunction(MachineFunction &F);
  };

  /**
   * @brief Pass which implements a fully predicated instruction set based on
   *        condition codes of the status register.
   */
  class PredicatedInstrCCPass : public PredicatedBlocksCCPass {
  protected:
    virtual void changeToPredicatedInstruction(MachineBasicBlock &Pre,
                                               MachineBasicBlock &MBB,
                                               unsigned ConditionCode);
    virtual bool insertPredicatedBlock(MachineBasicBlock &TBB, MachineBasicBlock &FBB, branchType btype);
  public:
    explicit PredicatedInstrCCPass(TargetMachine &tm) : PredicatedBlocksCCPass(tm) {}
  };

  /**
   * @brief Pass which implements a fully predicated instruction set based on
   *        predication registers.
   */
  class PredicatedInstrRegPass : public PredicatedRegPass {
  protected:
    virtual void changeToPredicatedInstruction(MachineBasicBlock &Pre,
                                               MachineBasicBlock &MBB,
                                               unsigned PReg,
                                               unsigned TF);
    virtual bool insertPredicatedBlock(MachineBasicBlock &TBB, MachineBasicBlock &FBB, branchType btype);
  public:
    explicit PredicatedInstrRegPass(TargetMachine &tm) : PredicatedRegPass(tm) {}
    virtual bool runOnMachineFunction(MachineFunction &F);
  };

}

/**
 * @brief Returns the condition code of the last conditional branch of
 *        a machine basic block.
 * @param MBB The Machine basic block to analyze.
 * @return Either the condition code of the last conditional branch
 *         or -1 if there is no conditional branch.
 */
int PredicationPass::getConditionCode(MachineBasicBlock &MBB) {
  MachineBasicBlock::reverse_iterator mbb_iter;
  for (mbb_iter = MBB.rbegin(); mbb_iter != MBB.rend(); ++mbb_iter) {
    if (mbb_iter->getOpcode() == CBG::BCOND) {
      return mbb_iter->getOperand(1).getImm();
    }
  }
  return -1;
}


/**
 * @brief Analyzes whether to given machine basic blocks may be part of
 *        of an if-then-else construction.
 * @param TBB Represents the "if-then" branch.
 * @param FBB Represents the "else" branch in the case of an IFELSEBRANCH or
 *            the machine basic block logically following TBB in the case of
 *            an IFBRANCH.
 * @return NOBRANCH in the case the machine basic blocks cannot be analyzed,
 *         IFELSEBRANCH in the case both basic blocks conform to an if-then-else
 *         graph, or IFBRANCH in the case that TBB is an if-branch and FBB is
 *         the (logically) following basic block.
 */
PredicationPass::branchType PredicationPass::getBranchType(MachineBasicBlock &TBB, MachineBasicBlock &FBB) {

  branchType returnValue = NOBRANCH;
  MachineBasicBlock::pred_iterator mbb_iter;

  if (&TBB == &FBB) {
   returnValue = NOBRANCH;
  } // if-else-branch ? => both only have one predecessor
  else if (TBB.pred_size() == 1 && FBB.pred_size() == 1) {
    // both have the same predecessor
    if (*(TBB.pred_begin()) == *(FBB.pred_begin())) {
      // both have only one successor
      if (TBB.succ_size() == 1 && FBB.succ_size() == 1) {
        // both have the same successor
        if (*(TBB.succ_begin()) == *(FBB.succ_begin())) {
          // we assume the following layout: Pre -> FBB -> TBB
          if  ((*(TBB.pred_begin()))->isLayoutSuccessor(&FBB) &&
                  FBB.isLayoutSuccessor(&TBB))
            returnValue = IFELSEBRANCH;
        }
      }
    }
  } else if (TBB.pred_size() == 1 &&
           (*TBB.pred_begin())->isLayoutSuccessor(&TBB)) {
    // only if-branch: The (single) predecessor of TBB also has
    // to be the layout predecessor of TBB.
    if (TBB.succ_size() == 1 && TBB.isSuccessor(&FBB)) {
      // TBB must only have one successor and this
      // successor has to be FBB.
      if ((*(TBB.pred_begin()))->isSuccessor(&FBB)) {
        // FBB and TBB must have the same predecessor, but
        // FBB does not need to be the layout successor of
        // TBB.
        returnValue = IFBRANCH;
      }
    }
  }
  return returnValue;
}


/**
 * @brief Appends the second machine basic block to the first one and
 *        renumbers all basic blocks of the current function.
 * @param Pre  Basic block which finally holds the instructions of both basic
 *             blocks.
 * @param Post The basic block which will finally removed.
 */
void PredicationPass::mergeBlocks(MachineBasicBlock &Pre, MachineBasicBlock &Post) {

  MachineBasicBlock::iterator mbb_iter;
  MachineBasicBlock::succ_iterator succ_iter;
  MachineFunction* F = Pre.getParent();

  // add all instructions of TBB to Pre basic block
  mbb_iter = Post.begin();
  while (mbb_iter != Post.end()) {
    MachineInstr* MI = mbb_iter->removeFromParent();
    Pre.push_back(MI);
    mbb_iter = Post.begin();
  }

  // removing all (old) successors from pre
  succ_iter = Pre.succ_begin();
  while (succ_iter != Pre.succ_end()) {
    Pre.removeSuccessor(*succ_iter);
    succ_iter = Pre.succ_begin();
  }
  Pre.transferSuccessors(&Post);

  // removing Post
  Post.eraseFromParent();

  F->RenumberBlocks(&Pre);

}

/**
 * @brief Removes all conditional and unconditional branches of the given
 *        machine basic block.
 * @param MBB The machine basic block from which all branches will be removed.
 */
void PredicationPass::removeBranches(MachineBasicBlock &MBB) {
  MachineBasicBlock::reverse_iterator mbb_iter;
  std::list<MachineInstr*> branchesToRemove;
  std::list<MachineInstr*>::iterator branch_iter;

  // save all branches in list
  for (mbb_iter = MBB.rbegin(); mbb_iter != MBB.rend(); ++mbb_iter) {
    if (mbb_iter->getOpcode() == CBG::BCOND || mbb_iter->getOpcode() == CBG::BA) {
      branchesToRemove.push_front(&(*mbb_iter));
    }
  }

  // remove all branches from basic block
  for (branch_iter = branchesToRemove.begin();
       branch_iter != branchesToRemove.end();
       ++branch_iter) {
    (*branch_iter)->eraseFromParent();
  }
}

/**
 * @brief Analyzes whether the given register has already been saved in the
 *        given list.
 * @param rList List of registers which are represented by unsigned numbers.
 * @param RegNo Register number of the register to be tested.
 * @return True if the given register is part of the list, false otherwise.
 */
bool PredicatedRegPass::isPredRegInList(RegList &rList, unsigned RegNo) {
  RegList::iterator reg_iter;
  bool inList = false;
  for (reg_iter = rList.begin(); reg_iter != rList.end(); ++reg_iter) {
    if (*reg_iter == RegNo) {
      inList = true;
      break;
    }
  }

  return inList;
}

/**
 * @brief Analyzes the used predication registers of a given machine basic block.
 * @param MBB The machine basic block to analyze.
 * @return A set (i.e. a list) of used predication registers.
 */
PredicatedRegPass::RegList PredicatedRegPass::getUsedPredRegisters(MachineBasicBlock &MBB) {

  RegList rList;
  MachineBasicBlock::iterator mbb_iter;
  unsigned OpCode;
  unsigned RegNo;

  for (mbb_iter = MBB.begin(); mbb_iter != MBB.end(); ++mbb_iter) {
    OpCode = mbb_iter->getOpcode();
    // add all used registers to list
    if (OpCode == CBG::PREDREGSETCC || OpCode == CBG::PREDREGSET || OpCode == CBG::PREDREGCLEAR ||
        OpCode == CBG::PREDREGSETCC_PREG) {
      RegNo = mbb_iter->getOperand(0).getReg();
      if (!isPredRegInList(rList, RegNo)) {
        rList.push_back(RegNo);
      }
    }
  }

  return rList;
}

/**
 * @brief Analyzes the used predication registers of two given machine basic blocks.
 * @param TBB The first machine basic block to analyze.
 * @param FBB The first machine basic block to analyze.
 * @return A set (i.e. a list) of used predication registers.
 */
PredicatedRegPass::RegList PredicatedRegPass::getUsedPredRegisters(MachineBasicBlock &TBB, MachineBasicBlock &FBB) {

  RegList rList;
  MachineBasicBlock::iterator mbb_iter;
  unsigned OpCode;
  unsigned RegNo;

  for (mbb_iter = TBB.begin(); mbb_iter != TBB.end(); ++mbb_iter) {
    OpCode = mbb_iter->getOpcode();
    // add all used registers to list
    if (OpCode == CBG::PREDREGSETCC || OpCode == CBG::PREDREGSET || OpCode == CBG::PREDREGCLEAR ||
        OpCode == CBG::PREDREGSETCC_PREG) {
      RegNo = mbb_iter->getOperand(0).getReg();
      if (!isPredRegInList(rList, RegNo)) {
        rList.push_back(RegNo);
      }
    }
  }

  for (mbb_iter = FBB.begin(); mbb_iter != FBB.end(); ++mbb_iter) {
    OpCode = mbb_iter->getOpcode();
    // add all used registers to list
    if (OpCode == CBG::PREDREGSETCC || OpCode == CBG::PREDREGSET || OpCode == CBG::PREDREGCLEAR ||
        OpCode == CBG::PREDREGSETCC_PREG) {
      RegNo = mbb_iter->getOperand(0).getReg();
      if (!isPredRegInList(rList, RegNo)) {
        rList.push_back(RegNo);
      }
    }
  }

  return rList;
}

/**
 * @brief Gets a set of registers which are part of both given lists.
 * @param rList1 First input set of registers.
 * @param rList2 Second input set of registers.
 * @return A set (i.e. a list) of registers representing a cut of rList1 and rList2.
 */
PredicatedRegPass::RegList PredicatedRegPass::cutPredRegLists(RegList &rList1, RegList &rList2) {
  RegList rList = rList1;
  RegList::iterator reglist_iter = rList.begin();

  while(reglist_iter != rList.end()) {
    if (!isPredRegInList(rList2, *reglist_iter)) {
      rList.remove(*reglist_iter);
      reglist_iter = rList.begin();
      continue;
    }
    ++reglist_iter;
  }

  return rList;

}

/**
 * @brief Register allocator for predication registers based on the
 *        given machine basic block.
 * @param MBB The machine basic block which is the basis for the register
 *        allocator.
 * @return The first predication register which is not used in MBB or (-1)
 *         if there is no free register.
 */
unsigned PredicatedRegPass::getNextFreePredRegister(MachineBasicBlock &MBB) {

  RegList rList = getUsedPredRegisters(MBB);
  CBG::PREDRegsClass predRegs;
  CBG::PREDRegsClass::iterator pregclass_iter;
  unsigned nextFreePredRegister = static_cast<unsigned>(-1);

  for (pregclass_iter = predRegs.begin(); pregclass_iter != predRegs.end(); ++pregclass_iter) {
    if (!isPredRegInList(rList, *pregclass_iter)) {
      nextFreePredRegister = *pregclass_iter;
      break;
    }
  }

  return nextFreePredRegister;

}

/**
 * @brief Register allocator for predication registers based on the
 *        given machine basic blocks.
 * @param MBB First machine basic block which is the basis for the register
 *        allocator.
 * @param FBB Second machine basic block which is the basis for the register
 *        allocator.
 * @return The first predication register which is not used in MBB and FBB
 *         or (-1) if there is no free register.
 */
unsigned PredicatedRegPass::getNextFreePredRegister(MachineBasicBlock &TBB, MachineBasicBlock &FBB) {

  RegList rList = getUsedPredRegisters(TBB, FBB);

  CBG::PREDRegsClass predRegs;
  CBG::PREDRegsClass::iterator pregclass_iter;
  unsigned nextFreePredRegister = static_cast<unsigned>(-1);

  for (pregclass_iter = predRegs.begin(); pregclass_iter != predRegs.end(); ++pregclass_iter) {
    if (!isPredRegInList(rList, *pregclass_iter)) {
      nextFreePredRegister = *pregclass_iter;
      break;
    }
  }

  return nextFreePredRegister;

}

/**
 * @brief Removes all predclear instructions before any predicated
 *        block begin instruction from the given MBB.
 * @param MBB The machine basic block to analyze.
 * @return A List of all removed predicated registers.
 */
PredicatedRegPass::RegList PredicatedRegPass::removePredRegClears(MachineBasicBlock &MBB) {

  RegList rList;

  MachineBasicBlock::iterator mbb_iter = MBB.begin();
  while (mbb_iter != MBB.end()) {
    if (mbb_iter->getOpcode() == CBG::PREDREGCLEAR) {
      rList.push_back(mbb_iter->getOperand(0).getReg());
      mbb_iter->eraseFromParent();
      mbb_iter = MBB.begin();
      continue;
    }
    if (mbb_iter->getOpcode() == CBG::PREDBLOCKREG_BEGIN_T ||
        mbb_iter->getOpcode() == CBG::PREDBLOCKREG_BEGIN_F) {
      break;
    }
    ++mbb_iter;
  }

  return rList;

}

/**
 * @brief Removes all redundant instructions terminating a predicated block
 *        from the given MBB.
 * @param MBB The machine basic block to analyze.
 */
void PredicatedBlocksRegPass::removePredEnds(MachineBasicBlock &MBB) {
  MachineBasicBlock::iterator mbb_iter;
  MachineBasicBlock::iterator pred_end = MBB.end();

  for (mbb_iter = MBB.begin(); mbb_iter != MBB.end(); ) {
    // we have found a prendend instruction
    if (mbb_iter->getOpcode() == CBG::PREDBLOCKREG_END) {
      // if it is the first, save that we have found one
      // and continue
      if (pred_end == MBB.end()) {
        pred_end = mbb_iter;
        ++mbb_iter;
        continue;
      } else {
        // we have found a second predend instruction,
        // so we have to remove it
        pred_end->eraseFromParent();
        pred_end = MBB.end();
        mbb_iter = MBB.begin();
        continue;
      }
    } else if (mbb_iter->getOpcode() == CBG::PREDBLOCKREG_BEGIN_T ||
               mbb_iter->getOpcode() == CBG::PREDBLOCKREG_BEGIN_F) {
      // we have found the beginning of a predicated block, so
      // we should remove any predecessing predend instructions
      if (pred_end != MBB.end()) {
        // we have found a predend instruction predecessing
        // a predbegin instruction which means that we can
        // remove it
        pred_end->eraseFromParent();
        pred_end = MBB.end();
        mbb_iter = MBB.begin();
        continue;
      }

    }
    // if pred_end has been defined and we did not have any
    // predend or predbegin block, we have to clear the pred_end
    // instruction
    pred_end = MBB.end();
    ++mbb_iter;
  }


}

/**
 * @brief Tries to insert a predicated block by merging the predecessor of
 *        TBB, FBB and their successor, depending on the branch type.
 * @param TBB   Represents the "if-then" branch.
 * @param FBB   Represents the "else" branch in the case of an IFELSEBRANCH or
 *              the machine basic block logically following TBB in the case of
 *              an IFBRANCH.
 * @param btype Result of the getBranchType() method and may be either IFELSEBRANCH
 *              or IFBRANCH.
 * @see getBranchType()
 * @return True if the predicated block could be inserted successfully, false if the
 *         basic blocks have not been touched.
 */
bool PredicatedBlocksCCPass::insertPredicatedBlock(MachineBasicBlock &TBB,
                                                   MachineBasicBlock &FBB,
                                                   branchType btype) {

  DebugLoc dbg_loc = TBB.begin()->getDebugLoc();
  MachineBasicBlock* predecessor;
  MachineBasicBlock* successor;
  MachineBasicBlock::iterator mbb_iter;
  int conditionCode;
  bool Changed = false;

  if (btype == IFELSEBRANCH) {

    // in the case of an if-then-else construction,
    // we have to handle the following CFG:
    // Pre -> FBB -> TBB -> [possible other MBBs] -> Post

    // get predecessor (by definition, TBB has only one predecessor)
    predecessor = *(TBB.pred_begin());

    // save the condition code for TBB
    conditionCode = getConditionCode(*predecessor);

    // if we could not find any conditional branch, do nothing
    if (conditionCode >= 0) {

      // TBB has exactly one successor
      successor = *(TBB.succ_begin());

      // if the successor of TBB is also a layout successor
      // we can remove branches from FBB and insert the
      // PREDBLOCKCC_END instruction at the end
      if (TBB.isLayoutSuccessor(successor)) {
        removeBranches(FBB);
        mbb_iter = FBB.end();
      } else {
        // otherwise, we have to insert the PREDBLOCKCC_END instruction
        // before the unconditional branch
        mbb_iter = FBB.end();
        --mbb_iter;
      }

      // remove any branches from TBB and predecessor
      // because they will be merged with FBB which would
      // handle the final branch
      removeBranches(TBB);
      removeBranches(*predecessor);

      // insert predicated block begin at begin of TBB with the given condition code
      BuildMI(TBB, TBB.begin(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKCC_BEGIN))
        .addImm(conditionCode);

      // insert predicated block begin at begin of FBB with the opposite condition code
      conditionCode = CBG::getOppositeBranchCondition(static_cast<CBGCC::CondCodes>(conditionCode));
      BuildMI(FBB, FBB.begin(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKCC_BEGIN))
        .addImm(conditionCode);

      // insert predicated block end at end of FBB, resp. before the final unconditional
      // branch
      BuildMI(FBB, mbb_iter, dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKCC_END));

      // merge predecessor with TBB and FBB
      mergeBlocks(*predecessor, TBB);
      mergeBlocks(*predecessor, FBB);

      // if the successor has only one predecessor,
      // we can merge the two blocks
      if (successor->pred_size() == 1) {
        mergeBlocks(*predecessor, *successor);
      }

      // save that we have changed the structure of the function
      Changed = true;

    }

  } else if (btype == IFBRANCH) {

    // in the case of an if-then-end construction,
    // we have to handle the following CFG:
    // Pre -> TBB -> [possible other MBBs] -> FBB

    // get the only predecessor of TBB
    predecessor = *(TBB.pred_begin());

    // save the condition code for TBB (which is the opposite for TBB)
    conditionCode = getConditionCode(*predecessor);

    // if we could not find any conditional branch, do nothing
    if (conditionCode >= 0) {

      // remove all branches from predecessor
      removeBranches(*predecessor);

      // depending whether FBB is a layout successor of
      // TBB, we have to insert PREBLOCKCC_END at the
      // end of TBB or straight before the unconditional
      // branch
      if (!TBB.isLayoutSuccessor(&FBB)) {
        mbb_iter = TBB.end();
        --mbb_iter;
      } else {
        removeBranches(TBB);
        mbb_iter = TBB.end();
      }

      // get the opposite condition code for TBB
      conditionCode = CBG::getOppositeBranchCondition(static_cast<CBGCC::CondCodes>(conditionCode));

      // start predicated block for TBB
      BuildMI(TBB, TBB.begin(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKCC_BEGIN))
        .addImm(conditionCode);
      // end predicated block
      BuildMI(TBB, mbb_iter, dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKCC_END));

      // merge Pre with TBB
      mergeBlocks(*predecessor, TBB);

      // only merge FBB with rest if it is a layout successor and it only has
      // one predecessor
      if (predecessor->isLayoutSuccessor(&FBB) && FBB.pred_size() == 1) {
        mergeBlocks(*predecessor, FBB);
      }

      // save that we have changed the structure of the function
      Changed = true;
    }

  }

  return Changed;

}

/**
 * @brief Checks for any two MBBs of the given machine function, whether they form an
 *        if-then-else of if-then construction and insert a predicated block if possible.
 * @param F The Machine function to analyze.
 * @return True, if any predicated block has been inserted, false otherwise.
 */
bool PredicatedBlocksCCPass::runOnMachineFunction(MachineFunction &F) {
  bool Changed = false;
  MachineFunction::iterator f_outer_iter;
  MachineFunction::iterator f_inner_iter;
  BranchMBB_list branches;
  BranchMBB_list::iterator branch_iter;
  branchType btype;

  // check for all basic blocks if they form any branches
  for (f_outer_iter = F.begin(); f_outer_iter != F.end(); ++f_outer_iter) {
    for (f_inner_iter = F.begin(); f_inner_iter != F.end(); ++f_inner_iter) {
      btype = getBranchType(*f_inner_iter, *f_outer_iter);
      if (btype != NOBRANCH) {
        BranchMBB newBranch = {&(*f_inner_iter), &(*f_outer_iter), btype};
        branches.push_back(newBranch);
      }
    }
  }

  for (branch_iter = branches.begin(); branch_iter != branches.end(); branch_iter++) {
    Changed |= insertPredicatedBlock(*(branch_iter->TBB), *(branch_iter->FBB), branch_iter->btype);
  }

  return Changed;
}

/**
 * @brief Creates a new predicated block pass based on the condition codes of the
 *        status register.
 * @param tm The target machine for the given pass.
 * @return FunctionPass pointer to the just created pass.
 */
FunctionPass* llvm::createcbgPredBlockCCPass(TargetMachine &tm) {
  return new PredicatedBlocksCCPass(tm);
}

/**
 * @brief Tries to insert a predicated block by merging the predecessor of
 *        TBB, FBB and their successor, depending on the branch type.
 * @details TBB and FBB may already contain predicated blocks.
 * @param TBB   Represents the "if-then" branch.
 * @param FBB   Represents the "else" branch in the case of an IFELSEBRANCH or
 *              the machine basic block logically following TBB in the case of
 *              an IFBRANCH.
 * @param btype Result of the getBranchType() method and may be either IFELSEBRANCH
 *              or IFBRANCH.
 * @see getBranchType()
 * @return True if the predicated block could be inserted successfully, false if the
 *         basic blocks have not been touched.
 */
bool PredicatedBlocksRegPass::insertPredicatedBlock(MachineBasicBlock &TBB, MachineBasicBlock &FBB, branchType btype) {

  DebugLoc dbg_loc = TBB.begin()->getDebugLoc();
  RegList pregList;
  RegList pregList2;
  RegList::iterator preglist_iterator;
  MachineBasicBlock* predecessor;
  MachineBasicBlock* successor;
  MachineBasicBlock::iterator mbb_iter;
  int conditionCode;
  unsigned nextPReg;
  bool Changed = false;

  if (btype == IFELSEBRANCH) {

    // in the case of an if-then-else construction,
    // we have to handle the following CFG:
    // Pre -> FBB -> TBB -> [possible other MBBs] -> Post

    // get predecessor (by definition, TBB has only one predecessor)
    predecessor = *(TBB.pred_begin());

    // save the condition code for TBB
    conditionCode = getConditionCode(*predecessor);

    // if we could not find any conditional branch, do nothing
    if (conditionCode >= 0) {

      // TBB has exactly one successor
      successor = *(TBB.succ_begin());

      // if the successor of TBB is also a layout successor
      // we can remove branches from FBB and insert the
      // PREDBLOCKCC_END instruction at the end
      if (TBB.isLayoutSuccessor(successor)) {
        removeBranches(FBB);
        mbb_iter = FBB.end();
      } else {
        // otherwise, we have to insert the PREDBLOCKREG_END instruction
        // before the unconditional branch
        mbb_iter = FBB.end();
        --mbb_iter;
      }

      // allocate the next free predication register
      nextPReg = getNextFreePredRegister(TBB, FBB);

      // remove any branches from TBB and predecessor
      removeBranches(TBB);
      removeBranches(*predecessor);

      // remove any clear predication register from TBB and FBB
      // and save the used registers in list
      pregList = removePredRegClears(TBB);
      pregList2 = removePredRegClears(FBB);

      // clear the current predicate register
      BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGCLEAR))
        .addReg(nextPReg, RegState::Define);

      // add clear instructions for all used predicate registers of TBB and FBB at end of predecessor
      for (preglist_iterator = pregList.begin(); preglist_iterator != pregList.end(); ++preglist_iterator) {
        BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGCLEAR))
          .addReg(*preglist_iterator, RegState::Define);
      }

      // set the current predication register based on the condition code
      BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGSETCC))
        .addReg(nextPReg, RegState::Define).addImm(conditionCode);

      // add begin predicated block instruction at begin of TBB
      BuildMI(TBB, TBB.begin(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKREG_BEGIN_T))
        .addReg(nextPReg);

      // if the list of used pred registers of FBB contains any entries, we have to insert a block end instruction
      if (pregList2.size() > 0) {
        BuildMI(TBB, TBB.end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKREG_END));
      }

      // add clear instructions for all used predicate registers of FBB at end of true basic block
      for (preglist_iterator = pregList2.begin(); preglist_iterator != pregList2.end(); ++preglist_iterator) {
        BuildMI(TBB, TBB.end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGCLEAR))
          .addReg(*preglist_iterator, RegState::Define);
      }

      // add begin predicated block instruction at begin of FBB
      BuildMI(FBB, FBB.begin(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKREG_BEGIN_F))
        .addReg(nextPReg);

      // add end predicated block instruction at end of FBB, resp. before the unconditional branch of FBB
      BuildMI(FBB, mbb_iter, dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKREG_END));

      // merge predecessor with TBB and FBB
      mergeBlocks(*predecessor, TBB);
      mergeBlocks(*predecessor, FBB);

      // if the successor has only one predecessor,
      // we can merge the two blocks
      if (successor->pred_size() == 1) {
        mergeBlocks(*predecessor, *successor);
      }

      // remove redundant predends from newly built MBB
      removePredEnds(*predecessor);

      // save that we have changed the structure of the function
      Changed = true;

    }

  } else if (btype == IFBRANCH) {

    // in the case of an if-then-end construction,
    // we have to handle the following CFG:
    // Pre -> TBB -> [possible other MBBs] -> FBB

    // get the only predecessor of TBB
    predecessor = *(TBB.pred_begin());

    // save the condition code for TBB (which is the opposite for TBB)
    conditionCode = getConditionCode(*predecessor);

    // if we could not find any conditional branch, do nothing
    if (conditionCode >= 0) {

      // remove all branches from predecessor
      removeBranches(*predecessor);

      // depending whether FBB is a layout successor of
      // TBB, we have to insert PREBLOCKREG_END at the
      // end of TBB or straight before the unconditional
      // branch
      if (!TBB.isLayoutSuccessor(&FBB)) {
        mbb_iter = TBB.end();
        --mbb_iter;
      } else {
        removeBranches(TBB);
        mbb_iter = TBB.end();
      }

      // allocate the next free predication register
      nextPReg = getNextFreePredRegister(TBB);

      // remove any clear predication register from TBB
      // and get a set of all used predication registers of TBB
      pregList = removePredRegClears(TBB);

      // clear the current predicate register
      BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGCLEAR))
        .addReg(nextPReg, RegState::Define);

      // add clear instructions for all used predicate registers of TBB at end of predecessor
      for (preglist_iterator = pregList.begin(); preglist_iterator != pregList.end(); ++preglist_iterator) {
        BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGCLEAR))
          .addReg(*preglist_iterator, RegState::Define);
      }

      // get the opposite condition code for TBB
      conditionCode = CBG::getOppositeBranchCondition(static_cast<CBGCC::CondCodes>(conditionCode));

      // set the current predication register based on the condition code
      BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGSETCC))
        .addReg(nextPReg, RegState::Define).addImm(conditionCode);

      // add start predicated block instruction at begin of TBB
      BuildMI(TBB, TBB.begin(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKREG_BEGIN_T))
        .addReg(nextPReg);

      // add end predicated block instruction at end of TBB
      BuildMI(TBB, mbb_iter, dbg_loc, TM.getInstrInfo()->get(CBG::PREDBLOCKREG_END));

      // merge predecessor with TBB
      mergeBlocks(*predecessor, TBB);

      // only merge FBB with rest if it is a layout successor and it only has
      // one predecessor
      if (predecessor->isLayoutSuccessor(&FBB) && FBB.pred_size() == 1) {
        mergeBlocks(*predecessor, FBB);
      }

      // remove redundant end predicted block instruction from newly built MBB
      removePredEnds(*predecessor);

      // save that we have changed the structure of the function
      Changed = true;

    }

  }

  return Changed;

}

/**
 * @brief Checks for any two MBBs of the given machine function, whether they form an
 *        if-then-else of if-then construction and insert a predicated block if possible.
 * @param F The Machine function to analyze.
 * @return True, if any predicated block has been inserted, false otherwise.
 */
bool PredicatedBlocksRegPass::runOnMachineFunction(MachineFunction &F) {
  bool Changed = false;
  bool RemovedBranches = true;
  MachineFunction::iterator f_outer_iter;
  MachineFunction::iterator f_inner_iter;
  BranchMBB_list branches;
  BranchMBB_list::iterator branch_iter;
  branchType btype;
//  unsigned passCounter = 1;

  // as long as we can find any branches...
  while (RemovedBranches) {
//    std::cerr << "Pass Number: " << passCounter++ << std::endl;

    // set the condition false such that the loop is terminated
    // in the case we cannot find or remove any more branches
    RemovedBranches = false;
    // check for all basic blocks if the form any branches
    for (f_outer_iter = F.begin(); f_outer_iter != F.end(); ++f_outer_iter) {
      for (f_inner_iter = F.begin(); f_inner_iter != F.end(); ++f_inner_iter) {
        btype = getBranchType(*f_inner_iter, *f_outer_iter);
        if (btype != NOBRANCH) {
          BranchMBB newBranch = {&(*f_inner_iter), &(*f_outer_iter), btype};
          branches.push_back(newBranch);
        }
      }
    }

    for (branch_iter = branches.begin(); branch_iter != branches.end(); branch_iter++) {
      if (insertPredicatedBlock(*(branch_iter->TBB), *(branch_iter->FBB), branch_iter->btype)) {
        Changed = true;
        RemovedBranches = true;
      }
    }

    // we have to clear all possible branches for the next pass...
    branches.clear();
  }

  return Changed;
}

/**
 * @brief Creates a new predicated block pass based on predication registers.
 * @param tm The target machine for the given pass.
 * @return FunctionPass pointer to the just created pass.
 */
FunctionPass* llvm::createcbgPredBlockRegPass(TargetMachine &tm) {
  return new PredicatedBlocksRegPass(tm);
}

/**
 * @brief Merges the two given basic blocks and replaces all instructions
 *        of MBB with predicated versions.
 * @param Pre           The predecessor of MBB which will finally contain the instructions
 *                      of Pre and MBB.
 * @param MBB           The machine basic block which will be appended to Pre. Moreover,
 *                      all its instructions are replaced with predicated versions.
 * @param conditionCode Indicates on which condition code the predicated instruction
 *                      will be executed.
 */
void PredicatedInstrCCPass::changeToPredicatedInstruction(MachineBasicBlock &Pre,
                                                          MachineBasicBlock &MBB,
                                                          unsigned conditionCode) {

  MachineFunction* F = MBB.getParent();
  MachineBasicBlock::iterator mbb_iter;
  MachineBasicBlock::succ_iterator succ_iter;
  DebugLoc dbg_loc = Pre.begin()->getDebugLoc();
  unsigned OpCode;
  unsigned numOps;
  bool newPredicatedInstr;
  MachineInstrBuilder mi;

  for (mbb_iter = MBB.begin(); mbb_iter != MBB.end(); ++mbb_iter) {
    newPredicatedInstr = true;
    OpCode = mbb_iter->getOpcode();
    numOps = mbb_iter->getNumOperands();

    switch (OpCode) {
      // ignore unknown Opcodes...
      default:
//        std::cerr << "Warning: cannot predicate the following instruction:"
//          << std::endl;
//        mbb_iter->dump();
        newPredicatedInstr = false;
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(OpCode));
        break;
      // convert RET into predicated RET instructions
      case CBG::RET:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RET_pcc));
        break;
      case CBG::RETL:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RETL_pcc));
        break;
      // convert integer LOADs into predicated integer LOADs
      case CBG::LDSBri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDSB_pccri));
        break;
      case CBG::LDSBrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDSB_pccrr));
        break;
      case CBG::LDSHri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDSH_pccri));
        break;
      case CBG::LDSHrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDSH_pccrr));
        break;
      case CBG::LDUBri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDUB_pccri));
        break;
      case CBG::LDUBrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDUB_pccrr));
        break;
      case CBG::LDUHri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDUH_pccri));
        break;
      case CBG::LDUHrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDUH_pccrr));
        break;
      case CBG::LDri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LD_pccri));
        break;
      case CBG::LDrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LD_pccrr));
        break;
      // convert integer STOREs into predicated integer STOREs
      case CBG::STBri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::STB_pccri));
        break;
      case CBG::STBrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::STB_pccrr));
        break;
      case CBG::STHri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::STH_pccri));
        break;
      case CBG::STHrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::STH_pccrr));
        break;
      case CBG::STri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ST_pccri));
        break;
      case CBG::STrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ST_pccrr));
        break;
      // convert SETHI into predicated instruction
      case CBG::SETHIi:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SETHIi_pcc));
      // convert logical instructions into predicated instructions
      case CBG::ANDri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::AND_pccri));
        break;
      case CBG::ANDrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::AND_pccrr));
        break;
      case CBG::ANDNri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ANDN_pccri));
        break;
      case CBG::ANDNrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ANDN_pccrr));
        break;
      case CBG::ORri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::OR_pccri));
        break;
      case CBG::ORrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::OR_pccrr));
        break;
      case CBG::ORNri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ORN_pccri));
        break;
      case CBG::ORNrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ORN_pccrr));
        break;
      case CBG::XORri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::XOR_pccri));
        break;
      case CBG::XORrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::XOR_pccrr));
        break;
      case CBG::XNORri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::XNOR_pccri));
        break;
      case CBG::XNORrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::XNOR_pccrr));
        break;
      // convert SHIFT instructions into predicated instructions
      case CBG::SLLri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SLL_pccri));
        break;
      case CBG::SLLrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SLL_pccrr));
        break;
      case CBG::SRLri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SRL_pccri));
        break;
      case CBG::SRLrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SRL_pccrr));
        break;
      case CBG::SRAri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SRA_pccri));
        break;
      case CBG::SRArr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SRA_pccrr));
        break;
      // convert ADD instructions into predicated instructions
      case CBG::ADDri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ADD_pccri));
        break;
      case CBG::ADDrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ADD_pccrr));
        break;
      case CBG::ADDXri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ADDX_pccri));
        break;
      case CBG::ADDXrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ADDX_pccrr));
        break;
      // convert LEA instructions into predicated instructions
      case CBG::LEA_ADDri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LEA_ADD_pccri));
        break;
      // convert SUB instructions into predicated instructions
      case CBG::SUBri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUB_pccri));
        break;
      case CBG::SUBrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUB_pccrr));
        break;
      case CBG::SUBXri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUBX_pccri));
        break;
      case CBG::SUBXrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUBX_pccrr));
        break;
      // convert MUL instructions into predicated instructions
      case CBG::UMULri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::UMUL_pccri));
        break;
      case CBG::UMULrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::UMUL_pccrr));
        break;
      case CBG::SMULri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SMUL_pccri));
        break;
      case CBG::SMULrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SMUL_pccrr));
        break;
      // convert DIV instructions into predicated instructions
      case CBG::UDIVri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::UDIV_pccri));
        break;
      case CBG::UDIVrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::UDIV_pccrr));
        break;
      case CBG::SDIVri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SDIV_pccri));
        break;
      case CBG::SDIVrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SDIV_pccrr));
        break;
      // convert save and restore instructions into predicated instructions
      case CBG::SAVEri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SAVE_pccri));
        break;
      case CBG::SAVErr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SAVE_pccrr));
        break;
      case CBG::RESTOREri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RESTORE_pccri));
        break;
      case CBG::RESTORErr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RESTORE_pccrr));
        break;
      // convert RDY instruction into predicated instruction
      case CBG::RDY:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RDY_pcc));
        break;
    }

    // add all operands
    for (unsigned idx = 0; idx < numOps; ++idx) {
      mi.addOperand(mbb_iter->getOperand(idx));
    }

    // add condition code to new inserted instructions
    if (newPredicatedInstr) {
      mi.addImm(conditionCode);
    }

  }

  // removing all (old) successors from pre
  succ_iter = Pre.succ_begin();
  while (succ_iter != Pre.succ_end()) {
    Pre.removeSuccessor(*succ_iter);
    succ_iter = Pre.succ_begin();
  }

  Pre.transferSuccessors(&MBB);

  // removing MBB
  MBB.eraseFromParent();

  F->RenumberBlocks(&Pre);

}


/**
 * @brief Tries to insert a block with fully predicated instructions by merging
 *        the predecessor of TBB, FBB and their successor, depending on the
 *        branch type.
 * @param TBB   Represents the "if-then" branch.
 * @param FBB   Represents the "else" branch in the case of an IFELSEBRANCH or
 *              the machine basic block logically following TBB in the case of
 *              an IFBRANCH.
 * @param btype Result of the getBranchType() method and may be either IFELSEBRANCH
 *              or IFBRANCH.
 * @see getBranchType()
 * @return True if the predicated block could be inserted successfully, false if the
 *         basic blocks have not been touched.
 */
bool PredicatedInstrCCPass::insertPredicatedBlock(MachineBasicBlock &TBB, MachineBasicBlock &FBB, branchType btype) {

  MachineBasicBlock* predecessor;
  MachineBasicBlock* successor;
  int conditionCode;
  bool Changed = false;

  if (btype == IFELSEBRANCH) {

    // in the case of an if-then-else construction,
    // we have to handle the following CFG:
    // Pre -> FBB -> TBB -> [possible other MBBs] -> Post

    // get predecessor (by definition, TBB has only one predecessor)
    predecessor = *(TBB.pred_begin());

    // save condition code for TBB
    conditionCode = getConditionCode(*predecessor);

    // if we could not find any conditional branch, do nothing
    if (conditionCode >= 0) {

      // TBB has exactly one successor
      successor = *(TBB.succ_begin());

      // if the successor of TBB is also a layout successor
      // we can remove branches from FBB
      if (TBB.isLayoutSuccessor(successor)) {
        removeBranches(FBB);
      }

      // remove any branches from TBB and predecessor
      removeBranches(TBB);
      removeBranches(*predecessor);

      // let all instructions of TBB be predicated, based on the
      // given condition code
      changeToPredicatedInstruction(*predecessor, TBB, conditionCode);

      // get the opposite condition code for FBB
      conditionCode = CBG::getOppositeBranchCondition(static_cast<CBGCC::CondCodes>(conditionCode));
      // let all instructions of FBB be predicated, based on the
      // given condition code
      changeToPredicatedInstruction(*predecessor, FBB, conditionCode);

      // if the successor has only one predecessor,
      // we can merge the two blocks
      if (successor->pred_size() == 1) {
        mergeBlocks(*predecessor, *successor);
      }

      // save that we have changed the structure of the function
      Changed = true;

    }

  } else if (btype == IFBRANCH) {

    // in the case of an if-then-end construction,
    // we have to handle the following CFG:
    // Pre -> TBB -> [possible other MBBs] -> FBB

    // get the only predecessor of TBB
    predecessor = *(TBB.pred_begin());

    // save the condition code for TBB (which is the opposite for TBB)
    conditionCode = getConditionCode(*predecessor);

    // if we could not find any conditional branch, do nothing
    if (conditionCode >= 0) {

      // remove any branches from predecessor
      removeBranches(*predecessor);

      // remove branches from TBB if FBB is layout successor
      if (TBB.isLayoutSuccessor(&FBB)) {
        removeBranches(TBB);
      }

      // get the opposite condition code for TBB
      conditionCode = CBG::getOppositeBranchCondition(static_cast<CBGCC::CondCodes>(conditionCode));

      // let TBB be predicated based on the given condition code
      changeToPredicatedInstruction(*predecessor, TBB, conditionCode);

      // only merge FBB with rest if it is a layout successor and it only has
      // one predecessor
      if (predecessor->isLayoutSuccessor(&FBB) && FBB.pred_size() == 1) {
        mergeBlocks(*predecessor, FBB);
      }

      // save that we have changed the structure of the function
      Changed = true;

    }

  }

  return Changed;

}

/**
 * @brief Creates a new predicated instruction pass based on the condition code
 *        of the status register.
 * @param tm The target machine for the given pass.
 * @return FunctionPass pointer to the just created pass.
 */
FunctionPass* llvm::createcbgPredInstrCCPass(TargetMachine &tm) {
  return new PredicatedInstrCCPass(tm);
}

/**
 * @brief Merges the two given basic blocks and replaces all instructions
 *        of MBB with predicated versions.
 * @param Pre  The predecessor of MBB which will finally contain the instructions
 *             of Pre and MBB.
 * @param MBB  The machine basic block which will be appended to Pre. Moreover,
 *             all its instructions are replaced with predicated versions.
 * @param Preg The predication register on which the predication is based.
 * @param TF   Indicates whether the instruction shall be executed if the
 *             predication register is set (true) or cleared (false).
 */
void PredicatedInstrRegPass::changeToPredicatedInstruction(MachineBasicBlock &Pre,
                                                          MachineBasicBlock &MBB,
                                                          unsigned Preg,
                                                          unsigned TF) {

  MachineFunction* F = MBB.getParent();
  MachineBasicBlock::iterator mbb_iter;
  MachineBasicBlock::succ_iterator succ_iter;
  DebugLoc dbg_loc = Pre.begin()->getDebugLoc();
  unsigned OpCode;
  unsigned numOps;
  bool newPredicatedInstr;
  MachineInstrBuilder mi;

  for (mbb_iter = MBB.begin(); mbb_iter != MBB.end(); ++mbb_iter) {
    newPredicatedInstr = true;
    OpCode = mbb_iter->getOpcode();
    numOps = mbb_iter->getNumOperands();

    switch (OpCode) {
      // ignore unknown Opcodes...
      default:
//        std::cerr << "Warning: cannot predicate the following instruction:"
//          << std::endl;
//        mbb_iter->dump();
        newPredicatedInstr = false;
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(OpCode));
        break;
      // convert RET into predicated RET instructions
      case CBG::RET:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RET_preg));
        break;
      case CBG::RETL:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RETL_preg));
        break;
      // convert integer LOADs into predicated integer LOADs
      case CBG::LDSBri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDSB_pregri));
        break;
      case CBG::LDSBrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDSB_pregrr));
        break;
      case CBG::LDSHri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDSH_pregri));
        break;
      case CBG::LDSHrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDSH_pregrr));
        break;
      case CBG::LDUBri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDUB_pregri));
        break;
      case CBG::LDUBrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDUB_pregrr));
        break;
      case CBG::LDUHri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDUH_pregri));
        break;
      case CBG::LDUHrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LDUH_pregrr));
        break;
      case CBG::LDri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LD_pregri));
        break;
      case CBG::LDrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LD_pregrr));
        break;
      // convert integer STOREs into predicated integer STOREs
      case CBG::STBri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::STB_pregri));
        break;
      case CBG::STBrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::STB_pregrr));
        break;
      case CBG::STHri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::STH_pregri));
        break;
      case CBG::STHrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::STH_pregrr));
        break;
      case CBG::STri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ST_pregri));
        break;
      case CBG::STrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ST_pregrr));
        break;
      // convert SETHI into predicated instruction
      case CBG::SETHIi:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SETHIi_preg));
      // convert logical instructions into predicated instructions
      case CBG::ANDri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::AND_pregri));
        break;
      case CBG::ANDrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::AND_pregrr));
        break;
      case CBG::ANDNri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ANDN_pregri));
        break;
      case CBG::ANDNrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ANDN_pregrr));
        break;
      case CBG::ORri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::OR_pregri));
        break;
      case CBG::ORrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::OR_pregrr));
        break;
      case CBG::ORNri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ORN_pregri));
        break;
      case CBG::ORNrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ORN_pregrr));
        break;
      case CBG::XORri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::XOR_pregri));
        break;
      case CBG::XORrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::XOR_pregrr));
        break;
      case CBG::XNORri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::XNOR_pregri));
        break;
      case CBG::XNORrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::XNOR_pregrr));
        break;
      // convert SHIFT instructions into predicated instructions
      case CBG::SLLri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SLL_pregri));
        break;
      case CBG::SLLrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SLL_pregrr));
        break;
      case CBG::SRLri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SRL_pregri));
        break;
      case CBG::SRLrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SRL_pregrr));
        break;
      case CBG::SRAri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SRA_pregri));
        break;
      case CBG::SRArr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SRA_pregrr));
        break;
      // convert ADD instructions into predicated instructions
      case CBG::ADDri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ADD_pregri));
        break;
      case CBG::ADDrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ADD_pregrr));
        break;
      case CBG::ADDXri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ADDX_pregri));
        break;
      case CBG::ADDXrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::ADDX_pregrr));
        break;
      // convert LEA instructions into predicated instructions
      case CBG::LEA_ADDri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::LEA_ADD_pregri));
        break;
      // convert SUB instructions into predicated instructions
      case CBG::SUBri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUB_pregri));
        break;
      case CBG::SUBrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUB_pregrr));
        break;
      case CBG::SUBXri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUBX_pregri));
        break;
      case CBG::SUBXrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUBX_pregrr));
        break;
      case CBG::SUBCCri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUBCC_pregri));
        break;
      case CBG::SUBCCrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SUBCC_pregrr));
        break;
      // convert MUL instructions into predicated instructions
      case CBG::UMULri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::UMUL_pregri));
        break;
      case CBG::UMULrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::UMUL_pregrr));
        break;
      case CBG::SMULri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SMUL_pregri));
        break;
      case CBG::SMULrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SMUL_pregrr));
        break;
      // convert DIV instructions into predicated instructions
      case CBG::UDIVri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::UDIV_pregri));
        break;
      case CBG::UDIVrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::UDIV_pregrr));
        break;
      case CBG::SDIVri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SDIV_pregri));
        break;
      case CBG::SDIVrr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SDIV_pregrr));
        break;
      // convert save and restore instructions into predicated instructions
      case CBG::SAVEri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SAVE_pregri));
        break;
      case CBG::SAVErr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::SAVE_pregrr));
        break;
      case CBG::RESTOREri:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RESTORE_pregri));
        break;
      case CBG::RESTORErr:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RESTORE_pregrr));
        break;
      // convert RDY instruction into predicated instruction
      case CBG::RDY:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::RDY_preg));
        break;
      case CBG::PREDREGSETCC:
        mi = BuildMI(Pre, Pre.end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGSETCC_PREG));
        break;
    }

    // add all operands
    for (unsigned idx = 0; idx < numOps; ++idx) {
      mi.addOperand(mbb_iter->getOperand(idx));
    }

    // add condition code to new inserted instructions
    if (newPredicatedInstr) {
      mi.addReg(Preg).addImm(TF);
    }

  }

  // removing all (old) successors from pre
  succ_iter = Pre.succ_begin();
  while (succ_iter != Pre.succ_end()) {
    Pre.removeSuccessor(*succ_iter);
    succ_iter = Pre.succ_begin();
  }

  Pre.transferSuccessors(&MBB);

  // removing MBB
  MBB.eraseFromParent();

  F->RenumberBlocks(&Pre);

}

/**
 * @brief Tries to insert a block with fully predicated instructions by merging
 *        the predecessor of TBB, FBB and their successor, depending on the
 *        branch type.
 * @details TBB and FBB may already contain predicated blocks.
 * @param TBB   Represents the "if-then" branch.
 * @param FBB   Represents the "else" branch in the case of an IFELSEBRANCH or
 *              the machine basic block logically following TBB in the case of
 *              an IFBRANCH.
 * @param btype Result of the getBranchType() method and may be either IFELSEBRANCH
 *              or IFBRANCH.
 * @see getBranchType()
 * @return True if the predicated block could be inserted successfully, false if the
 *         basic blocks have not been touched.
 */
bool PredicatedInstrRegPass::insertPredicatedBlock(MachineBasicBlock &TBB, MachineBasicBlock &FBB, branchType btype) {

  DebugLoc dbg_loc = TBB.begin()->getDebugLoc();
  MachineBasicBlock* predecessor;
  MachineBasicBlock* successor;
  int conditionCode;
  unsigned nextPReg;
  bool Changed = false;

  if (btype == IFELSEBRANCH) {

    // in the case of an if-then-else construction,
    // we have to handle the following CFG:
    // Pre -> FBB -> TBB -> [possible other MBBs] -> Post

    // get predecessor (by definition, TBB has only one predecessor)
    predecessor = *(TBB.pred_begin());

    // save condition code for TBB
    conditionCode = getConditionCode(*predecessor);

    // if we could not find any conditional branch, do nothing
    if (conditionCode >= 0) {

      // TBB has exactly one successor
      successor = *(TBB.succ_begin());

      // if the successor of TBB is also a layout successor
      // we can remove branches from FBB
      if (TBB.isLayoutSuccessor(successor)) {
        removeBranches(FBB);
      }

      // allocate the next free predication register
      nextPReg = getNextFreePredRegister(TBB, FBB);

      // remove any branches from TBB and predecessor
      removeBranches(TBB);
      removeBranches(*predecessor);

      // clear currently used predicate register
      BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGCLEAR))
        .addReg(nextPReg, RegState::Define);

      // set the current predication register based on the condition code
      BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGSETCC))
        .addReg(nextPReg, RegState::Define).addImm(conditionCode);

      // merge predecessor with TBB
      changeToPredicatedInstruction(*predecessor, TBB, nextPReg, 1);

      // merge predecessor with FBB
      changeToPredicatedInstruction(*predecessor, FBB, nextPReg, 0);

      // if the successor has only one predecessor,
      // we can merge the two blocks
      if (successor->pred_size() == 1) {
        mergeBlocks(*predecessor, *successor);
      }

      // save that we have changed the structure of the function
      Changed = true;

    }

  } else if (btype == IFBRANCH) {

    // in the case of an if-then-end construction,
    // we have to handle the following CFG:
    // Pre -> TBB -> [possible other MBBs] -> FBB

    // get the only predecessor of TBB
    predecessor = *(TBB.pred_begin());

    // save the condition code for TBB (which is the opposite for TBB)
    conditionCode = getConditionCode(*predecessor);

    // if we could not find any conditional branch, do nothing
    if (conditionCode >= 0) {

      // remove all branches from predecessor
      removeBranches(*predecessor);

      // remove branches from TBB if FBB is layout successor
      if (TBB.isLayoutSuccessor(&FBB)) {
        removeBranches(TBB);
      }

      // allocate the next free predication register
      nextPReg = getNextFreePredRegister(TBB);

      // get the opposite condition code for TBB
      conditionCode = CBG::getOppositeBranchCondition(static_cast<CBGCC::CondCodes>(conditionCode));

      // clear currently used predicate register
      BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGCLEAR))
        .addReg(nextPReg, RegState::Define);

      // set the current predication register based on the condition code
      BuildMI(*predecessor, predecessor->end(), dbg_loc, TM.getInstrInfo()->get(CBG::PREDREGSETCC))
        .addReg(nextPReg, RegState::Define).addImm(conditionCode);

      // merge predecessor with TBB
      changeToPredicatedInstruction(*predecessor, TBB, nextPReg, 1);

      // only merge FBB with rest if it is a layout successor and it only has
      // one predecessor
      if (predecessor->isLayoutSuccessor(&FBB) && FBB.pred_size() == 1) {
        mergeBlocks(*predecessor, FBB);
      }

      // save that we have changed the structure of the function
      Changed = true;

    }

  }

  return Changed;

}

/**
 * @brief Checks for any two MBBs of the given machine function, whether they form an
 *        if-then-else of if-then construction and insert a predicated block if possible.
 * @param F The Machine function to analyze.
 * @return True, if any predicated block has been inserted, false otherwise.
 */
bool PredicatedInstrRegPass::runOnMachineFunction(MachineFunction &F) {
  bool Changed = false;
  bool RemovedBranches = true;
  MachineFunction::iterator f_outer_iter;
  MachineFunction::iterator f_inner_iter;
  BranchMBB_list branches;
  BranchMBB_list::iterator branch_iter;
  branchType btype;
//  unsigned passCounter = 1;

  // as long as we can find any branches...
  while (RemovedBranches) {
//    std::cerr << "Pass Number: " << passCounter++ << std::endl;
    // set the condition false such that the loop is terminated
    // in the case we cannot find or remove any more branches
    RemovedBranches = false;
    // check for all basic blocks if the form any branches
    for (f_outer_iter = F.begin(); f_outer_iter != F.end(); ++f_outer_iter) {
      for (f_inner_iter = F.begin(); f_inner_iter != F.end(); ++f_inner_iter) {
        btype = getBranchType(*f_inner_iter, *f_outer_iter);
        if (btype != NOBRANCH) {
          BranchMBB newBranch = {&(*f_inner_iter), &(*f_outer_iter), btype};
          branches.push_back(newBranch);
        }
      }
    }

    for (branch_iter = branches.begin(); branch_iter != branches.end(); branch_iter++) {
      if (insertPredicatedBlock(*(branch_iter->TBB), *(branch_iter->FBB), branch_iter->btype)) {
        Changed = true;
        RemovedBranches = true;
      }
    }
    // we have to clear all possible branches for the next pass...
    branches.clear();
  }
  return Changed;
}

/**
 * @brief Creates a new predicated instruction pass based on predication
 *        registers.
 * @param tm The target machine for the given pass.
 * @return FunctionPass pointer to the just created pass.
 */
FunctionPass* llvm::createcbgPredInstrRegPass(TargetMachine &tm) {
  return new PredicatedInstrRegPass(tm);
}
