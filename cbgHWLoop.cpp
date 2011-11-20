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
    typedef PredecessorList::MBB_list MBB_list;
    static char ID;

    struct LoopBounds {
      bool isValidLoop;
      bool needsIncrement;
      MachineOperand LoopBound;
      unsigned IndexVar;
    };

  private:
    static bool isIncrement(const MachineInstr &instr);
    static bool isDecrement(const MachineInstr &instr);
    static bool isNoDestination(unsigned regNumber,
                                MachineBasicBlock &MBB,
                                MachineBasicBlock::reverse_iterator &mbb_iter);

    static bool machineBasicBlockInSet(MachineBasicBlock* const &MBB, MBB_list &mbb_set);
    static LoopBounds findLoopBound(MachineBasicBlock &MBB);
    static void updatePredecessors(MachineBasicBlock* newMBB, MachineBasicBlock* MBB);

  protected:
    TargetMachine &TM;
    unsigned LoopDepth;
    unsigned LoopCount;

    virtual MBB_list getPossibleLoops(MachineBasicBlock &MBB) const;
    virtual void insertSingleHWLoop(MachineBasicBlock &MBB, MachineOperand &MO, bool needsIncrement);
    virtual void removeIndexVar(MachineBasicBlock &MBB, unsigned regNumber);

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

/**
 * @brief Returns whether a given MBB is already saved in the given list.
 * @param MBB     Given machine basic block to check.
 * @param mbb_set Given list (set) of machine basic blocks.
 * @return True if machine basic block is already part of the list, false otherwise.
 */
bool HWLoopPass::machineBasicBlockInSet(MachineBasicBlock* const &MBB, MBB_list &mbb_set) {
  MBB_list::iterator mbb_iterator;
  for (mbb_iterator = mbb_set.begin(); mbb_iterator != mbb_set.end(); ++mbb_iterator) {
    if ((*mbb_iterator)->getNumber() == MBB->getNumber()) {
      return true;
    }
  }
  return false;
}

/**
 * @brief Finds out all MBBs which may be involved in the smallest possible loop
 *        starting from MBB.
 * @param MBB
 * @return A list of MBBs which are part of the loop.
 */
HWLoopPass::MBB_list HWLoopPass::getPossibleLoops(MachineBasicBlock &MBB) const {

  MBB_list possible_loops;
  MBB_list all_successors;
  MachineBasicBlock::succ_iterator s_iter;
  MachineBasicBlock::succ_iterator s_end_iter = MBB.succ_end();
  PredecessorList all_predecessors;
  bool foundLoop;

  // return empty vector if BB has no successor
  if (MBB.succ_empty()) {
    return possible_loops;
  }

  // insert current basic block as "root element" into predecessor
  all_predecessors.insertRoot(&MBB);

  // init successor set
  for (s_iter = MBB.succ_begin(); s_iter != s_end_iter; ++s_iter) {
    all_successors.push_back(*s_iter);
    all_predecessors.insertSuccessor(*s_iter, &MBB);
    if (((*s_iter)->getNumber() == MBB.getNumber()) && (!machineBasicBlockInSet(&MBB, possible_loops))) {
      possible_loops.push_back(&MBB);
      // we have found a small basic block loop => ignore bigger ones
      return possible_loops;
    }
  }

  // add all successors to successor set
  for (MBB_list::iterator suc_it = all_successors.begin();
      suc_it != all_successors.end();
      ++suc_it) {

    foundLoop = false;

    s_end_iter = (*suc_it)->succ_end();
    for (s_iter = (*suc_it)->succ_begin(); s_iter != s_end_iter; ++s_iter) {

      // in any case, add predecessor to set
      all_predecessors.insertSuccessor(*s_iter, const_cast<const MachineBasicBlock*&>(*suc_it));
      // if successor of node in successor set is current BB, we have found a loop
      if ((*s_iter)->getNumber() == MBB.getNumber()) {
        possible_loops = all_predecessors.getPredecessors(const_cast<const MachineBasicBlock*&>(*suc_it));
        foundLoop = true;
        break;
      } else {
        // otherwise we have to add it to the successor set
        if (!machineBasicBlockInSet(*s_iter, all_successors)) {
          all_successors.push_back(*s_iter);
        }
      }

    }

    // we have found a loop, so exit current for loop
    if (foundLoop) {
      break;
    }

  }

  return possible_loops;

}

/**
 * @brief Changes the successor value of the predecessors of the old machine basic block,
 *        including jump labels.
 * @param newMBB Inserted machine basic block, representing the hwloop initialization.
 * @param MBB    Old machine basic block, representing the simple loop.
 */
void HWLoopPass::updatePredecessors(MachineBasicBlock* newMBB, MachineBasicBlock* MBB) {

  std::vector<MachineBasicBlock*> predecessors;
  MachineBasicBlock::pred_iterator pred_iter;
  for (pred_iter = MBB->pred_begin();
       pred_iter != MBB->pred_end();
       ++pred_iter) {
    // add all predecessors except own basic block to set
    if (*pred_iter != MBB) {
      predecessors.push_back(*pred_iter);
    }
  }

  for (pred_iter = predecessors.begin();
       pred_iter != predecessors.end();
       ++pred_iter) {
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
  }

}

/**
 * @brief Inserts a hwloop instruction before the given machine basic block.
 * @param MBB            The machine basic block representing the simple loop before which
 *                       the hwloop instructions have to be inserted.
 * @param MO             The machine operand respresenting the loop bound. May either be an
 *                       immediate or a register value.
 * @param needsIncrement This boolean indicates, whether the given loop bound register in MO
 *                       has to be incremented before initialization. (Compare while
 *                       (index < loopbound) vs. (index <= loopbound)).
 */
void HWLoopPass::insertSingleHWLoop(MachineBasicBlock &MBB, MachineOperand &MO, bool needsIncrement) {

  // get current function
  MachineFunction *F = MBB.getParent();
  const BasicBlock *LLVM_BB = MBB.getBasicBlock();

  // create new machine basic block in current function
  MachineBasicBlock* newMBB = F->CreateMachineBasicBlock(LLVM_BB);
  MachineFunction::iterator func_it = F->begin();
  MachineInstrBuilder mi;
  DebugLoc dbg_loc = newMBB->begin()->getDebugLoc();

  // search current basic block
  while ((*func_it).getNumber() != MBB.getNumber()) {
    ++func_it;
  }
  ++func_it;

  // insert new basic block in current position
  F->insert(func_it, newMBB);
  newMBB->moveBefore(&MBB);

  // initialize all howloop registers:
  mi = BuildMI(*newMBB, newMBB->end(), dbg_loc, TM.getInstrInfo()->get(CBG::HWLOOPinit));
  mi.addReg(CBG::HWLOOP1, RegState::Define).addMBB(&MBB);
  mi = BuildMI(*newMBB, newMBB->end(), dbg_loc, TM.getInstrInfo()->get(CBG::HWLOOPinit));
  mi.addReg(CBG::HWLOOP2, RegState::Define).addMBB(MBB.getNextNode());
  // if the needsIncrement boolean is set, we have to increment
  // the register value before using it
  if (needsIncrement) {
    mi = BuildMI(*newMBB, newMBB->end(), dbg_loc, TM.getInstrInfo()->get(CBG::ADDri));
    mi.addReg(MO.getReg(), RegState::Define).addReg(MO.getReg(), RegState::Kill).addImm(1);
  }
  mi = BuildMI(*newMBB, newMBB->end(), dbg_loc, TM.getInstrInfo()->get(CBG::HWLOOPinit));
  mi.addReg(CBG::HWLOOP3, RegState::Define).addOperand(MO);

  // insert hardware loop instruction at end of new block
  mi = BuildMI(*newMBB, newMBB->end(), dbg_loc, TM.getInstrInfo()->get(CBG::HWLOOP));

  // update all predecessors of old block
  updatePredecessors(newMBB, &MBB);
  newMBB->addSuccessor(&MBB);

  F->RenumberBlocks(newMBB);

}

/**
 * @brief Checks, whether the current instruction simply increments
 *        a register by adding "1".
 * @param instr Machine instruction to check.
 * @return True, if the register is incrementend, false otherwise.
 */
bool HWLoopPass::isIncrement(const MachineInstr &instr) {

  int opCode = instr.getOpcode();
  int incrementValue;
  bool isIncrement = false;

  // we only allow ADDri and SUBri to be valid
  // incremental instructions
  if (opCode == CBG::ADDri) {
    // if we have an ADDri instruction, the increment
    // value has to be +1
    incrementValue = instr.getOperand(2).getImm();
    if (incrementValue == 1) {
      isIncrement = true;
    }
  } else if (opCode == CBG::SUBri) {
    // if we have a SUBri instruction, the increment
    // value has to be -1
    incrementValue = instr.getOperand(2).getImm();
    if (incrementValue == -1) {
     isIncrement = true;
    }
  }
  // else => we have no increment instruction

  return isIncrement;

}

/**
 * @brief Checks, whether the current instruction simply decrements
 *        a register by subtracting "1".
 * @param instr Machine instruction to check.
 * @return True, if the register is decrementend, false otherwise.
 */
bool HWLoopPass::isDecrement(const MachineInstr &instr) {

  int opCode = instr.getOpcode();
  int decrementValue;
  bool isDecrement = false;

  // we only allow ADDri and SUBri to be valid
  // incremental instructions
  if (opCode == CBG::ADDri) {
    // if we have an ADDri instruction, the decrement
    // value has to be -1
    decrementValue = instr.getOperand(2).getImm();
    if (decrementValue == -1) {
      isDecrement = true;
    }
  } else if (opCode == CBG::SUBri) {
    // if we have a SUBri instruction, the decrement
    // value has to be 1
    decrementValue = instr.getOperand(2).getImm();
    if (decrementValue == 1) {
     isDecrement = true;
    }
  }
  // else => we have no increment instruction

  return isDecrement;

}

/**
 * @brief Checks, whether the given register number is no destination register of any
 *        previous instruction within the current machine basic block.
 * @param regNumber Register number to check.
 * @param MBB       Current machine basic block.
 * @param mbb_iter  Reverse iterater, indicating the last instruction within the
 *                  current machine basic block where check has to start.
 * @return True if the current register is no destination register in any previous
 *         instruction of the current machine basic block, false otherwise.
 */
bool HWLoopPass::isNoDestination(unsigned regNumber,
                                 MachineBasicBlock &MBB,
                                 MachineBasicBlock::reverse_iterator &mbb_iter) {
  bool isNoDestination = true;

  // check the whole basic block whether the given register
  // is the destination of any instruction
  for (; mbb_iter != MBB.rend(); ++mbb_iter) {
    // only check if the destination is a valid register
    if (mbb_iter->getNumOperands() > 0 && mbb_iter->getOperand(0).isReg()) {
      // if the destination register of current instruction
      // is equal to given regNumber, we have to stop
      if (mbb_iter->getOperand(0).getReg() == regNumber) {
        isNoDestination = false;
        break;
      }
    }
  }

  return isNoDestination;

}

/**
 * @brief Tries to guess the index variable (register) and loop bound (register or
 *        immediate) of the given MBB which has to be a loop.
 * @param MBB Block to check.
 * @return A struct which indicates whether the block is a valid loop we can handle,
 *         whether the loop bound has to be incremented, the loop bound as machine
 *         operand and the register number of the index variable.
 */
HWLoopPass::LoopBounds HWLoopPass::findLoopBound(MachineBasicBlock &MBB) {

  MachineBasicBlock::reverse_iterator mbb_iter = MBB.rbegin();
  MachineBasicBlock::reverse_iterator mbb_tmp_iter;
  MachineBasicBlock::reverse_iterator mbb_tmp_iter2;
  MachineOperand* possibleLoopBound;
  MachineOperand* possibleIndexVar;
  unsigned branchCond = 0;
  MachineOperand LoopBound = MachineOperand::CreateImm(0);
  unsigned IndexVar = 0;
  LoopBounds returnValue = {false, false, LoopBound, IndexVar};
  unsigned OpCode;

  // (1) last instruction has to be a conditional branch with target to
  // current basic block
  if ((mbb_iter->getOpcode() == CBG::BCOND) &&
      (mbb_iter->getOperand(0).getMBB() == &MBB)) {
    branchCond = mbb_iter->getOperand(1).getImm();

    do {
      ++mbb_iter;
      OpCode = mbb_iter->getOpcode();
      if (OpCode == CBG::SUBCCri || OpCode == CBG::SUBCCrr || OpCode == CBG::ADDCCri || OpCode == CBG::ADDCCrr) {
        break;
      }
    } while (mbb_iter != MBB.rend());

//    std::cerr << "Last instruction of BB#" << MBB.getNumber() << " was conditional branch." << std::endl;
//    std::cerr << "Opcode of second last instruction: " << mbb_iter->getOpcode() << std::endl;

    // (2) second last instruction only can be a SUBCCrx
    if ((mbb_iter != MBB.rend()) &&
       ((mbb_iter->getOpcode() == CBG::SUBCCri) || (mbb_iter->getOpcode() == CBG::SUBCCrr))) {

//      std::cerr << "Second last instruction was SUBCCrx." << std::endl;

      // result of compare operation will be ignored
      if (mbb_iter->getOperand(0).isDead()) {

//        std::cerr << "Result of compare operation will not be used any more." << std::endl;

        possibleIndexVar = &(mbb_iter->getOperand(1));
        possibleLoopBound = &(mbb_iter->getOperand(2));

        do {
          ++mbb_iter;
          OpCode = mbb_iter->getOpcode();
          if (OpCode == CBG::SUBri || OpCode == CBG::SUBrr || OpCode == CBG::ADDri || OpCode == CBG::ADDrr) {
            break;
          }
        } while (mbb_iter != MBB.rend());

        if (mbb_iter != MBB.rend()) {

          // (3) src1 register may only be the destination of a predecessing increment or decrement
          // (3)(a) if idx is decremented, we only allow immediates to be loop bounds
          if ( isDecrement(*mbb_iter) &&
               possibleLoopBound->isImm() &&
              (possibleLoopBound->getImm() <= 0)) {
            mbb_tmp_iter = mbb_iter;
            ++mbb_tmp_iter;

            if (isNoDestination(possibleIndexVar->getReg(), MBB, mbb_tmp_iter)) {
              // if immediate is less that 0, we simply have to change sign
              if (possibleLoopBound->getImm() < 0) {
//                std::cerr << "Found constant loop bound " << (-1 * possibleLoopBound->getImm()) << std::endl;
                returnValue.LoopBound.setImm(-1 * possibleLoopBound->getImm());
                returnValue.IndexVar = possibleIndexVar->getReg();
              } else {
                // if immediate is zero, index variable contains loop bound
//                std::cerr << "Found loop bound in register " << possibleIndexVar->getReg() << std::endl;
                returnValue.LoopBound.ChangeToRegister(possibleIndexVar->getReg(), false);
                returnValue.IndexVar = possibleIndexVar->getReg();
                if (branchCond == CBGCC::ICC_LE) {
                  returnValue.needsIncrement = true;
                }
              }
              returnValue.isValidLoop = true;
            }
          }
          // (3)(b) if idx is incremented and compared with constant
          else if( isIncrement(*mbb_iter) &&
                   possibleLoopBound->isImm() &&
                  (possibleLoopBound->getImm() > 0)) {
            mbb_tmp_iter = mbb_iter;
            ++mbb_tmp_iter;

            if (isNoDestination(possibleIndexVar->getReg(), MBB, mbb_tmp_iter)) {
//              std::cerr << "Found constant loop bound " << possibleLoopBound->getImm() << std::endl;
              returnValue.LoopBound.setImm(possibleLoopBound->getImm());
              returnValue.IndexVar = possibleIndexVar->getReg();
              returnValue.isValidLoop = true;
            }

          }

          // (3)(c) if idx is incremented and compared with a constant register
          else if( isIncrement(*mbb_iter) &&
                   possibleLoopBound->isReg()) {

            mbb_tmp_iter = mbb_iter;
            ++mbb_tmp_iter;
            mbb_tmp_iter2 = mbb_tmp_iter;

            // neither index variable, nor loop bound register may be used as
            // destination register

            if (isNoDestination(possibleIndexVar->getReg(), MBB, mbb_tmp_iter) &&
                isNoDestination(possibleLoopBound->getReg(), MBB, mbb_tmp_iter2)) {
//              std::cerr << "Found register loop bound in register " << possibleLoopBound->getReg() << std::endl;
              returnValue.LoopBound.ChangeToRegister(possibleLoopBound->getReg(), false);
              returnValue.IndexVar = possibleIndexVar->getReg();
              returnValue.isValidLoop = true;
              if (branchCond == CBGCC::ICC_LE) {
                returnValue.needsIncrement = true;
              }
            }

          }


        }

      }

    }

  }

  return returnValue;

}

/**
 * @brief Tries to remove all instructions of the given basic block where the
 *        index register is involved if this is possible. At least compare and
 *        conditional branch at the end of the given machine basic block are
 *        removed.
 * @param MBB       Machine basic block to check.
 * @param regNumber Register number containing the index variable.
 */
void HWLoopPass::removeIndexVar(MachineBasicBlock &MBB, unsigned regNumber) {

  MachineBasicBlock::reverse_iterator mbb_iter;
  bool isUsed = false;

  // remove last two instructions (conditional branch and compare)
  MBB.rbegin()->eraseFromParent();

  mbb_iter = MBB.rbegin();
  while (mbb_iter != MBB.rend()) {
    if (mbb_iter->getOpcode() == CBG::SUBCCri || mbb_iter->getOpcode() == CBG::SUBCCrr) {
      mbb_iter->eraseFromParent();
      break;
    }
    ++mbb_iter;
  }


  // check, whether loop index is used as input for any other value
  for (mbb_iter = MBB.rbegin(); mbb_iter != MBB.rend(); ++mbb_iter) {
    for (unsigned numOps = 1; numOps < mbb_iter->getNumOperands(); ++numOps) {
      if ( mbb_iter->getOperand(numOps).isReg() &&
          (mbb_iter->getOperand(numOps).getReg() == regNumber)) {
        if (!isDecrement(*mbb_iter) && !isIncrement(*mbb_iter)) {
          isUsed = true;
          break;
        }
      }
    }
    if (isUsed) {
      break;
    }
  }

  // we can remove last increment/decrement instruction, if index var is not used
  if (!isUsed) {
    for (mbb_iter = MBB.rbegin(); mbb_iter != MBB.rend(); ++mbb_iter) {
      if ( mbb_iter->getOperand(0).isReg() &&
          (mbb_iter->getOperand(0).getReg() == regNumber)) {
        if (isDecrement(*mbb_iter) || isIncrement(*mbb_iter)) {
          mbb_iter->eraseFromParent();
          break;
        }
      }
    }
  }

}

bool HWLoopPass::runOnMachineFunction(MachineFunction &F) {
  bool Changed = false;
  MBB_list possible_loops;
  for (MachineFunction::iterator FI = F.begin(), FE = F.end();
      FI != FE; ++FI) {
    possible_loops = getPossibleLoops(*FI);
    if (possible_loops.size() == 1) {
      Changed |= runOnMachineBasicBlock(*FI);
    }
  }
  return Changed;
}

bool HWLoopPass::runOnMachineBasicBlock(MachineBasicBlock &MBB) {
  LoopBounds loopOperand = findLoopBound(MBB);
  if (loopOperand.isValidLoop) {
    insertSingleHWLoop(MBB, loopOperand.LoopBound, loopOperand.needsIncrement);
    removeIndexVar(MBB, loopOperand.IndexVar);
    return true;
  }
  return false;
}

FunctionPass* llvm::createcbgHWLoopPass(TargetMachine &tm, unsigned loopDepth) {
  DD_PRINT(__func__);
  return new HWLoopPass(tm, loopDepth);
}
