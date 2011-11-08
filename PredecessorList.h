/*
 * PredecessorList.h
 *
 *  Created on: Nov 7, 2011
 *      Author: cbg
 */

#ifndef PREDECESSORLIST_H_
#define PREDECESSORLIST_H_

#include "llvm/CodeGen/MachineBasicBlock.h"

#include <list>

using namespace llvm;

class PredecessorList {
protected:

  class ListEntry {
  private:
    MachineBasicBlock*  MBB;
    ListEntry*          predecessor;
  public:
    ListEntry(MachineBasicBlock* mbb, ListEntry* pred) :
      MBB(mbb), predecessor(pred) {}

    ListEntry(MachineBasicBlock* mbb) :
      MBB(mbb), predecessor(0) {}

    ListEntry* getPredecessor(void) const {
      return predecessor;
    }

    MachineBasicBlock* getMachineBasicBlock(void) const {
      return MBB;
    }

    ListEntry& operator=(const ListEntry&);

    bool operator==(const MachineBasicBlock*) const;
    bool operator!=(const MachineBasicBlock*) const;
  };

  typedef std::list<ListEntry*> PredList;

private:
  PredList listEntries;

protected:
  ListEntry* findMachineBasicBlock(const MachineBasicBlock*);

public:

  typedef std::list<MachineBasicBlock*> MBB_list;

  PredecessorList() :
    listEntries(0) {}

  ~PredecessorList();

  void insertRoot(MachineBasicBlock* mbb);
  void insertSuccessor(MachineBasicBlock* mbb,
                       const MachineBasicBlock* predecessor);

  MBB_list getPredecessors(const MachineBasicBlock* &mbb);

};


#endif /* PREDECESSORLIST_H_ */
