/*
 * PredecessorList.cpp
 *
 *  Created on: Nov 7, 2011
 *      Author: cbg
 */

#include "PredecessorList.h"

#include <iostream>

using namespace llvm;

PredecessorList::~PredecessorList() {
  for (PredList::iterator mbb_it = listEntries.begin();
       mbb_it != listEntries.end();
       ++mbb_it) {
    delete(*mbb_it);
  }
}

PredecessorList::ListEntry& PredecessorList::ListEntry::operator=(const ListEntry& le) {
  if (this != &le) {
    MBB = le.MBB;
    predecessor = le.predecessor;
  }
  return (*this);
}

bool PredecessorList::ListEntry::operator==(const MachineBasicBlock* mbb) const {
  return (mbb->getNumber() == MBB->getNumber());
}

bool PredecessorList::ListEntry::operator!=(const MachineBasicBlock* mbb) const {
  return (mbb->getNumber() != MBB->getNumber());
}

/**
 * @brief       Searches the list if it contains the specified machine basic block and returns
 *              the corresponding list entry.
 * @param mbb   Machine basic block to find.
 * @return      The corresponding list entry on success, NULL if the list does not contain
 *              the specified entry.
 */
PredecessorList::ListEntry* PredecessorList::findMachineBasicBlock(const MachineBasicBlock* mbb) {
  ListEntry* listEntry = 0;
  PredList::iterator list_it;

  for (list_it = listEntries.begin(); list_it != listEntries.end(); ++list_it) {
    if (*(*list_it) == mbb) {
      listEntry = *list_it;
      break;
    }
  }
  return listEntry;
}

void PredecessorList::insertRoot(MachineBasicBlock* mbb) {
  ListEntry* newEntry = new ListEntry(mbb);
  listEntries.push_back(newEntry);
}

/**
 * @brief               Inserts a machine basic block with the specified predecessor
 *                      into list. If mbb is NULL, nothing will be inserted.
 * @param mbb           The machine basic block which will be inserted.
 * @param predecessor   The predecessor of the basic block.
 */
void PredecessorList::insertSuccessor(MachineBasicBlock* mbb,
                                      const MachineBasicBlock* predecessor) {

  ListEntry* pred = findMachineBasicBlock(predecessor);
  ListEntry* newEntry;
  if (mbb) {
    newEntry = new ListEntry(mbb, pred);
    listEntries.push_back(newEntry);
  }

}

PredecessorList::MBB_list PredecessorList::getPredecessors(const MachineBasicBlock* &mbb) {
  ListEntry* listEntry;
//  ListEntry* predecessor;
  MBB_list mbb_list;

//  std::cerr << "Adding all predecessors of BB#" << mbb->getNumber() << " to list." << std::endl;

  listEntry = findMachineBasicBlock(mbb);
  if (mbb) {
    mbb_list.push_back(const_cast<MachineBasicBlock*>(mbb));
//    std::cerr << "Add BB#" << mbb->getNumber() << std::endl;
  }

//  std::cerr << "Address of predecessor: " << listEntry->getPredecessor() << std::endl;

  while (listEntry->getPredecessor()) {
    listEntry = listEntry->getPredecessor();
//    std::cerr << "Add BB#" << listEntry->getMachineBasicBlock()->getNumber() << std::endl;
    mbb_list.push_front(listEntry->getMachineBasicBlock());
  }
  return mbb_list;
}
