//===-- SparcSelectionDAGInfo.h - Sparc SelectionDAG Info -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Sparc subclass for TargetSelectionDAGInfo.
//
//===----------------------------------------------------------------------===//

#ifndef SPARCSELECTIONDAGINFO_H
#define SPARCSELECTIONDAGINFO_H

#include "llvm/Target/TargetSelectionDAGInfo.h"

namespace llvm {

class cbgTargetMachine;

class cbgSelectionDAGInfo : public TargetSelectionDAGInfo {
public:
  explicit cbgSelectionDAGInfo(const cbgTargetMachine &TM);
  ~cbgSelectionDAGInfo();
};

}

#endif
