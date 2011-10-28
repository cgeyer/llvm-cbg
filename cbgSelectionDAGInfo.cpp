//===-- SparcSelectionDAGInfo.cpp - Sparc SelectionDAG Info ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the SparcSelectionDAGInfo class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "cbg-selectiondag-info"
#include "cbgTargetMachine.h"
using namespace llvm;

cbgSelectionDAGInfo::cbgSelectionDAGInfo(const cbgTargetMachine &TM)
  : TargetSelectionDAGInfo(TM) {
}

cbgSelectionDAGInfo::~cbgSelectionDAGInfo() {
}
