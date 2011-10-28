//=====-- cbgMCAsmInfo.h - cbg asm properties -------------*- C++ -*--====//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the cbgMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef CBGTARGETASMINFO_H
#define CBGTARGETASMINFO_H

#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCAsmInfo.h"

namespace llvm {
  class Target;

  struct cbgELFMCAsmInfo : public MCAsmInfo {
    explicit cbgELFMCAsmInfo(const Target &T, StringRef TT);
  };

} // namespace llvm

#endif
