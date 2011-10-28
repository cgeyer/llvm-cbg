//===-- cbg.h - Top-level interface for Sparc representation --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// Sparc back-end.
//
//===----------------------------------------------------------------------===//

#ifndef TARGET_CBG_H
#define TARGET_CBG_H

#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"
#include <cassert>

namespace llvm {
  class FunctionPass;
  class cbgTargetMachine;
  class formatted_raw_ostream;

  FunctionPass *createcbgISelDag(cbgTargetMachine &TM);
  FunctionPass *createcbgDelaySlotFillerPass(TargetMachine &TM);
  FunctionPass *createcbgFPMoverPass(TargetMachine &TM);

  extern Target ThecbgTarget;
//  extern Target TheSparcV9Target;

} // end namespace llvm;

// Defines symbolic names for Sparc registers.  This defines a mapping from
// register name to register number.
//
#include "cbgGenRegisterNames.inc"

// Defines symbolic names for the Sparc instructions.
//
#include "cbgGenInstrNames.inc"


namespace llvm {
  // Enums corresponding to Sparc condition codes, both icc's and fcc's.  These
  // values must be kept in sync with the ones in the .td file.
  namespace CBGCC {
    enum CondCodes {
      //ICC_A   =  8   ,  // Always
      //ICC_N   =  0   ,  // Never
      ICC_NE  =  9   ,  // Not Equal
      ICC_E   =  1   ,  // Equal
      ICC_G   = 10   ,  // Greater
      ICC_LE  =  2   ,  // Less or Equal
      ICC_GE  = 11   ,  // Greater or Equal
      ICC_L   =  3   ,  // Less
      ICC_GU  = 12   ,  // Greater Unsigned
      ICC_LEU =  4   ,  // Less or Equal Unsigned
      ICC_CC  = 13   ,  // Carry Clear/Great or Equal Unsigned
      ICC_CS  =  5   ,  // Carry Set/Less Unsigned
      ICC_POS = 14   ,  // Positive
      ICC_NEG =  6   ,  // Negative
      ICC_VC  = 15   ,  // Overflow Clear
      ICC_VS  =  7   ,  // Overflow Set
      
      //FCC_A   =  8+16,  // Always
      //FCC_N   =  0+16,  // Never
      FCC_U   =  7+16,  // Unordered
      FCC_G   =  6+16,  // Greater
      FCC_UG  =  5+16,  // Unordered or Greater
      FCC_L   =  4+16,  // Less
      FCC_UL  =  3+16,  // Unordered or Less
      FCC_LG  =  2+16,  // Less or Greater
      FCC_NE  =  1+16,  // Not Equal
      FCC_E   =  9+16,  // Equal
      FCC_UE  = 10+16,  // Unordered or Equal
      FCC_GE  = 11+16,  // Greater or Equal
      FCC_UGE = 12+16,  // Unordered or Greater or Equal
      FCC_LE  = 13+16,  // Less or Equal
      FCC_ULE = 14+16,  // Unordered or Less or Equal
      FCC_O   = 15+16   // Ordered
    };
  }
  
  inline static const char *CBGCondCodeToString(CBGCC::CondCodes CC) {
    switch (CC) {
    default: llvm_unreachable("Unknown condition code");
    case CBGCC::ICC_NE:  return "ne";
    case CBGCC::ICC_E:   return "e";
    case CBGCC::ICC_G:   return "g";
    case CBGCC::ICC_LE:  return "le";
    case CBGCC::ICC_GE:  return "ge";
    case CBGCC::ICC_L:   return "l";
    case CBGCC::ICC_GU:  return "gu";
    case CBGCC::ICC_LEU: return "leu";
    case CBGCC::ICC_CC:  return "cc";
    case CBGCC::ICC_CS:  return "cs";
    case CBGCC::ICC_POS: return "pos";
    case CBGCC::ICC_NEG: return "neg";
    case CBGCC::ICC_VC:  return "vc";
    case CBGCC::ICC_VS:  return "vs";
    case CBGCC::FCC_U:   return "u";
    case CBGCC::FCC_G:   return "g";
    case CBGCC::FCC_UG:  return "ug";
    case CBGCC::FCC_L:   return "l";
    case CBGCC::FCC_UL:  return "ul";
    case CBGCC::FCC_LG:  return "lg";
    case CBGCC::FCC_NE:  return "ne";
    case CBGCC::FCC_E:   return "e";
    case CBGCC::FCC_UE:  return "ue";
    case CBGCC::FCC_GE:  return "ge";
    case CBGCC::FCC_UGE: return "uge";
    case CBGCC::FCC_LE:  return "le";
    case CBGCC::FCC_ULE: return "ule";
    case CBGCC::FCC_O:   return "o";
    }       
  }
}  // end namespace llvm
#endif
