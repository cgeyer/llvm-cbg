//===- cbg.td - Describe the cbg Target Machine --------*- tablegen -*-===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// CBG Subtarget features.
//
 
def FeaturePredicatedInstr
  : SubtargetFeature<"pins", "IsPins", "true",
                     "Enable predicated instructions">;

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "cbgRegisterInfo.td"
include "cbgCallingConv.td"
include "cbgInstrInfo.td"

def cbgInstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// CBG processors supported.
//===----------------------------------------------------------------------===//

class Proc<string Name, list<SubtargetFeature> Features>
 : Processor<Name, NoItineraries, Features>;

def : Proc<"v8",              []>;
def : Proc<"v8-pred",         [FeaturePredicatedInstr]>;


//===----------------------------------------------------------------------===//
// Declare the target which we are implementing
//===----------------------------------------------------------------------===//

def cbg : Target {
  // Pull in Instruction Info:
  let InstructionSet = cbgInstrInfo;
}
