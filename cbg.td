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
 
def FeatureSelCC
  : SubtargetFeature<"selcc", "HasSelCC", "true",
                     "Enable selCC instructions">;
def FeatureMovCC
  : SubtargetFeature<"movcc", "HasMovCC", "true",
                     "Enable selCC instructions">;
def FeaturePredBlocksCC
  : SubtargetFeature<"predblockscc", "HasPredBlocksCC", "true",
                     "Enable predicated blocks with condition codes">;
def FeaturePredBlocksReg
  : SubtargetFeature<"predblocksreg", "HasPredBlocksReg", "true",
                     "Enable predicated blocks with predicate register - currently not implemented">;
def FeaturePredInstrCC
  : SubtargetFeature<"predinstrcc", "HasPredInstrCC", "true",
                     "Enable fully predicated instructions with condition codes - currently not implemented">;
def FeaturePredInstrReg
  : SubtargetFeature<"predinstrreg", "HasPredInstrReg", "true",
                     "Enable fully predicated instructions with predicate register - currently not implemented">;
def FeatureHWLoop
  : SubtargetFeature<"singleloop", "HasHWLoop", "true",
                     "Enable hardware support for single loop">;
def FeatureHWLoops
  : SubtargetFeature<"nestedloops", "HasHWLoops", "true",
                     "Enable hardware support for nested loops - currently not implemented (same as singleloop)">;
def FeatureVLIWIfElse
  : SubtargetFeature<"vliwbranches", "HasVLIWIfElse", "true",
                     "Enable concurrent if and else execution - currently not implemented">;

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
def : Proc<"v8-selcc",        [FeatureSelCC]>;
def : Proc<"v8-movcc",        [FeatureMovCC]>;
def : Proc<"v8-predblockscc", [FeaturePredBlocksCC]>;
def : Proc<"v8-predblocksreg",[FeaturePredBlocksReg]>;
def : Proc<"v8-predinstrcc",  [FeaturePredInstrCC]>;
def : Proc<"v8-predinstrreg", [FeaturePredInstrReg]>;
def : Proc<"v8-singleloop",   [FeatureHWLoop]>;
def : Proc<"v8-nestedloop",   [FeatureHWLoops]>;
def : Proc<"v8-vliwbranches", [FeatureVLIWIfElse]>;
def : Proc<"v8-block-loops",  [FeaturePredBlocksCC, FeatureHWLoops]>;
def : Proc<"v8-pred-loops",   [FeaturePredInstrCC, FeatureHWLoops]>;

//===----------------------------------------------------------------------===//
// Declare the target which we are implementing
//===----------------------------------------------------------------------===//

def cbg : Target {
  // Pull in Instruction Info:
  let InstructionSet = cbgInstrInfo;
}
