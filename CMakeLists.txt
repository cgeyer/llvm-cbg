set(LLVM_TARGET_DEFINITIONS cbg.td)

tablegen(cbgGenRegisterInfo.h.inc -gen-register-desc-header)
tablegen(cbgGenRegisterNames.inc -gen-register-enums)
tablegen(cbgGenRegisterInfo.inc -gen-register-desc)
tablegen(cbgGenInstrNames.inc -gen-instr-enums)
tablegen(cbgGenInstrInfo.inc -gen-instr-desc)
tablegen(cbgGenAsmWriter.inc -gen-asm-writer)
tablegen(cbgGenDAGISel.inc -gen-dag-isel)
tablegen(cbgGenSubtarget.inc -gen-subtarget)
tablegen(cbgGenCallingConv.inc -gen-callingconv)

add_llvm_target(cbgCodeGen
  DelaySlotFiller.cpp
  FPMover.cpp
  cbgAsmPrinter.cpp
  cbgInstrInfo.cpp
  cbgISelDAGToDAG.cpp
  cbgISelLowering.cpp
  cbgFrameLowering.cpp
  cbgMCAsmInfo.cpp
  cbgRegisterInfo.cpp
  cbgSubtarget.cpp
  cbgTargetMachine.cpp
  cbgSelectionDAGInfo.cpp
  )

add_subdirectory(TargetInfo)
