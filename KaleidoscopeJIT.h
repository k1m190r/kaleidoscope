#ifndef LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
#define LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"

#include <memory>
#include <utility>

using std::unique_ptr, std::make_unique;

namespace llvm {
namespace orc {

class KaleidoscopeJIT {
private:
  unique_ptr<ExecutionSession> ES;

  DataLayout DL;
  MangleAndInterner Mangle;

  RTDyldObjectLinkingLayer ObjLayer;
  IRCompileLayer CompLayer;

  JITDylib &MainJD;

public:
  KaleidoscopeJIT(unique_ptr<ExecutionSession> es, JITTargetMachineBuilder jtmb,
                  DataLayout dl)
      : ES(std::move(es)), DL(std::move(dl)), Mangle(*this->ES, this->DL),
        ObjLayer(*this->ES, []() { return make_unique<SectionMemoryManager>(); }),
        CompLayer(*this->ES, ObjLayer,
                  make_unique<ConcurrentIRCompiler>(std::move(jtmb))),
        MainJD(this->ES->createBareJITDylib("<main>")) {
    MainJD.addGenerator(cantFail(
        DynamicLibrarySearchGenerator::GetForCurrentProcess(dl.getGlobalPrefix())));
    if (jtmb.getTargetTriple().isOSBinFormatCOFF()) {
      ObjLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);
      ObjLayer.setAutoClaimResponsibilityForObjectSymbols(true);
    }
  }

  ~KaleidoscopeJIT() {
    if (auto err = ES->endSession())
      ES->reportError(std::move(err));
  }

  static Expected<unique_ptr<KaleidoscopeJIT>>
  Create() {
    auto epc = SelfExecutorProcessControl::Create();
    if (!epc)
      return epc.takeError();

    auto es = make_unique<ExecutionSession>(std::move(*epc));

    JITTargetMachineBuilder jtmb(es->getExecutorProcessControl().getTargetTriple());

    auto dl = jtmb.getDefaultDataLayoutForTarget();
    if (!dl)
      return dl.takeError();

    return make_unique<KaleidoscopeJIT>(std::move(es), std::move(jtmb), std::move(*dl));
  }

  const DataLayout &
  getDataLayout() const {
    return DL;
  }

  JITDylib &
  getMainJITDylib() {
    return MainJD;
  }

  Error
  addModule(ThreadSafeModule tsm, ResourceTrackerSP rt = nullptr) {
    if (!rt)
      rt = MainJD.getDefaultResourceTracker();
    return CompLayer.add(rt, std::move(tsm));
  }

  Expected<ExecutorSymbolDef>
  lookup(StringRef name) {
    return ES->lookup({&MainJD}, Mangle(name.str()));
  }
};

} // namespace orc
} // namespace llvm

#endif // LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H