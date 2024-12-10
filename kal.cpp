// #####################################################################################
// #include

#include "./KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"

#include <cctype>  // is...{space digit alpha alnum ascii}
#include <cstdio>  // getchar fprintf EOF
#include <cstdlib> // strtod
#include <map>     // map
#include <memory>  // unique_ptr, make_unique
#include <string>
#include <utility> // move
#include <vector>

using namespace llvm;

using std::string, std::vector;
using std::unique_ptr, std::make_unique;

// #####################################################################################
// # LEXER
// #####################################################################################

// LEXER ∊ [0-255]
enum Token {
  TOK_EOF = -1,

  // commands
  TOK_DEF = -2,
  TOK_EXTERN = -3,

  // primary
  TOK_IDENT = -4,
  TOK_NUM = -5,

  // control
  TOK_IF = -6,
  TOK_THEN = -7,
  TOK_ELSE = -8
};

static string IDENT_STR; // ← tok_ident
static double NUM_VAL;   // ← tok_num

// next token
static int get_tok() {
  static int last_ch = ' ';

  // skip whitespace
  while (isspace(last_ch))
    last_ch = getchar();

  // ident ∧ kw ∊ [a-zA-Z][a-zA-Z0-9]*
  if (isalpha(last_ch)) {
    IDENT_STR = last_ch;

    // build a full word
    while (isalnum(last_ch = getchar()))
      IDENT_STR += last_ch;

    // either kw ∊ {def extern} or identifier
    if (IDENT_STR == "def")
      return TOK_DEF;
    if (IDENT_STR == "extern")
      return TOK_EXTERN;
    if (IDENT_STR == "if")
      return TOK_IF;
    if (IDENT_STR == "then")
      return TOK_THEN;
    if (IDENT_STR == "else")
      return TOK_ELSE;
    return TOK_IDENT;
  }

  // number [0-9.]+
  if (isdigit(last_ch) || last_ch == '.') {
    string num_str;
    do {
      num_str += last_ch;
      last_ch = getchar();
    } while (isdigit(last_ch) || last_ch == '.');

    NUM_VAL = strtod(num_str.c_str(), nullptr);
    return TOK_NUM;
  }

  // comments till end of line
  if (last_ch == '#') {
    do
      last_ch = getchar();
    while (last_ch != EOF && last_ch != '\n' && last_ch != '\r');

    if (last_ch != EOF)
      return get_tok();
  }

  // keep EOF
  if (last_ch == EOF)
    return TOK_EOF;

  // return last char
  int this_ch = last_ch;
  last_ch = getchar();
  return this_ch;
}

// #####################################################################################
// # AST
// #####################################################################################

namespace {

// ###################
class Expr_AST {
public:
  virtual ~Expr_AST() = default;
  virtual Value *codegen() = 0;
};

using UP_AST = unique_ptr<Expr_AST>;

// ###################
class Num_Expr_AST : public Expr_AST {
  double Val;

public:
  Num_Expr_AST(double value) : Val(value) {}
  Value *codegen() override;
};

// ###################
class Var_Expr_AST : public Expr_AST {
  string Name;

public:
  Var_Expr_AST(const string &name) : Name(name) {}
  Value *codegen() override;
};

// ###################
class Bin_Expr_AST : public Expr_AST {
  char Op;
  UP_AST Lhs, Rhs;

public:
  Bin_Expr_AST(char op, UP_AST lhs, UP_AST rhs)
      : Op(op), Lhs(std::move(lhs)), Rhs(std::move(rhs)) {}
  Value *codegen() override;
};

// ###################
class Call_Expr_AST : public Expr_AST {
  string Callee;
  vector<UP_AST> Args;

public:
  Call_Expr_AST(const string &callee, vector<UP_AST> args)
      : Callee(callee), Args(std::move(args)) {}
  Value *codegen() override;
};

// ###################
class Proto_AST {
  string Name;
  vector<string> Args;

public:
  Proto_AST(const string &name, vector<string> args)
      : Name(name), Args(std::move(args)) {}

  const string &getName() const { return Name; }
  Function *codegen();
};

// ###################
class Func_AST {
  unique_ptr<Proto_AST> Proto;
  UP_AST Body;

public:
  Func_AST(unique_ptr<Proto_AST> proto, UP_AST body)
      : Proto(std::move(proto)), Body(std::move(body)) {}
  Function *codegen();
};

// ###################
class If_Expr_AST : public Expr_AST {
  UP_AST Cond, Then, Else;

public:
  If_Expr_AST(UP_AST cond, UP_AST then, UP_AST _else)
      : Cond(std::move(cond)), Then(std::move(then)), Else(std::move(_else)) {}

  Value *codegen() override;
};

} // namespace

// #####################################################################################
// # PARSER
// #####################################################################################

static int CURR_TOK_KIND;
static int get_next_tok() { return CURR_TOK_KIND = get_tok(); }

// binop precedence
static std::map<char, int> BINOP_PREC;

// precedence of the pending binop
static int get_prec_of_tok() {
  if (!isascii(CURR_TOK_KIND))
    return -1;

  int tok_prec = BINOP_PREC[CURR_TOK_KIND];
  if (tok_prec <= 0)
    return -1;
  return tok_prec;
}

UP_AST LOG_ERR(const char *msg) {
  fprintf(stderr, "Error: %s\n", msg);
  return nullptr;
}

unique_ptr<Proto_AST> LOG_ERR_P(const char *msg) {
  LOG_ERR(msg);
  return nullptr;
}

static UP_AST parse_expr(); // FWD

// num ::= num
static UP_AST parse_num_expr() {
  auto res = make_unique<Num_Expr_AST>(NUM_VAL);
  get_next_tok();
  return std::move(res);
}

// paren_expr ::= '(' expr ')'
static UP_AST parse_paren_expr() {
  get_next_tok(); // eat (
  auto v = parse_expr();
  if (!v)
    return nullptr;
  if (CURR_TOK_KIND != ')')
    return LOG_ERR("expected ')'");
  get_next_tok(); // eat )
  return v;
}

// ident_expr, ::= ident, ::= ident '(' expr* ')'
static UP_AST parse_ident_expr() {
  string ident_name = IDENT_STR;
  get_next_tok(); // eat ident

  if (CURR_TOK_KIND != '(')
    return make_unique<Var_Expr_AST>(ident_name);

  get_next_tok(); // eat (
  vector<UP_AST> args;
  if (CURR_TOK_KIND != ')') {
    while (true) {
      if (auto arg = parse_expr())
        args.push_back(std::move(arg));
      else
        return nullptr;

      if (CURR_TOK_KIND == ')')
        break;

      if (CURR_TOK_KIND != ',')
        return LOG_ERR("Expected ')' or ',' in arg list");
      get_next_tok();
    }
  }
  get_next_tok(); // eat )
  return make_unique<Call_Expr_AST>(ident_name, std::move(args));
}

static UP_AST parse_if_expr(); // FWD

// primary ::= < ident_expr | num_expr | paren_expr | if_expr>
static UP_AST parse_prima() {
  switch (CURR_TOK_KIND) {
  default:
    return LOG_ERR("unknown token when expecting an expression");
  case TOK_IDENT:
    return parse_ident_expr();
  case TOK_NUM:
    return parse_num_expr();
  case '(':
    return parse_paren_expr();
  case TOK_IF:
    return parse_if_expr();
  }
}

// after lhs is parsed [ + primary]
// binop_rhs ::= ('+' primary)*
static UP_AST parse_binop_rhs(int expr_prec, UP_AST lhs) {
  while (true) {
    int tok_prec = get_prec_of_tok();

    if (tok_prec < expr_prec)
      return lhs; // keep lhs if higher prec

    int binop = CURR_TOK_KIND;
    get_next_tok(); // eat binop

    auto rhs = parse_prima();
    if (!rhs)
      return nullptr;

    int next_prec = get_prec_of_tok();
    if (tok_prec < next_prec) {
      rhs = parse_binop_rhs(tok_prec + 1, std::move(rhs));
      if (!rhs)
        return nullptr;
    }

    // merge lhs/rhs
    lhs = make_unique<Bin_Expr_AST>(binop, std::move(lhs), std::move(rhs));
  } // while
}

// expr ::= primary binop_rhs
static UP_AST parse_expr() {
  auto lhs = parse_prima();
  if (!lhs)
    return nullptr;
  return parse_binop_rhs(0, std::move(lhs));
}

// prototype ::= id '(' id* ')'
static unique_ptr<Proto_AST> parse_proto() {
  if (CURR_TOK_KIND != TOK_IDENT)
    return LOG_ERR_P("expected func name in prototype");

  string fn_name = IDENT_STR;
  get_next_tok(); // eat fn name, expect (

  if (CURR_TOK_KIND != '(')
    return LOG_ERR_P("Expected '(' in prototype");

  vector<string> arg_names;
  while (get_next_tok() == TOK_IDENT)
    arg_names.push_back(IDENT_STR); // collect func args

  if (CURR_TOK_KIND != ')')
    return LOG_ERR_P("Expected ')' in prototype");

  get_next_tok(); // eat )
  return make_unique<Proto_AST>(fn_name, std::move(arg_names));
}

// definition ::= 'def' proto
static unique_ptr<Func_AST> parse_def() {
  get_next_tok(); // eat def
  auto proto = parse_proto();
  if (!proto)
    return nullptr;

  if (auto body = parse_expr())
    return make_unique<Func_AST>(std::move(proto), std::move(body));
  return nullptr;
}

// top_level_expr ::= expr
static unique_ptr<Func_AST> parse_top_lev_expr() {
  if (auto body = parse_expr()) {
    auto proto = make_unique<Proto_AST>("__anon_expr", vector<string>());
    return make_unique<Func_AST>(std::move(proto), std::move(body));
  }
  return nullptr;
}

// extern ::= 'extern' proto
static unique_ptr<Proto_AST> parse_extern() {
  get_next_tok(); // eat extern
  return parse_proto();
}

// if_expr ::= 'if' expr 'then' expr 'else' expr
static UP_AST parse_if_expr() {
  get_next_tok();

  auto cond = parse_expr();
  if (!cond)
    return nullptr;

  if (CURR_TOK_KIND != TOK_THEN)
    return LOG_ERR("expected then");

  get_next_tok();

  auto then = parse_expr();
  if (!then)
    return nullptr;

  if (CURR_TOK_KIND != TOK_ELSE)
    return LOG_ERR("expected else");

  get_next_tok();

  auto _else = parse_expr();
  if (!_else)
    return nullptr;

  return make_unique<If_Expr_AST>(std::move(cond), std::move(then),
                                  std::move(_else));
};

// #####################################################################################
// # codegen()
// #####################################################################################

// LLVM vars
static unique_ptr<LLVMContext> CTX;
static unique_ptr<Module> MODULE;
static unique_ptr<IRBuilder<>> IR_BLD;
static std::map<string, Value *> NAMED_VALs;

// JIT
static unique_ptr<orc::KaleidoscopeJIT> JIT;
static unique_ptr<FunctionPassManager> FPM;

static unique_ptr<LoopAnalysisManager> LAM;
static unique_ptr<FunctionAnalysisManager> FAM;
static unique_ptr<CGSCCAnalysisManager> CGAM;
static unique_ptr<ModuleAnalysisManager> MAM;

static unique_ptr<PassInstrumentationCallbacks> PIC;
static unique_ptr<StandardInstrumentations> SI;
static std::map<string, unique_ptr<Proto_AST>> FUNC_PROTOs;
static ExitOnError EXIT_ON_ERR;

Value *LogErrorV(const char *msg) {
  LOG_ERR(msg);
  return nullptr;
}

Function *getFunction(string name) {
  if (auto *f = MODULE->getFunction(name))
    return f;

  auto fi = FUNC_PROTOs.find(name);
  if (fi != FUNC_PROTOs.end())
    return fi->second->codegen();

  return nullptr;
}

Value *Num_Expr_AST::codegen() { return ConstantFP::get(*CTX, APFloat(Val)); }

Value *Var_Expr_AST::codegen() {
  Value *v = NAMED_VALs[Name];
  if (!v)
    return LogErrorV("Unknown var name");
  return v;
}

Value *Bin_Expr_AST::codegen() {
  auto *l = Lhs->codegen();
  auto *r = Rhs->codegen();

  if (!l || !r)
    return nullptr;

  switch (Op) {
  case '+':
    return IR_BLD->CreateFAdd(l, r, "add");
  case '-':
    return IR_BLD->CreateFSub(l, r, "sub");
  case '*':
    return IR_BLD->CreateFMul(l, r, "mul");
  case '<':
    l = IR_BLD->CreateFCmpULT(l, r, "cmp");
    return IR_BLD->CreateUIToFP(l, Type::getDoubleTy(*CTX), "bool");
  default:
    return LogErrorV("invalid binary operator");
  } // sw
}

Value *Call_Expr_AST::codegen() {
  Function *callee_fn = getFunction(Callee);
  if (!callee_fn)
    return LogErrorV("Unknown func ref");

  if (callee_fn->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  vector<Value *> args_v;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    args_v.push_back(Args[i]->codegen());
    if (!args_v.back())
      return nullptr;
  }
  return IR_BLD->CreateCall(callee_fn, args_v, "call");
}

Value *If_Expr_AST::codegen() {
  Value *cond_v = Cond->codegen();
  if (!cond_v)
    return nullptr;

  // convert cond to bool
  cond_v = IR_BLD->CreateFCmpONE(cond_v, ConstantFP::get(*CTX, APFloat(0.0)),
                                 "Ifcond");
  Function *fn = IR_BLD->GetInsertBlock()->getParent();

  // block for {then, else}
  auto then_bb = BasicBlock::Create(*CTX, "then", fn);
  auto else_bb = BasicBlock::Create(*CTX, "else");
  auto merge_bb = BasicBlock::Create(*CTX, "ifcond");

  IR_BLD->CreateCondBr(cond_v, then_bb, else_bb);

  // emit then value
  IR_BLD->SetInsertPoint(then_bb);
  auto then_v = Then->codegen();
  if (!then_v)
    return nullptr;

  IR_BLD->CreateBr(merge_bb);

  then_bb = IR_BLD->GetInsertBlock();

  // emit else block
  fn->insert(fn->end(), else_bb);
  IR_BLD->SetInsertPoint(else_bb);
  auto else_v = Else->codegen();
  if (!else_v)
    return nullptr;

  IR_BLD->CreateBr(merge_bb);
  else_bb = IR_BLD->GetInsertBlock();

  // emit merge block
  fn->insert(fn->end(), merge_bb);
  IR_BLD->SetInsertPoint(merge_bb);
  auto *phi = IR_BLD->CreatePHI(Type::getDoubleTy(*CTX), 2, "if");

  phi->addIncoming(then_v, then_bb);
  phi->addIncoming(else_v, else_bb);
  return phi;
}

Function *Proto_AST::codegen() {
  vector<Type *> doubles(Args.size(), Type::getDoubleTy(*CTX));
  FunctionType *ft = FunctionType::get(Type::getDoubleTy(*CTX), doubles, false);
  Function *f =
      Function::Create(ft, Function::ExternalLinkage, Name, MODULE.get());

  unsigned idx = 0;
  for (auto &arg : f->args())
    arg.setName(Args[idx++]);

  return f;
}

Function *Func_AST::codegen() {

  auto &p = *Proto;
  FUNC_PROTOs[Proto->getName()] = std::move(Proto);
  Function *fn = getFunction(p.getName());
  if (!fn)
    return nullptr;

  BasicBlock *bb = BasicBlock::Create(*CTX, "entry", fn);
  IR_BLD->SetInsertPoint(bb);

  NAMED_VALs.clear();
  for (auto &arg : fn->args())
    NAMED_VALs[string(arg.getName())] = &arg;

  if (Value *ret = Body->codegen()) {
    IR_BLD->CreateRet(ret);

    verifyFunction(*fn);

    FPM->run(*fn, *FAM);
    return fn;
  }

  fn->eraseFromParent();
  return nullptr;
}

// #####################################################################################
// # Top Level Parsing
// #####################################################################################

static void init_module_and_mgrs() {

  // init CTX and Module
  CTX = make_unique<LLVMContext>();
  MODULE = make_unique<Module>("kaleidoscope jit", *CTX);
  MODULE->setDataLayout(JIT->getDataLayout());

  IR_BLD = make_unique<IRBuilder<>>(*CTX);

  // pass manager

  // new pass and analysis managers
  FPM = make_unique<FunctionPassManager>();
  LAM = make_unique<LoopAnalysisManager>();
  FAM = make_unique<FunctionAnalysisManager>();
  CGAM = make_unique<CGSCCAnalysisManager>();
  MAM = make_unique<ModuleAnalysisManager>();

  PIC = make_unique<PassInstrumentationCallbacks>();
  SI = make_unique<StandardInstrumentations>(*CTX, true); // debug logging t/f
  SI->registerCallbacks(*PIC, MAM.get());

  // transform passes
  // peephole opt and bit twiddling
  FPM->addPass(InstCombinePass());
  // reassociate expr
  FPM->addPass(ReassociatePass());
  // eliminate common sub-expressions
  FPM->addPass(GVNPass());
  // simplify CFG (delete unreachable blcoks etc.)
  FPM->addPass(SimplifyCFGPass());

  // add passes
  PassBuilder pb;
  pb.registerModuleAnalyses(*MAM);
  pb.registerFunctionAnalyses(*FAM);
  pb.crossRegisterProxies(*LAM, *FAM, *CGAM, *MAM);
};

static void handle_def() {
  if (auto fn_ast = parse_def()) {
    if (auto *fn_ir = fn_ast->codegen()) {
      fprintf(stderr, "Read function definition:\n\n");
      fn_ir->print(llvm::errs());
      fprintf(stderr, "\n");

      EXIT_ON_ERR(JIT->addModule(
          orc::ThreadSafeModule(std::move(MODULE), std::move(CTX))));
      init_module_and_mgrs();
    }
  } else
    get_next_tok();
}

static void handle_extern() {
  if (auto proto = parse_extern()) {
    if (auto *fn_ir = proto->codegen()) {
      fprintf(stderr, "Read extern:\n\n");
      fn_ir->print(llvm::errs());
      fprintf(stderr, "\n");

      FUNC_PROTOs[proto->getName()] = std::move(proto);
    }
  } else
    get_next_tok();
}

static void handle_top_level_expr() {
  if (auto fn_ast = parse_top_lev_expr()) {
    if (fn_ast->codegen()) {

      auto rt = JIT->getMainJITDylib().createResourceTracker();

      auto tsm = orc::ThreadSafeModule(std::move(MODULE), std::move(CTX));
      EXIT_ON_ERR(JIT->addModule(std::move(tsm), rt));
      init_module_and_mgrs();

      auto expr_sym = EXIT_ON_ERR(JIT->lookup("__anon_expr"));

      double (*FP)() = expr_sym.getAddress().toPtr<double (*)()>();
      fprintf(stderr, "Evaluated to %f\n", FP());

      EXIT_ON_ERR(rt->remove());
    }
  } else
    get_next_tok();
}

// top ::= def | extern | expr | ';'
static void LOOP() {

  while (true) {
    fprintf(stderr, "ready> ");

    switch (CURR_TOK_KIND) {
    case TOK_EOF:
      return;
    case ';':
      get_next_tok();
      break;
    case TOK_DEF:
      handle_def();
      break;
    case TOK_EXTERN:
      handle_extern();
      break;
    default:
      handle_top_level_expr();
      break;
    } // sw
  } // wh
}

// #####################################################################################
// # MAIN
// #####################################################################################

int main() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  // setup binops, 1 is lowest
  BINOP_PREC['<'] = 10;
  BINOP_PREC['+'] = 20;
  BINOP_PREC['-'] = 20;
  BINOP_PREC['*'] = 40; // highest

  fprintf(stderr, "ready> ");
  get_next_tok(); // first token

  JIT = EXIT_ON_ERR(orc::KaleidoscopeJIT::Create());

  init_module_and_mgrs();

  LOOP();

  printf("\n");

  MODULE->print(llvm::errs(), nullptr);
  printf("\n");

  return 0;
}
