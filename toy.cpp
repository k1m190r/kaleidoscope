#include "llvm/ADT/APFloat.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <cctype>  // is...{space digit alpha alnum ascii}
#include <cstdio>  // getchar fprintf EOF
#include <cstdlib> // strtod
#include <map>     // map
#include <memory>  // unique_ptr, make_unique
#include <string>
#include <utility> // move
#include <vector>

using std::string, std::vector;
using std::unique_ptr, std::make_unique;

typedef llvm::Type TP;
typedef llvm::Value V;
typedef llvm::Function FN;
typedef llvm::FunctionType FNT;
typedef llvm::BasicBlock BB;

// #####################################################################################
// # LEXER
// #####################################################################################

// LEXER ∊ [0-255]
enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_ident = -4,
  tok_num = -5,
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
      return tok_def;
    if (IDENT_STR == "extern")
      return tok_extern;
    return tok_ident;
  }

  // number [0-9.]+
  if (isdigit(last_ch) || last_ch == '.') {
    string num_str;
    do {
      num_str += last_ch;
      last_ch = getchar();
    } while (isdigit(last_ch) || last_ch == '.');

    NUM_VAL = strtod(num_str.c_str(), nullptr);
    return tok_num;
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
    return tok_eof;

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
  virtual V *codegen() = 0;
};

// ###################
class Num_Expr_AST : public Expr_AST {
  double value;

public:
  Num_Expr_AST(double value) : value(value) {}
  V *codegen() override;
};

// ###################
class Var_Expr_AST : public Expr_AST {
  string name;

public:
  Var_Expr_AST(const string &name) : name(name) {}
  V *codegen() override;
};

// ###################
class Bin_Expr_AST : public Expr_AST {
  char op;
  unique_ptr<Expr_AST> lhs, rhs;

public:
  Bin_Expr_AST(char op, unique_ptr<Expr_AST> lhs, unique_ptr<Expr_AST> rhs)
      : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
  V *codegen() override;
};

// ###################
class Call_Expr_AST : public Expr_AST {
  string callee;
  vector<unique_ptr<Expr_AST>> args;

public:
  Call_Expr_AST(const string &callee, vector<unique_ptr<Expr_AST>> args)
      : callee(callee), args(std::move(args)) {}
  V *codegen() override;
};

// ###################
class Proto_AST {
  string name;
  vector<string> args;

public:
  Proto_AST(const string &name, vector<string> args)
      : name(name), args(std::move(args)) {}

  const string &getName() const { return name; }
  FN *codegen();
};

// ###################
class Func_AST {
  unique_ptr<Proto_AST> proto;
  unique_ptr<Expr_AST> body;

public:
  Func_AST(unique_ptr<Proto_AST> proto, unique_ptr<Expr_AST> body)
      : proto(std::move(proto)), body(std::move(body)) {}
  FN *codegen();
};

} // namespace

// #####################################################################################
// # PARSER
// #####################################################################################

static int CURR_TOK;
static int get_next_tok() { return CURR_TOK = get_tok(); }

// binop precedence
static std::map<char, int> BINOP_PREC;

// precedence of the pending binop
static int get_prec_of_tok() {
  if (!isascii(CURR_TOK))
    return -1;

  int tok_prec = BINOP_PREC[CURR_TOK];
  if (tok_prec <= 0)
    return -1;
  return tok_prec;
}

unique_ptr<Expr_AST> LOG_err(const char *msg) {
  fprintf(stderr, "Error: %s\n", msg);
  return nullptr;
}

unique_ptr<Proto_AST> LOG_err_p(const char *msg) {
  LOG_err(msg);
  return nullptr;
}

static unique_ptr<Expr_AST> parse_expr();

// num ::= num
static unique_ptr<Expr_AST> parse_num_expr() {
  auto res = make_unique<Num_Expr_AST>(NUM_VAL);
  get_next_tok();
  return std::move(res);
}

// paren_expr ::= '(' expr ')'
static unique_ptr<Expr_AST> parse_paren_expr() {
  get_next_tok(); // eat (
  auto v = parse_expr();
  if (!v)
    return nullptr;
  if (CURR_TOK != ')')
    return LOG_err("expected ')'");
  get_next_tok(); // eat )
  return v;
}

// ident_expr, ::= ident, ::= ident '(' expr* ')'
static unique_ptr<Expr_AST> parse_ident_expr() {
  string ident_name = IDENT_STR;
  get_next_tok(); // eat ident

  if (CURR_TOK != '(')
    return make_unique<Var_Expr_AST>(ident_name);

  get_next_tok(); // eat (
  vector<unique_ptr<Expr_AST>> args;
  if (CURR_TOK != ')') {
    while (true) {
      if (auto arg = parse_expr())
        args.push_back(std::move(arg));
      else
        return nullptr;

      if (CURR_TOK == ')')
        break;

      if (CURR_TOK != ',')
        return LOG_err("Expected ')' or ',' in arg list");
      get_next_tok();
    }
  }
  get_next_tok(); // eat )
  return make_unique<Call_Expr_AST>(ident_name, std::move(args));
}

// primary ::= < ident_expr | num_expr | paren_expr >
static unique_ptr<Expr_AST> parse_prime() {
  switch (CURR_TOK) {
  default:
    return LOG_err("unknown token when expecting an expression");
  case tok_ident:
    return parse_ident_expr();
  case tok_num:
    return parse_num_expr();
  case '(':
    return parse_paren_expr();
  }
}

// after lhs is parsed [ + primary]
// binop_rhs ::= ('+' primary)*
static unique_ptr<Expr_AST> parse_binop_rhs(int expr_prec,
                                            unique_ptr<Expr_AST> lhs) {
  while (true) {
    int tok_prec = get_prec_of_tok();

    if (tok_prec < expr_prec)
      return lhs; // keep lhs if hiher prec

    int binop = CURR_TOK;
    get_next_tok(); // eat binop

    auto rhs = parse_prime();
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
static unique_ptr<Expr_AST> parse_expr() {
  auto lhs = parse_prime();
  if (!lhs)
    return nullptr;
  return parse_binop_rhs(0, std::move(lhs));
}

// prototype ::= id '(' id* ')'
static unique_ptr<Proto_AST> parse_proto() {
  if (CURR_TOK != tok_ident)
    return LOG_err_p("expected func name in prototype");

  string fn_name = IDENT_STR;
  get_next_tok(); // eat fn name, expect (

  if (CURR_TOK != '(')
    return LOG_err_p("Expected '(' in prototype");

  vector<string> arg_names;
  while (get_next_tok() == tok_ident)
    arg_names.push_back(IDENT_STR); // collect func args

  if (CURR_TOK != ')')
    return LOG_err_p("Expected ')' in prototype");

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

// #####################################################################################
// #
// #####################################################################################

// LLVM vars
static unique_ptr<llvm::LLVMContext> CTX;
static unique_ptr<llvm::Module> MODULE;
static unique_ptr<llvm::IRBuilder<>> IR_BLD;
static std::map<string, V *> NAMED_Vs;

V *LOG_err_v(const char *msg) {
  LOG_err(msg);
  return nullptr;
}

V *Num_Expr_AST::codegen() {
  return llvm::ConstantFP::get(*CTX, llvm::APFloat(value));
}

V *Var_Expr_AST::codegen() {
  V *v = NAMED_Vs[name];
  if (!v)
    LOG_err_v("Unknown var name");
  return v;
}

V *Bin_Expr_AST::codegen() {
  V *l = lhs->codegen();
  V *r = rhs->codegen();

  if (!l || !r)
    return nullptr;

  switch (op) {
  case '+':
    return IR_BLD->CreateFAdd(l, r, "add");
  case '-':
    return IR_BLD->CreateFSub(l, r, "sub");
  case '*':
    return IR_BLD->CreateFMul(l, r, "mul");
  case '<':
    l = IR_BLD->CreateFCmpULT(l, r, "cmp");
    return IR_BLD->CreateUIToFP(l, TP::getDoubleTy(*CTX), "bool");
  default:
    return LOG_err_v("invalid binary oeprator");
  } // sw
}

V *Call_Expr_AST::codegen() {
  FN *callee_fn = MODULE->getFunction(callee);
  if (!callee_fn)
    return LOG_err_v("Unknown func ref");

  if (callee_fn->arg_size() != args.size())
    return LOG_err_v("Incorrect # arguments passed");

  vector<V *> args_v;
  for (unsigned i = 0, e = args.size(); i != e; ++i) {
    args_v.push_back(args[i]->codegen());
    if (!args_v.back())
      return nullptr;
  }
  return IR_BLD->CreateCall(callee_fn, args_v, "call");
}

FN *Proto_AST::codegen() {
  vector<TP *> doubles(args.size(), TP::getDoubleTy(*CTX));
  FNT *ft = FNT::get(TP::getDoubleTy(*CTX), doubles, false);
  FN *f = FN::Create(ft, FN::ExternalLinkage, name, MODULE.get());

  unsigned idx = 0;
  for (auto &arg : f->args())
    arg.setName(args[idx++]);

  return f;
}

FN *Func_AST::codegen() {
  FN *fn = MODULE->getFunction(proto->getName());

  if (!fn)
    fn = proto->codegen();

  if (!fn)
    return nullptr;

  // if (!fn->empty())
  //   return (FN *)LOG_err_v("Func cannot be re-defined");

  BB *bb = BB::Create(*CTX, "entry", fn);
  IR_BLD->SetInsertPoint(bb);

  NAMED_Vs.clear();
  for (auto &arg : fn->args())
    NAMED_Vs[string(arg.getName())] = &arg;

  if (V *ret = body->codegen()) {
    IR_BLD->CreateRet(ret);
    llvm::verifyFunction(*fn);
    return fn;
  }

  fn->eraseFromParent();
  return nullptr;
}

// #####################################################################################
// # Top Level Parsing
// #####################################################################################

static void init_module() {
  CTX = make_unique<llvm::LLVMContext>();
  MODULE = make_unique<llvm::Module>("kaleidoscope jit", *CTX);
  IR_BLD = make_unique<llvm::IRBuilder<>>(*CTX);
}

static void handle_def() {
  if (auto fn_ast = parse_def()) {
    if (auto *fn_ir = fn_ast->codegen()) {
      fprintf(stderr, "Read function definition:\n\n");
      fn_ir->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else
    get_next_tok();
}

static void handle_extern() {
  if (auto ProtoAST = parse_extern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern:\n\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else
    get_next_tok();
}

static void handle_top_lev_expr() {
  if (auto fn_ast = parse_top_lev_expr()) {
    if (auto *fn_ir = fn_ast->codegen()) {
      fprintf(stderr, "Read top-level expression:\n\n");
      fn_ir->print(llvm::errs());
      fprintf(stderr, "\n");

      // Remove the anonymous expression.
      fn_ir->eraseFromParent();
    }
  } else
    get_next_tok();
}

// top ::= def | extern | expr | ';'
static void token_loop() {

  while (true) {
    fprintf(stderr, "ready> ");

    switch (CURR_TOK) {
    case tok_eof:
      return;
    case ';':
      get_next_tok();
      break;
    case tok_def:
      handle_def();
      break;
    case tok_extern:
      handle_extern();
      break;
    default:
      handle_top_lev_expr();
      break;
    } // sw
  } // wh
}

// #####################################################################################
// # MAIN
// #####################################################################################

int main() {
  // setup binops, 1 is lowest
  BINOP_PREC['<'] = 10;
  BINOP_PREC['+'] = 20;
  BINOP_PREC['-'] = 20;
  BINOP_PREC['*'] = 40; // highest

  fprintf(stderr, "ready> ");
  get_next_tok(); // first token

  init_module();
  token_loop();

  MODULE->print(llvm::errs(), nullptr);
  printf("\n");
}