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
  static int last_char = ' ';

  // skip whitespace
  while (isspace(last_char))
    last_char = getchar();

  // ident ∧ kw ∊ [a-zA-Z][a-zA-Z0-9]*
  if (isalpha(last_char)) {
    IDENT_STR = last_char;

    // build a full word
    while (isalnum(last_char = getchar()))
      IDENT_STR += last_char;

    // either kw ∊ {def extern} or identifier
    if (IDENT_STR == "def")
      return tok_def;
    if (IDENT_STR == "extern")
      return tok_extern;
    return tok_ident;
  }

  // number [0-9.]+
  if (isdigit(last_char) || last_char == '.') {
    string num_str;
    do {
      num_str += last_char;
      last_char = getchar();
    } while (isdigit(last_char) || last_char == '.');

    NUM_VAL = strtod(num_str.c_str(), nullptr);
    return tok_num;
  }

  // comments till end of line
  if (last_char == '#') {
    do
      last_char = getchar();
    while (last_char != EOF && last_char != '\n' && last_char != '\r');

    if (last_char != EOF)
      return get_tok();
  }

  // keep EOF
  if (last_char == EOF)
    return tok_eof;

  // return last char
  int this_char = last_char;
  last_char = getchar();
  return this_char;
}

// #####################################################################################
// # AST
// #####################################################################################

namespace {

class ExprAST {
public:
  virtual ~ExprAST() = default;
};

class NumExprAST : public ExprAST {
  double value;

public:
  NumExprAST(double value) : value(value) {}
};

class VarExprAST : public ExprAST {
  string name;

public:
  VarExprAST(const string &name) : name(name) {}
};

class BinExprAST : public ExprAST {
  char op;
  unique_ptr<ExprAST> lhs, rhs;

public:
  BinExprAST(char op, unique_ptr<ExprAST> lhs, unique_ptr<ExprAST> rhs)
      : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
};

class CallExprAST : public ExprAST {
  string callee;
  vector<unique_ptr<ExprAST>> args;

public:
  CallExprAST(const string &callee, vector<unique_ptr<ExprAST>> args)
      : callee(callee), args(std::move(args)) {}
};

class ProtoAST {
  string name;
  vector<string> args;

public:
  ProtoAST(const string &name, vector<string> args)
      : name(name), args(std::move(args)) {}

  const string &get_name() const { return name; }
};

class FuncAST {
  unique_ptr<ProtoAST> proto;
  unique_ptr<ExprAST> body;

public:
  FuncAST(unique_ptr<ProtoAST> proto, unique_ptr<ExprAST> body)
      : proto(std::move(proto)), body(std::move(body)) {}
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
  if (isascii(CURR_TOK))
    return -1;

  int tok_prec = BINOP_PREC[CURR_TOK];
  if (tok_prec <= 0)
    return -1;
  return tok_prec;
}

// log error
unique_ptr<ExprAST> log_err(const char *msg) {
  fprintf(stderr, "Error: %s\n", msg);
  return nullptr;
}

unique_ptr<ProtoAST> log_err_p(const char *msg) {
  log_err(msg);
  return nullptr;
}

static unique_ptr<ExprAST> parse_expr();

// num ::= num
static unique_ptr<ExprAST> parse_num_expr() {
  auto res = make_unique<NumExprAST>(NUM_VAL);
  get_next_tok();
  return std::move(res);
}

// paren_expr ::= '(' expr ')'
static unique_ptr<ExprAST> parse_paren_expr() {
  get_next_tok(); // eat (
  auto v = parse_expr();
  if (!v)
    return nullptr;
  if (CURR_TOK != ')')
    return log_err("expected ')'");
  get_next_tok(); // eat )
  return v;
}

// ident_expr, ::= ident, ::= ident '(' expr* ')'
static unique_ptr<ExprAST> parse_ident_expr() {
  string ident_name = IDENT_STR;
  get_next_tok(); // eat ident

  if (CURR_TOK != '(')
    return make_unique<VarExprAST>(ident_name);

  get_next_tok(); // eat (
  vector<unique_ptr<ExprAST>> args;
  if (CURR_TOK != ')') {
    while (true) {
      if (auto arg = parse_expr())
        args.push_back(std::move(arg));
      else
        return nullptr;

      if (CURR_TOK == ')')
        break;

      if (CURR_TOK != ',')
        return log_err("Expected ')' or ',' in arg list");
      get_next_tok();
    }
  }
  get_next_tok(); // eat )
  return make_unique<CallExprAST>(ident_name, std::move(args));
}

// primary ::= < ident_expr | num_expr | paren_expr >
static unique_ptr<ExprAST> parse_prime() {
  switch (CURR_TOK) {
  default:
    return log_err("unknown token when expecting an expression");
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
static unique_ptr<ExprAST> parse_binop_rhs(int expr_prec,
                                           unique_ptr<ExprAST> lhs) {
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
    lhs = make_unique<BinExprAST>(binop, std::move(lhs), std::move(rhs));
  } // while
}

// expr ::= primary binop_rhs
static unique_ptr<ExprAST> parse_expr() {
  auto lhs = parse_prime();
  if (!lhs)
    return nullptr;
  return parse_binop_rhs(0, std::move(lhs));
}

// prototype ::= id '(' id* ')'
static unique_ptr<ProtoAST> parse_proto() {
  if (CURR_TOK != tok_ident)
    return log_err_p("expected func name in prototype");

  string fn_name = IDENT_STR;
  get_next_tok(); // eat fn name, expect (

  if (CURR_TOK != '(')
    return log_err_p("Expected '(' in prototype");

  vector<string> arg_names;
  while (get_next_tok() == tok_ident)
    arg_names.push_back(IDENT_STR); // collect func args

  if (CURR_TOK != ')')
    return log_err_p("Expected ')' in prototype");

  get_next_tok(); // eat )
  return make_unique<ProtoAST>(fn_name, std::move(arg_names));
}

// definition ::= 'def' proto
static unique_ptr<FuncAST> parse_def() {
  get_next_tok(); // eat def
  auto proto = parse_proto();
  if (!proto)
    return nullptr;

  if (auto body = parse_expr())
    return make_unique<FuncAST>(std::move(proto), std::move(body));
  return nullptr;
}

// extern ::= 'extern' proto
static unique_ptr<ProtoAST> parse_extern() {
  get_next_tok(); // eat extern
  return parse_proto();
}

// top_level_expr ::= expr
static unique_ptr<FuncAST> parse_top_lev_expr() {
  if (auto body = parse_expr()) {
    auto proto = make_unique<ProtoAST>("__anon_expr", vector<string>());
    return make_unique<FuncAST>(std::move(proto), std::move(body));
  }
  return nullptr;
}

// #####################################################################################
// # Top Level Parsing
// #####################################################################################

void handle_def() {
  if (parse_def())
    fprintf(stderr, "Parsed a func def.\n");
  else
    get_next_tok();
}

void handle_extern() {
  if (parse_extern())
    fprintf(stderr, "Parsed an extern\n");
  else
    get_next_tok();
}

void handle_top_lev_expr() {
  if (parse_top_lev_expr())
    fprintf(stderr, "Parsed a top-leve expr\n");
  else
    get_next_tok();
}

// top ::= def | extern | expr | ';'
static void token_loop() {

  get_next_tok(); // first token

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

  // prime
  fprintf(stderr, "ready> ");

  token_loop();
// 
  printf("\n");
}