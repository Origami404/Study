#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
using std::unique_ptr;
using std::string;
using std::map;
using std::move;
using namespace llvm;
//===--------------------------------------------------------------------==//
// AST
//===--------------------------------------------------------------------==//
namespace {
    class ExprAST {
        public:
        virtual ~ExprAST() {}
        virtual Value* codegen() = 0;
    };
    using ASTptr = unique_ptr<ExprAST>;
    class NumAST : public ExprAST { 
        double val;
        public: 
        NumAST(double v): val(v) {}
        virtual Value* codegen();
    };
    class VarAST : public ExprAST {
        string name;
        public:
        VarAST(const string& n): name(n) {}
        virtual Value* codegen();
    };
    class BinOpAST : public ExprAST {
        char op;
        unique_ptr<ExprAST> lhs, rhs;
        public:
        BinOpAST(char op, ASTptr LHS, ASTptr RHS) 
            : op(op), lhs(move(LHS)), rhs(move(RHS)) {}
        virtual Value* codegen();
    };
    class DefAST {
        string name;
        ASTptr val;
        public:
        DefAST(const string& name, ASTptr val) 
            : name(name), val(move(val)) {}
        void codegen();
    };
}
//===--------------------------------------------------------------------==//
// 辅助函数
//===--------------------------------------------------------------------==//
enum Token {
    tok_eof = -1,
    tok_identifier = -2,
    tok_number = -3,
    tok_def = -4,
    tok_end = -5
};

static string identifier_str;
static double num_val;

int gettok() {
    static int last_char = ' ';
    while (isspace(last_char))
        last_char = getchar();
    // identifier || def
    if (isalpha(last_char)) {
        identifier_str = last_char;
        while (isalnum(last_char = getchar()))
            identifier_str += last_char;
        // def
        if (identifier_str == "def")
            return tok_def;
        return tok_identifier;
    }
    // Number, a small bug
    if (isdigit(last_char) || last_char == '.') {
        string num_str;
        do {
            num_str += last_char;
            last_char = getchar();
        } while (isdigit(last_char) || last_char == '.');
        num_val = strtod(num_str.c_str(), nullptr);
        return tok_number;
    }
    // EOF
    if (last_char == EOF)
        return tok_eof;
    if (last_char == ';'){
        last_char = getchar();
        return tok_end;
    }
        
    // Operators 
    int this_char = last_char;
    last_char = getchar();
    return this_char;
}
int cur_tok;
int next_token() {
    return (cur_tok = gettok());
}
ASTptr LogError(const char* str) {
    fprintf(stderr, "Error: %s\n", str);
    return nullptr;
}

map<char, int> op_prec;
int get_prec(char c) {
    auto prec = op_prec.find(c);
    if (prec == op_prec.end())
        return -1;
    else return prec->second;
}


//===--------------------------------------------------------------------==//
// 解析器
//===--------------------------------------------------------------------==//

ASTptr parse_expression();
unique_ptr<DefAST> parse_define();
ASTptr parse_primary();
ASTptr parse_binop(int expr_prec, ASTptr lhs);
ASTptr parse_id();
ASTptr parse_num();
ASTptr parse_paren();

/// programmer ::= define | expression
// 同时承担程序入口的责任
void parse_programmer() {
    while (true) {
        printf("ready> ");
        switch (cur_tok) {
            case tok_eof: return;
            case tok_def: parse_define()->codegen(); break;
            case tok_end: break;
            default: parse_expression()->codegen()->print(errs());
            fprintf(stderr, "\n");
        }
        next_token();
    }
}
/// define ::= 'def' Id '=' expression
unique_ptr<DefAST> parse_define() {
    next_token(); // eat def
    auto name = identifier_str;
    next_token(); // eat '='
    next_token();
    auto val = parse_expression();
    return llvm::make_unique<DefAST>(DefAST(name, move(val)));
}
/// expr ::= primary [binop]
ASTptr parse_expression() {
    return parse_binop(0, parse_primary());
}
/// primary ::= Id | Num | paren
ASTptr parse_primary() {
    switch (cur_tok) {
        case tok_identifier:
            return parse_id();
        case tok_number: 
            return parse_num();
        case '(':
            return parse_paren();
        default:
            return LogError("Unknown token in primary");
    }
}
/// paren ::= '(' expression ')'
ASTptr parse_paren() {
    next_token(); // eat (
    auto expr = parse_expression();
    if (cur_tok != ')')
        return LogError("Unmatch paren");
    next_token(); // eat )
    return expr;
}
/// binop ::= primary Op binop
ASTptr parse_binop(int expr_prec, ASTptr lhs) {
    while (true) {
        int now_prec = get_prec(cur_tok);
        if (now_prec < expr_prec)
            return lhs;
            
        int binop = cur_tok;
        next_token(); 
        auto rhs = parse_primary();

        int next_prec = get_prec(cur_tok);
        if (now_prec < next_prec) 
            rhs = parse_binop(expr_prec + 1, move(rhs));
        lhs = llvm::make_unique<BinOpAST>(binop, move(lhs), move(rhs));
    }
}

ASTptr parse_num() {
    auto res = llvm::make_unique<NumAST>(num_val);
    next_token();
    return move(res);
}

ASTptr parse_id() {
    auto res = llvm::make_unique<VarAST>(identifier_str);
    next_token();
    return move(res);
}
//===--------------------------------------------------------------------==//
// IR生成
//===--------------------------------------------------------------------==//
static LLVMContext my_context;
static IRBuilder<> builder(my_context);
static unique_ptr<Module> my_module;
static map<string, Value *> named_vals;
Value *LogErrorV(const char *str) {
  LogError(str);
  return nullptr;
}

Value* NumAST::codegen() {
    return ConstantFP::get(my_context, APFloat(this->val));
}
Value* VarAST::codegen() {
    Value* v = named_vals[this->name];
    if (!v)
        return LogErrorV("undefine variable name");
    return v;
}
Value* BinOpAST::codegen() {
    Value* l = this->lhs->codegen();
    Value* r = this->rhs->codegen();
    if (!l || !r)
        return nullptr;
    switch (this->op) {
        case '+':
            return builder.CreateFAdd(l, r, "addtmp");
        case '-':
            return builder.CreateFSub(l, r, "subtmp");
        case '*':
            return builder.CreateFMul(l, r, "multmp");
        case '/':
            return builder.CreateFDiv(l, r, "divtmp");
        default: return LogErrorV("unexpect binop");
    }
}
void DefAST::codegen() {
    named_vals[this->name] = this->val->codegen();
}

//===--------------------------------------------------------------------==//
//===--------------------------------------------------------------------==//

int main() {
    op_prec['+'] = 10;
    op_prec['-'] = 10;
    op_prec['*'] = 20;
    op_prec['/'] = 20;
    printf("ready> ");
    next_token();
    parse_programmer();
    return 0;
}

