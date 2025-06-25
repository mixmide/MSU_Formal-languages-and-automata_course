#ifndef CHOCOPY_PARSER_H
#define CHOCOPY_PARSER_H

#include "chocopy-llvm/AST/ASTContext.h"
#include "chocopy-llvm/AST/AST.h"
#include "chocopy-llvm/Lexer/Token.h"
#include "chocopy-llvm/Lexer/Lexer.h"
#include "chocopy-llvm/Sema/Sema.h"
#include <vector>

namespace chocopy {

class Parser {
public:
    Parser(ASTContext &C, Lexer &Lex, Sema &Acts);

    Program *parse();

private:
    DiagnosticsEngine &Diags;
    ASTContext &Context;
    Lexer &TheLexer;
    Token Tok;

    bool consumeToken(tok::TokenKind ExpectedTok);
    bool consumeToken();
    bool expect(tok::TokenKind ExpectedTok);
    bool expectAndConsume(tok::TokenKind ExpectedTok);
    void skipToNextLine();
    void emitUnexpected();
    const Token &getLookAheadToken(int N);

    Stmt *parseStmtDemo();
    Stmt *parseAssignOrExprDemo();
    Stmt *parseForLoop();
    Stmt *parseWhileLoop();
    Stmt *parseIf();

    Expr *parseExprDemo();
    Expr *parseUnaryOperation();
    Expr *parseBinaryExpr(int minPrecedence);
    int getOperatorPrecedence(const Token &t);
    BinaryExpr::OpKind getOpKind(const Token &t);
    Expr *parseFnCallDemo();
    Expr *parseAtomicExprDemo();

    TypeAnnotation *parseType();
    VarDef *parseVarDef();
    GlobalDecl *parseGlobalDecl();
    NonLocalDecl *parseNonLocalDecl();
    ClassDef *parseClassDeclaration();
    FuncDef *parseFunctionDef();
    Literal *parseLiteral();

    bool parseBlock(StmtList &body);
    bool parseFunctionParameters(ParamDeclList &params);
    bool parseBodyBlock(StmtList &body, DeclList &decls);
};
} // namespace chocopy
#endif // CHOCOPY_PARSER_H
