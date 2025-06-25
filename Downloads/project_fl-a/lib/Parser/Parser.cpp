#include "chocopy-llvm/Parser/Parser.h"
#include "chocopy-llvm/AST/ASTContext.h"
#include "chocopy-llvm/Basic/Diagnostic.h"
#include "chocopy-llvm/Sema/Scope.h"
#include "chocopy-llvm/Sema/Sema.h"
#include <llvm/ADT/APInt.h>
#include <stack>
#include <iostream>

namespace chocopy 
{

Parser::Parser(ASTContext &C, Lexer &Lex, Sema &Acts)
    : Diags(Lex.getDiagnostics()), 
      Context(C), 
      TheLexer(Lex) {}

// функции для работы с токенами
bool Parser::consumeToken(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
        TheLexer.lex(Tok);
        return true;
    }
    return false;
}

bool Parser::consumeToken() {
    TheLexer.lex(Tok);
    return true;
}

bool Parser::expect(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
        return true;
    }
    Diags.emitError(Tok.getLocation().Start, diag::err_near_token) << Tok;
    Diags.emitError(Tok.getLocation().Start, diag::err_expected) 
        << tok::getTokenName(ExpectedTok);
    return false;
}

bool Parser::expectAndConsume(tok::TokenKind ExpectedTok) {
    if (expect(ExpectedTok)) {
        return consumeToken();
    }
    return false;
}

void Parser::skipToNextLine() {
    while (!Tok.is(tok::eof) && !consumeToken(tok::NEWLINE)) {
        consumeToken();
    }
}

void Parser::emitUnexpected() {
    Diags.emitError(Tok.getLocation().Start, diag::err_unexpected) << Tok;
}

const Token &Parser::getLookAheadToken(int N) {
    assert(N);
    return TheLexer.LookAhead(N - 1);
}

// парсинг проги
Program *Parser::parse() {
    DeclList decls;
    StmtList stmts;

    auto checkVarDef = [this](Token &curTok) {
        // deleted tok::idstring due to grammar contradiction
        bool isVar = curTok.is(tok::identifier)
                     && TheLexer.LookAhead(0).is(tok::colon);
        return isVar;
    };

    consumeToken();
    while (checkVarDef(Tok) || Tok.is(tok::kw_class) || Tok.is(tok::kw_def)) {
        if (VarDef *var = parseVarDef()) {
            decls.push_back(var);
        } else if (ClassDef *cls = parseClassDeclaration()) {
            decls.push_back(cls);
        } else if (FuncDef *func = parseFunctionDef()) {
            decls.push_back(func);
        } else {
            skipToNextLine();
        }
    }

    while (Tok.isNot(tok::eof)) {
        if (Stmt *stmt = parseStmtDemo()) {
            stmts.push_back(stmt);
        } else {
            skipToNextLine();
        }
    }

    return Context.createProgram(decls, stmts);
}

// for ID in 'expr' : 'block'
Stmt *Parser::parseForLoop() {
    if (!Tok.is(tok::kw_for)) return nullptr;

    SMLoc beginLoc = Tok.getLocation().Start;
    consumeToken();

    if (!Tok.is(tok::identifier)) return nullptr;
    SMRange idRange = Tok.getLocation();
    SymbolInfo *idInfo = Tok.getSymbolInfo();
    DeclRef *loopVar = Context.createDeclRef(idRange, idInfo);
    consumeToken();

    if (!expectAndConsume(tok::kw_in)) return nullptr;

    Expr *iterableExpr = parseExprDemo();
    if (!iterableExpr) return nullptr;

    if (!expectAndConsume(tok::colon) || 
        !expectAndConsume(tok::NEWLINE) || 
        !expectAndConsume(tok::INDENT)) {
        return nullptr;
    }

    StmtList loopBody;
    while (Tok.isNot(tok::DEDENT)) {
        if (Stmt *bodyStmt = parseStmtDemo()) {
            loopBody.push_back(bodyStmt);
        } else {
            skipToNextLine();
        }
    }

    if (!expectAndConsume(tok::DEDENT)) return nullptr;

    SMLoc endLoc = Tok.getLocation().Start;
    return Context.createForStmt(
        SMRange(beginLoc, endLoc), 
        loopVar, 
        iterableExpr, 
        loopBody
    );
}


//парсинг while
// while 'expr' : 'block'
Stmt *Parser::parseWhileLoop() {
    if (!Tok.is(tok::kw_while)) return nullptr;

    SMLoc startPos = Tok.getLocation().Start;
    consumeToken();

    Expr *condExpr = parseExprDemo();
    if (!condExpr) return nullptr;

    if (!expectAndConsume(tok::colon)) return nullptr;
    
    StmtList bodyStmts;
    if (!parseBlock(bodyStmts)) return nullptr;

    SMLoc endLoc = Tok.getLocation().Start;
    return Context.createWhileStmt(
        SMRange(startPos, endLoc), 
        condExpr, 
        bodyStmts
    );
}

// парсинг if
// if 'expr' : 'block' [elif 'expr' : 'block']* [else : 'block']?
Stmt *Parser::parseIf() {
    SMLoc startLoc = Tok.getLocation().Start;
    consumeToken();

    Expr *condition = parseExprDemo();
    if (!condition) return nullptr;

    if (!expectAndConsume(tok::colon)) return nullptr;
    
    StmtList thenStmts;
    if (!parseBlock(thenStmts)) return nullptr;
    
    StmtList elseStmts;
    if (Tok.is(tok::kw_elif)) {
        elseStmts.push_back(parseIf());
    } else if (Tok.is(tok::kw_else)) {
        consumeToken();
        if (!expectAndConsume(tok::colon)) return nullptr;
    
        if (!parseBlock(elseStmts)) return nullptr;
    }

    SMLoc endLoc = Tok.getLocation().Start;
    return Context.createIfStmt(
        SMRange(startLoc, endLoc), 
        condition, 
        thenStmts, 
        elseStmts
    );
}

 // Парсинг операторов
Stmt *Parser::parseStmtDemo() {
    if (Tok.is(tok::kw_pass)) {
        consumeToken();
        return nullptr;
    }
    if (Tok.is(tok::kw_if)) {
        return parseIf();
    }
    if (Tok.is(tok::kw_while)) {
        return parseWhileLoop();
    }
    if (Tok.is(tok::kw_for)) {
        return parseForLoop();
    }

    Stmt *stmt = parseAssignOrExprDemo();
    if (stmt && expectAndConsume(tok::NEWLINE)) {
        return stmt;
    }

    return nullptr;
}

// парсинг присваиваний/выражений
Stmt *Parser::parseAssignOrExprDemo() {
    auto isValidTarget = [](const Expr *expr) {
        return llvm::isa<DeclRef>(expr) || 
               llvm::isa<IndexExpr>(expr) || 
               llvm::isa<MemberExpr>(expr);
    };

    SMLoc stmtStart = Tok.getLocation().Start;
    ExprList targets;
    Expr *resultExpr = nullptr;

    do {
        if (resultExpr)
            targets.push_back(resultExpr);

        if (Tok.is(tok::kw_return)) {
            SMLoc retLoc = Tok.getLocation().Start;
            consumeToken();
            resultExpr = parseExprDemo();
            return Context.createReturnStmt(
                SMRange(retLoc, resultExpr ? resultExpr->getLocation().End : retLoc), 
                resultExpr
            );
        }
        resultExpr = parseExprDemo();
        if (!resultExpr) return nullptr;
    } while (isValidTarget(resultExpr) && consumeToken(tok::equal));

    if (!expect(tok::NEWLINE)) return nullptr;

    SMLoc stmtEnd = Tok.getLocation().Start;
    SMRange stmtRange(stmtStart, stmtEnd);
    if (!targets.empty()) {
        return Context.createAssignStmt(
            stmtRange, 
            targets, 
            resultExpr
        );
    }
    return Context.createExprStmt(
        stmtRange, 
        resultExpr
    );
}

//функции для парсинга выражений
int Parser::getOperatorPrecedence(const Token &t) {
    switch (t.getKind()) {
        case tok::kw_or: return 1;
        case tok::kw_and: return 2;
        case tok::kw_is:
        case tok::less:
        case tok::lessequal:
        case tok::equalequal:
        case tok::exclaimequal:
        case tok::greaterequal:
        case tok::greater: return 3;
        case tok::plus:
        case tok::minus: return 4;
        case tok::star:
        case tok::slashslash:
        case tok::percent: return 5;
        default: return 0;
    }
}

BinaryExpr::OpKind Parser::getOpKind(const Token &t) {
    switch (t.getKind()) {
        case tok::plus: return BinaryExpr::OpKind::Add;
        case tok::minus: return BinaryExpr::OpKind::Sub;
        case tok::star: return BinaryExpr::OpKind::Mul;
        case tok::slashslash: return BinaryExpr::OpKind::FloorDiv;
        case tok::percent: return BinaryExpr::OpKind::Mod;
        case tok::kw_and: return BinaryExpr::OpKind::And;
        case tok::kw_or: return BinaryExpr::OpKind::Or;
        case tok::kw_is: return BinaryExpr::OpKind::Is;
        case tok::less: return BinaryExpr::OpKind::LCmp;
        case tok::lessequal: return BinaryExpr::OpKind::LEqCmp;
        case tok::equalequal: return BinaryExpr::OpKind::EqCmp;
        case tok::exclaimequal: return BinaryExpr::OpKind::NEqCmp;
        case tok::greaterequal: return BinaryExpr::OpKind::GEqCmp;
        case tok::greater: return BinaryExpr::OpKind::GCmp;
        default: return BinaryExpr::OpKind::Add;    
    }
}

Expr *Parser::parseBinaryExpr(int minPrecedence) {
    Expr *left = parseUnaryOperation();
    if (!left) return nullptr;

    while (true) {
        int opPrecedence = getOperatorPrecedence(Tok);
        if (opPrecedence < minPrecedence) break;

        BinaryExpr::OpKind opKind = getOpKind(Tok);
        consumeToken();

        Expr *right = parseBinaryExpr(opPrecedence + 1);
        if (!right) return nullptr;

        left = Context.createBinaryExpr(
            SMRange(left->getLocation().Start, right->getLocation().End),
            left, opKind, right
        );
    }
    return left;
}

Expr *Parser::parseUnaryOperation() {
    if (Tok.is(tok::minus) || Tok.is(tok::kw_not)) {
        SMLoc opLoc = Tok.getLocation().Start;
        UnaryExpr::OpKind opKind = Tok.is(tok::minus) ? UnaryExpr::OpKind::Minus : UnaryExpr::OpKind::Not;
        consumeToken();
        Expr *operand = parseUnaryOperation();
        if (!operand) return nullptr;
        return Context.createUnaryExpr(
            SMRange(opLoc, operand->getLocation().End),
            opKind, operand
        );
    }
    return parseFnCallDemo();
}

Expr *Parser::parseExprDemo() {
    Expr *left = parseBinaryExpr(1); // Начинаем с минимального приоритета
    if (!left) return nullptr;

    if (Tok.is(tok::kw_if)) {
        consumeToken();
        Expr *condition = parseBinaryExpr(1);
        if (!condition || !expectAndConsume(tok::kw_else)) return nullptr;
        Expr *elseBranch = parseExprDemo();
        if (!elseBranch) return nullptr;
        return Context.createIfExpr(
            SMRange(left->getLocation().Start, elseBranch->getLocation().End),
            condition, left, elseBranch
        );
    }
    return left;
}

//парсинг вызовов функций и методов
Expr *Parser::parseFnCallDemo() {
    Expr *baseExpr = parseAtomicExprDemo();
    if (!baseExpr) return nullptr;
    
    auto keepParsing = [](const Token &t) {
        return !t.is(tok::eof) && 
               (t.is(tok::l_paren) || t.is(tok::l_square) || t.is(tok::period));
    };

    auto parseArguments = [this](ExprList &argsList) {
        do {
            Expr *arg = parseExprDemo();
            if (!arg) return false;
            argsList.push_back(arg);
        } while (consumeToken(tok::comma));
        return true;
    };

    while (keepParsing(Tok)) {
        if (Tok.is(tok::l_square)) {
            SMLoc exprStart = baseExpr->getLocation().Start;
            consumeToken();
            Expr *idx = parseExprDemo();
            if (!idx || !expect(tok::r_square)) return nullptr;
            baseExpr = Context.createIndexExpr(
                SMRange(exprStart, Tok.getLocation().End), 
                baseExpr, 
                idx
            );
            consumeToken();
            continue;
        }

        if (DeclRef *ref = dyn_cast<DeclRef>(baseExpr)) {
            if (Tok.is(tok::l_paren)) {
                ExprList callArgs;
                consumeToken();
                if (Tok.isNot(tok::r_paren)) {
                    if (!parseArguments(callArgs)) return nullptr;
                }
                if (!expectAndConsume(tok::r_paren)) return nullptr;

                baseExpr = Context.createCallExpr(
                    SMRange(ref->getLocation().Start, Tok.getLocation().Start),
                    ref, 
                    callArgs
                );
                continue;
            }
        }

        if (Tok.is(tok::period)) {
            consumeToken();
            DeclRef *member = Context.createDeclRef(
                Tok.getLocation(), 
                Tok.getSymbolInfo()
            );
            consumeToken();
            if (!member) return nullptr;
            baseExpr = Context.createMemberExpr(
                SMRange(baseExpr->getLocation().Start, member->getLocation().End),
                baseExpr, 
                member
            );
            if (consumeToken(tok::l_paren)) {
                ExprList methodArgs;
                if (Tok.isNot(tok::r_paren)) {
                    if (!parseArguments(methodArgs)) return nullptr;
                }
                if (!expectAndConsume(tok::r_paren)) return nullptr;

                baseExpr = Context.createMethodCallExpr(
                    SMRange(baseExpr->getLocation().Start, Tok.getLocation().Start),
                    cast_if_present<MemberExpr>(baseExpr), 
                    methodArgs
                );
            }
        }
    }
    return baseExpr;
}

// парсинг атомарных выражений
Expr *Parser::parseAtomicExprDemo() {
  SMRange pos = Tok.getLocation();
  tok::TokenKind tokenType = Tok.getKind();

  if (tokenType == tok::eof || tokenType == tok::BADENT) {
      return nullptr;
  }

  if (tokenType == tok::identifier) {
      Expr *expr = Context.createDeclRef(pos, Tok.getSymbolInfo());
      consumeToken();
      return expr;
  }
    if (tokenType == tok::kw_True || tokenType == tok::kw_False) {
        bool val = tokenType == tok::kw_True;
        consumeToken();
        return Context.createBooleanLiteral(pos, val);
    }
    if (tokenType == tok::kw_None) {
        consumeToken();
        return Context.createNoneLiteral(pos);
    }
    if (tokenType == tok::string_literal || tokenType == tok::idstring) {
        StringRef strData = Tok.getLiteralData();
        consumeToken();
        return Context.createStringLiteral(pos, strData);
    }
    if (tokenType == tok::integer_literal) {
        llvm::APInt num(32, Tok.getLiteralData(), 10);
        consumeToken();
        return Context.createIntegerLiteral(pos, num.getSExtValue());
    }
    if (tokenType == tok::l_square) {
      consumeToken(); // пропускаем [
      SMLoc startPos = pos.Start;
      ExprList elements;
      if (Tok.isNot(tok::r_square)) { // если список не пустой
          do {
              Expr *item = parseExprDemo(); // используем parseExprDemo для поддержки любых выражений
              if (!item) return nullptr;
              elements.push_back(item);
          } while (consumeToken(tok::comma)); // продолжаем пока есть запятые
      }
      if (!expectAndConsume(tok::r_square)) return nullptr; // ожидаем ]
      return Context.createListExpr(
          SMRange(startPos, Tok.getLocation().Start), 
          elements
      );
  }
    if (tokenType == tok::l_paren) {
        consumeToken();
        Expr *innerExpr = parseExprDemo();
        if (!innerExpr || !expectAndConsume(tok::r_paren)) return nullptr;
        return innerExpr;
    }

    emitUnexpected();
    return nullptr;
}

// парсинг аннотаций типов
TypeAnnotation *Parser::parseType() {
    SMRange typeLoc = Tok.getLocation();
    tok::TokenKind kind = Tok.getKind();

    if (kind == tok::identifier) {
        StringRef typeName = Tok.getSymbolInfo()->getName();
        consumeToken();
        return Context.createClassType(typeLoc, typeName);
    }
    if (kind == tok::idstring)
    {
        StringRef typeStr = Tok.getLiteralData();
        consumeToken();
        return Context.createClassType(typeLoc, typeStr);
    }
    if (kind == tok::l_square) 
    {
        consumeToken();
        TypeAnnotation *innerType = parseType();
        if (!innerType || !expectAndConsume(tok::r_square)) return nullptr;
        typeLoc = SMRange(typeLoc.Start, Tok.getLocation().End);
        return Context.createListType(typeLoc, innerType);
    }
    return nullptr;
}

// парсинг nonlocal объявлений
NonLocalDecl *Parser::parseNonLocalDecl() {
    if (!Tok.is(tok::kw_nonlocal)) return nullptr;

    SMLoc start = Tok.getLocation().Start;
    consumeToken();
    SymbolInfo *nameInfo = Tok.getSymbolInfo();
    SMRange namePos = Tok.getLocation();
    Identifier *name = Context.createIdentifier(namePos, nameInfo);
    consumeToken();
    consumeToken(tok::NEWLINE);
    
    return Context.createNonLocalDecl(
        SMRange(start, name->getLocation().End), 
        name
    );
}

//арсинг global объявлений
GlobalDecl *Parser::parseGlobalDecl() {
    SMLoc begin = Tok.getLocation().Start;
    consumeToken();

    SymbolInfo *varInfo = Tok.getSymbolInfo();
    SMRange varLoc = Tok.getLocation();
    Identifier *var = Context.createIdentifier(varLoc, varInfo);
    consumeToken();
    consumeToken(tok::NEWLINE);
    
    return Context.createGlobalDecl(
        SMRange(begin, var->getLocation().End), 
        var
    );
}

//арсинг объявлений переменных
VarDef *Parser::parseVarDef() {
    if (!Tok.is(tok::identifier)) return nullptr;
    SymbolInfo *varName = Tok.getSymbolInfo();
    SMRange varPos = Tok.getLocation();
    consumeToken();
    if (!expectAndConsume(tok::colon)) return nullptr;
    TypeAnnotation *varType = parseType();
    if (!expectAndConsume(tok::equal)) return nullptr;

    Literal *value = parseLiteral();
    if (value && expectAndConsume(tok::NEWLINE)) {
        SMLoc endPos = value->getLocation().End;
        SMRange range(varPos.Start, endPos);
        Identifier *id = Context.createIdentifier(varPos, varName);
        return Context.createVarDef(range, id, varType, value);
    }
    return nullptr;
}

// это вспомогательная функция для парсинга параметров функций
bool Parser::parseFunctionParameters(ParamDeclList &params) {
    while (Tok.isNot(tok::r_paren)) {
        if (!Tok.is(tok::identifier)) {
            Diags.emitError(Tok.getLocation().Start, diag::err_expected) << "identifier";
            return false;
        }

        SMRange paramLoc = Tok.getLocation();
        Identifier *paramId = Context.createIdentifier(paramLoc, Tok.getSymbolInfo());
        consumeToken();

        if (!expectAndConsume(tok::colon)) return false;

        TypeAnnotation *paramType = parseType();
        if (!paramType) return false;

        params.push_back(Context.createParamDecl(
            SMRange(paramLoc.Start, Tok.getLocation().Start), 
            paramId, 
            paramType
        ));

        if (!Tok.is(tok::r_paren) && !consumeToken(tok::comma)) {
            Diags.emitError(Tok.getLocation().Start, diag::err_expected) << ",";
            return false;
        }
    }
    return expectAndConsume(tok::r_paren);
}

// NEWLINE INDENT stmt+ DEDENT
bool Parser::parseBlock(StmtList &body) {
  if (!expectAndConsume(tok::NEWLINE) || 
      !expectAndConsume(tok::INDENT)) {
      return false;
  }
  while (!Tok.is(tok::DEDENT) && !Tok.is(tok::eof) && !Tok.is(tok::BADENT)) {
      if (Stmt *stmt = parseStmtDemo()) {
          body.push_back(stmt);
      } else {
          skipToNextLine();
      }
  }
  if (Tok.is(tok::BADENT) || Tok.is(tok::eof)) {
      return false; // Прерываем парсинг блока при ошибке
  }
  return expectAndConsume(tok::DEDENT);
}

// это вспомогательная функция для парсинга тела функции или класса
bool Parser::parseBodyBlock(StmtList &body, DeclList &decls) {
    if (!expectAndConsume(tok::colon) || 
        !expectAndConsume(tok::NEWLINE) || 
        !expectAndConsume(tok::INDENT)) {
        return false;
    }

    auto isVarDef = [this](Token &t) {
        return t.isOneOf(tok::identifier, tok::idstring) && 
               TheLexer.LookAhead(0).is(tok::colon);
    };

    while (Tok.isNot(tok::DEDENT)) {
        switch (Tok.getKind()) {
        case tok::kw_global:
            if (GlobalDecl *g = parseGlobalDecl()) decls.push_back(g);
            else skipToNextLine();
            break;
        case tok::kw_nonlocal:
            if (NonLocalDecl *n = parseNonLocalDecl()) decls.push_back(n);
            else skipToNextLine();
            break;
        case tok::kw_class:
            if (ClassDef *c = parseClassDeclaration()) decls.push_back(c);
            else skipToNextLine();
            break;
        case tok::kw_def:
            if (FuncDef *f = parseFunctionDef()) decls.push_back(f);
            else skipToNextLine();
            break;
        default:
            if (isVarDef(Tok)) {
                if (VarDef *v = parseVarDef()) decls.push_back(v);
                else skipToNextLine();
            } else if (Stmt *s = parseStmtDemo()) {
                body.push_back(s);
            } else {
                skipToNextLine();
            }
            break;
        }
    }
    return expectAndConsume(tok::DEDENT);
}

// парсинг определений классов
ClassDef *Parser::parseClassDeclaration() {
    if (!Tok.is(tok::kw_class)) return nullptr;
    SMRange classPos = Tok.getLocation();
    consumeToken();

    if (!Tok.is(tok::identifier)) {
        Diags.emitError(Tok.getLocation().Start, diag::err_expected) << "identifier";
        return nullptr;
    }
    Identifier *className = Context.createIdentifier(Tok.getLocation(), Tok.getSymbolInfo());
    consumeToken();

    if (!expectAndConsume(tok::l_paren) || !Tok.is(tok::identifier)) {
        Diags.emitError(Tok.getLocation().Start, diag::err_expected) << "identifier";
        return nullptr;
    }
    Identifier *super = Context.createIdentifier(Tok.getLocation(), Tok.getSymbolInfo());
    consumeToken();
    if (!expectAndConsume(tok::r_paren)) return nullptr;

    DeclList members;
    StmtList dummyBody;
    if (!parseBodyBlock(dummyBody, members)) return nullptr;

    SMLoc endLoc = members.empty() ? classPos.End : members.back()->getLocation().End;
    return Context.createClassDef(SMRange(classPos.Start, endLoc), className, super, members);
}

// парсинг определений функций
FuncDef *Parser::parseFunctionDef() {
    if (!Tok.is(tok::kw_def)) return nullptr;
    SMLoc funcBegin = Tok.getLocation().Start;
    consumeToken();

    if (!Tok.is(tok::identifier)) {
        Diags.emitError(Tok.getLocation().Start, diag::err_expected) << "identifier";
        return nullptr;
    }
    Identifier *funcName = Context.createIdentifier(Tok.getLocation(), Tok.getSymbolInfo());
    consumeToken();

    if (!expectAndConsume(tok::l_paren)) return nullptr;
    ParamDeclList params;
    if (!parseFunctionParameters(params)) return nullptr;

    TypeAnnotation *retType = Context.createClassType(
        SMRange(Tok.getLocation().Start, Tok.getLocation().Start), StringRef("<None>")
    );
    if (Tok.is(tok::arrow)) {
        consumeToken();
        retType = parseType();
        if (!retType) return nullptr;
    }

    StmtList funcBody;
    DeclList localDecls;
    if (!parseBodyBlock(funcBody, localDecls)) return nullptr;

    SMLoc funcEnd = funcBody.empty() && localDecls.empty() 
        ? Tok.getLocation().Start 
        : (funcBody.empty() ? localDecls.back()->getLocation().End : funcBody.back()->getLocation().End);
    return Context.createFuncDef(
        SMRange(funcBegin, funcEnd), 
        funcName, 
        params, 
        retType, 
        localDecls, 
        funcBody
    );
}

// парсинг литералов
Literal *Parser::parseLiteral() {
    SMRange litLoc = Tok.getLocation();

    if (consumeToken(tok::kw_None))
        return Context.createNoneLiteral(litLoc);
    if (consumeToken(tok::kw_True))
        return Context.createBooleanLiteral(litLoc, true);
    if (consumeToken(tok::kw_False))
        return Context.createBooleanLiteral(litLoc, false);
    if (Tok.is(tok::integer_literal)) {
        llvm::APInt val(32, Tok.getLiteralData(), 10);
        consumeToken();
        return Context.createIntegerLiteral(litLoc, val.getSExtValue());
    }
    if (Tok.isOneOf(tok::idstring, tok::string_literal)) {
        StringRef strVal = Tok.getLiteralData();
        consumeToken();
        return Context.createStringLiteral(litLoc, strVal);
    }

    Diags.emitError(Tok.getLocation().Start, diag::err_near_token) << Tok;
    return nullptr;
}

} // namespace chocopy

// ГОООЛЛ МЫ ЭТО СДЕЛАЛИ!