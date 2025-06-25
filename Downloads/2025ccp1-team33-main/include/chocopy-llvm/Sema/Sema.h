#ifndef CHOCOPY_LLVM_SEMA_SEMA_H
#define CHOCOPY_LLVM_SEMA_SEMA_H

#include "chocopy-llvm/AST/AST.h"
#include "chocopy-llvm/Sema/IdentifierResolver.h"

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>

namespace chocopy {
class Scope;
class ASTContext;
class DiagnosticsEngine;
class Lexer;
class ValueType;

class Sema {
  class Analysis;

public:
  Sema(Lexer &L, ASTContext &C);

  void initialize();
  void initializeGlobalScope();

public:
  DiagnosticsEngine &getDiagnosticEngine() const { return Diags; }

  void run();

private:
  std::shared_ptr<Scope> getGlobalScope() const { return GlobalScope; }
  void setGlobalScope(std::shared_ptr<Scope> S) { GlobalScope = std::move(S); }

  std::shared_ptr<Scope> getCurScope() const { return CurScope; }
  void setCurScope(std::shared_ptr<Scope> S) { CurScope = std::move(S); }

  template <class T>
  void handleDeclaration(T *D);
  
  void actOnPopScope(Scope *S);

  bool checkNonlocalDecl(NonLocalDecl *NLD);
  bool checkGlobalDecl(GlobalDecl *GD);
  bool checkSuperClass(ClassDef *D);
  void checkClassShadow(Declaration *D);
  bool checkWasInSuper(ClassDef *C, Declaration *D);
  bool handleInit(ClassDef* C, FuncDef* constructor);
  bool checkClassAttrs(ClassDef *D);
  bool checkMethodOverride(FuncDef *OM, FuncDef *M);
  bool checkClassDef(ClassDef *D);
  bool checkFirstMethodParam(ClassDef *CD, FuncDef *FD);
  bool checkAssignTarget(Expr *E);
  bool checkReturnStmt(ReturnStmt *S, ValueType *ERTy);
  bool checkReturnMissing(ArrayRef<Stmt*> Stmts);
  bool checkTypeAnnotation(ClassType *T);

  void actOnVarDef(VarDef *V);
  void actOnBinaryExpr(BinaryExpr *B);
  void actOnDeclRef(DeclRef *DR);

  Scope *getScopeForDecl(Scope *S, Declaration *D);

  ClassDef *getSuperClass(ClassDef *CD);

  bool isSameType(TypeAnnotation *TyA, TypeAnnotation *TyB);

  Declaration *lookupName(Scope *S, SymbolInfo *SI);
  Declaration *lookupClass(Scope *S, ClassType *CT);
  Declaration *lookupDecl(DeclRef *DR);

private:
  using Nonlocals = SmallVector<NonLocalDecl *, 4>;
  using Globals = SmallVector<GlobalDecl *, 4>;

private:
  Lexer &TheLexer;
  DiagnosticsEngine &Diags;
  ASTContext &Ctx;
  std::shared_ptr<Scope> GlobalScope;
  std::shared_ptr<Scope> CurScope;
  IdentifierResolver IdResolver;
};
} // namespace chocopy
#endif // CHOCOPY_LLVM_SEMA_SEMA_H
