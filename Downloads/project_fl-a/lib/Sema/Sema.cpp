#include "chocopy-llvm/Sema/Sema.h"
#include "chocopy-llvm/AST/ASTContext.h"
#include "chocopy-llvm/AST/RecursiveASTVisitor.h"
#include "chocopy-llvm/AST/Type.h"
#include "chocopy-llvm/Analysis/CFG.h"
#include "chocopy-llvm/Basic/Diagnostic.h"
#include "chocopy-llvm/Lexer/Lexer.h"
#include "chocopy-llvm/Sema/Scope.h"

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/TypeSwitch.h>

namespace chocopy {
static raw_ostream &operator<<(raw_ostream &Stream, const Type &T) {
  llvm::TypeSwitch<const Type *>(&T)
      .Case([&Stream](const FuncType *FT) {
        Stream << "(";
        for (ValueType *T : FT->getParametersTypes()) {
          Stream << T;
          if (T != FT->getParametersTypes().back())
            Stream << ", ";
        }
        Stream << ") -> " << FT->getReturnType();
      })
      .Case([&Stream](const ClassValueType *CVT) {
        Stream << CVT->getClassName();
      })
      .Case([&Stream](const ListValueType *LVT) {
        std::string Str;
        llvm::raw_string_ostream(Str) << LVT->getElementType();
        Stream << llvm::formatv("[{}]", Str);
      });
  return Stream;
}

static InFlightDiagnostic &&operator<<(InFlightDiagnostic &&D, const Type &T) {
  std::string Err;
  llvm::raw_string_ostream(Err) << T;
  D << Err;
  return std::move(D);
}

class Sema::Analysis : public RecursiveASTVisitor<Analysis> {
  using Base = RecursiveASTVisitor<Analysis>;

  class SemaScope {
  public:
    SemaScope() = delete;
    SemaScope(const SemaScope &) = delete;
    SemaScope(SemaScope &&) = delete;
    SemaScope &operator=(const SemaScope &) = delete;
    SemaScope &operator=(SemaScope &&) = delete;

    SemaScope(Analysis *Self, Scope::ScopeKind Kind = Scope::ScopeKind::Global)
        : Self(Self) {
      std::shared_ptr<Scope> NewScope =
          std::make_shared<Scope>(Self->Actions.getCurScope(), Kind);
      Self->Actions.setCurScope(NewScope);
    }

    ~SemaScope() {
      std::shared_ptr<Scope> S = Self->Actions.getCurScope();
      Self->Actions.actOnPopScope(S.get());
      Self->Actions.setCurScope(S->getParent());
    }

  private:
    Analysis *Self;
  };

public:
  Analysis(Sema &Actions)
      : Actions(Actions), Diags(Actions.getDiagnosticEngine()) {}

  bool traverseProgram(Program *P) {
    SemaScope ScopeGuard(this);
    Actions.setGlobalScope(Actions.getCurScope());
    Actions.initializeGlobalScope();
    for (Declaration *D : P->getDeclarations())
      handleDeclaration(D);
    return Base::traverseProgram(P);
  }

  bool traverseClassDef(ClassDef *C) {
    llvm::outs() << "traverse NEWSCOPE Class " << C->getName() << "\n";
    SemaScope classScope(this, Scope::ScopeKind::Class);
    
    if(Actions.checkSuperClass(C)){
      for(Declaration* decl : C->getDeclarations()){
        handleDeclaration(decl);
        Actions.checkWasInSuper(Actions.getSuperClass(C), decl);

        if(isa<FuncDef>(decl)){ //if method
          Actions.handleInit(C, dyn_cast<FuncDef>(decl));
          Actions.checkFirstMethodParam(C, dyn_cast<FuncDef>(decl));
        }
      }
    }
    auto base = Base::traverseClassDef(C);
    llvm::outs() << "EXIT SCOPE of " << C->getName() << "\n";
    return base;
  }

  bool traverseFuncDef(FuncDef *F) {
    llvm::outs() << "traverse NEWSCOPE func " << F->getName() << "\n";
    SemaScope funcScope(this, Scope::ScopeKind::Func);
    
    for(ParamDecl *P : F->getParams()){
      handleDeclaration(P);
      Actions.checkTypeAnnotation(dyn_cast<ClassType>(P->getType()));
    }

    for(Declaration *D : F->getDeclarations()){
      handleDeclaration(D);
      if (isa<FuncDef>(D))
        traverseFuncDef(dyn_cast<FuncDef>(D));
      if (isa<VarDef>(D))
        Actions.checkTypeAnnotation(
            dyn_cast<ClassType>(dyn_cast<VarDef>(D)->getType()));
    }

    ValueType *ERTy = Actions.Ctx.convertAnnotationToVType(F->getReturnType());
    for (Stmt *S : F->getStatements()) {
      Base::traverseStmt(S);
      if (isa<ReturnStmt>(S) && Actions.checkTypeAnnotation(dyn_cast<ClassType>(F->getReturnType()))) {
        ReturnStmt *RS = dyn_cast<ReturnStmt>(S);
        Actions.checkReturnStmt(RS, ERTy);
      }
    }
    if ((ERTy->isInt() || ERTy->isBool() || ERTy->isStr()) &&
        !Actions.checkReturnMissing(F->getStatements()))
      Diags.emitError(F->getLocation().Start, diag::err_maybe_falloff_nonvoid)
          << F->getName();
    return true;
  }

  bool traverseVarDef(VarDef *V) {
    Base::traverseLiteral(V->getValue());
    Actions.actOnVarDef(V);
    return true;
  }

  bool traverseAssignStmt(AssignStmt *A) {
    Base::traverseExpr(A->getValue());
    for (Expr *T : A->getTargets()) {
      Base::traverseExpr(T);
      Actions.checkAssignTarget(T);
    }
    return true;
  }

  bool traverseReturnStmt(ReturnStmt *R) {
    llvm::outs() << "traverse RETURN" << "\n";
    Base::traverseExpr(R->getValue());
    if (Actions.getCurScope() == Actions.GlobalScope) {
      Diags.emitError(R->getLocation().Start, diag::err_bad_return_top);
      return false;
    }
    return true;
  }

  bool traverseBinaryExpr(BinaryExpr *B) {
    Base::traverseExpr(B->getLeft());
    Base::traverseExpr(B->getRight());
    Actions.actOnBinaryExpr(B);
    return true;
  }

  bool traverseCallExpr(CallExpr *C) {
    for (Expr *P : C->getArgs())
      if (DeclRef *R = dyn_cast<DeclRef>(P))
        Actions.actOnDeclRef(R);

    return true;
  }

  bool traverseDeclRef(DeclRef *DR) {
    Actions.actOnDeclRef(DR);
    return true;
  }

  bool traverseBooleanLiteral(BooleanLiteral *B) {
    B->setInferredType(Actions.Ctx.getBoolTy());
    return true;
  }

  bool traverseIntegerLiteral(IntegerLiteral *I) {
    I->setInferredType(Actions.Ctx.getIntTy());
    return true;
  }

  bool traverseNoneLiteral(NoneLiteral *N) {
    N->setInferredType(Actions.Ctx.getNoneTy());
    return true;
  }

  bool traverseStringLiteral(StringLiteral *S) {
    S->setInferredType(Actions.Ctx.getStrTy());
    return true;
  }

  bool traverseMemberExpr(MemberExpr *M) {
    return true;
  }

private:
  void handleDeclaration(Declaration *D) {
    llvm::outs() << "declarating " << D->getName() << "\n";
    StringRef Name = D->getName();
    Scope *S = Actions.getCurScope().get();
    Declaration* it = Actions.lookupName(S, D->getSymbolInfo());
    if (it) {
      Diags.emitError(D->getNameId()->getLocation().Start, diag::err_dup_decl) << Name;
      return;
    }
    
    if(isa<VarDef>(D)){
      Actions.handleDeclaration(D);
      Actions.checkClassShadow(D);
    }else if (isa<ParamDecl>(D)){
      Actions.handleDeclaration(D);
      Actions.checkClassShadow(D);
    }else if (isa<FuncDef>(D)){
      Actions.handleDeclaration(dyn_cast<FuncDef>(D));
      Actions.checkClassShadow(D);
    }else if (isa<ClassDef>(D)){
      Actions.handleDeclaration(dyn_cast<ClassDef>(D));
    }else if (isa<GlobalDecl>(D)){
      Actions.checkGlobalDecl(dyn_cast<GlobalDecl>(D));
    }else if (isa<NonLocalDecl>(D)){
      Actions.checkNonlocalDecl(dyn_cast<NonLocalDecl>(D));
    }else{
      llvm::report_fatal_error(
          "Unsupported kind of declaration! Add support...");
    }
  }

private:
  Sema &Actions;
  DiagnosticsEngine &Diags;
};

Sema::Sema(Lexer &L, ASTContext &C)
    : TheLexer(L), Diags(TheLexer.getDiagnostics()), Ctx(C) {}

void Sema::initialize() {
  ClassDef *ObjCD = Ctx.getObjectClass();
  ClassDef *IntCD = Ctx.getIntClass();
  ClassDef *StrCD = Ctx.getStrClass();
  ClassDef *BoolCD = Ctx.getBoolClass();
  ClassDef *NoneCD = Ctx.getNoneClass();

  FuncDef *PrintFD = Ctx.getPrintFunc();
  FuncDef *InputFD = Ctx.getInputFunc();
  FuncDef *LenFD = Ctx.getLenFunc();

  IdResolver.addDecl(ObjCD);
  IdResolver.addDecl(IntCD);
  IdResolver.addDecl(StrCD);
  IdResolver.addDecl(BoolCD);
  IdResolver.addDecl(NoneCD);

  IdResolver.addDecl(PrintFD);
  IdResolver.addDecl(InputFD);
  IdResolver.addDecl(LenFD);
}

void Sema::initializeGlobalScope() {
  ClassDef *ObjCD = Ctx.getObjectClass();
  ClassDef *IntCD = Ctx.getIntClass();
  ClassDef *StrCD = Ctx.getStrClass();
  ClassDef *BoolCD = Ctx.getBoolClass();
  ClassDef *NoneCD = Ctx.getNoneClass();

  FuncDef *PrintFD = Ctx.getPrintFunc();
  FuncDef *InputFD = Ctx.getInputFunc();
  FuncDef *LenFD = Ctx.getLenFunc();

  GlobalScope->addDecl(ObjCD);
  GlobalScope->addDecl(IntCD);
  GlobalScope->addDecl(StrCD);
  GlobalScope->addDecl(BoolCD);
  GlobalScope->addDecl(NoneCD);

  GlobalScope->addDecl(PrintFD);
  GlobalScope->addDecl(InputFD);
  GlobalScope->addDecl(LenFD);
}

template <class T>
void Sema::handleDeclaration(T *D) {
  if (CurScope->isDeclInScope(D)){
    llvm::outs() << "---" << D->getName() << " already in scope\n";
    return;
  }
  llvm::outs() << "----- Addding " << D->getName() << "\n";
  CurScope->addDecl(D);
  IdResolver.addDecl(D);
}

bool Sema::checkWasInSuper(ClassDef* C, Declaration* D){
  if(D->getName() == "__init__")
    return false;
  else
    for(Declaration* decl : C->getDeclarations()){
      if(decl->getName() == D->getName()){
        if(isa<FuncDef>(decl) && isa<FuncDef>(D)){
          return checkMethodOverride(
                    dyn_cast<FuncDef>(decl), 
                    dyn_cast<FuncDef>(D));
        } else {
          Diags.emitError(D->getLocation().Start, diag::err_redefine_attr) << D->getName();
          return true;
        }
      }
    }
  return true;
}

void Sema::checkClassShadow(Declaration *D) {
  IdentifierResolver::iterator it = IdResolver.begin(D->getSymbolInfo());
  IdentifierResolver::iterator it_end = IdResolver.end();
  while(it != it_end){
    if (ClassDef *C = dyn_cast<ClassDef>(*it)) {
      if (C->getName() == D->getName()) {
        Diags.emitError(D->getLocation().Start, diag::err_bad_shadow)
            << C->getName();
        break;
      }
    }
    ++it;
  }
}

void Sema::run() {
  Analysis V(*this);
  V.traverseAST(Ctx);
}

void Sema::actOnPopScope(Scope *S) {
  auto Decls = S->getDecls();
  for (Declaration *D : Decls)
    IdResolver.removeDecl(D);
}

bool Sema::checkNonlocalDecl(NonLocalDecl *NLD) {
  Scope *external = getCurScope().get()->getParent().get();
  if (external == GlobalScope.get()) {
    Diags.emitError(NLD->getLocation().Start, diag::err_not_nonlocal)
        << NLD->getName();
    return false;
  }
  Declaration *D = lookupName(external, NLD->getSymbolInfo());
  if (D != nullptr) {
    if (isa<VarDef>(D)) {
      handleDeclaration(D);
      return true;
    } else {
      Diags.emitError(NLD->getLocation().Start, diag::err_not_nonlocal)
          << NLD->getName();
      return false;
    }
  } else {
    Diags.emitError(NLD->getLocation().Start, diag::err_not_nonlocal)
        << NLD->getName();
    return false;
  }
}

bool Sema::checkGlobalDecl(GlobalDecl *GD) {
  Declaration *D = lookupName(GlobalScope.get(), GD->getSymbolInfo());
  if (D != nullptr) {
    if (VarDef::classof(D)) {
      VarDef *V = cast<VarDef>(D);
      actOnVarDef(V);
      handleDeclaration(D);
      return true;
    } else {
      Diags.emitError(GD->getLocation().Start, diag::err_not_global)
          << D->getName();
    }
  } else {
    Diags.emitError(GD->getLocation().Start, diag::err_not_global)
        << GD->getName();
  }
  return false;
}

bool Sema::checkSuperClass(ClassDef *D) {
  //for errors
  SMLoc loc_start = D->getSuperClass()->getLocation().Start;
  StringRef supername = D->getSuperClass()->getName();

  IdentifierResolver::iterator it = IdResolver.begin(
                                              D->getSuperClass()->getSymbolInfo());
  IdentifierResolver::iterator it_end = IdResolver.end();
  
  if(it == it_end){
    Diags.emitError(loc_start, diag::err_supclass_not_def) << supername;
    return false;
  }
  ClassDef* supclass = dyn_cast<ClassDef>(*it);
  if(supclass == nullptr){
    Diags.emitError(loc_start, diag::err_supclass_isnot_class) << supername;
    return false;
  }
  if(supclass == Ctx.getObjectClass())
      return true;
  if(supclass == Ctx.getIntClass() ||
     supclass == Ctx.getBoolClass() ||
     supclass == Ctx.getStrClass() ||
     supclass == Ctx.getNoneClass()){
      Diags.emitError(loc_start, diag::err_supclass_is_special_class) << supername;
      return false;
  }
  //if superclass was declared earlier
  IdentifierResolver::iterator my = IdResolver.begin(D->getSymbolInfo());
  if(std::distance(it, it_end) < std::distance(my, it_end)){
    Diags.emitError(loc_start, diag::err_supclass_isnot_class) << supername;
    return false;
  }else{
    return true;
  }
}

bool Sema::checkClassAttrs(ClassDef *D) {
  
  return true;
}

bool Sema::checkMethodOverride(FuncDef *OM, FuncDef *M) {
  ArrayRef<ParamDecl *> OP = OM->getParams();
  ArrayRef<ParamDecl *> P = M->getParams();
  if (OP.size() != 0 && OP[0]->getName() == "self")
    OP = OP.slice(1);
  if (P.size() != 0 && P[0]->getName() == "self")
    P = P.slice(1);
  if (OP.size() != P.size()) {
    Diags.emitError(M->getLocation().Start, diag::err_method_override)
        << OM->getName();
    return false;
  }
  for (size_t i = 0; i < OP.size(); ++i) {
    if (Ctx.convertAnnotationToVType(OP[i]->getType()) !=
        Ctx.convertAnnotationToVType(P[i]->getType())) {
      Diags.emitError(M->getLocation().Start, diag::err_method_override)
          << OM->getName();
      return false;
    }
  }
  if (Ctx.convertAnnotationToVType(OM->getReturnType()) !=
      Ctx.convertAnnotationToVType(M->getReturnType())) {
    Diags.emitError(M->getLocation().Start, diag::err_method_override)
        << OM->getName();
    return false;
  }
  return true;
}



bool Sema::checkClassDef(ClassDef *D) {
  /// @todo: Here should be your code
  return true;
}

bool Sema::handleInit(ClassDef* C, FuncDef* constructor){
  if(constructor->getName() != "__init__"){
    return true;
  }
  for(ParamDecl *param : constructor->getParams()){
    StringRef paramName = param->getName();
    ValueType *paramType =
        cast<ValueType>(Ctx.convertAnnotationToVType(param->getType()));

    for(Declaration *decl : C->getDeclarations()){
      if(VarDef *field = dyn_cast<VarDef>(decl)){
        if(field->getValue()){
          ValueType *fieldType =
              cast<ValueType>(Ctx.convertAnnotationToVType(field->getType()));
          StringRef field_name = field->getName();
          if(field_name == paramName && fieldType == paramType){
            Diags.emitError(field->getLocation().Start, diag::err_method_override)
                << field->getName();
            return false;
          }
        }
      }
    }
  }
  return true;
}

bool Sema::checkFirstMethodParam(ClassDef *C, FuncDef *F) {
  ArrayRef<ParamDecl*> params = F->getParams();
  if(!params.empty()){ 
    ParamDecl *first = params[0];
    if(first->getName() != "self" ||
       !isa<ClassType>(first->getType()) ||
       dyn_cast<ClassType>(first->getType())->getClassName() != C->getName()) {
      Diags.emitError(F->getNameId()->getLocation().Start,
                      diag::err_first_method_param)
            << F->getName();
      return false;
    }
  } else {
    Diags.emitError(F->getNameId()->getLocation().Start,
                    diag::err_first_method_param)
        << F->getName();
    return false;
  }
  return true;
}

bool Sema::checkAssignTarget(Expr *E) {
  DeclRef *DR = dyn_cast<DeclRef>(E);

  IdentifierResolver::iterator It = IdResolver.begin(DR->getSymbolInfo());

  if (It == IdResolver.end() || !CurScope->isDeclInScope(*It)) {
    Diags.emitError(DR->getLocation().Start, diag::err_bad_local_assign)
        << DR->getName();
    return false;
  }
  return true;
}

bool Sema::checkReturnStmt(ReturnStmt *S, ValueType *ERTy) {
  Expr *RE = dyn_cast<ReturnStmt>(S)->getValue();
  ValueType *RTy = nullptr;
  if (MemberExpr *ME = dyn_cast<MemberExpr>(RE))
    RTy = dyn_cast<ValueType>(ME->getMember()->getInferredType());
  else
    RTy = dyn_cast<ValueType>(RE->getInferredType());

  if((ERTy != RTy) &&
     (ERTy->isInt() ||
      ERTy->isBool() ||
      ERTy->isStr() ||
      !RTy->isNone())){
    Diags.emitError(S->getLocation().Start, diag::err_tc_assign)
        << *dyn_cast<Type>(ERTy) << *dyn_cast<Type>(RTy);
    return false;
  }
  return true;
}

bool Sema::checkReturnMissing(ArrayRef<Stmt *> Stmts) {
  for (Stmt *S : Stmts) {
    if (ReturnStmt::classof(S))
      return true;
  }
  return false;
}

bool Sema::checkTypeAnnotation(ClassType *C) {
  Scope *scope = getCurScope().get();
  Declaration *D = lookupClass(scope, C);
  if(D == nullptr){
    llvm::outs() << "cant find class " << C->getClassName() << " in this scope" << "\n";
    D = lookupClass(GlobalScope.get(), C);
    if(!D){
      llvm::outs() << "cant find class " << C->getClassName() << " in global scope" << "\n";
    }else{
      llvm::outs() << C->getClassName() << " found in global scope" << "\n";
    }
  }

  // if(D && isa<ClassDef>(D)) ||
  //    D->getName() == "bool" ||
  //    D->getName() == "int" ||
  //    D->getName() == "str")
  if(D)
    return true;
  Diags.emitError(C->getLocation().Start, diag::err_invalid_type_annotation)
      << C->getClassName();
  return false;
}

Declaration *Sema::lookupClass(Scope *S, ClassType *CT) {
  auto Decls = S->getDecls();
  auto It = llvm::find_if(Decls, [CT](Declaration *D) {
    return D->getName() == CT->getClassName();
  });
  if (It != Decls.end())
    return *It;
  return nullptr;
}

void Sema::actOnVarDef(VarDef *V) {
  auto &RTy = *cast<ValueType>(V->getValue()->getInferredType());
  auto &LTy = *cast<ValueType>(Ctx.convertAnnotationToVType(V->getType()));
  if (!(RTy <= LTy))
    Diags.emitError(V->getLocation().Start, diag::err_tc_assign) << LTy << RTy;
}

void Sema::actOnBinaryExpr(BinaryExpr *B) {
  ValueType &LTy = *cast<ValueType>(B->getLeft()->getInferredType());
  ValueType &RTy = *cast<ValueType>(B->getRight()->getInferredType());

  bool Err = false;

  switch (B->getOpKind()) {
  case BinaryExpr::OpKind::Add:
    if (LTy.isInt() || RTy.isInt()) {
      Err = &LTy != &RTy;
      B->setInferredType(Ctx.getIntTy());
    } else if (LTy.isStr() || RTy.isStr()) {
      Err = &LTy != &RTy;
      B->setInferredType(Ctx.getStrTy());
    } else {
      auto *LListTy = dyn_cast<ListValueType>(&LTy);
      auto *RListTy = dyn_cast<ListValueType>(&RTy);
      if (LListTy && RListTy)
        Err = LListTy->getElementType() != RListTy->getElementType();
      else
        Err = true;
      B->setInferredType(Err ? static_cast<ValueType *>(Ctx.getObjectTy())
                             : static_cast<ValueType *>(LListTy));
    }
    break;
  case BinaryExpr::OpKind::Sub:
  case BinaryExpr::OpKind::Mul:
  case BinaryExpr::OpKind::Mod:
  case BinaryExpr::OpKind::FloorDiv:
    Err = !LTy.isInt() || !RTy.isInt();
    B->setInferredType(Ctx.getIntTy());
    break;

  case BinaryExpr::OpKind::And:
  case BinaryExpr::OpKind::Or:
    Err = !LTy.isBool() || !RTy.isBool();
    B->setInferredType(Ctx.getBoolTy());
    break;

  case BinaryExpr::OpKind::EqCmp:
  case BinaryExpr::OpKind::NEqCmp:
    Err = (!LTy.isStr() && !LTy.isInt() && !LTy.isBool()) || (&LTy != &RTy);
    B->setInferredType(Ctx.getBoolTy());
    break;

  case BinaryExpr::OpKind::LEqCmp:
  case BinaryExpr::OpKind::GEqCmp:
  case BinaryExpr::OpKind::LCmp:
  case BinaryExpr::OpKind::GCmp:
    Err = !LTy.isInt() || !RTy.isInt();
    B->setInferredType(Ctx.getBoolTy());
    break;

  case BinaryExpr::OpKind::Is:
    Err = LTy.isStr() || LTy.isInt() || LTy.isBool() ||
          RTy.isStr() || RTy.isInt() || RTy.isBool();
    B->setInferredType(Ctx.getBoolTy());
    break;
  }

  if (Err) {
    Diags.emitError(B->getLocation().Start, diag::err_tc_binary)
        << B->getOpKindStr() << LTy << RTy;
  }
}

void Sema::actOnDeclRef(DeclRef *DR) {
  Declaration *D = lookupDecl(DR);

  if (!D || isa<FuncDef>(D)) {
    SMLoc Loc = DR->getLocation().Start;
    Diags.emitError(Loc, diag::err_not_variable) << DR->getName();
    DR->setInferredType(Ctx.getObjectTy());
    return;
  }

  auto convertToValueTy = [DR, this](auto *D) {
    ValueType *VT = Ctx.convertAnnotationToVType(D->getType());
    DR->setDeclInfo(D);
    DR->setInferredType(VT);
  };

  llvm::TypeSwitch<Declaration *>(D)
      .Case<ParamDecl>(convertToValueTy)
      .Case<VarDef>(convertToValueTy)
      .Default([](auto) {
        llvm::report_fatal_error("ACTonDECLREF Unsupported declaration! Add support...");
      });
}

Scope *Sema::getScopeForDecl(Scope *S, Declaration *D) {
  do {
    if (S->isDeclInScope(D))
      return S;
  } while ((S = S->getParent().get()));
  return nullptr;
}

ClassDef *Sema::getSuperClass(ClassDef *CD) {
  if(CD == Ctx.getObjectClass())
    return nullptr;
  SymbolInfo *supclass = CD->getSuperClass()->getSymbolInfo();
  IdentifierResolver::iterator it = IdResolver.begin(supclass);
  if (it != IdResolver.end()) {
    return dyn_cast<ClassDef>(*it);
  }
  return nullptr;
}

bool Sema::isSameType(TypeAnnotation *TyA, TypeAnnotation *TyB) {
  /// @todo: Here should be your code
  llvm::report_fatal_error("is SAMETYPE Unsupported feature! Add support...");
  return false;
}

Declaration *Sema::lookupName(Scope *S, SymbolInfo *SI) {
  auto Decls = S->getDecls();
  auto It = llvm::find_if(
      Decls, [SI](Declaration *D) { return D->getSymbolInfo() == SI; });
  if (It != Decls.end())
    return *It;
  return nullptr;
}

Declaration *Sema::lookupDecl(DeclRef *DR) {
  SymbolInfo *SI = DR->getSymbolInfo();
  IdentifierResolver::iterator I = IdResolver.begin(SI);
  IdentifierResolver::iterator E = IdResolver.end();

  Declaration *D = nullptr;

  for (; !D && I != E; ++I) {
    if (GlobalDecl *GD = dyn_cast<GlobalDecl>(*I)) {
      D = lookupName(GlobalScope.get(), GD->getSymbolInfo());
      return cast<VarDef>(D);
    }

    if (isa<NonLocalDecl>(*I))
      continue;
    D = *I;
  }

  return D;
}
} // namespace chocopy
