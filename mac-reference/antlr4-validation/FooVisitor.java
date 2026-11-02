// Generated from Foo.g4 by ANTLR 4.13.2
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link FooParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface FooVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link FooParser#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(FooParser.ProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#programDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgramDef(FooParser.ProgramDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#moduleDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitModuleDef(FooParser.ModuleDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#moduleName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitModuleName(FooParser.ModuleNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#moduleDataItem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitModuleDataItem(FooParser.ModuleDataItemContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#typeDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeDef(FooParser.TypeDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#moduleProcItem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitModuleProcItem(FooParser.ModuleProcItemContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#useStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUseStmt(FooParser.UseStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#moduleRef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitModuleRef(FooParser.ModuleRefContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#implicitStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplicitStmt(FooParser.ImplicitStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#interfaceBlock}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceBlock(FooParser.InterfaceBlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#genericItem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGenericItem(FooParser.GenericItemContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#abstractItem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAbstractItem(FooParser.AbstractItemContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#procDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcDef(FooParser.ProcDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#procHeader}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcHeader(FooParser.ProcHeaderContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#procArgs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcArgs(FooParser.ProcArgsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#procResult}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcResult(FooParser.ProcResultContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#procAttrs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcAttrs(FooParser.ProcAttrsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#attrList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttrList(FooParser.AttrListContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#attr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttr(FooParser.AttrContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#getFromArg}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGetFromArg(FooParser.GetFromArgContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#getFromKey}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGetFromKey(FooParser.GetFromKeyContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#getFromVal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGetFromVal(FooParser.GetFromValContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#procBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProcBody(FooParser.ProcBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#localDecl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLocalDecl(FooParser.LocalDeclContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#varDecl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVarDecl(FooParser.VarDeclContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#declTail}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDeclTail(FooParser.DeclTailContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#ptrSuffix}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPtrSuffix(FooParser.PtrSuffixContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#dataStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataStmt(FooParser.DataStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#dataValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDataValue(FooParser.DataValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#identList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdentList(FooParser.IdentListContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#declName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDeclName(FooParser.DeclNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#name}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitName(FooParser.NameContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#endKw}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEndKw(FooParser.EndKwContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#attrSuffix}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttrSuffix(FooParser.AttrSuffixContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#initSuffix}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInitSuffix(FooParser.InitSuffixContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#typeSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeSpec(FooParser.TypeSpecContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#baseType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBaseType(FooParser.BaseTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#primitiveType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimitiveType(FooParser.PrimitiveTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#paramType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamType(FooParser.ParamTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#arrayType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayType(FooParser.ArrayTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#dimSpec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDimSpec(FooParser.DimSpecContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#dimArg}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDimArg(FooParser.DimArgContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#stmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStmt(FooParser.StmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#forallStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForallStmt(FooParser.ForallStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#forallHeader}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForallHeader(FooParser.ForallHeaderContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#simpleLine}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSimpleLine(FooParser.SimpleLineContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#lineStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLineStmt(FooParser.LineStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#oneLineIf}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOneLineIf(FooParser.OneLineIfContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#oneLineWhere}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOneLineWhere(FooParser.OneLineWhereContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#simpleStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSimpleStmt(FooParser.SimpleStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#ioTail}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIoTail(FooParser.IoTailContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#ifStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfStmt(FooParser.IfStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#inlineBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInlineBody(FooParser.InlineBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#elseIfClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElseIfClause(FooParser.ElseIfClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#elseClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElseClause(FooParser.ElseClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#doStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDoStmt(FooParser.DoStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#loopHeader}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLoopHeader(FooParser.LoopHeaderContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#reduceClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReduceClause(FooParser.ReduceClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#selectStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSelectStmt(FooParser.SelectStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#whereStmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWhereStmt(FooParser.WhereStmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#elsewhereClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElsewhereClause(FooParser.ElsewhereClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#caseClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCaseClause(FooParser.CaseClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#caseLabel}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCaseLabel(FooParser.CaseLabelContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr(FooParser.ExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#postfix}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPostfix(FooParser.PostfixContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#head}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitHead(FooParser.HeadContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#callHead}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCallHead(FooParser.CallHeadContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#trailer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTrailer(FooParser.TrailerContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#qualifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQualifier(FooParser.QualifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#argList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgList(FooParser.ArgListContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#arg}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArg(FooParser.ArgContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteral(FooParser.LiteralContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#arrayConstructor}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayConstructor(FooParser.ArrayConstructorContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#acElem}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAcElem(FooParser.AcElemContext ctx);
	/**
	 * Visit a parse tree produced by {@link FooParser#binOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinOp(FooParser.BinOpContext ctx);
}