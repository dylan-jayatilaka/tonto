// Generated from Foo.g4 by ANTLR 4.13.2
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link FooParser}.
 */
public interface FooListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link FooParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(FooParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(FooParser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#programDef}.
	 * @param ctx the parse tree
	 */
	void enterProgramDef(FooParser.ProgramDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#programDef}.
	 * @param ctx the parse tree
	 */
	void exitProgramDef(FooParser.ProgramDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#moduleDef}.
	 * @param ctx the parse tree
	 */
	void enterModuleDef(FooParser.ModuleDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#moduleDef}.
	 * @param ctx the parse tree
	 */
	void exitModuleDef(FooParser.ModuleDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#moduleName}.
	 * @param ctx the parse tree
	 */
	void enterModuleName(FooParser.ModuleNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#moduleName}.
	 * @param ctx the parse tree
	 */
	void exitModuleName(FooParser.ModuleNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#moduleDataItem}.
	 * @param ctx the parse tree
	 */
	void enterModuleDataItem(FooParser.ModuleDataItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#moduleDataItem}.
	 * @param ctx the parse tree
	 */
	void exitModuleDataItem(FooParser.ModuleDataItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#typeDef}.
	 * @param ctx the parse tree
	 */
	void enterTypeDef(FooParser.TypeDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#typeDef}.
	 * @param ctx the parse tree
	 */
	void exitTypeDef(FooParser.TypeDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#moduleProcItem}.
	 * @param ctx the parse tree
	 */
	void enterModuleProcItem(FooParser.ModuleProcItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#moduleProcItem}.
	 * @param ctx the parse tree
	 */
	void exitModuleProcItem(FooParser.ModuleProcItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#useStmt}.
	 * @param ctx the parse tree
	 */
	void enterUseStmt(FooParser.UseStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#useStmt}.
	 * @param ctx the parse tree
	 */
	void exitUseStmt(FooParser.UseStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#moduleRef}.
	 * @param ctx the parse tree
	 */
	void enterModuleRef(FooParser.ModuleRefContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#moduleRef}.
	 * @param ctx the parse tree
	 */
	void exitModuleRef(FooParser.ModuleRefContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#implicitStmt}.
	 * @param ctx the parse tree
	 */
	void enterImplicitStmt(FooParser.ImplicitStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#implicitStmt}.
	 * @param ctx the parse tree
	 */
	void exitImplicitStmt(FooParser.ImplicitStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#interfaceBlock}.
	 * @param ctx the parse tree
	 */
	void enterInterfaceBlock(FooParser.InterfaceBlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#interfaceBlock}.
	 * @param ctx the parse tree
	 */
	void exitInterfaceBlock(FooParser.InterfaceBlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#genericItem}.
	 * @param ctx the parse tree
	 */
	void enterGenericItem(FooParser.GenericItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#genericItem}.
	 * @param ctx the parse tree
	 */
	void exitGenericItem(FooParser.GenericItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#abstractItem}.
	 * @param ctx the parse tree
	 */
	void enterAbstractItem(FooParser.AbstractItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#abstractItem}.
	 * @param ctx the parse tree
	 */
	void exitAbstractItem(FooParser.AbstractItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#procDef}.
	 * @param ctx the parse tree
	 */
	void enterProcDef(FooParser.ProcDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#procDef}.
	 * @param ctx the parse tree
	 */
	void exitProcDef(FooParser.ProcDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#procHeader}.
	 * @param ctx the parse tree
	 */
	void enterProcHeader(FooParser.ProcHeaderContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#procHeader}.
	 * @param ctx the parse tree
	 */
	void exitProcHeader(FooParser.ProcHeaderContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#procArgs}.
	 * @param ctx the parse tree
	 */
	void enterProcArgs(FooParser.ProcArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#procArgs}.
	 * @param ctx the parse tree
	 */
	void exitProcArgs(FooParser.ProcArgsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#procResult}.
	 * @param ctx the parse tree
	 */
	void enterProcResult(FooParser.ProcResultContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#procResult}.
	 * @param ctx the parse tree
	 */
	void exitProcResult(FooParser.ProcResultContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#procAttrs}.
	 * @param ctx the parse tree
	 */
	void enterProcAttrs(FooParser.ProcAttrsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#procAttrs}.
	 * @param ctx the parse tree
	 */
	void exitProcAttrs(FooParser.ProcAttrsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#attrList}.
	 * @param ctx the parse tree
	 */
	void enterAttrList(FooParser.AttrListContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#attrList}.
	 * @param ctx the parse tree
	 */
	void exitAttrList(FooParser.AttrListContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#attr}.
	 * @param ctx the parse tree
	 */
	void enterAttr(FooParser.AttrContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#attr}.
	 * @param ctx the parse tree
	 */
	void exitAttr(FooParser.AttrContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#getFromArg}.
	 * @param ctx the parse tree
	 */
	void enterGetFromArg(FooParser.GetFromArgContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#getFromArg}.
	 * @param ctx the parse tree
	 */
	void exitGetFromArg(FooParser.GetFromArgContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#getFromKey}.
	 * @param ctx the parse tree
	 */
	void enterGetFromKey(FooParser.GetFromKeyContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#getFromKey}.
	 * @param ctx the parse tree
	 */
	void exitGetFromKey(FooParser.GetFromKeyContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#getFromVal}.
	 * @param ctx the parse tree
	 */
	void enterGetFromVal(FooParser.GetFromValContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#getFromVal}.
	 * @param ctx the parse tree
	 */
	void exitGetFromVal(FooParser.GetFromValContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#procBody}.
	 * @param ctx the parse tree
	 */
	void enterProcBody(FooParser.ProcBodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#procBody}.
	 * @param ctx the parse tree
	 */
	void exitProcBody(FooParser.ProcBodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#localDecl}.
	 * @param ctx the parse tree
	 */
	void enterLocalDecl(FooParser.LocalDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#localDecl}.
	 * @param ctx the parse tree
	 */
	void exitLocalDecl(FooParser.LocalDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#varDecl}.
	 * @param ctx the parse tree
	 */
	void enterVarDecl(FooParser.VarDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#varDecl}.
	 * @param ctx the parse tree
	 */
	void exitVarDecl(FooParser.VarDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#declTail}.
	 * @param ctx the parse tree
	 */
	void enterDeclTail(FooParser.DeclTailContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#declTail}.
	 * @param ctx the parse tree
	 */
	void exitDeclTail(FooParser.DeclTailContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#ptrSuffix}.
	 * @param ctx the parse tree
	 */
	void enterPtrSuffix(FooParser.PtrSuffixContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#ptrSuffix}.
	 * @param ctx the parse tree
	 */
	void exitPtrSuffix(FooParser.PtrSuffixContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#dataStmt}.
	 * @param ctx the parse tree
	 */
	void enterDataStmt(FooParser.DataStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#dataStmt}.
	 * @param ctx the parse tree
	 */
	void exitDataStmt(FooParser.DataStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#dataValue}.
	 * @param ctx the parse tree
	 */
	void enterDataValue(FooParser.DataValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#dataValue}.
	 * @param ctx the parse tree
	 */
	void exitDataValue(FooParser.DataValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#identList}.
	 * @param ctx the parse tree
	 */
	void enterIdentList(FooParser.IdentListContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#identList}.
	 * @param ctx the parse tree
	 */
	void exitIdentList(FooParser.IdentListContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#declName}.
	 * @param ctx the parse tree
	 */
	void enterDeclName(FooParser.DeclNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#declName}.
	 * @param ctx the parse tree
	 */
	void exitDeclName(FooParser.DeclNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#name}.
	 * @param ctx the parse tree
	 */
	void enterName(FooParser.NameContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#name}.
	 * @param ctx the parse tree
	 */
	void exitName(FooParser.NameContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#endKw}.
	 * @param ctx the parse tree
	 */
	void enterEndKw(FooParser.EndKwContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#endKw}.
	 * @param ctx the parse tree
	 */
	void exitEndKw(FooParser.EndKwContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#attrSuffix}.
	 * @param ctx the parse tree
	 */
	void enterAttrSuffix(FooParser.AttrSuffixContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#attrSuffix}.
	 * @param ctx the parse tree
	 */
	void exitAttrSuffix(FooParser.AttrSuffixContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#initSuffix}.
	 * @param ctx the parse tree
	 */
	void enterInitSuffix(FooParser.InitSuffixContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#initSuffix}.
	 * @param ctx the parse tree
	 */
	void exitInitSuffix(FooParser.InitSuffixContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#typeSpec}.
	 * @param ctx the parse tree
	 */
	void enterTypeSpec(FooParser.TypeSpecContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#typeSpec}.
	 * @param ctx the parse tree
	 */
	void exitTypeSpec(FooParser.TypeSpecContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#baseType}.
	 * @param ctx the parse tree
	 */
	void enterBaseType(FooParser.BaseTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#baseType}.
	 * @param ctx the parse tree
	 */
	void exitBaseType(FooParser.BaseTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#primitiveType}.
	 * @param ctx the parse tree
	 */
	void enterPrimitiveType(FooParser.PrimitiveTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#primitiveType}.
	 * @param ctx the parse tree
	 */
	void exitPrimitiveType(FooParser.PrimitiveTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#paramType}.
	 * @param ctx the parse tree
	 */
	void enterParamType(FooParser.ParamTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#paramType}.
	 * @param ctx the parse tree
	 */
	void exitParamType(FooParser.ParamTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#arrayType}.
	 * @param ctx the parse tree
	 */
	void enterArrayType(FooParser.ArrayTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#arrayType}.
	 * @param ctx the parse tree
	 */
	void exitArrayType(FooParser.ArrayTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#dimSpec}.
	 * @param ctx the parse tree
	 */
	void enterDimSpec(FooParser.DimSpecContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#dimSpec}.
	 * @param ctx the parse tree
	 */
	void exitDimSpec(FooParser.DimSpecContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#dimArg}.
	 * @param ctx the parse tree
	 */
	void enterDimArg(FooParser.DimArgContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#dimArg}.
	 * @param ctx the parse tree
	 */
	void exitDimArg(FooParser.DimArgContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#stmt}.
	 * @param ctx the parse tree
	 */
	void enterStmt(FooParser.StmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#stmt}.
	 * @param ctx the parse tree
	 */
	void exitStmt(FooParser.StmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#forallStmt}.
	 * @param ctx the parse tree
	 */
	void enterForallStmt(FooParser.ForallStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#forallStmt}.
	 * @param ctx the parse tree
	 */
	void exitForallStmt(FooParser.ForallStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#forallHeader}.
	 * @param ctx the parse tree
	 */
	void enterForallHeader(FooParser.ForallHeaderContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#forallHeader}.
	 * @param ctx the parse tree
	 */
	void exitForallHeader(FooParser.ForallHeaderContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#simpleLine}.
	 * @param ctx the parse tree
	 */
	void enterSimpleLine(FooParser.SimpleLineContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#simpleLine}.
	 * @param ctx the parse tree
	 */
	void exitSimpleLine(FooParser.SimpleLineContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#lineStmt}.
	 * @param ctx the parse tree
	 */
	void enterLineStmt(FooParser.LineStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#lineStmt}.
	 * @param ctx the parse tree
	 */
	void exitLineStmt(FooParser.LineStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#oneLineIf}.
	 * @param ctx the parse tree
	 */
	void enterOneLineIf(FooParser.OneLineIfContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#oneLineIf}.
	 * @param ctx the parse tree
	 */
	void exitOneLineIf(FooParser.OneLineIfContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#oneLineWhere}.
	 * @param ctx the parse tree
	 */
	void enterOneLineWhere(FooParser.OneLineWhereContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#oneLineWhere}.
	 * @param ctx the parse tree
	 */
	void exitOneLineWhere(FooParser.OneLineWhereContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#simpleStmt}.
	 * @param ctx the parse tree
	 */
	void enterSimpleStmt(FooParser.SimpleStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#simpleStmt}.
	 * @param ctx the parse tree
	 */
	void exitSimpleStmt(FooParser.SimpleStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#ioTail}.
	 * @param ctx the parse tree
	 */
	void enterIoTail(FooParser.IoTailContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#ioTail}.
	 * @param ctx the parse tree
	 */
	void exitIoTail(FooParser.IoTailContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#ifStmt}.
	 * @param ctx the parse tree
	 */
	void enterIfStmt(FooParser.IfStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#ifStmt}.
	 * @param ctx the parse tree
	 */
	void exitIfStmt(FooParser.IfStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#inlineBody}.
	 * @param ctx the parse tree
	 */
	void enterInlineBody(FooParser.InlineBodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#inlineBody}.
	 * @param ctx the parse tree
	 */
	void exitInlineBody(FooParser.InlineBodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#elseIfClause}.
	 * @param ctx the parse tree
	 */
	void enterElseIfClause(FooParser.ElseIfClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#elseIfClause}.
	 * @param ctx the parse tree
	 */
	void exitElseIfClause(FooParser.ElseIfClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#elseClause}.
	 * @param ctx the parse tree
	 */
	void enterElseClause(FooParser.ElseClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#elseClause}.
	 * @param ctx the parse tree
	 */
	void exitElseClause(FooParser.ElseClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#doStmt}.
	 * @param ctx the parse tree
	 */
	void enterDoStmt(FooParser.DoStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#doStmt}.
	 * @param ctx the parse tree
	 */
	void exitDoStmt(FooParser.DoStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#loopHeader}.
	 * @param ctx the parse tree
	 */
	void enterLoopHeader(FooParser.LoopHeaderContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#loopHeader}.
	 * @param ctx the parse tree
	 */
	void exitLoopHeader(FooParser.LoopHeaderContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#reduceClause}.
	 * @param ctx the parse tree
	 */
	void enterReduceClause(FooParser.ReduceClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#reduceClause}.
	 * @param ctx the parse tree
	 */
	void exitReduceClause(FooParser.ReduceClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#selectStmt}.
	 * @param ctx the parse tree
	 */
	void enterSelectStmt(FooParser.SelectStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#selectStmt}.
	 * @param ctx the parse tree
	 */
	void exitSelectStmt(FooParser.SelectStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#whereStmt}.
	 * @param ctx the parse tree
	 */
	void enterWhereStmt(FooParser.WhereStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#whereStmt}.
	 * @param ctx the parse tree
	 */
	void exitWhereStmt(FooParser.WhereStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#elsewhereClause}.
	 * @param ctx the parse tree
	 */
	void enterElsewhereClause(FooParser.ElsewhereClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#elsewhereClause}.
	 * @param ctx the parse tree
	 */
	void exitElsewhereClause(FooParser.ElsewhereClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#caseClause}.
	 * @param ctx the parse tree
	 */
	void enterCaseClause(FooParser.CaseClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#caseClause}.
	 * @param ctx the parse tree
	 */
	void exitCaseClause(FooParser.CaseClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#caseLabel}.
	 * @param ctx the parse tree
	 */
	void enterCaseLabel(FooParser.CaseLabelContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#caseLabel}.
	 * @param ctx the parse tree
	 */
	void exitCaseLabel(FooParser.CaseLabelContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(FooParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(FooParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#postfix}.
	 * @param ctx the parse tree
	 */
	void enterPostfix(FooParser.PostfixContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#postfix}.
	 * @param ctx the parse tree
	 */
	void exitPostfix(FooParser.PostfixContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#head}.
	 * @param ctx the parse tree
	 */
	void enterHead(FooParser.HeadContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#head}.
	 * @param ctx the parse tree
	 */
	void exitHead(FooParser.HeadContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#callHead}.
	 * @param ctx the parse tree
	 */
	void enterCallHead(FooParser.CallHeadContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#callHead}.
	 * @param ctx the parse tree
	 */
	void exitCallHead(FooParser.CallHeadContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#trailer}.
	 * @param ctx the parse tree
	 */
	void enterTrailer(FooParser.TrailerContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#trailer}.
	 * @param ctx the parse tree
	 */
	void exitTrailer(FooParser.TrailerContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#qualifier}.
	 * @param ctx the parse tree
	 */
	void enterQualifier(FooParser.QualifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#qualifier}.
	 * @param ctx the parse tree
	 */
	void exitQualifier(FooParser.QualifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#argList}.
	 * @param ctx the parse tree
	 */
	void enterArgList(FooParser.ArgListContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#argList}.
	 * @param ctx the parse tree
	 */
	void exitArgList(FooParser.ArgListContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#arg}.
	 * @param ctx the parse tree
	 */
	void enterArg(FooParser.ArgContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#arg}.
	 * @param ctx the parse tree
	 */
	void exitArg(FooParser.ArgContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterLiteral(FooParser.LiteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitLiteral(FooParser.LiteralContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#arrayConstructor}.
	 * @param ctx the parse tree
	 */
	void enterArrayConstructor(FooParser.ArrayConstructorContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#arrayConstructor}.
	 * @param ctx the parse tree
	 */
	void exitArrayConstructor(FooParser.ArrayConstructorContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#acElem}.
	 * @param ctx the parse tree
	 */
	void enterAcElem(FooParser.AcElemContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#acElem}.
	 * @param ctx the parse tree
	 */
	void exitAcElem(FooParser.AcElemContext ctx);
	/**
	 * Enter a parse tree produced by {@link FooParser#binOp}.
	 * @param ctx the parse tree
	 */
	void enterBinOp(FooParser.BinOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FooParser#binOp}.
	 * @param ctx the parse tree
	 */
	void exitBinOp(FooParser.BinOpContext ctx);
}