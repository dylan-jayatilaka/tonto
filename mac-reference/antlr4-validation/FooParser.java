// Generated from Foo.g4 by ANTLR 4.13.2
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class FooParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		MODULE=1, PROGRAM=2, END=3, USE=4, INTERFACE=5, CONTAINS=6, IMPLICIT=7, 
		NONE=8, RESULT=9, GET_FROM=10, IN=11, OUT=12, INOUT=13, IF=14, THEN=15, 
		WHILE=16, WHERE=17, FORALL=18, ELSEIF=19, ELSEWHERE=20, ELSE=21, SELECT=22, 
		CASE=23, DEFAULT=24, DO=25, EXIT=26, CYCLE=27, RETURN=28, DATA=29, INT=30, 
		REAL=31, CPX=32, BIN=33, STR=34, OBJECT=35, INTRINSIC=36, TYPE=37, VEC=38, 
		MAT=39, MAT3=40, MAT4=41, MAT5=42, MAT6=43, MAT7=44, DCOLON=45, ARROW=46, 
		COLON=47, COMMA=48, LPAREN=49, RPAREN=50, LBRACE=51, RBRACE=52, LBRACKET=53, 
		RBRACKET=54, SEMI=55, AT=56, PERCENT=57, PLUS=58, MINUS=59, POWER=60, 
		STAR=61, SLASH=62, EQUAL=63, EQUAL_OP=64, CONCAT=65, NOT_EQUAL=66, LT=67, 
		LE=68, GT=69, GE=70, AND=71, OR=72, NOT=73, EQV=74, NEQV=75, EQ=76, NE=77, 
		LT_OP=78, LE_OP=79, GT_OP=80, GE_OP=81, DOT=82, QUESTION=83, TRUE=84, 
		FALSE=85, NULL=86, ZERO=87, ONE=88, BOZ=89, IDENTIFIER=90, INTEGER=91, 
		REAL_NUMBER=92, STRING=93, COMMENT_CONT=94, COMMENT=95, PP_LINE=96, CONTINUATION=97, 
		NEWLINE=98, WS=99;
	public static final int
		RULE_program = 0, RULE_programDef = 1, RULE_moduleDef = 2, RULE_moduleName = 3, 
		RULE_moduleDataItem = 4, RULE_typeDef = 5, RULE_moduleProcItem = 6, RULE_useStmt = 7, 
		RULE_moduleRef = 8, RULE_implicitStmt = 9, RULE_interfaceBlock = 10, RULE_genericItem = 11, 
		RULE_abstractItem = 12, RULE_procDef = 13, RULE_procHeader = 14, RULE_procArgs = 15, 
		RULE_procResult = 16, RULE_procAttrs = 17, RULE_attrList = 18, RULE_attr = 19, 
		RULE_getFromArg = 20, RULE_getFromKey = 21, RULE_getFromVal = 22, RULE_procBody = 23, 
		RULE_localDecl = 24, RULE_varDecl = 25, RULE_declTail = 26, RULE_ptrSuffix = 27, 
		RULE_dataStmt = 28, RULE_dataValue = 29, RULE_identList = 30, RULE_declName = 31, 
		RULE_name = 32, RULE_endKw = 33, RULE_attrSuffix = 34, RULE_initSuffix = 35, 
		RULE_typeSpec = 36, RULE_baseType = 37, RULE_primitiveType = 38, RULE_paramType = 39, 
		RULE_arrayType = 40, RULE_dimSpec = 41, RULE_dimArg = 42, RULE_stmt = 43, 
		RULE_forallStmt = 44, RULE_forallHeader = 45, RULE_simpleLine = 46, RULE_lineStmt = 47, 
		RULE_oneLineIf = 48, RULE_oneLineWhere = 49, RULE_simpleStmt = 50, RULE_ioTail = 51, 
		RULE_ifStmt = 52, RULE_inlineBody = 53, RULE_elseIfClause = 54, RULE_elseClause = 55, 
		RULE_doStmt = 56, RULE_loopHeader = 57, RULE_reduceClause = 58, RULE_selectStmt = 59, 
		RULE_whereStmt = 60, RULE_elsewhereClause = 61, RULE_caseClause = 62, 
		RULE_caseLabel = 63, RULE_expr = 64, RULE_postfix = 65, RULE_head = 66, 
		RULE_callHead = 67, RULE_trailer = 68, RULE_qualifier = 69, RULE_argList = 70, 
		RULE_arg = 71, RULE_literal = 72, RULE_arrayConstructor = 73, RULE_acElem = 74, 
		RULE_binOp = 75;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "programDef", "moduleDef", "moduleName", "moduleDataItem", 
			"typeDef", "moduleProcItem", "useStmt", "moduleRef", "implicitStmt", 
			"interfaceBlock", "genericItem", "abstractItem", "procDef", "procHeader", 
			"procArgs", "procResult", "procAttrs", "attrList", "attr", "getFromArg", 
			"getFromKey", "getFromVal", "procBody", "localDecl", "varDecl", "declTail", 
			"ptrSuffix", "dataStmt", "dataValue", "identList", "declName", "name", 
			"endKw", "attrSuffix", "initSuffix", "typeSpec", "baseType", "primitiveType", 
			"paramType", "arrayType", "dimSpec", "dimArg", "stmt", "forallStmt", 
			"forallHeader", "simpleLine", "lineStmt", "oneLineIf", "oneLineWhere", 
			"simpleStmt", "ioTail", "ifStmt", "inlineBody", "elseIfClause", "elseClause", 
			"doStmt", "loopHeader", "reduceClause", "selectStmt", "whereStmt", "elsewhereClause", 
			"caseClause", "caseLabel", "expr", "postfix", "head", "callHead", "trailer", 
			"qualifier", "argList", "arg", "literal", "arrayConstructor", "acElem", 
			"binOp"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'module'", "'program'", null, null, "'interface'", "'contains'", 
			"'implicit'", "'none'", "'result'", "'get_from'", "'IN'", "'OUT'", "'INOUT'", 
			"'if'", "'then'", "'while'", "'where'", "'forall'", null, "'elsewhere'", 
			"'else'", "'select'", "'case'", null, "'do'", "'exit'", "'cycle'", "'return'", 
			"'data'", "'INT'", "'REAL'", "'CPX'", "'BIN'", "'STR'", "'OBJECT'", "'INTRINSIC'", 
			null, "'VEC'", "'MAT'", "'MAT3'", "'MAT4'", "'MAT5'", "'MAT6'", "'MAT7'", 
			"'::'", "'=>'", "':'", "','", "'('", "')'", "'{'", "'}'", "'['", "']'", 
			"';'", "'@'", "'%'", "'+'", "'-'", "'**'", "'*'", "'/'", "'='", "'=='", 
			"'//'", "'/='", "'<'", "'<='", "'>'", "'>='", null, null, null, null, 
			null, null, null, "'.lt.'", "'.le.'", "'.gt.'", "'.ge.'", "'.'", "'?'", 
			null, null, null, "'ZERO'", "'ONE'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "MODULE", "PROGRAM", "END", "USE", "INTERFACE", "CONTAINS", "IMPLICIT", 
			"NONE", "RESULT", "GET_FROM", "IN", "OUT", "INOUT", "IF", "THEN", "WHILE", 
			"WHERE", "FORALL", "ELSEIF", "ELSEWHERE", "ELSE", "SELECT", "CASE", "DEFAULT", 
			"DO", "EXIT", "CYCLE", "RETURN", "DATA", "INT", "REAL", "CPX", "BIN", 
			"STR", "OBJECT", "INTRINSIC", "TYPE", "VEC", "MAT", "MAT3", "MAT4", "MAT5", 
			"MAT6", "MAT7", "DCOLON", "ARROW", "COLON", "COMMA", "LPAREN", "RPAREN", 
			"LBRACE", "RBRACE", "LBRACKET", "RBRACKET", "SEMI", "AT", "PERCENT", 
			"PLUS", "MINUS", "POWER", "STAR", "SLASH", "EQUAL", "EQUAL_OP", "CONCAT", 
			"NOT_EQUAL", "LT", "LE", "GT", "GE", "AND", "OR", "NOT", "EQV", "NEQV", 
			"EQ", "NE", "LT_OP", "LE_OP", "GT_OP", "GE_OP", "DOT", "QUESTION", "TRUE", 
			"FALSE", "NULL", "ZERO", "ONE", "BOZ", "IDENTIFIER", "INTEGER", "REAL_NUMBER", 
			"STRING", "COMMENT_CONT", "COMMENT", "PP_LINE", "CONTINUATION", "NEWLINE", 
			"WS"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "Foo.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public FooParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(FooParser.EOF, 0); }
		public List<ModuleDefContext> moduleDef() {
			return getRuleContexts(ModuleDefContext.class);
		}
		public ModuleDefContext moduleDef(int i) {
			return getRuleContext(ModuleDefContext.class,i);
		}
		public List<ProgramDefContext> programDef() {
			return getRuleContexts(ProgramDefContext.class);
		}
		public ProgramDefContext programDef(int i) {
			return getRuleContext(ProgramDefContext.class,i);
		}
		public List<TerminalNode> NEWLINE() { return getTokens(FooParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(FooParser.NEWLINE, i);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterProgram(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitProgram(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitProgram(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(157);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==MODULE || _la==PROGRAM || _la==IDENTIFIER || _la==NEWLINE) {
				{
				setState(155);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case MODULE:
				case IDENTIFIER:
					{
					setState(152);
					moduleDef();
					}
					break;
				case PROGRAM:
					{
					setState(153);
					programDef();
					}
					break;
				case NEWLINE:
					{
					setState(154);
					match(NEWLINE);
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				}
				setState(159);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(160);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramDefContext extends ParserRuleContext {
		public TerminalNode PROGRAM() { return getToken(FooParser.PROGRAM, 0); }
		public ModuleNameContext moduleName() {
			return getRuleContext(ModuleNameContext.class,0);
		}
		public List<TerminalNode> NEWLINE() { return getTokens(FooParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(FooParser.NEWLINE, i);
		}
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public List<ProcBodyContext> procBody() {
			return getRuleContexts(ProcBodyContext.class);
		}
		public ProcBodyContext procBody(int i) {
			return getRuleContext(ProcBodyContext.class,i);
		}
		public ProgramDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_programDef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterProgramDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitProgramDef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitProgramDef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProgramDefContext programDef() throws RecognitionException {
		ProgramDefContext _localctx = new ProgramDefContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_programDef);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(162);
			match(PROGRAM);
			setState(163);
			moduleName();
			setState(164);
			match(NEWLINE);
			setState(168);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(165);
					procBody();
					}
					} 
				}
				setState(170);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			}
			setState(171);
			endKw();
			setState(173);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				{
				setState(172);
				match(NEWLINE);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ModuleDefContext extends ParserRuleContext {
		public TerminalNode MODULE() { return getToken(FooParser.MODULE, 0); }
		public ModuleNameContext moduleName() {
			return getRuleContext(ModuleNameContext.class,0);
		}
		public List<TerminalNode> NEWLINE() { return getTokens(FooParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(FooParser.NEWLINE, i);
		}
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public List<ModuleDataItemContext> moduleDataItem() {
			return getRuleContexts(ModuleDataItemContext.class);
		}
		public ModuleDataItemContext moduleDataItem(int i) {
			return getRuleContext(ModuleDataItemContext.class,i);
		}
		public TerminalNode CONTAINS() { return getToken(FooParser.CONTAINS, 0); }
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public List<ModuleProcItemContext> moduleProcItem() {
			return getRuleContexts(ModuleProcItemContext.class);
		}
		public ModuleProcItemContext moduleProcItem(int i) {
			return getRuleContext(ModuleProcItemContext.class,i);
		}
		public ModuleDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_moduleDef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterModuleDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitModuleDef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitModuleDef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ModuleDefContext moduleDef() throws RecognitionException {
		ModuleDefContext _localctx = new ModuleDefContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_moduleDef);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(176);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENTIFIER) {
				{
				setState(175);
				match(IDENTIFIER);
				}
			}

			setState(178);
			match(MODULE);
			setState(179);
			moduleName();
			setState(180);
			match(NEWLINE);
			setState(184);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(181);
					moduleDataItem();
					}
					} 
				}
				setState(186);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			}
			setState(195);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==CONTAINS) {
				{
				setState(187);
				match(CONTAINS);
				setState(188);
				match(NEWLINE);
				setState(192);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(189);
						moduleProcItem();
						}
						} 
					}
					setState(194);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
				}
				}
			}

			setState(198);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==END) {
				{
				setState(197);
				endKw();
				}
			}

			setState(201);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,9,_ctx) ) {
			case 1:
				{
				setState(200);
				match(NEWLINE);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ModuleNameContext extends ParserRuleContext {
		public TypeSpecContext typeSpec() {
			return getRuleContext(TypeSpecContext.class,0);
		}
		public List<TerminalNode> DOT() { return getTokens(FooParser.DOT); }
		public TerminalNode DOT(int i) {
			return getToken(FooParser.DOT, i);
		}
		public List<TerminalNode> IDENTIFIER() { return getTokens(FooParser.IDENTIFIER); }
		public TerminalNode IDENTIFIER(int i) {
			return getToken(FooParser.IDENTIFIER, i);
		}
		public ModuleNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_moduleName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterModuleName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitModuleName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitModuleName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ModuleNameContext moduleName() throws RecognitionException {
		ModuleNameContext _localctx = new ModuleNameContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_moduleName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(203);
			typeSpec();
			setState(208);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==DOT) {
				{
				{
				setState(204);
				match(DOT);
				setState(205);
				match(IDENTIFIER);
				}
				}
				setState(210);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ModuleDataItemContext extends ParserRuleContext {
		public UseStmtContext useStmt() {
			return getRuleContext(UseStmtContext.class,0);
		}
		public ImplicitStmtContext implicitStmt() {
			return getRuleContext(ImplicitStmtContext.class,0);
		}
		public InterfaceBlockContext interfaceBlock() {
			return getRuleContext(InterfaceBlockContext.class,0);
		}
		public TypeDefContext typeDef() {
			return getRuleContext(TypeDefContext.class,0);
		}
		public DataStmtContext dataStmt() {
			return getRuleContext(DataStmtContext.class,0);
		}
		public VarDeclContext varDecl() {
			return getRuleContext(VarDeclContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public ModuleDataItemContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_moduleDataItem; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterModuleDataItem(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitModuleDataItem(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitModuleDataItem(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ModuleDataItemContext moduleDataItem() throws RecognitionException {
		ModuleDataItemContext _localctx = new ModuleDataItemContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_moduleDataItem);
		try {
			setState(218);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,11,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(211);
				useStmt();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(212);
				implicitStmt();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(213);
				interfaceBlock();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(214);
				typeDef();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(215);
				dataStmt();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(216);
				varDecl();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(217);
				match(NEWLINE);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeDefContext extends ParserRuleContext {
		public TerminalNode TYPE() { return getToken(FooParser.TYPE, 0); }
		public TypeSpecContext typeSpec() {
			return getRuleContext(TypeSpecContext.class,0);
		}
		public List<TerminalNode> NEWLINE() { return getTokens(FooParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(FooParser.NEWLINE, i);
		}
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public List<VarDeclContext> varDecl() {
			return getRuleContexts(VarDeclContext.class);
		}
		public VarDeclContext varDecl(int i) {
			return getRuleContext(VarDeclContext.class,i);
		}
		public TypeDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeDef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterTypeDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitTypeDef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitTypeDef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeDefContext typeDef() throws RecognitionException {
		TypeDefContext _localctx = new TypeDefContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_typeDef);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(221);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==IDENTIFIER) {
				{
				setState(220);
				match(IDENTIFIER);
				}
			}

			setState(223);
			match(TYPE);
			setState(224);
			typeSpec();
			setState(225);
			match(NEWLINE);
			setState(230);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					setState(228);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,13,_ctx) ) {
					case 1:
						{
						setState(226);
						varDecl();
						}
						break;
					case 2:
						{
						setState(227);
						match(NEWLINE);
						}
						break;
					}
					} 
				}
				setState(232);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
			}
			setState(233);
			endKw();
			setState(235);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,15,_ctx) ) {
			case 1:
				{
				setState(234);
				match(NEWLINE);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ModuleProcItemContext extends ParserRuleContext {
		public ProcDefContext procDef() {
			return getRuleContext(ProcDefContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public ModuleProcItemContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_moduleProcItem; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterModuleProcItem(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitModuleProcItem(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitModuleProcItem(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ModuleProcItemContext moduleProcItem() throws RecognitionException {
		ModuleProcItemContext _localctx = new ModuleProcItemContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_moduleProcItem);
		try {
			setState(239);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IDENTIFIER:
				enterOuterAlt(_localctx, 1);
				{
				setState(237);
				procDef();
				}
				break;
			case NEWLINE:
				enterOuterAlt(_localctx, 2);
				{
				setState(238);
				match(NEWLINE);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class UseStmtContext extends ParserRuleContext {
		public TerminalNode USE() { return getToken(FooParser.USE, 0); }
		public ModuleRefContext moduleRef() {
			return getRuleContext(ModuleRefContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public TerminalNode COLON() { return getToken(FooParser.COLON, 0); }
		public UseStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_useStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterUseStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitUseStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitUseStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final UseStmtContext useStmt() throws RecognitionException {
		UseStmtContext _localctx = new UseStmtContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_useStmt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(241);
			match(USE);
			setState(242);
			moduleRef();
			setState(254);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(243);
				match(COMMA);
				setState(244);
				name();
				setState(245);
				match(COLON);
				setState(246);
				name();
				setState(251);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(247);
					match(COMMA);
					setState(248);
					name();
					}
					}
					setState(253);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
			}

			setState(256);
			match(NEWLINE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ModuleRefContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public PrimitiveTypeContext primitiveType() {
			return getRuleContext(PrimitiveTypeContext.class,0);
		}
		public ModuleRefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_moduleRef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterModuleRef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitModuleRef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitModuleRef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ModuleRefContext moduleRef() throws RecognitionException {
		ModuleRefContext _localctx = new ModuleRefContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_moduleRef);
		try {
			setState(260);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IDENTIFIER:
				enterOuterAlt(_localctx, 1);
				{
				setState(258);
				match(IDENTIFIER);
				}
				break;
			case INT:
			case REAL:
			case CPX:
			case BIN:
			case STR:
			case OBJECT:
			case INTRINSIC:
				enterOuterAlt(_localctx, 2);
				{
				setState(259);
				primitiveType();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ImplicitStmtContext extends ParserRuleContext {
		public TerminalNode IMPLICIT() { return getToken(FooParser.IMPLICIT, 0); }
		public TerminalNode NONE() { return getToken(FooParser.NONE, 0); }
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public ImplicitStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_implicitStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterImplicitStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitImplicitStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitImplicitStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ImplicitStmtContext implicitStmt() throws RecognitionException {
		ImplicitStmtContext _localctx = new ImplicitStmtContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_implicitStmt);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(262);
			match(IMPLICIT);
			setState(263);
			match(NONE);
			setState(264);
			match(NEWLINE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class InterfaceBlockContext extends ParserRuleContext {
		public TerminalNode INTERFACE() { return getToken(FooParser.INTERFACE, 0); }
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public List<TerminalNode> NEWLINE() { return getTokens(FooParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(FooParser.NEWLINE, i);
		}
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public List<GenericItemContext> genericItem() {
			return getRuleContexts(GenericItemContext.class);
		}
		public GenericItemContext genericItem(int i) {
			return getRuleContext(GenericItemContext.class,i);
		}
		public List<AbstractItemContext> abstractItem() {
			return getRuleContexts(AbstractItemContext.class);
		}
		public AbstractItemContext abstractItem(int i) {
			return getRuleContext(AbstractItemContext.class,i);
		}
		public InterfaceBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_interfaceBlock; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterInterfaceBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitInterfaceBlock(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitInterfaceBlock(this);
			else return visitor.visitChildren(this);
		}
	}

	public final InterfaceBlockContext interfaceBlock() throws RecognitionException {
		InterfaceBlockContext _localctx = new InterfaceBlockContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_interfaceBlock);
		int _la;
		try {
			setState(291);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,24,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(266);
				match(INTERFACE);
				setState(267);
				match(IDENTIFIER);
				setState(268);
				match(NEWLINE);
				setState(272);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==IDENTIFIER || _la==NEWLINE) {
					{
					{
					setState(269);
					genericItem();
					}
					}
					setState(274);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(275);
				endKw();
				setState(277);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,21,_ctx) ) {
				case 1:
					{
					setState(276);
					match(NEWLINE);
					}
					break;
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(279);
				match(INTERFACE);
				setState(280);
				match(NEWLINE);
				setState(284);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==IDENTIFIER || _la==NEWLINE) {
					{
					{
					setState(281);
					abstractItem();
					}
					}
					setState(286);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(287);
				endKw();
				setState(289);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,23,_ctx) ) {
				case 1:
					{
					setState(288);
					match(NEWLINE);
					}
					break;
				}
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class GenericItemContext extends ParserRuleContext {
		public List<ProcHeaderContext> procHeader() {
			return getRuleContexts(ProcHeaderContext.class);
		}
		public ProcHeaderContext procHeader(int i) {
			return getRuleContext(ProcHeaderContext.class,i);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public GenericItemContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_genericItem; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterGenericItem(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitGenericItem(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitGenericItem(this);
			else return visitor.visitChildren(this);
		}
	}

	public final GenericItemContext genericItem() throws RecognitionException {
		GenericItemContext _localctx = new GenericItemContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_genericItem);
		int _la;
		try {
			setState(304);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IDENTIFIER:
				enterOuterAlt(_localctx, 1);
				{
				setState(293);
				procHeader();
				setState(298);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(294);
					match(COMMA);
					setState(295);
					procHeader();
					}
					}
					setState(300);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(301);
				match(NEWLINE);
				}
				break;
			case NEWLINE:
				enterOuterAlt(_localctx, 2);
				{
				setState(303);
				match(NEWLINE);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AbstractItemContext extends ParserRuleContext {
		public ProcDefContext procDef() {
			return getRuleContext(ProcDefContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public AbstractItemContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_abstractItem; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterAbstractItem(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitAbstractItem(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitAbstractItem(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AbstractItemContext abstractItem() throws RecognitionException {
		AbstractItemContext _localctx = new AbstractItemContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_abstractItem);
		try {
			setState(308);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IDENTIFIER:
				enterOuterAlt(_localctx, 1);
				{
				setState(306);
				procDef();
				}
				break;
			case NEWLINE:
				enterOuterAlt(_localctx, 2);
				{
				setState(307);
				match(NEWLINE);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProcDefContext extends ParserRuleContext {
		public ProcHeaderContext procHeader() {
			return getRuleContext(ProcHeaderContext.class,0);
		}
		public List<TerminalNode> NEWLINE() { return getTokens(FooParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(FooParser.NEWLINE, i);
		}
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public List<ProcBodyContext> procBody() {
			return getRuleContexts(ProcBodyContext.class);
		}
		public ProcBodyContext procBody(int i) {
			return getRuleContext(ProcBodyContext.class,i);
		}
		public ProcDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_procDef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterProcDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitProcDef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitProcDef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProcDefContext procDef() throws RecognitionException {
		ProcDefContext _localctx = new ProcDefContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_procDef);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(310);
			procHeader();
			setState(311);
			match(NEWLINE);
			setState(315);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,28,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(312);
					procBody();
					}
					} 
				}
				setState(317);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,28,_ctx);
			}
			setState(318);
			endKw();
			setState(320);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,29,_ctx) ) {
			case 1:
				{
				setState(319);
				match(NEWLINE);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProcHeaderContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public ProcArgsContext procArgs() {
			return getRuleContext(ProcArgsContext.class,0);
		}
		public ProcResultContext procResult() {
			return getRuleContext(ProcResultContext.class,0);
		}
		public ProcAttrsContext procAttrs() {
			return getRuleContext(ProcAttrsContext.class,0);
		}
		public ProcHeaderContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_procHeader; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterProcHeader(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitProcHeader(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitProcHeader(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProcHeaderContext procHeader() throws RecognitionException {
		ProcHeaderContext _localctx = new ProcHeaderContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_procHeader);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(322);
			match(IDENTIFIER);
			setState(324);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LPAREN) {
				{
				setState(323);
				procArgs();
				}
			}

			setState(327);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==RESULT) {
				{
				setState(326);
				procResult();
				}
			}

			setState(330);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==DCOLON) {
				{
				setState(329);
				procAttrs();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProcArgsContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public IdentListContext identList() {
			return getRuleContext(IdentListContext.class,0);
		}
		public ProcArgsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_procArgs; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterProcArgs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitProcArgs(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitProcArgs(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProcArgsContext procArgs() throws RecognitionException {
		ProcArgsContext _localctx = new ProcArgsContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_procArgs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(332);
			match(LPAREN);
			setState(334);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,33,_ctx) ) {
			case 1:
				{
				setState(333);
				identList();
				}
				break;
			}
			setState(336);
			match(RPAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProcResultContext extends ParserRuleContext {
		public TerminalNode RESULT() { return getToken(FooParser.RESULT, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public ProcResultContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_procResult; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterProcResult(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitProcResult(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitProcResult(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProcResultContext procResult() throws RecognitionException {
		ProcResultContext _localctx = new ProcResultContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_procResult);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(338);
			match(RESULT);
			setState(339);
			match(LPAREN);
			setState(340);
			match(IDENTIFIER);
			setState(341);
			match(RPAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProcAttrsContext extends ParserRuleContext {
		public TerminalNode DCOLON() { return getToken(FooParser.DCOLON, 0); }
		public AttrListContext attrList() {
			return getRuleContext(AttrListContext.class,0);
		}
		public ProcAttrsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_procAttrs; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterProcAttrs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitProcAttrs(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitProcAttrs(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProcAttrsContext procAttrs() throws RecognitionException {
		ProcAttrsContext _localctx = new ProcAttrsContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_procAttrs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(343);
			match(DCOLON);
			setState(344);
			attrList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AttrListContext extends ParserRuleContext {
		public List<AttrContext> attr() {
			return getRuleContexts(AttrContext.class);
		}
		public AttrContext attr(int i) {
			return getRuleContext(AttrContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public List<TerminalNode> DOT() { return getTokens(FooParser.DOT); }
		public TerminalNode DOT(int i) {
			return getToken(FooParser.DOT, i);
		}
		public AttrListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attrList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterAttrList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitAttrList(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitAttrList(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AttrListContext attrList() throws RecognitionException {
		AttrListContext _localctx = new AttrListContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_attrList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(346);
			attr();
			setState(353);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,35,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(348);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,34,_ctx) ) {
					case 1:
						{
						setState(347);
						_la = _input.LA(1);
						if ( !(_la==COMMA || _la==DOT) ) {
						_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						}
						break;
					}
					setState(350);
					attr();
					}
					} 
				}
				setState(355);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,35,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AttrContext extends ParserRuleContext {
		public TerminalNode GET_FROM() { return getToken(FooParser.GET_FROM, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public List<GetFromArgContext> getFromArg() {
			return getRuleContexts(GetFromArgContext.class);
		}
		public GetFromArgContext getFromArg(int i) {
			return getRuleContext(GetFromArgContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public TerminalNode IN() { return getToken(FooParser.IN, 0); }
		public TerminalNode OUT() { return getToken(FooParser.OUT, 0); }
		public TerminalNode INOUT() { return getToken(FooParser.INOUT, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public DimSpecContext dimSpec() {
			return getRuleContext(DimSpecContext.class,0);
		}
		public AttrContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterAttr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitAttr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitAttr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AttrContext attr() throws RecognitionException {
		AttrContext _localctx = new AttrContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_attr);
		int _la;
		try {
			setState(375);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,38,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(356);
				match(GET_FROM);
				setState(357);
				match(LPAREN);
				setState(358);
				getFromArg();
				setState(363);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(359);
					match(COMMA);
					setState(360);
					getFromArg();
					}
					}
					setState(365);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(366);
				match(RPAREN);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(368);
				match(IN);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(369);
				match(OUT);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(370);
				match(INOUT);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(371);
				name();
				setState(373);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,37,_ctx) ) {
				case 1:
					{
					setState(372);
					dimSpec();
					}
					break;
				}
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class GetFromArgContext extends ParserRuleContext {
		public GetFromKeyContext getFromKey() {
			return getRuleContext(GetFromKeyContext.class,0);
		}
		public TerminalNode ARROW() { return getToken(FooParser.ARROW, 0); }
		public TerminalNode QUESTION() { return getToken(FooParser.QUESTION, 0); }
		public GetFromValContext getFromVal() {
			return getRuleContext(GetFromValContext.class,0);
		}
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode EQUAL() { return getToken(FooParser.EQUAL, 0); }
		public TypeSpecContext typeSpec() {
			return getRuleContext(TypeSpecContext.class,0);
		}
		public TerminalNode COLON() { return getToken(FooParser.COLON, 0); }
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public GetFromArgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_getFromArg; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterGetFromArg(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitGetFromArg(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitGetFromArg(this);
			else return visitor.visitChildren(this);
		}
	}

	public final GetFromArgContext getFromArg() throws RecognitionException {
		GetFromArgContext _localctx = new GetFromArgContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_getFromArg);
		int _la;
		try {
			setState(395);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,41,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(377);
				getFromKey();
				setState(379);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==QUESTION) {
					{
					setState(378);
					match(QUESTION);
					}
				}

				setState(381);
				match(ARROW);
				setState(383);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,40,_ctx) ) {
				case 1:
					{
					setState(382);
					getFromVal();
					}
					break;
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(385);
				name();
				setState(386);
				match(EQUAL);
				setState(387);
				getFromVal();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(389);
				typeSpec();
				setState(390);
				match(COLON);
				setState(391);
				match(IDENTIFIER);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(393);
				typeSpec();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(394);
				expr();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class GetFromKeyContext extends ParserRuleContext {
		public TypeSpecContext typeSpec() {
			return getRuleContext(TypeSpecContext.class,0);
		}
		public BinOpContext binOp() {
			return getRuleContext(BinOpContext.class,0);
		}
		public GetFromKeyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_getFromKey; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterGetFromKey(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitGetFromKey(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitGetFromKey(this);
			else return visitor.visitChildren(this);
		}
	}

	public final GetFromKeyContext getFromKey() throws RecognitionException {
		GetFromKeyContext _localctx = new GetFromKeyContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_getFromKey);
		try {
			setState(399);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
			case REAL:
			case CPX:
			case BIN:
			case STR:
			case OBJECT:
			case INTRINSIC:
			case TYPE:
			case VEC:
			case MAT:
			case MAT3:
			case MAT4:
			case MAT5:
			case MAT6:
			case MAT7:
			case IDENTIFIER:
				enterOuterAlt(_localctx, 1);
				{
				setState(397);
				typeSpec();
				}
				break;
			case PLUS:
			case MINUS:
			case POWER:
			case STAR:
			case SLASH:
			case EQUAL_OP:
			case CONCAT:
			case NOT_EQUAL:
			case LT:
			case LE:
			case GT:
			case GE:
			case AND:
			case OR:
			case EQV:
			case NEQV:
			case EQ:
			case NE:
			case LT_OP:
			case LE_OP:
			case GT_OP:
			case GE_OP:
				enterOuterAlt(_localctx, 2);
				{
				setState(398);
				binOp();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class GetFromValContext extends ParserRuleContext {
		public TypeSpecContext typeSpec() {
			return getRuleContext(TypeSpecContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public BinOpContext binOp() {
			return getRuleContext(BinOpContext.class,0);
		}
		public GetFromValContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_getFromVal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterGetFromVal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitGetFromVal(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitGetFromVal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final GetFromValContext getFromVal() throws RecognitionException {
		GetFromValContext _localctx = new GetFromValContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_getFromVal);
		try {
			setState(404);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,43,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(401);
				typeSpec();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(402);
				expr();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(403);
				binOp();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProcBodyContext extends ParserRuleContext {
		public LocalDeclContext localDecl() {
			return getRuleContext(LocalDeclContext.class,0);
		}
		public DataStmtContext dataStmt() {
			return getRuleContext(DataStmtContext.class,0);
		}
		public UseStmtContext useStmt() {
			return getRuleContext(UseStmtContext.class,0);
		}
		public ImplicitStmtContext implicitStmt() {
			return getRuleContext(ImplicitStmtContext.class,0);
		}
		public InterfaceBlockContext interfaceBlock() {
			return getRuleContext(InterfaceBlockContext.class,0);
		}
		public StmtContext stmt() {
			return getRuleContext(StmtContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public ProcBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_procBody; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterProcBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitProcBody(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitProcBody(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProcBodyContext procBody() throws RecognitionException {
		ProcBodyContext _localctx = new ProcBodyContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_procBody);
		try {
			setState(413);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,44,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(406);
				localDecl();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(407);
				dataStmt();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(408);
				useStmt();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(409);
				implicitStmt();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(410);
				interfaceBlock();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(411);
				stmt();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(412);
				match(NEWLINE);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LocalDeclContext extends ParserRuleContext {
		public IdentListContext identList() {
			return getRuleContext(IdentListContext.class,0);
		}
		public TerminalNode DCOLON() { return getToken(FooParser.DCOLON, 0); }
		public DeclTailContext declTail() {
			return getRuleContext(DeclTailContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public LocalDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_localDecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterLocalDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitLocalDecl(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitLocalDecl(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LocalDeclContext localDecl() throws RecognitionException {
		LocalDeclContext _localctx = new LocalDeclContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_localDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(415);
			identList();
			setState(416);
			match(DCOLON);
			setState(417);
			declTail();
			setState(418);
			match(NEWLINE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class VarDeclContext extends ParserRuleContext {
		public IdentListContext identList() {
			return getRuleContext(IdentListContext.class,0);
		}
		public TerminalNode DCOLON() { return getToken(FooParser.DCOLON, 0); }
		public DeclTailContext declTail() {
			return getRuleContext(DeclTailContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public VarDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varDecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterVarDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitVarDecl(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitVarDecl(this);
			else return visitor.visitChildren(this);
		}
	}

	public final VarDeclContext varDecl() throws RecognitionException {
		VarDeclContext _localctx = new VarDeclContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_varDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(420);
			identList();
			setState(421);
			match(DCOLON);
			setState(422);
			declTail();
			setState(423);
			match(NEWLINE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DeclTailContext extends ParserRuleContext {
		public TypeSpecContext typeSpec() {
			return getRuleContext(TypeSpecContext.class,0);
		}
		public PtrSuffixContext ptrSuffix() {
			return getRuleContext(PtrSuffixContext.class,0);
		}
		public AttrSuffixContext attrSuffix() {
			return getRuleContext(AttrSuffixContext.class,0);
		}
		public InitSuffixContext initSuffix() {
			return getRuleContext(InitSuffixContext.class,0);
		}
		public List<AttrContext> attr() {
			return getRuleContexts(AttrContext.class);
		}
		public AttrContext attr(int i) {
			return getRuleContext(AttrContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public DeclTailContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_declTail; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterDeclTail(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitDeclTail(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitDeclTail(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DeclTailContext declTail() throws RecognitionException {
		DeclTailContext _localctx = new DeclTailContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_declTail);
		int _la;
		try {
			setState(446);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,50,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(425);
				typeSpec();
				setState(427);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,45,_ctx) ) {
				case 1:
					{
					setState(426);
					ptrSuffix();
					}
					break;
				}
				setState(430);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,46,_ctx) ) {
				case 1:
					{
					setState(429);
					attrSuffix();
					}
					break;
				}
				setState(433);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==EQUAL) {
					{
					setState(432);
					initSuffix();
					}
				}

				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(435);
				attr();
				setState(440);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(436);
					match(COMMA);
					setState(437);
					attr();
					}
					}
					setState(442);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(444);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==EQUAL) {
					{
					setState(443);
					initSuffix();
					}
				}

				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PtrSuffixContext extends ParserRuleContext {
		public TerminalNode STAR() { return getToken(FooParser.STAR, 0); }
		public TerminalNode AT() { return getToken(FooParser.AT, 0); }
		public PtrSuffixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ptrSuffix; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterPtrSuffix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitPtrSuffix(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitPtrSuffix(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PtrSuffixContext ptrSuffix() throws RecognitionException {
		PtrSuffixContext _localctx = new PtrSuffixContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_ptrSuffix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(448);
			_la = _input.LA(1);
			if ( !(_la==AT || _la==STAR) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DataStmtContext extends ParserRuleContext {
		public TerminalNode DATA() { return getToken(FooParser.DATA, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public List<TerminalNode> SLASH() { return getTokens(FooParser.SLASH); }
		public TerminalNode SLASH(int i) {
			return getToken(FooParser.SLASH, i);
		}
		public List<DataValueContext> dataValue() {
			return getRuleContexts(DataValueContext.class);
		}
		public DataValueContext dataValue(int i) {
			return getRuleContext(DataValueContext.class,i);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public DimSpecContext dimSpec() {
			return getRuleContext(DimSpecContext.class,0);
		}
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public DataStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_dataStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterDataStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitDataStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitDataStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DataStmtContext dataStmt() throws RecognitionException {
		DataStmtContext _localctx = new DataStmtContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_dataStmt);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(450);
			match(DATA);
			setState(451);
			name();
			setState(453);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LPAREN) {
				{
				setState(452);
				dimSpec();
				}
			}

			setState(455);
			match(SLASH);
			setState(456);
			dataValue();
			setState(461);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(457);
					match(COMMA);
					setState(458);
					dataValue();
					}
					} 
				}
				setState(463);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			}
			setState(465);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(464);
				match(COMMA);
				}
			}

			setState(467);
			match(SLASH);
			setState(468);
			match(NEWLINE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DataValueContext extends ParserRuleContext {
		public LiteralContext literal() {
			return getRuleContext(LiteralContext.class,0);
		}
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode MINUS() { return getToken(FooParser.MINUS, 0); }
		public DataValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_dataValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterDataValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitDataValue(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitDataValue(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DataValueContext dataValue() throws RecognitionException {
		DataValueContext _localctx = new DataValueContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_dataValue);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(471);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,54,_ctx) ) {
			case 1:
				{
				setState(470);
				match(MINUS);
				}
				break;
			}
			setState(475);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,55,_ctx) ) {
			case 1:
				{
				setState(473);
				literal();
				}
				break;
			case 2:
				{
				setState(474);
				name();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class IdentListContext extends ParserRuleContext {
		public List<DeclNameContext> declName() {
			return getRuleContexts(DeclNameContext.class);
		}
		public DeclNameContext declName(int i) {
			return getRuleContext(DeclNameContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public IdentListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_identList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterIdentList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitIdentList(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitIdentList(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IdentListContext identList() throws RecognitionException {
		IdentListContext _localctx = new IdentListContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_identList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(477);
			declName();
			setState(482);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(478);
				match(COMMA);
				setState(479);
				declName();
				}
				}
				setState(484);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DeclNameContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public DimSpecContext dimSpec() {
			return getRuleContext(DimSpecContext.class,0);
		}
		public DeclNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_declName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterDeclName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitDeclName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitDeclName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DeclNameContext declName() throws RecognitionException {
		DeclNameContext _localctx = new DeclNameContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_declName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(485);
			name();
			setState(487);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LPAREN) {
				{
				setState(486);
				dimSpec();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class NameContext extends ParserRuleContext {
		public List<TerminalNode> IDENTIFIER() { return getTokens(FooParser.IDENTIFIER); }
		public TerminalNode IDENTIFIER(int i) {
			return getToken(FooParser.IDENTIFIER, i);
		}
		public List<TerminalNode> QUESTION() { return getTokens(FooParser.QUESTION); }
		public TerminalNode QUESTION(int i) {
			return getToken(FooParser.QUESTION, i);
		}
		public TerminalNode END() { return getToken(FooParser.END, 0); }
		public TerminalNode DATA() { return getToken(FooParser.DATA, 0); }
		public TerminalNode RESULT() { return getToken(FooParser.RESULT, 0); }
		public TerminalNode TYPE() { return getToken(FooParser.TYPE, 0); }
		public TerminalNode CASE() { return getToken(FooParser.CASE, 0); }
		public TerminalNode WHERE() { return getToken(FooParser.WHERE, 0); }
		public TerminalNode DEFAULT() { return getToken(FooParser.DEFAULT, 0); }
		public NameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_name; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NameContext name() throws RecognitionException {
		NameContext _localctx = new NameContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_name);
		try {
			int _alt;
			setState(508);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,60,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(489);
				match(IDENTIFIER);
				setState(494);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,58,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(490);
						match(QUESTION);
						setState(491);
						match(IDENTIFIER);
						}
						} 
					}
					setState(496);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,58,_ctx);
				}
				setState(498);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,59,_ctx) ) {
				case 1:
					{
					setState(497);
					match(QUESTION);
					}
					break;
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(500);
				if (!(_input.LA(2) != NEWLINE)) throw new FailedPredicateException(this, "_input.LA(2) != NEWLINE");
				setState(501);
				match(END);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(502);
				match(DATA);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(503);
				match(RESULT);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(504);
				match(TYPE);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(505);
				match(CASE);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(506);
				match(WHERE);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(507);
				match(DEFAULT);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class EndKwContext extends ParserRuleContext {
		public TerminalNode END() { return getToken(FooParser.END, 0); }
		public TerminalNode IF() { return getToken(FooParser.IF, 0); }
		public TerminalNode DO() { return getToken(FooParser.DO, 0); }
		public TerminalNode SELECT() { return getToken(FooParser.SELECT, 0); }
		public TerminalNode INTERFACE() { return getToken(FooParser.INTERFACE, 0); }
		public TerminalNode MODULE() { return getToken(FooParser.MODULE, 0); }
		public TerminalNode TYPE() { return getToken(FooParser.TYPE, 0); }
		public TerminalNode FORALL() { return getToken(FooParser.FORALL, 0); }
		public EndKwContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_endKw; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterEndKw(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitEndKw(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitEndKw(this);
			else return visitor.visitChildren(this);
		}
	}

	public final EndKwContext endKw() throws RecognitionException {
		EndKwContext _localctx = new EndKwContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_endKw);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(510);
			match(END);
			setState(512);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,61,_ctx) ) {
			case 1:
				{
				setState(511);
				_la = _input.LA(1);
				if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 137476980770L) != 0)) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AttrSuffixContext extends ParserRuleContext {
		public List<AttrContext> attr() {
			return getRuleContexts(AttrContext.class);
		}
		public AttrContext attr(int i) {
			return getRuleContext(AttrContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public AttrSuffixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attrSuffix; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterAttrSuffix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitAttrSuffix(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitAttrSuffix(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AttrSuffixContext attrSuffix() throws RecognitionException {
		AttrSuffixContext _localctx = new AttrSuffixContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_attrSuffix);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(518); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(515);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,62,_ctx) ) {
					case 1:
						{
						setState(514);
						match(COMMA);
						}
						break;
					}
					setState(517);
					attr();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(520); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,63,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class InitSuffixContext extends ParserRuleContext {
		public TerminalNode EQUAL() { return getToken(FooParser.EQUAL, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public InitSuffixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_initSuffix; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterInitSuffix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitInitSuffix(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitInitSuffix(this);
			else return visitor.visitChildren(this);
		}
	}

	public final InitSuffixContext initSuffix() throws RecognitionException {
		InitSuffixContext _localctx = new InitSuffixContext(_ctx, getState());
		enterRule(_localctx, 70, RULE_initSuffix);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(522);
			match(EQUAL);
			setState(523);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeSpecContext extends ParserRuleContext {
		public BaseTypeContext baseType() {
			return getRuleContext(BaseTypeContext.class,0);
		}
		public TerminalNode QUESTION() { return getToken(FooParser.QUESTION, 0); }
		public DimSpecContext dimSpec() {
			return getRuleContext(DimSpecContext.class,0);
		}
		public TypeSpecContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeSpec; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterTypeSpec(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitTypeSpec(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitTypeSpec(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeSpecContext typeSpec() throws RecognitionException {
		TypeSpecContext _localctx = new TypeSpecContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_typeSpec);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(525);
			baseType();
			setState(527);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,64,_ctx) ) {
			case 1:
				{
				setState(526);
				match(QUESTION);
				}
				break;
			}
			setState(530);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,65,_ctx) ) {
			case 1:
				{
				setState(529);
				dimSpec();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BaseTypeContext extends ParserRuleContext {
		public PrimitiveTypeContext primitiveType() {
			return getRuleContext(PrimitiveTypeContext.class,0);
		}
		public ParamTypeContext paramType() {
			return getRuleContext(ParamTypeContext.class,0);
		}
		public ArrayTypeContext arrayType() {
			return getRuleContext(ArrayTypeContext.class,0);
		}
		public TerminalNode TYPE() { return getToken(FooParser.TYPE, 0); }
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public BaseTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_baseType; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterBaseType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitBaseType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitBaseType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final BaseTypeContext baseType() throws RecognitionException {
		BaseTypeContext _localctx = new BaseTypeContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_baseType);
		try {
			setState(537);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,66,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(532);
				primitiveType();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(533);
				paramType();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(534);
				arrayType();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(535);
				match(TYPE);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(536);
				match(IDENTIFIER);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PrimitiveTypeContext extends ParserRuleContext {
		public TerminalNode INT() { return getToken(FooParser.INT, 0); }
		public TerminalNode REAL() { return getToken(FooParser.REAL, 0); }
		public TerminalNode CPX() { return getToken(FooParser.CPX, 0); }
		public TerminalNode BIN() { return getToken(FooParser.BIN, 0); }
		public TerminalNode STR() { return getToken(FooParser.STR, 0); }
		public TerminalNode OBJECT() { return getToken(FooParser.OBJECT, 0); }
		public TerminalNode INTRINSIC() { return getToken(FooParser.INTRINSIC, 0); }
		public PrimitiveTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_primitiveType; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterPrimitiveType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitPrimitiveType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitPrimitiveType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PrimitiveTypeContext primitiveType() throws RecognitionException {
		PrimitiveTypeContext _localctx = new PrimitiveTypeContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_primitiveType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(539);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 136365211648L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ParamTypeContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public TerminalNode LBRACE() { return getToken(FooParser.LBRACE, 0); }
		public List<TypeSpecContext> typeSpec() {
			return getRuleContexts(TypeSpecContext.class);
		}
		public TypeSpecContext typeSpec(int i) {
			return getRuleContext(TypeSpecContext.class,i);
		}
		public TerminalNode RBRACE() { return getToken(FooParser.RBRACE, 0); }
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public ParamTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_paramType; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterParamType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitParamType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitParamType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ParamTypeContext paramType() throws RecognitionException {
		ParamTypeContext _localctx = new ParamTypeContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_paramType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(541);
			match(IDENTIFIER);
			setState(542);
			match(LBRACE);
			setState(543);
			typeSpec();
			setState(548);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(544);
				match(COMMA);
				setState(545);
				typeSpec();
				}
				}
				setState(550);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(551);
			match(RBRACE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ArrayTypeContext extends ParserRuleContext {
		public TerminalNode VEC() { return getToken(FooParser.VEC, 0); }
		public TerminalNode LBRACE() { return getToken(FooParser.LBRACE, 0); }
		public TypeSpecContext typeSpec() {
			return getRuleContext(TypeSpecContext.class,0);
		}
		public TerminalNode RBRACE() { return getToken(FooParser.RBRACE, 0); }
		public TerminalNode MAT() { return getToken(FooParser.MAT, 0); }
		public TerminalNode MAT3() { return getToken(FooParser.MAT3, 0); }
		public TerminalNode MAT4() { return getToken(FooParser.MAT4, 0); }
		public TerminalNode MAT5() { return getToken(FooParser.MAT5, 0); }
		public TerminalNode MAT6() { return getToken(FooParser.MAT6, 0); }
		public TerminalNode MAT7() { return getToken(FooParser.MAT7, 0); }
		public ArrayTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arrayType; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterArrayType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitArrayType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitArrayType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArrayTypeContext arrayType() throws RecognitionException {
		ArrayTypeContext _localctx = new ArrayTypeContext(_ctx, getState());
		enterRule(_localctx, 80, RULE_arrayType);
		try {
			setState(588);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case VEC:
				enterOuterAlt(_localctx, 1);
				{
				setState(553);
				match(VEC);
				setState(554);
				match(LBRACE);
				setState(555);
				typeSpec();
				setState(556);
				match(RBRACE);
				}
				break;
			case MAT:
				enterOuterAlt(_localctx, 2);
				{
				setState(558);
				match(MAT);
				setState(559);
				match(LBRACE);
				setState(560);
				typeSpec();
				setState(561);
				match(RBRACE);
				}
				break;
			case MAT3:
				enterOuterAlt(_localctx, 3);
				{
				setState(563);
				match(MAT3);
				setState(564);
				match(LBRACE);
				setState(565);
				typeSpec();
				setState(566);
				match(RBRACE);
				}
				break;
			case MAT4:
				enterOuterAlt(_localctx, 4);
				{
				setState(568);
				match(MAT4);
				setState(569);
				match(LBRACE);
				setState(570);
				typeSpec();
				setState(571);
				match(RBRACE);
				}
				break;
			case MAT5:
				enterOuterAlt(_localctx, 5);
				{
				setState(573);
				match(MAT5);
				setState(574);
				match(LBRACE);
				setState(575);
				typeSpec();
				setState(576);
				match(RBRACE);
				}
				break;
			case MAT6:
				enterOuterAlt(_localctx, 6);
				{
				setState(578);
				match(MAT6);
				setState(579);
				match(LBRACE);
				setState(580);
				typeSpec();
				setState(581);
				match(RBRACE);
				}
				break;
			case MAT7:
				enterOuterAlt(_localctx, 7);
				{
				setState(583);
				match(MAT7);
				setState(584);
				match(LBRACE);
				setState(585);
				typeSpec();
				setState(586);
				match(RBRACE);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DimSpecContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public List<DimArgContext> dimArg() {
			return getRuleContexts(DimArgContext.class);
		}
		public DimArgContext dimArg(int i) {
			return getRuleContext(DimArgContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public DimSpecContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_dimSpec; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterDimSpec(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitDimSpec(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitDimSpec(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DimSpecContext dimSpec() throws RecognitionException {
		DimSpecContext _localctx = new DimSpecContext(_ctx, getState());
		enterRule(_localctx, 82, RULE_dimSpec);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(590);
			match(LPAREN);
			setState(591);
			dimArg();
			setState(596);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(592);
				match(COMMA);
				setState(593);
				dimArg();
				}
				}
				setState(598);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(599);
			match(RPAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DimArgContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public TerminalNode EQUAL() { return getToken(FooParser.EQUAL, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode COLON() { return getToken(FooParser.COLON, 0); }
		public TerminalNode DCOLON() { return getToken(FooParser.DCOLON, 0); }
		public TerminalNode STAR() { return getToken(FooParser.STAR, 0); }
		public DimArgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_dimArg; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterDimArg(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitDimArg(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitDimArg(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DimArgContext dimArg() throws RecognitionException {
		DimArgContext _localctx = new DimArgContext(_ctx, getState());
		enterRule(_localctx, 84, RULE_dimArg);
		try {
			setState(620);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,74,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(601);
				match(IDENTIFIER);
				setState(602);
				match(EQUAL);
				setState(603);
				expr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(605);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,70,_ctx) ) {
				case 1:
					{
					setState(604);
					expr();
					}
					break;
				}
				setState(607);
				match(COLON);
				setState(609);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,71,_ctx) ) {
				case 1:
					{
					setState(608);
					expr();
					}
					break;
				}
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(612);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,72,_ctx) ) {
				case 1:
					{
					setState(611);
					expr();
					}
					break;
				}
				setState(614);
				match(DCOLON);
				setState(616);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,73,_ctx) ) {
				case 1:
					{
					setState(615);
					expr();
					}
					break;
				}
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(618);
				expr();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(619);
				match(STAR);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class StmtContext extends ParserRuleContext {
		public IfStmtContext ifStmt() {
			return getRuleContext(IfStmtContext.class,0);
		}
		public DoStmtContext doStmt() {
			return getRuleContext(DoStmtContext.class,0);
		}
		public ForallStmtContext forallStmt() {
			return getRuleContext(ForallStmtContext.class,0);
		}
		public SelectStmtContext selectStmt() {
			return getRuleContext(SelectStmtContext.class,0);
		}
		public WhereStmtContext whereStmt() {
			return getRuleContext(WhereStmtContext.class,0);
		}
		public SimpleLineContext simpleLine() {
			return getRuleContext(SimpleLineContext.class,0);
		}
		public StmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StmtContext stmt() throws RecognitionException {
		StmtContext _localctx = new StmtContext(_ctx, getState());
		enterRule(_localctx, 86, RULE_stmt);
		try {
			setState(628);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,75,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(622);
				ifStmt();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(623);
				doStmt();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(624);
				forallStmt();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(625);
				selectStmt();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(626);
				whereStmt();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(627);
				simpleLine();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ForallStmtContext extends ParserRuleContext {
		public TerminalNode FORALL() { return getToken(FooParser.FORALL, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ForallHeaderContext forallHeader() {
			return getRuleContext(ForallHeaderContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public List<TerminalNode> NEWLINE() { return getTokens(FooParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(FooParser.NEWLINE, i);
		}
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public List<ProcBodyContext> procBody() {
			return getRuleContexts(ProcBodyContext.class);
		}
		public ProcBodyContext procBody(int i) {
			return getRuleContext(ProcBodyContext.class,i);
		}
		public SimpleStmtContext simpleStmt() {
			return getRuleContext(SimpleStmtContext.class,0);
		}
		public ForallStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_forallStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterForallStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitForallStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitForallStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ForallStmtContext forallStmt() throws RecognitionException {
		ForallStmtContext _localctx = new ForallStmtContext(_ctx, getState());
		enterRule(_localctx, 88, RULE_forallStmt);
		try {
			int _alt;
			setState(652);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,78,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(630);
				match(FORALL);
				setState(631);
				match(LPAREN);
				setState(632);
				forallHeader();
				setState(633);
				match(RPAREN);
				setState(634);
				match(NEWLINE);
				setState(638);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,76,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(635);
						procBody();
						}
						} 
					}
					setState(640);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,76,_ctx);
				}
				setState(641);
				endKw();
				setState(643);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,77,_ctx) ) {
				case 1:
					{
					setState(642);
					match(NEWLINE);
					}
					break;
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(645);
				match(FORALL);
				setState(646);
				match(LPAREN);
				setState(647);
				forallHeader();
				setState(648);
				match(RPAREN);
				setState(649);
				simpleStmt();
				setState(650);
				match(NEWLINE);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ForallHeaderContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public TerminalNode EQUAL() { return getToken(FooParser.EQUAL, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public List<TerminalNode> COLON() { return getTokens(FooParser.COLON); }
		public TerminalNode COLON(int i) {
			return getToken(FooParser.COLON, i);
		}
		public ForallHeaderContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_forallHeader; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterForallHeader(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitForallHeader(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitForallHeader(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ForallHeaderContext forallHeader() throws RecognitionException {
		ForallHeaderContext _localctx = new ForallHeaderContext(_ctx, getState());
		enterRule(_localctx, 90, RULE_forallHeader);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(654);
			match(IDENTIFIER);
			setState(655);
			match(EQUAL);
			setState(656);
			expr();
			setState(657);
			match(COLON);
			setState(658);
			expr();
			setState(661);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(659);
				match(COLON);
				setState(660);
				expr();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class SimpleLineContext extends ParserRuleContext {
		public List<LineStmtContext> lineStmt() {
			return getRuleContexts(LineStmtContext.class);
		}
		public LineStmtContext lineStmt(int i) {
			return getRuleContext(LineStmtContext.class,i);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public List<TerminalNode> SEMI() { return getTokens(FooParser.SEMI); }
		public TerminalNode SEMI(int i) {
			return getToken(FooParser.SEMI, i);
		}
		public SimpleLineContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_simpleLine; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterSimpleLine(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitSimpleLine(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitSimpleLine(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SimpleLineContext simpleLine() throws RecognitionException {
		SimpleLineContext _localctx = new SimpleLineContext(_ctx, getState());
		enterRule(_localctx, 92, RULE_simpleLine);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(663);
			lineStmt();
			setState(670);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEMI) {
				{
				{
				setState(664);
				match(SEMI);
				setState(666);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,80,_ctx) ) {
				case 1:
					{
					setState(665);
					lineStmt();
					}
					break;
				}
				}
				}
				setState(672);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(673);
			match(NEWLINE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LineStmtContext extends ParserRuleContext {
		public OneLineIfContext oneLineIf() {
			return getRuleContext(OneLineIfContext.class,0);
		}
		public OneLineWhereContext oneLineWhere() {
			return getRuleContext(OneLineWhereContext.class,0);
		}
		public SimpleStmtContext simpleStmt() {
			return getRuleContext(SimpleStmtContext.class,0);
		}
		public LineStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lineStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterLineStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitLineStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitLineStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LineStmtContext lineStmt() throws RecognitionException {
		LineStmtContext _localctx = new LineStmtContext(_ctx, getState());
		enterRule(_localctx, 94, RULE_lineStmt);
		try {
			setState(678);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,82,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(675);
				oneLineIf();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(676);
				oneLineWhere();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(677);
				simpleStmt();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class OneLineIfContext extends ParserRuleContext {
		public TerminalNode IF() { return getToken(FooParser.IF, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public SimpleStmtContext simpleStmt() {
			return getRuleContext(SimpleStmtContext.class,0);
		}
		public OneLineIfContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_oneLineIf; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterOneLineIf(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitOneLineIf(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitOneLineIf(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OneLineIfContext oneLineIf() throws RecognitionException {
		OneLineIfContext _localctx = new OneLineIfContext(_ctx, getState());
		enterRule(_localctx, 96, RULE_oneLineIf);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(680);
			match(IF);
			setState(681);
			match(LPAREN);
			setState(682);
			expr();
			setState(683);
			match(RPAREN);
			setState(684);
			simpleStmt();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class OneLineWhereContext extends ParserRuleContext {
		public TerminalNode WHERE() { return getToken(FooParser.WHERE, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public SimpleStmtContext simpleStmt() {
			return getRuleContext(SimpleStmtContext.class,0);
		}
		public OneLineWhereContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_oneLineWhere; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterOneLineWhere(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitOneLineWhere(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitOneLineWhere(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OneLineWhereContext oneLineWhere() throws RecognitionException {
		OneLineWhereContext _localctx = new OneLineWhereContext(_ctx, getState());
		enterRule(_localctx, 98, RULE_oneLineWhere);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(686);
			match(WHERE);
			setState(687);
			match(LPAREN);
			setState(688);
			expr();
			setState(689);
			match(RPAREN);
			setState(690);
			simpleStmt();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class SimpleStmtContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public ArgListContext argList() {
			return getRuleContext(ArgListContext.class,0);
		}
		public IoTailContext ioTail() {
			return getRuleContext(IoTailContext.class,0);
		}
		public PostfixContext postfix() {
			return getRuleContext(PostfixContext.class,0);
		}
		public TerminalNode EQUAL() { return getToken(FooParser.EQUAL, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode ARROW() { return getToken(FooParser.ARROW, 0); }
		public TerminalNode EXIT() { return getToken(FooParser.EXIT, 0); }
		public TerminalNode CYCLE() { return getToken(FooParser.CYCLE, 0); }
		public TerminalNode RETURN() { return getToken(FooParser.RETURN, 0); }
		public SimpleStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_simpleStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterSimpleStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitSimpleStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitSimpleStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SimpleStmtContext simpleStmt() throws RecognitionException {
		SimpleStmtContext _localctx = new SimpleStmtContext(_ctx, getState());
		enterRule(_localctx, 100, RULE_simpleStmt);
		try {
			setState(719);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,88,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(692);
				if (!(("write".equalsIgnoreCase(_input.LT(1).getText()) || "read".equalsIgnoreCase(_input.LT(1).getText()))
				        && _input.LT(2).getType()==LPAREN)) throw new FailedPredicateException(this, "(\"write\".equalsIgnoreCase(_input.LT(1).getText()) || \"read\".equalsIgnoreCase(_input.LT(1).getText()))\n        && _input.LT(2).getType()==LPAREN");
				setState(693);
				name();
				setState(694);
				match(LPAREN);
				setState(696);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,83,_ctx) ) {
				case 1:
					{
					setState(695);
					argList();
					}
					break;
				}
				setState(698);
				match(RPAREN);
				setState(700);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,84,_ctx) ) {
				case 1:
					{
					setState(699);
					ioTail();
					}
					break;
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(702);
				postfix();
				setState(708);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,85,_ctx) ) {
				case 1:
					{
					setState(703);
					match(EQUAL);
					setState(704);
					expr();
					}
					break;
				case 2:
					{
					setState(705);
					match(ARROW);
					setState(706);
					expr();
					}
					break;
				case 3:
					{
					setState(707);
					ioTail();
					}
					break;
				}
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(710);
				match(EXIT);
				setState(712);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,86,_ctx) ) {
				case 1:
					{
					setState(711);
					name();
					}
					break;
				}
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(714);
				match(CYCLE);
				setState(716);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,87,_ctx) ) {
				case 1:
					{
					setState(715);
					name();
					}
					break;
				}
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(718);
				match(RETURN);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class IoTailContext extends ParserRuleContext {
		public List<ArgContext> arg() {
			return getRuleContexts(ArgContext.class);
		}
		public ArgContext arg(int i) {
			return getRuleContext(ArgContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public IoTailContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ioTail; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterIoTail(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitIoTail(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitIoTail(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IoTailContext ioTail() throws RecognitionException {
		IoTailContext _localctx = new IoTailContext(_ctx, getState());
		enterRule(_localctx, 102, RULE_ioTail);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(721);
			arg();
			setState(726);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(722);
				match(COMMA);
				setState(723);
				arg();
				}
				}
				setState(728);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class IfStmtContext extends ParserRuleContext {
		public TerminalNode IF() { return getToken(FooParser.IF, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public TerminalNode THEN() { return getToken(FooParser.THEN, 0); }
		public InlineBodyContext inlineBody() {
			return getRuleContext(InlineBodyContext.class,0);
		}
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public List<ElseIfClauseContext> elseIfClause() {
			return getRuleContexts(ElseIfClauseContext.class);
		}
		public ElseIfClauseContext elseIfClause(int i) {
			return getRuleContext(ElseIfClauseContext.class,i);
		}
		public ElseClauseContext elseClause() {
			return getRuleContext(ElseClauseContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public IfStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ifStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterIfStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitIfStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitIfStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IfStmtContext ifStmt() throws RecognitionException {
		IfStmtContext _localctx = new IfStmtContext(_ctx, getState());
		enterRule(_localctx, 104, RULE_ifStmt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(729);
			match(IF);
			setState(730);
			match(LPAREN);
			setState(731);
			expr();
			setState(732);
			match(RPAREN);
			setState(733);
			match(THEN);
			setState(734);
			inlineBody();
			setState(738);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ELSEIF) {
				{
				{
				setState(735);
				elseIfClause();
				}
				}
				setState(740);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(742);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==ELSE) {
				{
				setState(741);
				elseClause();
				}
			}

			setState(744);
			endKw();
			setState(746);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,92,_ctx) ) {
			case 1:
				{
				setState(745);
				match(NEWLINE);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class InlineBodyContext extends ParserRuleContext {
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public List<TerminalNode> SEMI() { return getTokens(FooParser.SEMI); }
		public TerminalNode SEMI(int i) {
			return getToken(FooParser.SEMI, i);
		}
		public List<ProcBodyContext> procBody() {
			return getRuleContexts(ProcBodyContext.class);
		}
		public ProcBodyContext procBody(int i) {
			return getRuleContext(ProcBodyContext.class,i);
		}
		public List<SimpleStmtContext> simpleStmt() {
			return getRuleContexts(SimpleStmtContext.class);
		}
		public SimpleStmtContext simpleStmt(int i) {
			return getRuleContext(SimpleStmtContext.class,i);
		}
		public InlineBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_inlineBody; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterInlineBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitInlineBody(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitInlineBody(this);
			else return visitor.visitChildren(this);
		}
	}

	public final InlineBodyContext inlineBody() throws RecognitionException {
		InlineBodyContext _localctx = new InlineBodyContext(_ctx, getState());
		enterRule(_localctx, 106, RULE_inlineBody);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(754);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEMI) {
				{
				{
				setState(748);
				match(SEMI);
				setState(750);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,93,_ctx) ) {
				case 1:
					{
					setState(749);
					simpleStmt();
					}
					break;
				}
				}
				}
				setState(756);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(757);
			match(NEWLINE);
			setState(761);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,95,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(758);
					procBody();
					}
					} 
				}
				setState(763);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,95,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ElseIfClauseContext extends ParserRuleContext {
		public TerminalNode ELSEIF() { return getToken(FooParser.ELSEIF, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public TerminalNode THEN() { return getToken(FooParser.THEN, 0); }
		public InlineBodyContext inlineBody() {
			return getRuleContext(InlineBodyContext.class,0);
		}
		public ElseIfClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elseIfClause; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterElseIfClause(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitElseIfClause(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitElseIfClause(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ElseIfClauseContext elseIfClause() throws RecognitionException {
		ElseIfClauseContext _localctx = new ElseIfClauseContext(_ctx, getState());
		enterRule(_localctx, 108, RULE_elseIfClause);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(764);
			match(ELSEIF);
			setState(765);
			match(LPAREN);
			setState(766);
			expr();
			setState(767);
			match(RPAREN);
			setState(768);
			match(THEN);
			setState(769);
			inlineBody();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ElseClauseContext extends ParserRuleContext {
		public TerminalNode ELSE() { return getToken(FooParser.ELSE, 0); }
		public InlineBodyContext inlineBody() {
			return getRuleContext(InlineBodyContext.class,0);
		}
		public ElseClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elseClause; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterElseClause(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitElseClause(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitElseClause(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ElseClauseContext elseClause() throws RecognitionException {
		ElseClauseContext _localctx = new ElseClauseContext(_ctx, getState());
		enterRule(_localctx, 110, RULE_elseClause);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(771);
			match(ELSE);
			setState(772);
			inlineBody();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DoStmtContext extends ParserRuleContext {
		public TerminalNode DO() { return getToken(FooParser.DO, 0); }
		public List<TerminalNode> NEWLINE() { return getTokens(FooParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(FooParser.NEWLINE, i);
		}
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public TerminalNode COLON() { return getToken(FooParser.COLON, 0); }
		public LoopHeaderContext loopHeader() {
			return getRuleContext(LoopHeaderContext.class,0);
		}
		public TerminalNode WHILE() { return getToken(FooParser.WHILE, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public ReduceClauseContext reduceClause() {
			return getRuleContext(ReduceClauseContext.class,0);
		}
		public List<ProcBodyContext> procBody() {
			return getRuleContexts(ProcBodyContext.class);
		}
		public ProcBodyContext procBody(int i) {
			return getRuleContext(ProcBodyContext.class,i);
		}
		public DoStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_doStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterDoStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitDoStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitDoStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DoStmtContext doStmt() throws RecognitionException {
		DoStmtContext _localctx = new DoStmtContext(_ctx, getState());
		enterRule(_localctx, 112, RULE_doStmt);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(777);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,96,_ctx) ) {
			case 1:
				{
				setState(774);
				name();
				setState(775);
				match(COLON);
				}
				break;
			}
			setState(780);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,97,_ctx) ) {
			case 1:
				{
				setState(779);
				name();
				}
				break;
			}
			setState(782);
			match(DO);
			setState(789);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,98,_ctx) ) {
			case 1:
				{
				setState(783);
				loopHeader();
				}
				break;
			case 2:
				{
				setState(784);
				match(WHILE);
				setState(785);
				match(LPAREN);
				setState(786);
				expr();
				setState(787);
				match(RPAREN);
				}
				break;
			}
			setState(792);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,99,_ctx) ) {
			case 1:
				{
				setState(791);
				reduceClause();
				}
				break;
			}
			setState(794);
			match(NEWLINE);
			setState(798);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,100,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(795);
					procBody();
					}
					} 
				}
				setState(800);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,100,_ctx);
			}
			setState(801);
			endKw();
			setState(803);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,101,_ctx) ) {
			case 1:
				{
				setState(802);
				name();
				}
				break;
			}
			setState(806);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,102,_ctx) ) {
			case 1:
				{
				setState(805);
				match(NEWLINE);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LoopHeaderContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(FooParser.IDENTIFIER, 0); }
		public TerminalNode EQUAL() { return getToken(FooParser.EQUAL, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public LoopHeaderContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_loopHeader; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterLoopHeader(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitLoopHeader(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitLoopHeader(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LoopHeaderContext loopHeader() throws RecognitionException {
		LoopHeaderContext _localctx = new LoopHeaderContext(_ctx, getState());
		enterRule(_localctx, 114, RULE_loopHeader);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(808);
			match(IDENTIFIER);
			setState(809);
			match(EQUAL);
			setState(810);
			expr();
			setState(811);
			match(COMMA);
			setState(812);
			expr();
			setState(815);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,103,_ctx) ) {
			case 1:
				{
				setState(813);
				match(COMMA);
				setState(814);
				expr();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ReduceClauseContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public ReduceClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_reduceClause; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterReduceClause(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitReduceClause(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitReduceClause(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ReduceClauseContext reduceClause() throws RecognitionException {
		ReduceClauseContext _localctx = new ReduceClauseContext(_ctx, getState());
		enterRule(_localctx, 116, RULE_reduceClause);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(817);
			name();
			setState(818);
			match(LPAREN);
			setState(819);
			expr();
			setState(824);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(820);
				match(COMMA);
				setState(821);
				expr();
				}
				}
				setState(826);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(827);
			match(RPAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class SelectStmtContext extends ParserRuleContext {
		public TerminalNode SELECT() { return getToken(FooParser.SELECT, 0); }
		public TerminalNode CASE() { return getToken(FooParser.CASE, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public List<TerminalNode> NEWLINE() { return getTokens(FooParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(FooParser.NEWLINE, i);
		}
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public List<CaseClauseContext> caseClause() {
			return getRuleContexts(CaseClauseContext.class);
		}
		public CaseClauseContext caseClause(int i) {
			return getRuleContext(CaseClauseContext.class,i);
		}
		public SelectStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterSelectStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitSelectStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitSelectStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SelectStmtContext selectStmt() throws RecognitionException {
		SelectStmtContext _localctx = new SelectStmtContext(_ctx, getState());
		enterRule(_localctx, 118, RULE_selectStmt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(829);
			match(SELECT);
			setState(830);
			match(CASE);
			setState(831);
			match(LPAREN);
			setState(832);
			expr();
			setState(833);
			match(RPAREN);
			setState(834);
			match(NEWLINE);
			setState(839);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==CASE || _la==NEWLINE) {
				{
				setState(837);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case CASE:
					{
					setState(835);
					caseClause();
					}
					break;
				case NEWLINE:
					{
					setState(836);
					match(NEWLINE);
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				}
				setState(841);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(842);
			endKw();
			setState(844);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,107,_ctx) ) {
			case 1:
				{
				setState(843);
				match(NEWLINE);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class WhereStmtContext extends ParserRuleContext {
		public TerminalNode WHERE() { return getToken(FooParser.WHERE, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public InlineBodyContext inlineBody() {
			return getRuleContext(InlineBodyContext.class,0);
		}
		public EndKwContext endKw() {
			return getRuleContext(EndKwContext.class,0);
		}
		public List<ElsewhereClauseContext> elsewhereClause() {
			return getRuleContexts(ElsewhereClauseContext.class);
		}
		public ElsewhereClauseContext elsewhereClause(int i) {
			return getRuleContext(ElsewhereClauseContext.class,i);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public WhereStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_whereStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterWhereStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitWhereStmt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitWhereStmt(this);
			else return visitor.visitChildren(this);
		}
	}

	public final WhereStmtContext whereStmt() throws RecognitionException {
		WhereStmtContext _localctx = new WhereStmtContext(_ctx, getState());
		enterRule(_localctx, 120, RULE_whereStmt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(846);
			match(WHERE);
			setState(847);
			match(LPAREN);
			setState(848);
			expr();
			setState(849);
			match(RPAREN);
			setState(850);
			inlineBody();
			setState(854);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ELSEWHERE) {
				{
				{
				setState(851);
				elsewhereClause();
				}
				}
				setState(856);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(857);
			endKw();
			setState(859);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,109,_ctx) ) {
			case 1:
				{
				setState(858);
				match(NEWLINE);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ElsewhereClauseContext extends ParserRuleContext {
		public TerminalNode ELSEWHERE() { return getToken(FooParser.ELSEWHERE, 0); }
		public InlineBodyContext inlineBody() {
			return getRuleContext(InlineBodyContext.class,0);
		}
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public ElsewhereClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elsewhereClause; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterElsewhereClause(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitElsewhereClause(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitElsewhereClause(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ElsewhereClauseContext elsewhereClause() throws RecognitionException {
		ElsewhereClauseContext _localctx = new ElsewhereClauseContext(_ctx, getState());
		enterRule(_localctx, 122, RULE_elsewhereClause);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(861);
			match(ELSEWHERE);
			setState(866);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LPAREN) {
				{
				setState(862);
				match(LPAREN);
				setState(863);
				expr();
				setState(864);
				match(RPAREN);
				}
			}

			setState(868);
			inlineBody();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class CaseClauseContext extends ParserRuleContext {
		public CaseLabelContext caseLabel() {
			return getRuleContext(CaseLabelContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(FooParser.NEWLINE, 0); }
		public List<TerminalNode> SEMI() { return getTokens(FooParser.SEMI); }
		public TerminalNode SEMI(int i) {
			return getToken(FooParser.SEMI, i);
		}
		public List<ProcBodyContext> procBody() {
			return getRuleContexts(ProcBodyContext.class);
		}
		public ProcBodyContext procBody(int i) {
			return getRuleContext(ProcBodyContext.class,i);
		}
		public List<SimpleStmtContext> simpleStmt() {
			return getRuleContexts(SimpleStmtContext.class);
		}
		public SimpleStmtContext simpleStmt(int i) {
			return getRuleContext(SimpleStmtContext.class,i);
		}
		public CaseClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_caseClause; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterCaseClause(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitCaseClause(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitCaseClause(this);
			else return visitor.visitChildren(this);
		}
	}

	public final CaseClauseContext caseClause() throws RecognitionException {
		CaseClauseContext _localctx = new CaseClauseContext(_ctx, getState());
		enterRule(_localctx, 124, RULE_caseClause);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(870);
			caseLabel();
			setState(877);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEMI) {
				{
				{
				setState(871);
				match(SEMI);
				setState(873);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,111,_ctx) ) {
				case 1:
					{
					setState(872);
					simpleStmt();
					}
					break;
				}
				}
				}
				setState(879);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(880);
			match(NEWLINE);
			setState(884);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,113,_ctx);
			while ( _alt!=1 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1+1 ) {
					{
					{
					setState(881);
					procBody();
					}
					} 
				}
				setState(886);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,113,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class CaseLabelContext extends ParserRuleContext {
		public TerminalNode CASE() { return getToken(FooParser.CASE, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public List<ArgContext> arg() {
			return getRuleContexts(ArgContext.class);
		}
		public ArgContext arg(int i) {
			return getRuleContext(ArgContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public TerminalNode DEFAULT() { return getToken(FooParser.DEFAULT, 0); }
		public CaseLabelContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_caseLabel; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterCaseLabel(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitCaseLabel(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitCaseLabel(this);
			else return visitor.visitChildren(this);
		}
	}

	public final CaseLabelContext caseLabel() throws RecognitionException {
		CaseLabelContext _localctx = new CaseLabelContext(_ctx, getState());
		enterRule(_localctx, 126, RULE_caseLabel);
		int _la;
		try {
			setState(901);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,115,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(887);
				match(CASE);
				setState(888);
				match(LPAREN);
				setState(889);
				arg();
				setState(894);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(890);
					match(COMMA);
					setState(891);
					arg();
					}
					}
					setState(896);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(897);
				match(RPAREN);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(899);
				match(CASE);
				setState(900);
				match(DEFAULT);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExprContext extends ParserRuleContext {
		public List<PostfixContext> postfix() {
			return getRuleContexts(PostfixContext.class);
		}
		public PostfixContext postfix(int i) {
			return getRuleContext(PostfixContext.class,i);
		}
		public List<BinOpContext> binOp() {
			return getRuleContexts(BinOpContext.class);
		}
		public BinOpContext binOp(int i) {
			return getRuleContext(BinOpContext.class,i);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitExpr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 128, RULE_expr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(903);
			postfix();
			setState(909);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,116,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(904);
					binOp();
					setState(905);
					postfix();
					}
					} 
				}
				setState(911);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,116,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PostfixContext extends ParserRuleContext {
		public HeadContext head() {
			return getRuleContext(HeadContext.class,0);
		}
		public List<TrailerContext> trailer() {
			return getRuleContexts(TrailerContext.class);
		}
		public TrailerContext trailer(int i) {
			return getRuleContext(TrailerContext.class,i);
		}
		public PostfixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_postfix; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterPostfix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitPostfix(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitPostfix(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PostfixContext postfix() throws RecognitionException {
		PostfixContext _localctx = new PostfixContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_postfix);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(912);
			head();
			setState(916);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,117,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(913);
					trailer();
					}
					} 
				}
				setState(918);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,117,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class HeadContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ArgListContext argList() {
			return getRuleContext(ArgListContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public ArrayConstructorContext arrayConstructor() {
			return getRuleContext(ArrayConstructorContext.class,0);
		}
		public TerminalNode NOT() { return getToken(FooParser.NOT, 0); }
		public PostfixContext postfix() {
			return getRuleContext(PostfixContext.class,0);
		}
		public TerminalNode MINUS() { return getToken(FooParser.MINUS, 0); }
		public TerminalNode PLUS() { return getToken(FooParser.PLUS, 0); }
		public CallHeadContext callHead() {
			return getRuleContext(CallHeadContext.class,0);
		}
		public LiteralContext literal() {
			return getRuleContext(LiteralContext.class,0);
		}
		public HeadContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_head; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterHead(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitHead(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitHead(this);
			else return visitor.visitChildren(this);
		}
	}

	public final HeadContext head() throws RecognitionException {
		HeadContext _localctx = new HeadContext(_ctx, getState());
		enterRule(_localctx, 132, RULE_head);
		try {
			setState(932);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,118,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(919);
				match(LPAREN);
				setState(920);
				argList();
				setState(921);
				match(RPAREN);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(923);
				arrayConstructor();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(924);
				match(NOT);
				setState(925);
				postfix();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(926);
				match(MINUS);
				setState(927);
				postfix();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(928);
				match(PLUS);
				setState(929);
				postfix();
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(930);
				callHead();
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(931);
				literal();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class CallHeadContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode DCOLON() { return getToken(FooParser.DCOLON, 0); }
		public TerminalNode COLON() { return getToken(FooParser.COLON, 0); }
		public TerminalNode DOT() { return getToken(FooParser.DOT, 0); }
		public QualifierContext qualifier() {
			return getRuleContext(QualifierContext.class,0);
		}
		public CallHeadContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_callHead; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterCallHead(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitCallHead(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitCallHead(this);
			else return visitor.visitChildren(this);
		}
	}

	public final CallHeadContext callHead() throws RecognitionException {
		CallHeadContext _localctx = new CallHeadContext(_ctx, getState());
		enterRule(_localctx, 134, RULE_callHead);
		int _la;
		try {
			setState(945);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,121,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(935);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==DOT) {
					{
					setState(934);
					match(DOT);
					}
				}

				setState(938);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 30)) & ~0x3f) == 0 && ((1L << (_la - 30)) & 1152921504606879743L) != 0)) {
					{
					setState(937);
					qualifier();
					}
				}

				setState(940);
				_la = _input.LA(1);
				if ( !(_la==DCOLON || _la==COLON) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(941);
				name();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(942);
				match(DOT);
				setState(943);
				name();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(944);
				name();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TrailerContext extends ParserRuleContext {
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public TerminalNode DOT() { return getToken(FooParser.DOT, 0); }
		public TerminalNode PERCENT() { return getToken(FooParser.PERCENT, 0); }
		public TerminalNode DCOLON() { return getToken(FooParser.DCOLON, 0); }
		public TerminalNode COLON() { return getToken(FooParser.COLON, 0); }
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public ArgListContext argList() {
			return getRuleContext(ArgListContext.class,0);
		}
		public TerminalNode LBRACKET() { return getToken(FooParser.LBRACKET, 0); }
		public TerminalNode RBRACKET() { return getToken(FooParser.RBRACKET, 0); }
		public TrailerContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_trailer; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterTrailer(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitTrailer(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitTrailer(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TrailerContext trailer() throws RecognitionException {
		TrailerContext _localctx = new TrailerContext(_ctx, getState());
		enterRule(_localctx, 136, RULE_trailer);
		int _la;
		try {
			setState(967);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,124,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(947);
				_la = _input.LA(1);
				if ( !(_la==PERCENT || _la==DOT) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(948);
				name();
				setState(949);
				_la = _input.LA(1);
				if ( !(_la==DCOLON || _la==COLON) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(950);
				name();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(952);
				_la = _input.LA(1);
				if ( !(_la==PERCENT || _la==DOT) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(953);
				_la = _input.LA(1);
				if ( !(_la==DCOLON || _la==COLON) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(954);
				name();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(955);
				_la = _input.LA(1);
				if ( !(_la==PERCENT || _la==DOT) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(956);
				name();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(957);
				match(LPAREN);
				setState(959);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,122,_ctx) ) {
				case 1:
					{
					setState(958);
					argList();
					}
					break;
				}
				setState(961);
				match(RPAREN);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(962);
				match(LBRACKET);
				setState(964);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,123,_ctx) ) {
				case 1:
					{
					setState(963);
					argList();
					}
					break;
				}
				setState(966);
				match(RBRACKET);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class QualifierContext extends ParserRuleContext {
		public TypeSpecContext typeSpec() {
			return getRuleContext(TypeSpecContext.class,0);
		}
		public QualifierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qualifier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterQualifier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitQualifier(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitQualifier(this);
			else return visitor.visitChildren(this);
		}
	}

	public final QualifierContext qualifier() throws RecognitionException {
		QualifierContext _localctx = new QualifierContext(_ctx, getState());
		enterRule(_localctx, 138, RULE_qualifier);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(969);
			typeSpec();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ArgListContext extends ParserRuleContext {
		public List<ArgContext> arg() {
			return getRuleContexts(ArgContext.class);
		}
		public ArgContext arg(int i) {
			return getRuleContext(ArgContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public ArgListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_argList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterArgList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitArgList(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitArgList(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgListContext argList() throws RecognitionException {
		ArgListContext _localctx = new ArgListContext(_ctx, getState());
		enterRule(_localctx, 140, RULE_argList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(971);
			arg();
			setState(976);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(972);
				match(COMMA);
				setState(973);
				arg();
				}
				}
				setState(978);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ArgContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode EQUAL() { return getToken(FooParser.EQUAL, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode STAR() { return getToken(FooParser.STAR, 0); }
		public List<TerminalNode> COLON() { return getTokens(FooParser.COLON); }
		public TerminalNode COLON(int i) {
			return getToken(FooParser.COLON, i);
		}
		public TerminalNode DCOLON() { return getToken(FooParser.DCOLON, 0); }
		public ArgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arg; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterArg(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitArg(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitArg(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgContext arg() throws RecognitionException {
		ArgContext _localctx = new ArgContext(_ctx, getState());
		enterRule(_localctx, 142, RULE_arg);
		int _la;
		try {
			setState(1017);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,136,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(979);
				name();
				setState(980);
				match(EQUAL);
				setState(983);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,126,_ctx) ) {
				case 1:
					{
					setState(981);
					expr();
					}
					break;
				case 2:
					{
					setState(982);
					match(STAR);
					}
					break;
				}
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(985);
				match(STAR);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(986);
				expr();
				setState(1001);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case COLON:
					{
					setState(987);
					match(COLON);
					setState(989);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,127,_ctx) ) {
					case 1:
						{
						setState(988);
						expr();
						}
						break;
					}
					setState(995);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if (_la==COLON) {
						{
						setState(991);
						match(COLON);
						setState(993);
						_errHandler.sync(this);
						switch ( getInterpreter().adaptivePredict(_input,128,_ctx) ) {
						case 1:
							{
							setState(992);
							expr();
							}
							break;
						}
						}
					}

					}
					break;
				case DCOLON:
					{
					setState(997);
					match(DCOLON);
					setState(999);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,130,_ctx) ) {
					case 1:
						{
						setState(998);
						expr();
						}
						break;
					}
					}
					break;
				case COMMA:
				case RPAREN:
				case RBRACKET:
				case SEMI:
				case NEWLINE:
					break;
				default:
					break;
				}
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(1003);
				match(COLON);
				setState(1005);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,132,_ctx) ) {
				case 1:
					{
					setState(1004);
					expr();
					}
					break;
				}
				setState(1011);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==COLON) {
					{
					setState(1007);
					match(COLON);
					setState(1009);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,133,_ctx) ) {
					case 1:
						{
						setState(1008);
						expr();
						}
						break;
					}
					}
				}

				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(1013);
				match(DCOLON);
				setState(1015);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,135,_ctx) ) {
				case 1:
					{
					setState(1014);
					expr();
					}
					break;
				}
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LiteralContext extends ParserRuleContext {
		public TerminalNode INTEGER() { return getToken(FooParser.INTEGER, 0); }
		public TerminalNode REAL_NUMBER() { return getToken(FooParser.REAL_NUMBER, 0); }
		public TerminalNode STRING() { return getToken(FooParser.STRING, 0); }
		public TerminalNode BOZ() { return getToken(FooParser.BOZ, 0); }
		public TerminalNode TRUE() { return getToken(FooParser.TRUE, 0); }
		public TerminalNode FALSE() { return getToken(FooParser.FALSE, 0); }
		public TerminalNode NULL() { return getToken(FooParser.NULL, 0); }
		public TerminalNode ZERO() { return getToken(FooParser.ZERO, 0); }
		public TerminalNode ONE() { return getToken(FooParser.ONE, 0); }
		public LiteralContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_literal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterLiteral(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitLiteral(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitLiteral(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LiteralContext literal() throws RecognitionException {
		LiteralContext _localctx = new LiteralContext(_ctx, getState());
		enterRule(_localctx, 144, RULE_literal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1019);
			_la = _input.LA(1);
			if ( !(((((_la - 84)) & ~0x3f) == 0 && ((1L << (_la - 84)) & 959L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ArrayConstructorContext extends ParserRuleContext {
		public TerminalNode LBRACKET() { return getToken(FooParser.LBRACKET, 0); }
		public TerminalNode RBRACKET() { return getToken(FooParser.RBRACKET, 0); }
		public List<AcElemContext> acElem() {
			return getRuleContexts(AcElemContext.class);
		}
		public AcElemContext acElem(int i) {
			return getRuleContext(AcElemContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(FooParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(FooParser.COMMA, i);
		}
		public ArrayConstructorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arrayConstructor; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterArrayConstructor(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitArrayConstructor(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitArrayConstructor(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArrayConstructorContext arrayConstructor() throws RecognitionException {
		ArrayConstructorContext _localctx = new ArrayConstructorContext(_ctx, getState());
		enterRule(_localctx, 146, RULE_arrayConstructor);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1021);
			match(LBRACKET);
			setState(1030);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,138,_ctx) ) {
			case 1:
				{
				setState(1022);
				acElem();
				setState(1027);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(1023);
					match(COMMA);
					setState(1024);
					acElem();
					}
					}
					setState(1029);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
				break;
			}
			setState(1032);
			match(RBRACKET);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AcElemContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(FooParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode COMMA() { return getToken(FooParser.COMMA, 0); }
		public LoopHeaderContext loopHeader() {
			return getRuleContext(LoopHeaderContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(FooParser.RPAREN, 0); }
		public AcElemContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_acElem; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterAcElem(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitAcElem(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitAcElem(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AcElemContext acElem() throws RecognitionException {
		AcElemContext _localctx = new AcElemContext(_ctx, getState());
		enterRule(_localctx, 148, RULE_acElem);
		try {
			setState(1041);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,139,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(1034);
				match(LPAREN);
				setState(1035);
				expr();
				setState(1036);
				match(COMMA);
				setState(1037);
				loopHeader();
				setState(1038);
				match(RPAREN);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(1040);
				expr();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BinOpContext extends ParserRuleContext {
		public TerminalNode PLUS() { return getToken(FooParser.PLUS, 0); }
		public TerminalNode MINUS() { return getToken(FooParser.MINUS, 0); }
		public TerminalNode POWER() { return getToken(FooParser.POWER, 0); }
		public TerminalNode STAR() { return getToken(FooParser.STAR, 0); }
		public TerminalNode SLASH() { return getToken(FooParser.SLASH, 0); }
		public TerminalNode CONCAT() { return getToken(FooParser.CONCAT, 0); }
		public TerminalNode EQUAL_OP() { return getToken(FooParser.EQUAL_OP, 0); }
		public TerminalNode NOT_EQUAL() { return getToken(FooParser.NOT_EQUAL, 0); }
		public TerminalNode LT() { return getToken(FooParser.LT, 0); }
		public TerminalNode LE() { return getToken(FooParser.LE, 0); }
		public TerminalNode GT() { return getToken(FooParser.GT, 0); }
		public TerminalNode GE() { return getToken(FooParser.GE, 0); }
		public TerminalNode AND() { return getToken(FooParser.AND, 0); }
		public TerminalNode OR() { return getToken(FooParser.OR, 0); }
		public TerminalNode EQV() { return getToken(FooParser.EQV, 0); }
		public TerminalNode NEQV() { return getToken(FooParser.NEQV, 0); }
		public TerminalNode EQ() { return getToken(FooParser.EQ, 0); }
		public TerminalNode NE() { return getToken(FooParser.NE, 0); }
		public TerminalNode LT_OP() { return getToken(FooParser.LT_OP, 0); }
		public TerminalNode LE_OP() { return getToken(FooParser.LE_OP, 0); }
		public TerminalNode GT_OP() { return getToken(FooParser.GT_OP, 0); }
		public TerminalNode GE_OP() { return getToken(FooParser.GE_OP, 0); }
		public BinOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_binOp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).enterBinOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FooListener ) ((FooListener)listener).exitBinOp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof FooVisitor ) return ((FooVisitor<? extends T>)visitor).visitBinOp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final BinOpContext binOp() throws RecognitionException {
		BinOpContext _localctx = new BinOpContext(_ctx, getState());
		enterRule(_localctx, 150, RULE_binOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(1043);
			_la = _input.LA(1);
			if ( !(((((_la - 58)) & ~0x3f) == 0 && ((1L << (_la - 58)) & 16744415L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 32:
			return name_sempred((NameContext)_localctx, predIndex);
		case 50:
			return simpleStmt_sempred((SimpleStmtContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean name_sempred(NameContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return _input.LA(2) != NEWLINE;
		}
		return true;
	}
	private boolean simpleStmt_sempred(SimpleStmtContext _localctx, int predIndex) {
		switch (predIndex) {
		case 1:
			return ("write".equalsIgnoreCase(_input.LT(1).getText()) || "read".equalsIgnoreCase(_input.LT(1).getText()))
		        && _input.LT(2).getType()==LPAREN;
		}
		return true;
	}

	public static final String _serializedATN =
		"\u0004\u0001c\u0416\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002"+
		"\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f"+
		"\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002\u0012\u0007\u0012"+
		"\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014\u0002\u0015\u0007\u0015"+
		"\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017\u0002\u0018\u0007\u0018"+
		"\u0002\u0019\u0007\u0019\u0002\u001a\u0007\u001a\u0002\u001b\u0007\u001b"+
		"\u0002\u001c\u0007\u001c\u0002\u001d\u0007\u001d\u0002\u001e\u0007\u001e"+
		"\u0002\u001f\u0007\u001f\u0002 \u0007 \u0002!\u0007!\u0002\"\u0007\"\u0002"+
		"#\u0007#\u0002$\u0007$\u0002%\u0007%\u0002&\u0007&\u0002\'\u0007\'\u0002"+
		"(\u0007(\u0002)\u0007)\u0002*\u0007*\u0002+\u0007+\u0002,\u0007,\u0002"+
		"-\u0007-\u0002.\u0007.\u0002/\u0007/\u00020\u00070\u00021\u00071\u0002"+
		"2\u00072\u00023\u00073\u00024\u00074\u00025\u00075\u00026\u00076\u0002"+
		"7\u00077\u00028\u00078\u00029\u00079\u0002:\u0007:\u0002;\u0007;\u0002"+
		"<\u0007<\u0002=\u0007=\u0002>\u0007>\u0002?\u0007?\u0002@\u0007@\u0002"+
		"A\u0007A\u0002B\u0007B\u0002C\u0007C\u0002D\u0007D\u0002E\u0007E\u0002"+
		"F\u0007F\u0002G\u0007G\u0002H\u0007H\u0002I\u0007I\u0002J\u0007J\u0002"+
		"K\u0007K\u0001\u0000\u0001\u0000\u0001\u0000\u0005\u0000\u009c\b\u0000"+
		"\n\u0000\f\u0000\u009f\t\u0000\u0001\u0000\u0001\u0000\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0005\u0001\u00a7\b\u0001\n\u0001\f\u0001"+
		"\u00aa\t\u0001\u0001\u0001\u0001\u0001\u0003\u0001\u00ae\b\u0001\u0001"+
		"\u0002\u0003\u0002\u00b1\b\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0005\u0002\u00b7\b\u0002\n\u0002\f\u0002\u00ba\t\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0005\u0002\u00bf\b\u0002\n\u0002\f\u0002\u00c2"+
		"\t\u0002\u0003\u0002\u00c4\b\u0002\u0001\u0002\u0003\u0002\u00c7\b\u0002"+
		"\u0001\u0002\u0003\u0002\u00ca\b\u0002\u0001\u0003\u0001\u0003\u0001\u0003"+
		"\u0005\u0003\u00cf\b\u0003\n\u0003\f\u0003\u00d2\t\u0003\u0001\u0004\u0001"+
		"\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0003"+
		"\u0004\u00db\b\u0004\u0001\u0005\u0003\u0005\u00de\b\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0005\u0005\u00e5\b\u0005"+
		"\n\u0005\f\u0005\u00e8\t\u0005\u0001\u0005\u0001\u0005\u0003\u0005\u00ec"+
		"\b\u0005\u0001\u0006\u0001\u0006\u0003\u0006\u00f0\b\u0006\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0005\u0007\u00fa\b\u0007\n\u0007\f\u0007\u00fd\t\u0007\u0003"+
		"\u0007\u00ff\b\u0007\u0001\u0007\u0001\u0007\u0001\b\u0001\b\u0003\b\u0105"+
		"\b\b\u0001\t\u0001\t\u0001\t\u0001\t\u0001\n\u0001\n\u0001\n\u0001\n\u0005"+
		"\n\u010f\b\n\n\n\f\n\u0112\t\n\u0001\n\u0001\n\u0003\n\u0116\b\n\u0001"+
		"\n\u0001\n\u0001\n\u0005\n\u011b\b\n\n\n\f\n\u011e\t\n\u0001\n\u0001\n"+
		"\u0003\n\u0122\b\n\u0003\n\u0124\b\n\u0001\u000b\u0001\u000b\u0001\u000b"+
		"\u0005\u000b\u0129\b\u000b\n\u000b\f\u000b\u012c\t\u000b\u0001\u000b\u0001"+
		"\u000b\u0001\u000b\u0003\u000b\u0131\b\u000b\u0001\f\u0001\f\u0003\f\u0135"+
		"\b\f\u0001\r\u0001\r\u0001\r\u0005\r\u013a\b\r\n\r\f\r\u013d\t\r\u0001"+
		"\r\u0001\r\u0003\r\u0141\b\r\u0001\u000e\u0001\u000e\u0003\u000e\u0145"+
		"\b\u000e\u0001\u000e\u0003\u000e\u0148\b\u000e\u0001\u000e\u0003\u000e"+
		"\u014b\b\u000e\u0001\u000f\u0001\u000f\u0003\u000f\u014f\b\u000f\u0001"+
		"\u000f\u0001\u000f\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010\u0001"+
		"\u0010\u0001\u0011\u0001\u0011\u0001\u0011\u0001\u0012\u0001\u0012\u0003"+
		"\u0012\u015d\b\u0012\u0001\u0012\u0005\u0012\u0160\b\u0012\n\u0012\f\u0012"+
		"\u0163\t\u0012\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013"+
		"\u0005\u0013\u016a\b\u0013\n\u0013\f\u0013\u016d\t\u0013\u0001\u0013\u0001"+
		"\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0003"+
		"\u0013\u0176\b\u0013\u0003\u0013\u0178\b\u0013\u0001\u0014\u0001\u0014"+
		"\u0003\u0014\u017c\b\u0014\u0001\u0014\u0001\u0014\u0003\u0014\u0180\b"+
		"\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001"+
		"\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0003\u0014\u018c"+
		"\b\u0014\u0001\u0015\u0001\u0015\u0003\u0015\u0190\b\u0015\u0001\u0016"+
		"\u0001\u0016\u0001\u0016\u0003\u0016\u0195\b\u0016\u0001\u0017\u0001\u0017"+
		"\u0001\u0017\u0001\u0017\u0001\u0017\u0001\u0017\u0001\u0017\u0003\u0017"+
		"\u019e\b\u0017\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018"+
		"\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u001a"+
		"\u0001\u001a\u0003\u001a\u01ac\b\u001a\u0001\u001a\u0003\u001a\u01af\b"+
		"\u001a\u0001\u001a\u0003\u001a\u01b2\b\u001a\u0001\u001a\u0001\u001a\u0001"+
		"\u001a\u0005\u001a\u01b7\b\u001a\n\u001a\f\u001a\u01ba\t\u001a\u0001\u001a"+
		"\u0003\u001a\u01bd\b\u001a\u0003\u001a\u01bf\b\u001a\u0001\u001b\u0001"+
		"\u001b\u0001\u001c\u0001\u001c\u0001\u001c\u0003\u001c\u01c6\b\u001c\u0001"+
		"\u001c\u0001\u001c\u0001\u001c\u0001\u001c\u0005\u001c\u01cc\b\u001c\n"+
		"\u001c\f\u001c\u01cf\t\u001c\u0001\u001c\u0003\u001c\u01d2\b\u001c\u0001"+
		"\u001c\u0001\u001c\u0001\u001c\u0001\u001d\u0003\u001d\u01d8\b\u001d\u0001"+
		"\u001d\u0001\u001d\u0003\u001d\u01dc\b\u001d\u0001\u001e\u0001\u001e\u0001"+
		"\u001e\u0005\u001e\u01e1\b\u001e\n\u001e\f\u001e\u01e4\t\u001e\u0001\u001f"+
		"\u0001\u001f\u0003\u001f\u01e8\b\u001f\u0001 \u0001 \u0001 \u0005 \u01ed"+
		"\b \n \f \u01f0\t \u0001 \u0003 \u01f3\b \u0001 \u0001 \u0001 \u0001 "+
		"\u0001 \u0001 \u0001 \u0001 \u0003 \u01fd\b \u0001!\u0001!\u0003!\u0201"+
		"\b!\u0001\"\u0003\"\u0204\b\"\u0001\"\u0004\"\u0207\b\"\u000b\"\f\"\u0208"+
		"\u0001#\u0001#\u0001#\u0001$\u0001$\u0003$\u0210\b$\u0001$\u0003$\u0213"+
		"\b$\u0001%\u0001%\u0001%\u0001%\u0001%\u0003%\u021a\b%\u0001&\u0001&\u0001"+
		"\'\u0001\'\u0001\'\u0001\'\u0001\'\u0005\'\u0223\b\'\n\'\f\'\u0226\t\'"+
		"\u0001\'\u0001\'\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001"+
		"(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001"+
		"(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001"+
		"(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0001(\u0003(\u024d\b(\u0001"+
		")\u0001)\u0001)\u0001)\u0005)\u0253\b)\n)\f)\u0256\t)\u0001)\u0001)\u0001"+
		"*\u0001*\u0001*\u0001*\u0003*\u025e\b*\u0001*\u0001*\u0003*\u0262\b*\u0001"+
		"*\u0003*\u0265\b*\u0001*\u0001*\u0003*\u0269\b*\u0001*\u0001*\u0003*\u026d"+
		"\b*\u0001+\u0001+\u0001+\u0001+\u0001+\u0001+\u0003+\u0275\b+\u0001,\u0001"+
		",\u0001,\u0001,\u0001,\u0001,\u0005,\u027d\b,\n,\f,\u0280\t,\u0001,\u0001"+
		",\u0003,\u0284\b,\u0001,\u0001,\u0001,\u0001,\u0001,\u0001,\u0001,\u0003"+
		",\u028d\b,\u0001-\u0001-\u0001-\u0001-\u0001-\u0001-\u0001-\u0003-\u0296"+
		"\b-\u0001.\u0001.\u0001.\u0003.\u029b\b.\u0005.\u029d\b.\n.\f.\u02a0\t"+
		".\u0001.\u0001.\u0001/\u0001/\u0001/\u0003/\u02a7\b/\u00010\u00010\u0001"+
		"0\u00010\u00010\u00010\u00011\u00011\u00011\u00011\u00011\u00011\u0001"+
		"2\u00012\u00012\u00012\u00032\u02b9\b2\u00012\u00012\u00032\u02bd\b2\u0001"+
		"2\u00012\u00012\u00012\u00012\u00012\u00032\u02c5\b2\u00012\u00012\u0003"+
		"2\u02c9\b2\u00012\u00012\u00032\u02cd\b2\u00012\u00032\u02d0\b2\u0001"+
		"3\u00013\u00013\u00053\u02d5\b3\n3\f3\u02d8\t3\u00014\u00014\u00014\u0001"+
		"4\u00014\u00014\u00014\u00054\u02e1\b4\n4\f4\u02e4\t4\u00014\u00034\u02e7"+
		"\b4\u00014\u00014\u00034\u02eb\b4\u00015\u00015\u00035\u02ef\b5\u0005"+
		"5\u02f1\b5\n5\f5\u02f4\t5\u00015\u00015\u00055\u02f8\b5\n5\f5\u02fb\t"+
		"5\u00016\u00016\u00016\u00016\u00016\u00016\u00016\u00017\u00017\u0001"+
		"7\u00018\u00018\u00018\u00038\u030a\b8\u00018\u00038\u030d\b8\u00018\u0001"+
		"8\u00018\u00018\u00018\u00018\u00018\u00038\u0316\b8\u00018\u00038\u0319"+
		"\b8\u00018\u00018\u00058\u031d\b8\n8\f8\u0320\t8\u00018\u00018\u00038"+
		"\u0324\b8\u00018\u00038\u0327\b8\u00019\u00019\u00019\u00019\u00019\u0001"+
		"9\u00019\u00039\u0330\b9\u0001:\u0001:\u0001:\u0001:\u0001:\u0005:\u0337"+
		"\b:\n:\f:\u033a\t:\u0001:\u0001:\u0001;\u0001;\u0001;\u0001;\u0001;\u0001"+
		";\u0001;\u0001;\u0005;\u0346\b;\n;\f;\u0349\t;\u0001;\u0001;\u0003;\u034d"+
		"\b;\u0001<\u0001<\u0001<\u0001<\u0001<\u0001<\u0005<\u0355\b<\n<\f<\u0358"+
		"\t<\u0001<\u0001<\u0003<\u035c\b<\u0001=\u0001=\u0001=\u0001=\u0001=\u0003"+
		"=\u0363\b=\u0001=\u0001=\u0001>\u0001>\u0001>\u0003>\u036a\b>\u0005>\u036c"+
		"\b>\n>\f>\u036f\t>\u0001>\u0001>\u0005>\u0373\b>\n>\f>\u0376\t>\u0001"+
		"?\u0001?\u0001?\u0001?\u0001?\u0005?\u037d\b?\n?\f?\u0380\t?\u0001?\u0001"+
		"?\u0001?\u0001?\u0003?\u0386\b?\u0001@\u0001@\u0001@\u0001@\u0005@\u038c"+
		"\b@\n@\f@\u038f\t@\u0001A\u0001A\u0005A\u0393\bA\nA\fA\u0396\tA\u0001"+
		"B\u0001B\u0001B\u0001B\u0001B\u0001B\u0001B\u0001B\u0001B\u0001B\u0001"+
		"B\u0001B\u0001B\u0003B\u03a5\bB\u0001C\u0003C\u03a8\bC\u0001C\u0003C\u03ab"+
		"\bC\u0001C\u0001C\u0001C\u0001C\u0001C\u0003C\u03b2\bC\u0001D\u0001D\u0001"+
		"D\u0001D\u0001D\u0001D\u0001D\u0001D\u0001D\u0001D\u0001D\u0001D\u0003"+
		"D\u03c0\bD\u0001D\u0001D\u0001D\u0003D\u03c5\bD\u0001D\u0003D\u03c8\b"+
		"D\u0001E\u0001E\u0001F\u0001F\u0001F\u0005F\u03cf\bF\nF\fF\u03d2\tF\u0001"+
		"G\u0001G\u0001G\u0001G\u0003G\u03d8\bG\u0001G\u0001G\u0001G\u0001G\u0003"+
		"G\u03de\bG\u0001G\u0001G\u0003G\u03e2\bG\u0003G\u03e4\bG\u0001G\u0001"+
		"G\u0003G\u03e8\bG\u0003G\u03ea\bG\u0001G\u0001G\u0003G\u03ee\bG\u0001"+
		"G\u0001G\u0003G\u03f2\bG\u0003G\u03f4\bG\u0001G\u0001G\u0003G\u03f8\b"+
		"G\u0003G\u03fa\bG\u0001H\u0001H\u0001I\u0001I\u0001I\u0001I\u0005I\u0402"+
		"\bI\nI\fI\u0405\tI\u0003I\u0407\bI\u0001I\u0001I\u0001J\u0001J\u0001J"+
		"\u0001J\u0001J\u0001J\u0001J\u0003J\u0412\bJ\u0001K\u0001K\u0001K\u0001"+
		"\u0374\u0000L\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016"+
		"\u0018\u001a\u001c\u001e \"$&(*,.02468:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprt"+
		"vxz|~\u0080\u0082\u0084\u0086\u0088\u008a\u008c\u008e\u0090\u0092\u0094"+
		"\u0096\u0000\b\u0002\u000000RR\u0002\u000088==\u0007\u0000\u0001\u0001"+
		"\u0005\u0005\u000e\u000e\u0012\u0012\u0016\u0016\u0019\u0019%%\u0001\u0000"+
		"\u001e$\u0002\u0000--//\u0002\u000099RR\u0002\u0000TY[]\u0003\u0000:>"+
		"@HJQ\u0490\u0000\u009d\u0001\u0000\u0000\u0000\u0002\u00a2\u0001\u0000"+
		"\u0000\u0000\u0004\u00b0\u0001\u0000\u0000\u0000\u0006\u00cb\u0001\u0000"+
		"\u0000\u0000\b\u00da\u0001\u0000\u0000\u0000\n\u00dd\u0001\u0000\u0000"+
		"\u0000\f\u00ef\u0001\u0000\u0000\u0000\u000e\u00f1\u0001\u0000\u0000\u0000"+
		"\u0010\u0104\u0001\u0000\u0000\u0000\u0012\u0106\u0001\u0000\u0000\u0000"+
		"\u0014\u0123\u0001\u0000\u0000\u0000\u0016\u0130\u0001\u0000\u0000\u0000"+
		"\u0018\u0134\u0001\u0000\u0000\u0000\u001a\u0136\u0001\u0000\u0000\u0000"+
		"\u001c\u0142\u0001\u0000\u0000\u0000\u001e\u014c\u0001\u0000\u0000\u0000"+
		" \u0152\u0001\u0000\u0000\u0000\"\u0157\u0001\u0000\u0000\u0000$\u015a"+
		"\u0001\u0000\u0000\u0000&\u0177\u0001\u0000\u0000\u0000(\u018b\u0001\u0000"+
		"\u0000\u0000*\u018f\u0001\u0000\u0000\u0000,\u0194\u0001\u0000\u0000\u0000"+
		".\u019d\u0001\u0000\u0000\u00000\u019f\u0001\u0000\u0000\u00002\u01a4"+
		"\u0001\u0000\u0000\u00004\u01be\u0001\u0000\u0000\u00006\u01c0\u0001\u0000"+
		"\u0000\u00008\u01c2\u0001\u0000\u0000\u0000:\u01d7\u0001\u0000\u0000\u0000"+
		"<\u01dd\u0001\u0000\u0000\u0000>\u01e5\u0001\u0000\u0000\u0000@\u01fc"+
		"\u0001\u0000\u0000\u0000B\u01fe\u0001\u0000\u0000\u0000D\u0206\u0001\u0000"+
		"\u0000\u0000F\u020a\u0001\u0000\u0000\u0000H\u020d\u0001\u0000\u0000\u0000"+
		"J\u0219\u0001\u0000\u0000\u0000L\u021b\u0001\u0000\u0000\u0000N\u021d"+
		"\u0001\u0000\u0000\u0000P\u024c\u0001\u0000\u0000\u0000R\u024e\u0001\u0000"+
		"\u0000\u0000T\u026c\u0001\u0000\u0000\u0000V\u0274\u0001\u0000\u0000\u0000"+
		"X\u028c\u0001\u0000\u0000\u0000Z\u028e\u0001\u0000\u0000\u0000\\\u0297"+
		"\u0001\u0000\u0000\u0000^\u02a6\u0001\u0000\u0000\u0000`\u02a8\u0001\u0000"+
		"\u0000\u0000b\u02ae\u0001\u0000\u0000\u0000d\u02cf\u0001\u0000\u0000\u0000"+
		"f\u02d1\u0001\u0000\u0000\u0000h\u02d9\u0001\u0000\u0000\u0000j\u02f2"+
		"\u0001\u0000\u0000\u0000l\u02fc\u0001\u0000\u0000\u0000n\u0303\u0001\u0000"+
		"\u0000\u0000p\u0309\u0001\u0000\u0000\u0000r\u0328\u0001\u0000\u0000\u0000"+
		"t\u0331\u0001\u0000\u0000\u0000v\u033d\u0001\u0000\u0000\u0000x\u034e"+
		"\u0001\u0000\u0000\u0000z\u035d\u0001\u0000\u0000\u0000|\u0366\u0001\u0000"+
		"\u0000\u0000~\u0385\u0001\u0000\u0000\u0000\u0080\u0387\u0001\u0000\u0000"+
		"\u0000\u0082\u0390\u0001\u0000\u0000\u0000\u0084\u03a4\u0001\u0000\u0000"+
		"\u0000\u0086\u03b1\u0001\u0000\u0000\u0000\u0088\u03c7\u0001\u0000\u0000"+
		"\u0000\u008a\u03c9\u0001\u0000\u0000\u0000\u008c\u03cb\u0001\u0000\u0000"+
		"\u0000\u008e\u03f9\u0001\u0000\u0000\u0000\u0090\u03fb\u0001\u0000\u0000"+
		"\u0000\u0092\u03fd\u0001\u0000\u0000\u0000\u0094\u0411\u0001\u0000\u0000"+
		"\u0000\u0096\u0413\u0001\u0000\u0000\u0000\u0098\u009c\u0003\u0004\u0002"+
		"\u0000\u0099\u009c\u0003\u0002\u0001\u0000\u009a\u009c\u0005b\u0000\u0000"+
		"\u009b\u0098\u0001\u0000\u0000\u0000\u009b\u0099\u0001\u0000\u0000\u0000"+
		"\u009b\u009a\u0001\u0000\u0000\u0000\u009c\u009f\u0001\u0000\u0000\u0000"+
		"\u009d\u009b\u0001\u0000\u0000\u0000\u009d\u009e\u0001\u0000\u0000\u0000"+
		"\u009e\u00a0\u0001\u0000\u0000\u0000\u009f\u009d\u0001\u0000\u0000\u0000"+
		"\u00a0\u00a1\u0005\u0000\u0000\u0001\u00a1\u0001\u0001\u0000\u0000\u0000"+
		"\u00a2\u00a3\u0005\u0002\u0000\u0000\u00a3\u00a4\u0003\u0006\u0003\u0000"+
		"\u00a4\u00a8\u0005b\u0000\u0000\u00a5\u00a7\u0003.\u0017\u0000\u00a6\u00a5"+
		"\u0001\u0000\u0000\u0000\u00a7\u00aa\u0001\u0000\u0000\u0000\u00a8\u00a6"+
		"\u0001\u0000\u0000\u0000\u00a8\u00a9\u0001\u0000\u0000\u0000\u00a9\u00ab"+
		"\u0001\u0000\u0000\u0000\u00aa\u00a8\u0001\u0000\u0000\u0000\u00ab\u00ad"+
		"\u0003B!\u0000\u00ac\u00ae\u0005b\u0000\u0000\u00ad\u00ac\u0001\u0000"+
		"\u0000\u0000\u00ad\u00ae\u0001\u0000\u0000\u0000\u00ae\u0003\u0001\u0000"+
		"\u0000\u0000\u00af\u00b1\u0005Z\u0000\u0000\u00b0\u00af\u0001\u0000\u0000"+
		"\u0000\u00b0\u00b1\u0001\u0000\u0000\u0000\u00b1\u00b2\u0001\u0000\u0000"+
		"\u0000\u00b2\u00b3\u0005\u0001\u0000\u0000\u00b3\u00b4\u0003\u0006\u0003"+
		"\u0000\u00b4\u00b8\u0005b\u0000\u0000\u00b5\u00b7\u0003\b\u0004\u0000"+
		"\u00b6\u00b5\u0001\u0000\u0000\u0000\u00b7\u00ba\u0001\u0000\u0000\u0000"+
		"\u00b8\u00b6\u0001\u0000\u0000\u0000\u00b8\u00b9\u0001\u0000\u0000\u0000"+
		"\u00b9\u00c3\u0001\u0000\u0000\u0000\u00ba\u00b8\u0001\u0000\u0000\u0000"+
		"\u00bb\u00bc\u0005\u0006\u0000\u0000\u00bc\u00c0\u0005b\u0000\u0000\u00bd"+
		"\u00bf\u0003\f\u0006\u0000\u00be\u00bd\u0001\u0000\u0000\u0000\u00bf\u00c2"+
		"\u0001\u0000\u0000\u0000\u00c0\u00be\u0001\u0000\u0000\u0000\u00c0\u00c1"+
		"\u0001\u0000\u0000\u0000\u00c1\u00c4\u0001\u0000\u0000\u0000\u00c2\u00c0"+
		"\u0001\u0000\u0000\u0000\u00c3\u00bb\u0001\u0000\u0000\u0000\u00c3\u00c4"+
		"\u0001\u0000\u0000\u0000\u00c4\u00c6\u0001\u0000\u0000\u0000\u00c5\u00c7"+
		"\u0003B!\u0000\u00c6\u00c5\u0001\u0000\u0000\u0000\u00c6\u00c7\u0001\u0000"+
		"\u0000\u0000\u00c7\u00c9\u0001\u0000\u0000\u0000\u00c8\u00ca\u0005b\u0000"+
		"\u0000\u00c9\u00c8\u0001\u0000\u0000\u0000\u00c9\u00ca\u0001\u0000\u0000"+
		"\u0000\u00ca\u0005\u0001\u0000\u0000\u0000\u00cb\u00d0\u0003H$\u0000\u00cc"+
		"\u00cd\u0005R\u0000\u0000\u00cd\u00cf\u0005Z\u0000\u0000\u00ce\u00cc\u0001"+
		"\u0000\u0000\u0000\u00cf\u00d2\u0001\u0000\u0000\u0000\u00d0\u00ce\u0001"+
		"\u0000\u0000\u0000\u00d0\u00d1\u0001\u0000\u0000\u0000\u00d1\u0007\u0001"+
		"\u0000\u0000\u0000\u00d2\u00d0\u0001\u0000\u0000\u0000\u00d3\u00db\u0003"+
		"\u000e\u0007\u0000\u00d4\u00db\u0003\u0012\t\u0000\u00d5\u00db\u0003\u0014"+
		"\n\u0000\u00d6\u00db\u0003\n\u0005\u0000\u00d7\u00db\u00038\u001c\u0000"+
		"\u00d8\u00db\u00032\u0019\u0000\u00d9\u00db\u0005b\u0000\u0000\u00da\u00d3"+
		"\u0001\u0000\u0000\u0000\u00da\u00d4\u0001\u0000\u0000\u0000\u00da\u00d5"+
		"\u0001\u0000\u0000\u0000\u00da\u00d6\u0001\u0000\u0000\u0000\u00da\u00d7"+
		"\u0001\u0000\u0000\u0000\u00da\u00d8\u0001\u0000\u0000\u0000\u00da\u00d9"+
		"\u0001\u0000\u0000\u0000\u00db\t\u0001\u0000\u0000\u0000\u00dc\u00de\u0005"+
		"Z\u0000\u0000\u00dd\u00dc\u0001\u0000\u0000\u0000\u00dd\u00de\u0001\u0000"+
		"\u0000\u0000\u00de\u00df\u0001\u0000\u0000\u0000\u00df\u00e0\u0005%\u0000"+
		"\u0000\u00e0\u00e1\u0003H$\u0000\u00e1\u00e6\u0005b\u0000\u0000\u00e2"+
		"\u00e5\u00032\u0019\u0000\u00e3\u00e5\u0005b\u0000\u0000\u00e4\u00e2\u0001"+
		"\u0000\u0000\u0000\u00e4\u00e3\u0001\u0000\u0000\u0000\u00e5\u00e8\u0001"+
		"\u0000\u0000\u0000\u00e6\u00e4\u0001\u0000\u0000\u0000\u00e6\u00e7\u0001"+
		"\u0000\u0000\u0000\u00e7\u00e9\u0001\u0000\u0000\u0000\u00e8\u00e6\u0001"+
		"\u0000\u0000\u0000\u00e9\u00eb\u0003B!\u0000\u00ea\u00ec\u0005b\u0000"+
		"\u0000\u00eb\u00ea\u0001\u0000\u0000\u0000\u00eb\u00ec\u0001\u0000\u0000"+
		"\u0000\u00ec\u000b\u0001\u0000\u0000\u0000\u00ed\u00f0\u0003\u001a\r\u0000"+
		"\u00ee\u00f0\u0005b\u0000\u0000\u00ef\u00ed\u0001\u0000\u0000\u0000\u00ef"+
		"\u00ee\u0001\u0000\u0000\u0000\u00f0\r\u0001\u0000\u0000\u0000\u00f1\u00f2"+
		"\u0005\u0004\u0000\u0000\u00f2\u00fe\u0003\u0010\b\u0000\u00f3\u00f4\u0005"+
		"0\u0000\u0000\u00f4\u00f5\u0003@ \u0000\u00f5\u00f6\u0005/\u0000\u0000"+
		"\u00f6\u00fb\u0003@ \u0000\u00f7\u00f8\u00050\u0000\u0000\u00f8\u00fa"+
		"\u0003@ \u0000\u00f9\u00f7\u0001\u0000\u0000\u0000\u00fa\u00fd\u0001\u0000"+
		"\u0000\u0000\u00fb\u00f9\u0001\u0000\u0000\u0000\u00fb\u00fc\u0001\u0000"+
		"\u0000\u0000\u00fc\u00ff\u0001\u0000\u0000\u0000\u00fd\u00fb\u0001\u0000"+
		"\u0000\u0000\u00fe\u00f3\u0001\u0000\u0000\u0000\u00fe\u00ff\u0001\u0000"+
		"\u0000\u0000\u00ff\u0100\u0001\u0000\u0000\u0000\u0100\u0101\u0005b\u0000"+
		"\u0000\u0101\u000f\u0001\u0000\u0000\u0000\u0102\u0105\u0005Z\u0000\u0000"+
		"\u0103\u0105\u0003L&\u0000\u0104\u0102\u0001\u0000\u0000\u0000\u0104\u0103"+
		"\u0001\u0000\u0000\u0000\u0105\u0011\u0001\u0000\u0000\u0000\u0106\u0107"+
		"\u0005\u0007\u0000\u0000\u0107\u0108\u0005\b\u0000\u0000\u0108\u0109\u0005"+
		"b\u0000\u0000\u0109\u0013\u0001\u0000\u0000\u0000\u010a\u010b\u0005\u0005"+
		"\u0000\u0000\u010b\u010c\u0005Z\u0000\u0000\u010c\u0110\u0005b\u0000\u0000"+
		"\u010d\u010f\u0003\u0016\u000b\u0000\u010e\u010d\u0001\u0000\u0000\u0000"+
		"\u010f\u0112\u0001\u0000\u0000\u0000\u0110\u010e\u0001\u0000\u0000\u0000"+
		"\u0110\u0111\u0001\u0000\u0000\u0000\u0111\u0113\u0001\u0000\u0000\u0000"+
		"\u0112\u0110\u0001\u0000\u0000\u0000\u0113\u0115\u0003B!\u0000\u0114\u0116"+
		"\u0005b\u0000\u0000\u0115\u0114\u0001\u0000\u0000\u0000\u0115\u0116\u0001"+
		"\u0000\u0000\u0000\u0116\u0124\u0001\u0000\u0000\u0000\u0117\u0118\u0005"+
		"\u0005\u0000\u0000\u0118\u011c\u0005b\u0000\u0000\u0119\u011b\u0003\u0018"+
		"\f\u0000\u011a\u0119\u0001\u0000\u0000\u0000\u011b\u011e\u0001\u0000\u0000"+
		"\u0000\u011c\u011a\u0001\u0000\u0000\u0000\u011c\u011d\u0001\u0000\u0000"+
		"\u0000\u011d\u011f\u0001\u0000\u0000\u0000\u011e\u011c\u0001\u0000\u0000"+
		"\u0000\u011f\u0121\u0003B!\u0000\u0120\u0122\u0005b\u0000\u0000\u0121"+
		"\u0120\u0001\u0000\u0000\u0000\u0121\u0122\u0001\u0000\u0000\u0000\u0122"+
		"\u0124\u0001\u0000\u0000\u0000\u0123\u010a\u0001\u0000\u0000\u0000\u0123"+
		"\u0117\u0001\u0000\u0000\u0000\u0124\u0015\u0001\u0000\u0000\u0000\u0125"+
		"\u012a\u0003\u001c\u000e\u0000\u0126\u0127\u00050\u0000\u0000\u0127\u0129"+
		"\u0003\u001c\u000e\u0000\u0128\u0126\u0001\u0000\u0000\u0000\u0129\u012c"+
		"\u0001\u0000\u0000\u0000\u012a\u0128\u0001\u0000\u0000\u0000\u012a\u012b"+
		"\u0001\u0000\u0000\u0000\u012b\u012d\u0001\u0000\u0000\u0000\u012c\u012a"+
		"\u0001\u0000\u0000\u0000\u012d\u012e\u0005b\u0000\u0000\u012e\u0131\u0001"+
		"\u0000\u0000\u0000\u012f\u0131\u0005b\u0000\u0000\u0130\u0125\u0001\u0000"+
		"\u0000\u0000\u0130\u012f\u0001\u0000\u0000\u0000\u0131\u0017\u0001\u0000"+
		"\u0000\u0000\u0132\u0135\u0003\u001a\r\u0000\u0133\u0135\u0005b\u0000"+
		"\u0000\u0134\u0132\u0001\u0000\u0000\u0000\u0134\u0133\u0001\u0000\u0000"+
		"\u0000\u0135\u0019\u0001\u0000\u0000\u0000\u0136\u0137\u0003\u001c\u000e"+
		"\u0000\u0137\u013b\u0005b\u0000\u0000\u0138\u013a\u0003.\u0017\u0000\u0139"+
		"\u0138\u0001\u0000\u0000\u0000\u013a\u013d\u0001\u0000\u0000\u0000\u013b"+
		"\u0139\u0001\u0000\u0000\u0000\u013b\u013c\u0001\u0000\u0000\u0000\u013c"+
		"\u013e\u0001\u0000\u0000\u0000\u013d\u013b\u0001\u0000\u0000\u0000\u013e"+
		"\u0140\u0003B!\u0000\u013f\u0141\u0005b\u0000\u0000\u0140\u013f\u0001"+
		"\u0000\u0000\u0000\u0140\u0141\u0001\u0000\u0000\u0000\u0141\u001b\u0001"+
		"\u0000\u0000\u0000\u0142\u0144\u0005Z\u0000\u0000\u0143\u0145\u0003\u001e"+
		"\u000f\u0000\u0144\u0143\u0001\u0000\u0000\u0000\u0144\u0145\u0001\u0000"+
		"\u0000\u0000\u0145\u0147\u0001\u0000\u0000\u0000\u0146\u0148\u0003 \u0010"+
		"\u0000\u0147\u0146\u0001\u0000\u0000\u0000\u0147\u0148\u0001\u0000\u0000"+
		"\u0000\u0148\u014a\u0001\u0000\u0000\u0000\u0149\u014b\u0003\"\u0011\u0000"+
		"\u014a\u0149\u0001\u0000\u0000\u0000\u014a\u014b\u0001\u0000\u0000\u0000"+
		"\u014b\u001d\u0001\u0000\u0000\u0000\u014c\u014e\u00051\u0000\u0000\u014d"+
		"\u014f\u0003<\u001e\u0000\u014e\u014d\u0001\u0000\u0000\u0000\u014e\u014f"+
		"\u0001\u0000\u0000\u0000\u014f\u0150\u0001\u0000\u0000\u0000\u0150\u0151"+
		"\u00052\u0000\u0000\u0151\u001f\u0001\u0000\u0000\u0000\u0152\u0153\u0005"+
		"\t\u0000\u0000\u0153\u0154\u00051\u0000\u0000\u0154\u0155\u0005Z\u0000"+
		"\u0000\u0155\u0156\u00052\u0000\u0000\u0156!\u0001\u0000\u0000\u0000\u0157"+
		"\u0158\u0005-\u0000\u0000\u0158\u0159\u0003$\u0012\u0000\u0159#\u0001"+
		"\u0000\u0000\u0000\u015a\u0161\u0003&\u0013\u0000\u015b\u015d\u0007\u0000"+
		"\u0000\u0000\u015c\u015b\u0001\u0000\u0000\u0000\u015c\u015d\u0001\u0000"+
		"\u0000\u0000\u015d\u015e\u0001\u0000\u0000\u0000\u015e\u0160\u0003&\u0013"+
		"\u0000\u015f\u015c\u0001\u0000\u0000\u0000\u0160\u0163\u0001\u0000\u0000"+
		"\u0000\u0161\u015f\u0001\u0000\u0000\u0000\u0161\u0162\u0001\u0000\u0000"+
		"\u0000\u0162%\u0001\u0000\u0000\u0000\u0163\u0161\u0001\u0000\u0000\u0000"+
		"\u0164\u0165\u0005\n\u0000\u0000\u0165\u0166\u00051\u0000\u0000\u0166"+
		"\u016b\u0003(\u0014\u0000\u0167\u0168\u00050\u0000\u0000\u0168\u016a\u0003"+
		"(\u0014\u0000\u0169\u0167\u0001\u0000\u0000\u0000\u016a\u016d\u0001\u0000"+
		"\u0000\u0000\u016b\u0169\u0001\u0000\u0000\u0000\u016b\u016c\u0001\u0000"+
		"\u0000\u0000\u016c\u016e\u0001\u0000\u0000\u0000\u016d\u016b\u0001\u0000"+
		"\u0000\u0000\u016e\u016f\u00052\u0000\u0000\u016f\u0178\u0001\u0000\u0000"+
		"\u0000\u0170\u0178\u0005\u000b\u0000\u0000\u0171\u0178\u0005\f\u0000\u0000"+
		"\u0172\u0178\u0005\r\u0000\u0000\u0173\u0175\u0003@ \u0000\u0174\u0176"+
		"\u0003R)\u0000\u0175\u0174\u0001\u0000\u0000\u0000\u0175\u0176\u0001\u0000"+
		"\u0000\u0000\u0176\u0178\u0001\u0000\u0000\u0000\u0177\u0164\u0001\u0000"+
		"\u0000\u0000\u0177\u0170\u0001\u0000\u0000\u0000\u0177\u0171\u0001\u0000"+
		"\u0000\u0000\u0177\u0172\u0001\u0000\u0000\u0000\u0177\u0173\u0001\u0000"+
		"\u0000\u0000\u0178\'\u0001\u0000\u0000\u0000\u0179\u017b\u0003*\u0015"+
		"\u0000\u017a\u017c\u0005S\u0000\u0000\u017b\u017a\u0001\u0000\u0000\u0000"+
		"\u017b\u017c\u0001\u0000\u0000\u0000\u017c\u017d\u0001\u0000\u0000\u0000"+
		"\u017d\u017f\u0005.\u0000\u0000\u017e\u0180\u0003,\u0016\u0000\u017f\u017e"+
		"\u0001\u0000\u0000\u0000\u017f\u0180\u0001\u0000\u0000\u0000\u0180\u018c"+
		"\u0001\u0000\u0000\u0000\u0181\u0182\u0003@ \u0000\u0182\u0183\u0005?"+
		"\u0000\u0000\u0183\u0184\u0003,\u0016\u0000\u0184\u018c\u0001\u0000\u0000"+
		"\u0000\u0185\u0186\u0003H$\u0000\u0186\u0187\u0005/\u0000\u0000\u0187"+
		"\u0188\u0005Z\u0000\u0000\u0188\u018c\u0001\u0000\u0000\u0000\u0189\u018c"+
		"\u0003H$\u0000\u018a\u018c\u0003\u0080@\u0000\u018b\u0179\u0001\u0000"+
		"\u0000\u0000\u018b\u0181\u0001\u0000\u0000\u0000\u018b\u0185\u0001\u0000"+
		"\u0000\u0000\u018b\u0189\u0001\u0000\u0000\u0000\u018b\u018a\u0001\u0000"+
		"\u0000\u0000\u018c)\u0001\u0000\u0000\u0000\u018d\u0190\u0003H$\u0000"+
		"\u018e\u0190\u0003\u0096K\u0000\u018f\u018d\u0001\u0000\u0000\u0000\u018f"+
		"\u018e\u0001\u0000\u0000\u0000\u0190+\u0001\u0000\u0000\u0000\u0191\u0195"+
		"\u0003H$\u0000\u0192\u0195\u0003\u0080@\u0000\u0193\u0195\u0003\u0096"+
		"K\u0000\u0194\u0191\u0001\u0000\u0000\u0000\u0194\u0192\u0001\u0000\u0000"+
		"\u0000\u0194\u0193\u0001\u0000\u0000\u0000\u0195-\u0001\u0000\u0000\u0000"+
		"\u0196\u019e\u00030\u0018\u0000\u0197\u019e\u00038\u001c\u0000\u0198\u019e"+
		"\u0003\u000e\u0007\u0000\u0199\u019e\u0003\u0012\t\u0000\u019a\u019e\u0003"+
		"\u0014\n\u0000\u019b\u019e\u0003V+\u0000\u019c\u019e\u0005b\u0000\u0000"+
		"\u019d\u0196\u0001\u0000\u0000\u0000\u019d\u0197\u0001\u0000\u0000\u0000"+
		"\u019d\u0198\u0001\u0000\u0000\u0000\u019d\u0199\u0001\u0000\u0000\u0000"+
		"\u019d\u019a\u0001\u0000\u0000\u0000\u019d\u019b\u0001\u0000\u0000\u0000"+
		"\u019d\u019c\u0001\u0000\u0000\u0000\u019e/\u0001\u0000\u0000\u0000\u019f"+
		"\u01a0\u0003<\u001e\u0000\u01a0\u01a1\u0005-\u0000\u0000\u01a1\u01a2\u0003"+
		"4\u001a\u0000\u01a2\u01a3\u0005b\u0000\u0000\u01a31\u0001\u0000\u0000"+
		"\u0000\u01a4\u01a5\u0003<\u001e\u0000\u01a5\u01a6\u0005-\u0000\u0000\u01a6"+
		"\u01a7\u00034\u001a\u0000\u01a7\u01a8\u0005b\u0000\u0000\u01a83\u0001"+
		"\u0000\u0000\u0000\u01a9\u01ab\u0003H$\u0000\u01aa\u01ac\u00036\u001b"+
		"\u0000\u01ab\u01aa\u0001\u0000\u0000\u0000\u01ab\u01ac\u0001\u0000\u0000"+
		"\u0000\u01ac\u01ae\u0001\u0000\u0000\u0000\u01ad\u01af\u0003D\"\u0000"+
		"\u01ae\u01ad\u0001\u0000\u0000\u0000\u01ae\u01af\u0001\u0000\u0000\u0000"+
		"\u01af\u01b1\u0001\u0000\u0000\u0000\u01b0\u01b2\u0003F#\u0000\u01b1\u01b0"+
		"\u0001\u0000\u0000\u0000\u01b1\u01b2\u0001\u0000\u0000\u0000\u01b2\u01bf"+
		"\u0001\u0000\u0000\u0000\u01b3\u01b8\u0003&\u0013\u0000\u01b4\u01b5\u0005"+
		"0\u0000\u0000\u01b5\u01b7\u0003&\u0013\u0000\u01b6\u01b4\u0001\u0000\u0000"+
		"\u0000\u01b7\u01ba\u0001\u0000\u0000\u0000\u01b8\u01b6\u0001\u0000\u0000"+
		"\u0000\u01b8\u01b9\u0001\u0000\u0000\u0000\u01b9\u01bc\u0001\u0000\u0000"+
		"\u0000\u01ba\u01b8\u0001\u0000\u0000\u0000\u01bb\u01bd\u0003F#\u0000\u01bc"+
		"\u01bb\u0001\u0000\u0000\u0000\u01bc\u01bd\u0001\u0000\u0000\u0000\u01bd"+
		"\u01bf\u0001\u0000\u0000\u0000\u01be\u01a9\u0001\u0000\u0000\u0000\u01be"+
		"\u01b3\u0001\u0000\u0000\u0000\u01bf5\u0001\u0000\u0000\u0000\u01c0\u01c1"+
		"\u0007\u0001\u0000\u0000\u01c17\u0001\u0000\u0000\u0000\u01c2\u01c3\u0005"+
		"\u001d\u0000\u0000\u01c3\u01c5\u0003@ \u0000\u01c4\u01c6\u0003R)\u0000"+
		"\u01c5\u01c4\u0001\u0000\u0000\u0000\u01c5\u01c6\u0001\u0000\u0000\u0000"+
		"\u01c6\u01c7\u0001\u0000\u0000\u0000\u01c7\u01c8\u0005>\u0000\u0000\u01c8"+
		"\u01cd\u0003:\u001d\u0000\u01c9\u01ca\u00050\u0000\u0000\u01ca\u01cc\u0003"+
		":\u001d\u0000\u01cb\u01c9\u0001\u0000\u0000\u0000\u01cc\u01cf\u0001\u0000"+
		"\u0000\u0000\u01cd\u01cb\u0001\u0000\u0000\u0000\u01cd\u01ce\u0001\u0000"+
		"\u0000\u0000\u01ce\u01d1\u0001\u0000\u0000\u0000\u01cf\u01cd\u0001\u0000"+
		"\u0000\u0000\u01d0\u01d2\u00050\u0000\u0000\u01d1\u01d0\u0001\u0000\u0000"+
		"\u0000\u01d1\u01d2\u0001\u0000\u0000\u0000\u01d2\u01d3\u0001\u0000\u0000"+
		"\u0000\u01d3\u01d4\u0005>\u0000\u0000\u01d4\u01d5\u0005b\u0000\u0000\u01d5"+
		"9\u0001\u0000\u0000\u0000\u01d6\u01d8\u0005;\u0000\u0000\u01d7\u01d6\u0001"+
		"\u0000\u0000\u0000\u01d7\u01d8\u0001\u0000\u0000\u0000\u01d8\u01db\u0001"+
		"\u0000\u0000\u0000\u01d9\u01dc\u0003\u0090H\u0000\u01da\u01dc\u0003@ "+
		"\u0000\u01db\u01d9\u0001\u0000\u0000\u0000\u01db\u01da\u0001\u0000\u0000"+
		"\u0000\u01dc;\u0001\u0000\u0000\u0000\u01dd\u01e2\u0003>\u001f\u0000\u01de"+
		"\u01df\u00050\u0000\u0000\u01df\u01e1\u0003>\u001f\u0000\u01e0\u01de\u0001"+
		"\u0000\u0000\u0000\u01e1\u01e4\u0001\u0000\u0000\u0000\u01e2\u01e0\u0001"+
		"\u0000\u0000\u0000\u01e2\u01e3\u0001\u0000\u0000\u0000\u01e3=\u0001\u0000"+
		"\u0000\u0000\u01e4\u01e2\u0001\u0000\u0000\u0000\u01e5\u01e7\u0003@ \u0000"+
		"\u01e6\u01e8\u0003R)\u0000\u01e7\u01e6\u0001\u0000\u0000\u0000\u01e7\u01e8"+
		"\u0001\u0000\u0000\u0000\u01e8?\u0001\u0000\u0000\u0000\u01e9\u01ee\u0005"+
		"Z\u0000\u0000\u01ea\u01eb\u0005S\u0000\u0000\u01eb\u01ed\u0005Z\u0000"+
		"\u0000\u01ec\u01ea\u0001\u0000\u0000\u0000\u01ed\u01f0\u0001\u0000\u0000"+
		"\u0000\u01ee\u01ec\u0001\u0000\u0000\u0000\u01ee\u01ef\u0001\u0000\u0000"+
		"\u0000\u01ef\u01f2\u0001\u0000\u0000\u0000\u01f0\u01ee\u0001\u0000\u0000"+
		"\u0000\u01f1\u01f3\u0005S\u0000\u0000\u01f2\u01f1\u0001\u0000\u0000\u0000"+
		"\u01f2\u01f3\u0001\u0000\u0000\u0000\u01f3\u01fd\u0001\u0000\u0000\u0000"+
		"\u01f4\u01f5\u0004 \u0000\u0000\u01f5\u01fd\u0005\u0003\u0000\u0000\u01f6"+
		"\u01fd\u0005\u001d\u0000\u0000\u01f7\u01fd\u0005\t\u0000\u0000\u01f8\u01fd"+
		"\u0005%\u0000\u0000\u01f9\u01fd\u0005\u0017\u0000\u0000\u01fa\u01fd\u0005"+
		"\u0011\u0000\u0000\u01fb\u01fd\u0005\u0018\u0000\u0000\u01fc\u01e9\u0001"+
		"\u0000\u0000\u0000\u01fc\u01f4\u0001\u0000\u0000\u0000\u01fc\u01f6\u0001"+
		"\u0000\u0000\u0000\u01fc\u01f7\u0001\u0000\u0000\u0000\u01fc\u01f8\u0001"+
		"\u0000\u0000\u0000\u01fc\u01f9\u0001\u0000\u0000\u0000\u01fc\u01fa\u0001"+
		"\u0000\u0000\u0000\u01fc\u01fb\u0001\u0000\u0000\u0000\u01fdA\u0001\u0000"+
		"\u0000\u0000\u01fe\u0200\u0005\u0003\u0000\u0000\u01ff\u0201\u0007\u0002"+
		"\u0000\u0000\u0200\u01ff\u0001\u0000\u0000\u0000\u0200\u0201\u0001\u0000"+
		"\u0000\u0000\u0201C\u0001\u0000\u0000\u0000\u0202\u0204\u00050\u0000\u0000"+
		"\u0203\u0202\u0001\u0000\u0000\u0000\u0203\u0204\u0001\u0000\u0000\u0000"+
		"\u0204\u0205\u0001\u0000\u0000\u0000\u0205\u0207\u0003&\u0013\u0000\u0206"+
		"\u0203\u0001\u0000\u0000\u0000\u0207\u0208\u0001\u0000\u0000\u0000\u0208"+
		"\u0206\u0001\u0000\u0000\u0000\u0208\u0209\u0001\u0000\u0000\u0000\u0209"+
		"E\u0001\u0000\u0000\u0000\u020a\u020b\u0005?\u0000\u0000\u020b\u020c\u0003"+
		"\u0080@\u0000\u020cG\u0001\u0000\u0000\u0000\u020d\u020f\u0003J%\u0000"+
		"\u020e\u0210\u0005S\u0000\u0000\u020f\u020e\u0001\u0000\u0000\u0000\u020f"+
		"\u0210\u0001\u0000\u0000\u0000\u0210\u0212\u0001\u0000\u0000\u0000\u0211"+
		"\u0213\u0003R)\u0000\u0212\u0211\u0001\u0000\u0000\u0000\u0212\u0213\u0001"+
		"\u0000\u0000\u0000\u0213I\u0001\u0000\u0000\u0000\u0214\u021a\u0003L&"+
		"\u0000\u0215\u021a\u0003N\'\u0000\u0216\u021a\u0003P(\u0000\u0217\u021a"+
		"\u0005%\u0000\u0000\u0218\u021a\u0005Z\u0000\u0000\u0219\u0214\u0001\u0000"+
		"\u0000\u0000\u0219\u0215\u0001\u0000\u0000\u0000\u0219\u0216\u0001\u0000"+
		"\u0000\u0000\u0219\u0217\u0001\u0000\u0000\u0000\u0219\u0218\u0001\u0000"+
		"\u0000\u0000\u021aK\u0001\u0000\u0000\u0000\u021b\u021c\u0007\u0003\u0000"+
		"\u0000\u021cM\u0001\u0000\u0000\u0000\u021d\u021e\u0005Z\u0000\u0000\u021e"+
		"\u021f\u00053\u0000\u0000\u021f\u0224\u0003H$\u0000\u0220\u0221\u0005"+
		"0\u0000\u0000\u0221\u0223\u0003H$\u0000\u0222\u0220\u0001\u0000\u0000"+
		"\u0000\u0223\u0226\u0001\u0000\u0000\u0000\u0224\u0222\u0001\u0000\u0000"+
		"\u0000\u0224\u0225\u0001\u0000\u0000\u0000\u0225\u0227\u0001\u0000\u0000"+
		"\u0000\u0226\u0224\u0001\u0000\u0000\u0000\u0227\u0228\u00054\u0000\u0000"+
		"\u0228O\u0001\u0000\u0000\u0000\u0229\u022a\u0005&\u0000\u0000\u022a\u022b"+
		"\u00053\u0000\u0000\u022b\u022c\u0003H$\u0000\u022c\u022d\u00054\u0000"+
		"\u0000\u022d\u024d\u0001\u0000\u0000\u0000\u022e\u022f\u0005\'\u0000\u0000"+
		"\u022f\u0230\u00053\u0000\u0000\u0230\u0231\u0003H$\u0000\u0231\u0232"+
		"\u00054\u0000\u0000\u0232\u024d\u0001\u0000\u0000\u0000\u0233\u0234\u0005"+
		"(\u0000\u0000\u0234\u0235\u00053\u0000\u0000\u0235\u0236\u0003H$\u0000"+
		"\u0236\u0237\u00054\u0000\u0000\u0237\u024d\u0001\u0000\u0000\u0000\u0238"+
		"\u0239\u0005)\u0000\u0000\u0239\u023a\u00053\u0000\u0000\u023a\u023b\u0003"+
		"H$\u0000\u023b\u023c\u00054\u0000\u0000\u023c\u024d\u0001\u0000\u0000"+
		"\u0000\u023d\u023e\u0005*\u0000\u0000\u023e\u023f\u00053\u0000\u0000\u023f"+
		"\u0240\u0003H$\u0000\u0240\u0241\u00054\u0000\u0000\u0241\u024d\u0001"+
		"\u0000\u0000\u0000\u0242\u0243\u0005+\u0000\u0000\u0243\u0244\u00053\u0000"+
		"\u0000\u0244\u0245\u0003H$\u0000\u0245\u0246\u00054\u0000\u0000\u0246"+
		"\u024d\u0001\u0000\u0000\u0000\u0247\u0248\u0005,\u0000\u0000\u0248\u0249"+
		"\u00053\u0000\u0000\u0249\u024a\u0003H$\u0000\u024a\u024b\u00054\u0000"+
		"\u0000\u024b\u024d\u0001\u0000\u0000\u0000\u024c\u0229\u0001\u0000\u0000"+
		"\u0000\u024c\u022e\u0001\u0000\u0000\u0000\u024c\u0233\u0001\u0000\u0000"+
		"\u0000\u024c\u0238\u0001\u0000\u0000\u0000\u024c\u023d\u0001\u0000\u0000"+
		"\u0000\u024c\u0242\u0001\u0000\u0000\u0000\u024c\u0247\u0001\u0000\u0000"+
		"\u0000\u024dQ\u0001\u0000\u0000\u0000\u024e\u024f\u00051\u0000\u0000\u024f"+
		"\u0254\u0003T*\u0000\u0250\u0251\u00050\u0000\u0000\u0251\u0253\u0003"+
		"T*\u0000\u0252\u0250\u0001\u0000\u0000\u0000\u0253\u0256\u0001\u0000\u0000"+
		"\u0000\u0254\u0252\u0001\u0000\u0000\u0000\u0254\u0255\u0001\u0000\u0000"+
		"\u0000\u0255\u0257\u0001\u0000\u0000\u0000\u0256\u0254\u0001\u0000\u0000"+
		"\u0000\u0257\u0258\u00052\u0000\u0000\u0258S\u0001\u0000\u0000\u0000\u0259"+
		"\u025a\u0005Z\u0000\u0000\u025a\u025b\u0005?\u0000\u0000\u025b\u026d\u0003"+
		"\u0080@\u0000\u025c\u025e\u0003\u0080@\u0000\u025d\u025c\u0001\u0000\u0000"+
		"\u0000\u025d\u025e\u0001\u0000\u0000\u0000\u025e\u025f\u0001\u0000\u0000"+
		"\u0000\u025f\u0261\u0005/\u0000\u0000\u0260\u0262\u0003\u0080@\u0000\u0261"+
		"\u0260\u0001\u0000\u0000\u0000\u0261\u0262\u0001\u0000\u0000\u0000\u0262"+
		"\u026d\u0001\u0000\u0000\u0000\u0263\u0265\u0003\u0080@\u0000\u0264\u0263"+
		"\u0001\u0000\u0000\u0000\u0264\u0265\u0001\u0000\u0000\u0000\u0265\u0266"+
		"\u0001\u0000\u0000\u0000\u0266\u0268\u0005-\u0000\u0000\u0267\u0269\u0003"+
		"\u0080@\u0000\u0268\u0267\u0001\u0000\u0000\u0000\u0268\u0269\u0001\u0000"+
		"\u0000\u0000\u0269\u026d\u0001\u0000\u0000\u0000\u026a\u026d\u0003\u0080"+
		"@\u0000\u026b\u026d\u0005=\u0000\u0000\u026c\u0259\u0001\u0000\u0000\u0000"+
		"\u026c\u025d\u0001\u0000\u0000\u0000\u026c\u0264\u0001\u0000\u0000\u0000"+
		"\u026c\u026a\u0001\u0000\u0000\u0000\u026c\u026b\u0001\u0000\u0000\u0000"+
		"\u026dU\u0001\u0000\u0000\u0000\u026e\u0275\u0003h4\u0000\u026f\u0275"+
		"\u0003p8\u0000\u0270\u0275\u0003X,\u0000\u0271\u0275\u0003v;\u0000\u0272"+
		"\u0275\u0003x<\u0000\u0273\u0275\u0003\\.\u0000\u0274\u026e\u0001\u0000"+
		"\u0000\u0000\u0274\u026f\u0001\u0000\u0000\u0000\u0274\u0270\u0001\u0000"+
		"\u0000\u0000\u0274\u0271\u0001\u0000\u0000\u0000\u0274\u0272\u0001\u0000"+
		"\u0000\u0000\u0274\u0273\u0001\u0000\u0000\u0000\u0275W\u0001\u0000\u0000"+
		"\u0000\u0276\u0277\u0005\u0012\u0000\u0000\u0277\u0278\u00051\u0000\u0000"+
		"\u0278\u0279\u0003Z-\u0000\u0279\u027a\u00052\u0000\u0000\u027a\u027e"+
		"\u0005b\u0000\u0000\u027b\u027d\u0003.\u0017\u0000\u027c\u027b\u0001\u0000"+
		"\u0000\u0000\u027d\u0280\u0001\u0000\u0000\u0000\u027e\u027c\u0001\u0000"+
		"\u0000\u0000\u027e\u027f\u0001\u0000\u0000\u0000\u027f\u0281\u0001\u0000"+
		"\u0000\u0000\u0280\u027e\u0001\u0000\u0000\u0000\u0281\u0283\u0003B!\u0000"+
		"\u0282\u0284\u0005b\u0000\u0000\u0283\u0282\u0001\u0000\u0000\u0000\u0283"+
		"\u0284\u0001\u0000\u0000\u0000\u0284\u028d\u0001\u0000\u0000\u0000\u0285"+
		"\u0286\u0005\u0012\u0000\u0000\u0286\u0287\u00051\u0000\u0000\u0287\u0288"+
		"\u0003Z-\u0000\u0288\u0289\u00052\u0000\u0000\u0289\u028a\u0003d2\u0000"+
		"\u028a\u028b\u0005b\u0000\u0000\u028b\u028d\u0001\u0000\u0000\u0000\u028c"+
		"\u0276\u0001\u0000\u0000\u0000\u028c\u0285\u0001\u0000\u0000\u0000\u028d"+
		"Y\u0001\u0000\u0000\u0000\u028e\u028f\u0005Z\u0000\u0000\u028f\u0290\u0005"+
		"?\u0000\u0000\u0290\u0291\u0003\u0080@\u0000\u0291\u0292\u0005/\u0000"+
		"\u0000\u0292\u0295\u0003\u0080@\u0000\u0293\u0294\u0005/\u0000\u0000\u0294"+
		"\u0296\u0003\u0080@\u0000\u0295\u0293\u0001\u0000\u0000\u0000\u0295\u0296"+
		"\u0001\u0000\u0000\u0000\u0296[\u0001\u0000\u0000\u0000\u0297\u029e\u0003"+
		"^/\u0000\u0298\u029a\u00057\u0000\u0000\u0299\u029b\u0003^/\u0000\u029a"+
		"\u0299\u0001\u0000\u0000\u0000\u029a\u029b\u0001\u0000\u0000\u0000\u029b"+
		"\u029d\u0001\u0000\u0000\u0000\u029c\u0298\u0001\u0000\u0000\u0000\u029d"+
		"\u02a0\u0001\u0000\u0000\u0000\u029e\u029c\u0001\u0000\u0000\u0000\u029e"+
		"\u029f\u0001\u0000\u0000\u0000\u029f\u02a1\u0001\u0000\u0000\u0000\u02a0"+
		"\u029e\u0001\u0000\u0000\u0000\u02a1\u02a2\u0005b\u0000\u0000\u02a2]\u0001"+
		"\u0000\u0000\u0000\u02a3\u02a7\u0003`0\u0000\u02a4\u02a7\u0003b1\u0000"+
		"\u02a5\u02a7\u0003d2\u0000\u02a6\u02a3\u0001\u0000\u0000\u0000\u02a6\u02a4"+
		"\u0001\u0000\u0000\u0000\u02a6\u02a5\u0001\u0000\u0000\u0000\u02a7_\u0001"+
		"\u0000\u0000\u0000\u02a8\u02a9\u0005\u000e\u0000\u0000\u02a9\u02aa\u0005"+
		"1\u0000\u0000\u02aa\u02ab\u0003\u0080@\u0000\u02ab\u02ac\u00052\u0000"+
		"\u0000\u02ac\u02ad\u0003d2\u0000\u02ada\u0001\u0000\u0000\u0000\u02ae"+
		"\u02af\u0005\u0011\u0000\u0000\u02af\u02b0\u00051\u0000\u0000\u02b0\u02b1"+
		"\u0003\u0080@\u0000\u02b1\u02b2\u00052\u0000\u0000\u02b2\u02b3\u0003d"+
		"2\u0000\u02b3c\u0001\u0000\u0000\u0000\u02b4\u02b5\u00042\u0001\u0000"+
		"\u02b5\u02b6\u0003@ \u0000\u02b6\u02b8\u00051\u0000\u0000\u02b7\u02b9"+
		"\u0003\u008cF\u0000\u02b8\u02b7\u0001\u0000\u0000\u0000\u02b8\u02b9\u0001"+
		"\u0000\u0000\u0000\u02b9\u02ba\u0001\u0000\u0000\u0000\u02ba\u02bc\u0005"+
		"2\u0000\u0000\u02bb\u02bd\u0003f3\u0000\u02bc\u02bb\u0001\u0000\u0000"+
		"\u0000\u02bc\u02bd\u0001\u0000\u0000\u0000\u02bd\u02d0\u0001\u0000\u0000"+
		"\u0000\u02be\u02c4\u0003\u0082A\u0000\u02bf\u02c0\u0005?\u0000\u0000\u02c0"+
		"\u02c5\u0003\u0080@\u0000\u02c1\u02c2\u0005.\u0000\u0000\u02c2\u02c5\u0003"+
		"\u0080@\u0000\u02c3\u02c5\u0003f3\u0000\u02c4\u02bf\u0001\u0000\u0000"+
		"\u0000\u02c4\u02c1\u0001\u0000\u0000\u0000\u02c4\u02c3\u0001\u0000\u0000"+
		"\u0000\u02c4\u02c5\u0001\u0000\u0000\u0000\u02c5\u02d0\u0001\u0000\u0000"+
		"\u0000\u02c6\u02c8\u0005\u001a\u0000\u0000\u02c7\u02c9\u0003@ \u0000\u02c8"+
		"\u02c7\u0001\u0000\u0000\u0000\u02c8\u02c9\u0001\u0000\u0000\u0000\u02c9"+
		"\u02d0\u0001\u0000\u0000\u0000\u02ca\u02cc\u0005\u001b\u0000\u0000\u02cb"+
		"\u02cd\u0003@ \u0000\u02cc\u02cb\u0001\u0000\u0000\u0000\u02cc\u02cd\u0001"+
		"\u0000\u0000\u0000\u02cd\u02d0\u0001\u0000\u0000\u0000\u02ce\u02d0\u0005"+
		"\u001c\u0000\u0000\u02cf\u02b4\u0001\u0000\u0000\u0000\u02cf\u02be\u0001"+
		"\u0000\u0000\u0000\u02cf\u02c6\u0001\u0000\u0000\u0000\u02cf\u02ca\u0001"+
		"\u0000\u0000\u0000\u02cf\u02ce\u0001\u0000\u0000\u0000\u02d0e\u0001\u0000"+
		"\u0000\u0000\u02d1\u02d6\u0003\u008eG\u0000\u02d2\u02d3\u00050\u0000\u0000"+
		"\u02d3\u02d5\u0003\u008eG\u0000\u02d4\u02d2\u0001\u0000\u0000\u0000\u02d5"+
		"\u02d8\u0001\u0000\u0000\u0000\u02d6\u02d4\u0001\u0000\u0000\u0000\u02d6"+
		"\u02d7\u0001\u0000\u0000\u0000\u02d7g\u0001\u0000\u0000\u0000\u02d8\u02d6"+
		"\u0001\u0000\u0000\u0000\u02d9\u02da\u0005\u000e\u0000\u0000\u02da\u02db"+
		"\u00051\u0000\u0000\u02db\u02dc\u0003\u0080@\u0000\u02dc\u02dd\u00052"+
		"\u0000\u0000\u02dd\u02de\u0005\u000f\u0000\u0000\u02de\u02e2\u0003j5\u0000"+
		"\u02df\u02e1\u0003l6\u0000\u02e0\u02df\u0001\u0000\u0000\u0000\u02e1\u02e4"+
		"\u0001\u0000\u0000\u0000\u02e2\u02e0\u0001\u0000\u0000\u0000\u02e2\u02e3"+
		"\u0001\u0000\u0000\u0000\u02e3\u02e6\u0001\u0000\u0000\u0000\u02e4\u02e2"+
		"\u0001\u0000\u0000\u0000\u02e5\u02e7\u0003n7\u0000\u02e6\u02e5\u0001\u0000"+
		"\u0000\u0000\u02e6\u02e7\u0001\u0000\u0000\u0000\u02e7\u02e8\u0001\u0000"+
		"\u0000\u0000\u02e8\u02ea\u0003B!\u0000\u02e9\u02eb\u0005b\u0000\u0000"+
		"\u02ea\u02e9\u0001\u0000\u0000\u0000\u02ea\u02eb\u0001\u0000\u0000\u0000"+
		"\u02ebi\u0001\u0000\u0000\u0000\u02ec\u02ee\u00057\u0000\u0000\u02ed\u02ef"+
		"\u0003d2\u0000\u02ee\u02ed\u0001\u0000\u0000\u0000\u02ee\u02ef\u0001\u0000"+
		"\u0000\u0000\u02ef\u02f1\u0001\u0000\u0000\u0000\u02f0\u02ec\u0001\u0000"+
		"\u0000\u0000\u02f1\u02f4\u0001\u0000\u0000\u0000\u02f2\u02f0\u0001\u0000"+
		"\u0000\u0000\u02f2\u02f3\u0001\u0000\u0000\u0000\u02f3\u02f5\u0001\u0000"+
		"\u0000\u0000\u02f4\u02f2\u0001\u0000\u0000\u0000\u02f5\u02f9\u0005b\u0000"+
		"\u0000\u02f6\u02f8\u0003.\u0017\u0000\u02f7\u02f6\u0001\u0000\u0000\u0000"+
		"\u02f8\u02fb\u0001\u0000\u0000\u0000\u02f9\u02f7\u0001\u0000\u0000\u0000"+
		"\u02f9\u02fa\u0001\u0000\u0000\u0000\u02fak\u0001\u0000\u0000\u0000\u02fb"+
		"\u02f9\u0001\u0000\u0000\u0000\u02fc\u02fd\u0005\u0013\u0000\u0000\u02fd"+
		"\u02fe\u00051\u0000\u0000\u02fe\u02ff\u0003\u0080@\u0000\u02ff\u0300\u0005"+
		"2\u0000\u0000\u0300\u0301\u0005\u000f\u0000\u0000\u0301\u0302\u0003j5"+
		"\u0000\u0302m\u0001\u0000\u0000\u0000\u0303\u0304\u0005\u0015\u0000\u0000"+
		"\u0304\u0305\u0003j5\u0000\u0305o\u0001\u0000\u0000\u0000\u0306\u0307"+
		"\u0003@ \u0000\u0307\u0308\u0005/\u0000\u0000\u0308\u030a\u0001\u0000"+
		"\u0000\u0000\u0309\u0306\u0001\u0000\u0000\u0000\u0309\u030a\u0001\u0000"+
		"\u0000\u0000\u030a\u030c\u0001\u0000\u0000\u0000\u030b\u030d\u0003@ \u0000"+
		"\u030c\u030b\u0001\u0000\u0000\u0000\u030c\u030d\u0001\u0000\u0000\u0000"+
		"\u030d\u030e\u0001\u0000\u0000\u0000\u030e\u0315\u0005\u0019\u0000\u0000"+
		"\u030f\u0316\u0003r9\u0000\u0310\u0311\u0005\u0010\u0000\u0000\u0311\u0312"+
		"\u00051\u0000\u0000\u0312\u0313\u0003\u0080@\u0000\u0313\u0314\u00052"+
		"\u0000\u0000\u0314\u0316\u0001\u0000\u0000\u0000\u0315\u030f\u0001\u0000"+
		"\u0000\u0000\u0315\u0310\u0001\u0000\u0000\u0000\u0315\u0316\u0001\u0000"+
		"\u0000\u0000\u0316\u0318\u0001\u0000\u0000\u0000\u0317\u0319\u0003t:\u0000"+
		"\u0318\u0317\u0001\u0000\u0000\u0000\u0318\u0319\u0001\u0000\u0000\u0000"+
		"\u0319\u031a\u0001\u0000\u0000\u0000\u031a\u031e\u0005b\u0000\u0000\u031b"+
		"\u031d\u0003.\u0017\u0000\u031c\u031b\u0001\u0000\u0000\u0000\u031d\u0320"+
		"\u0001\u0000\u0000\u0000\u031e\u031c\u0001\u0000\u0000\u0000\u031e\u031f"+
		"\u0001\u0000\u0000\u0000\u031f\u0321\u0001\u0000\u0000\u0000\u0320\u031e"+
		"\u0001\u0000\u0000\u0000\u0321\u0323\u0003B!\u0000\u0322\u0324\u0003@"+
		" \u0000\u0323\u0322\u0001\u0000\u0000\u0000\u0323\u0324\u0001\u0000\u0000"+
		"\u0000\u0324\u0326\u0001\u0000\u0000\u0000\u0325\u0327\u0005b\u0000\u0000"+
		"\u0326\u0325\u0001\u0000\u0000\u0000\u0326\u0327\u0001\u0000\u0000\u0000"+
		"\u0327q\u0001\u0000\u0000\u0000\u0328\u0329\u0005Z\u0000\u0000\u0329\u032a"+
		"\u0005?\u0000\u0000\u032a\u032b\u0003\u0080@\u0000\u032b\u032c\u00050"+
		"\u0000\u0000\u032c\u032f\u0003\u0080@\u0000\u032d\u032e\u00050\u0000\u0000"+
		"\u032e\u0330\u0003\u0080@\u0000\u032f\u032d\u0001\u0000\u0000\u0000\u032f"+
		"\u0330\u0001\u0000\u0000\u0000\u0330s\u0001\u0000\u0000\u0000\u0331\u0332"+
		"\u0003@ \u0000\u0332\u0333\u00051\u0000\u0000\u0333\u0338\u0003\u0080"+
		"@\u0000\u0334\u0335\u00050\u0000\u0000\u0335\u0337\u0003\u0080@\u0000"+
		"\u0336\u0334\u0001\u0000\u0000\u0000\u0337\u033a\u0001\u0000\u0000\u0000"+
		"\u0338\u0336\u0001\u0000\u0000\u0000\u0338\u0339\u0001\u0000\u0000\u0000"+
		"\u0339\u033b\u0001\u0000\u0000\u0000\u033a\u0338\u0001\u0000\u0000\u0000"+
		"\u033b\u033c\u00052\u0000\u0000\u033cu\u0001\u0000\u0000\u0000\u033d\u033e"+
		"\u0005\u0016\u0000\u0000\u033e\u033f\u0005\u0017\u0000\u0000\u033f\u0340"+
		"\u00051\u0000\u0000\u0340\u0341\u0003\u0080@\u0000\u0341\u0342\u00052"+
		"\u0000\u0000\u0342\u0347\u0005b\u0000\u0000\u0343\u0346\u0003|>\u0000"+
		"\u0344\u0346\u0005b\u0000\u0000\u0345\u0343\u0001\u0000\u0000\u0000\u0345"+
		"\u0344\u0001\u0000\u0000\u0000\u0346\u0349\u0001\u0000\u0000\u0000\u0347"+
		"\u0345\u0001\u0000\u0000\u0000\u0347\u0348\u0001\u0000\u0000\u0000\u0348"+
		"\u034a\u0001\u0000\u0000\u0000\u0349\u0347\u0001\u0000\u0000\u0000\u034a"+
		"\u034c\u0003B!\u0000\u034b\u034d\u0005b\u0000\u0000\u034c\u034b\u0001"+
		"\u0000\u0000\u0000\u034c\u034d\u0001\u0000\u0000\u0000\u034dw\u0001\u0000"+
		"\u0000\u0000\u034e\u034f\u0005\u0011\u0000\u0000\u034f\u0350\u00051\u0000"+
		"\u0000\u0350\u0351\u0003\u0080@\u0000\u0351\u0352\u00052\u0000\u0000\u0352"+
		"\u0356\u0003j5\u0000\u0353\u0355\u0003z=\u0000\u0354\u0353\u0001\u0000"+
		"\u0000\u0000\u0355\u0358\u0001\u0000\u0000\u0000\u0356\u0354\u0001\u0000"+
		"\u0000\u0000\u0356\u0357\u0001\u0000\u0000\u0000\u0357\u0359\u0001\u0000"+
		"\u0000\u0000\u0358\u0356\u0001\u0000\u0000\u0000\u0359\u035b\u0003B!\u0000"+
		"\u035a\u035c\u0005b\u0000\u0000\u035b\u035a\u0001\u0000\u0000\u0000\u035b"+
		"\u035c\u0001\u0000\u0000\u0000\u035cy\u0001\u0000\u0000\u0000\u035d\u0362"+
		"\u0005\u0014\u0000\u0000\u035e\u035f\u00051\u0000\u0000\u035f\u0360\u0003"+
		"\u0080@\u0000\u0360\u0361\u00052\u0000\u0000\u0361\u0363\u0001\u0000\u0000"+
		"\u0000\u0362\u035e\u0001\u0000\u0000\u0000\u0362\u0363\u0001\u0000\u0000"+
		"\u0000\u0363\u0364\u0001\u0000\u0000\u0000\u0364\u0365\u0003j5\u0000\u0365"+
		"{\u0001\u0000\u0000\u0000\u0366\u036d\u0003~?\u0000\u0367\u0369\u0005"+
		"7\u0000\u0000\u0368\u036a\u0003d2\u0000\u0369\u0368\u0001\u0000\u0000"+
		"\u0000\u0369\u036a\u0001\u0000\u0000\u0000\u036a\u036c\u0001\u0000\u0000"+
		"\u0000\u036b\u0367\u0001\u0000\u0000\u0000\u036c\u036f\u0001\u0000\u0000"+
		"\u0000\u036d\u036b\u0001\u0000\u0000\u0000\u036d\u036e\u0001\u0000\u0000"+
		"\u0000\u036e\u0370\u0001\u0000\u0000\u0000\u036f\u036d\u0001\u0000\u0000"+
		"\u0000\u0370\u0374\u0005b\u0000\u0000\u0371\u0373\u0003.\u0017\u0000\u0372"+
		"\u0371\u0001\u0000\u0000\u0000\u0373\u0376\u0001\u0000\u0000\u0000\u0374"+
		"\u0375\u0001\u0000\u0000\u0000\u0374\u0372\u0001\u0000\u0000\u0000\u0375"+
		"}\u0001\u0000\u0000\u0000\u0376\u0374\u0001\u0000\u0000\u0000\u0377\u0378"+
		"\u0005\u0017\u0000\u0000\u0378\u0379\u00051\u0000\u0000\u0379\u037e\u0003"+
		"\u008eG\u0000\u037a\u037b\u00050\u0000\u0000\u037b\u037d\u0003\u008eG"+
		"\u0000\u037c\u037a\u0001\u0000\u0000\u0000\u037d\u0380\u0001\u0000\u0000"+
		"\u0000\u037e\u037c\u0001\u0000\u0000\u0000\u037e\u037f\u0001\u0000\u0000"+
		"\u0000\u037f\u0381\u0001\u0000\u0000\u0000\u0380\u037e\u0001\u0000\u0000"+
		"\u0000\u0381\u0382\u00052\u0000\u0000\u0382\u0386\u0001\u0000\u0000\u0000"+
		"\u0383\u0384\u0005\u0017\u0000\u0000\u0384\u0386\u0005\u0018\u0000\u0000"+
		"\u0385\u0377\u0001\u0000\u0000\u0000\u0385\u0383\u0001\u0000\u0000\u0000"+
		"\u0386\u007f\u0001\u0000\u0000\u0000\u0387\u038d\u0003\u0082A\u0000\u0388"+
		"\u0389\u0003\u0096K\u0000\u0389\u038a\u0003\u0082A\u0000\u038a\u038c\u0001"+
		"\u0000\u0000\u0000\u038b\u0388\u0001\u0000\u0000\u0000\u038c\u038f\u0001"+
		"\u0000\u0000\u0000\u038d\u038b\u0001\u0000\u0000\u0000\u038d\u038e\u0001"+
		"\u0000\u0000\u0000\u038e\u0081\u0001\u0000\u0000\u0000\u038f\u038d\u0001"+
		"\u0000\u0000\u0000\u0390\u0394\u0003\u0084B\u0000\u0391\u0393\u0003\u0088"+
		"D\u0000\u0392\u0391\u0001\u0000\u0000\u0000\u0393\u0396\u0001\u0000\u0000"+
		"\u0000\u0394\u0392\u0001\u0000\u0000\u0000\u0394\u0395\u0001\u0000\u0000"+
		"\u0000\u0395\u0083\u0001\u0000\u0000\u0000\u0396\u0394\u0001\u0000\u0000"+
		"\u0000\u0397\u0398\u00051\u0000\u0000\u0398\u0399\u0003\u008cF\u0000\u0399"+
		"\u039a\u00052\u0000\u0000\u039a\u03a5\u0001\u0000\u0000\u0000\u039b\u03a5"+
		"\u0003\u0092I\u0000\u039c\u039d\u0005I\u0000\u0000\u039d\u03a5\u0003\u0082"+
		"A\u0000\u039e\u039f\u0005;\u0000\u0000\u039f\u03a5\u0003\u0082A\u0000"+
		"\u03a0\u03a1\u0005:\u0000\u0000\u03a1\u03a5\u0003\u0082A\u0000\u03a2\u03a5"+
		"\u0003\u0086C\u0000\u03a3\u03a5\u0003\u0090H\u0000\u03a4\u0397\u0001\u0000"+
		"\u0000\u0000\u03a4\u039b\u0001\u0000\u0000\u0000\u03a4\u039c\u0001\u0000"+
		"\u0000\u0000\u03a4\u039e\u0001\u0000\u0000\u0000\u03a4\u03a0\u0001\u0000"+
		"\u0000\u0000\u03a4\u03a2\u0001\u0000\u0000\u0000\u03a4\u03a3\u0001\u0000"+
		"\u0000\u0000\u03a5\u0085\u0001\u0000\u0000\u0000\u03a6\u03a8\u0005R\u0000"+
		"\u0000\u03a7\u03a6\u0001\u0000\u0000\u0000\u03a7\u03a8\u0001\u0000\u0000"+
		"\u0000\u03a8\u03aa\u0001\u0000\u0000\u0000\u03a9\u03ab\u0003\u008aE\u0000"+
		"\u03aa\u03a9\u0001\u0000\u0000\u0000\u03aa\u03ab\u0001\u0000\u0000\u0000"+
		"\u03ab\u03ac\u0001\u0000\u0000\u0000\u03ac\u03ad\u0007\u0004\u0000\u0000"+
		"\u03ad\u03b2\u0003@ \u0000\u03ae\u03af\u0005R\u0000\u0000\u03af\u03b2"+
		"\u0003@ \u0000\u03b0\u03b2\u0003@ \u0000\u03b1\u03a7\u0001\u0000\u0000"+
		"\u0000\u03b1\u03ae\u0001\u0000\u0000\u0000\u03b1\u03b0\u0001\u0000\u0000"+
		"\u0000\u03b2\u0087\u0001\u0000\u0000\u0000\u03b3\u03b4\u0007\u0005\u0000"+
		"\u0000\u03b4\u03b5\u0003@ \u0000\u03b5\u03b6\u0007\u0004\u0000\u0000\u03b6"+
		"\u03b7\u0003@ \u0000\u03b7\u03c8\u0001\u0000\u0000\u0000\u03b8\u03b9\u0007"+
		"\u0005\u0000\u0000\u03b9\u03ba\u0007\u0004\u0000\u0000\u03ba\u03c8\u0003"+
		"@ \u0000\u03bb\u03bc\u0007\u0005\u0000\u0000\u03bc\u03c8\u0003@ \u0000"+
		"\u03bd\u03bf\u00051\u0000\u0000\u03be\u03c0\u0003\u008cF\u0000\u03bf\u03be"+
		"\u0001\u0000\u0000\u0000\u03bf\u03c0\u0001\u0000\u0000\u0000\u03c0\u03c1"+
		"\u0001\u0000\u0000\u0000\u03c1\u03c8\u00052\u0000\u0000\u03c2\u03c4\u0005"+
		"5\u0000\u0000\u03c3\u03c5\u0003\u008cF\u0000\u03c4\u03c3\u0001\u0000\u0000"+
		"\u0000\u03c4\u03c5\u0001\u0000\u0000\u0000\u03c5\u03c6\u0001\u0000\u0000"+
		"\u0000\u03c6\u03c8\u00056\u0000\u0000\u03c7\u03b3\u0001\u0000\u0000\u0000"+
		"\u03c7\u03b8\u0001\u0000\u0000\u0000\u03c7\u03bb\u0001\u0000\u0000\u0000"+
		"\u03c7\u03bd\u0001\u0000\u0000\u0000\u03c7\u03c2\u0001\u0000\u0000\u0000"+
		"\u03c8\u0089\u0001\u0000\u0000\u0000\u03c9\u03ca\u0003H$\u0000\u03ca\u008b"+
		"\u0001\u0000\u0000\u0000\u03cb\u03d0\u0003\u008eG\u0000\u03cc\u03cd\u0005"+
		"0\u0000\u0000\u03cd\u03cf\u0003\u008eG\u0000\u03ce\u03cc\u0001\u0000\u0000"+
		"\u0000\u03cf\u03d2\u0001\u0000\u0000\u0000\u03d0\u03ce\u0001\u0000\u0000"+
		"\u0000\u03d0\u03d1\u0001\u0000\u0000\u0000\u03d1\u008d\u0001\u0000\u0000"+
		"\u0000\u03d2\u03d0\u0001\u0000\u0000\u0000\u03d3\u03d4\u0003@ \u0000\u03d4"+
		"\u03d7\u0005?\u0000\u0000\u03d5\u03d8\u0003\u0080@\u0000\u03d6\u03d8\u0005"+
		"=\u0000\u0000\u03d7\u03d5\u0001\u0000\u0000\u0000\u03d7\u03d6\u0001\u0000"+
		"\u0000\u0000\u03d8\u03fa\u0001\u0000\u0000\u0000\u03d9\u03fa\u0005=\u0000"+
		"\u0000\u03da\u03e9\u0003\u0080@\u0000\u03db\u03dd\u0005/\u0000\u0000\u03dc"+
		"\u03de\u0003\u0080@\u0000\u03dd\u03dc\u0001\u0000\u0000\u0000\u03dd\u03de"+
		"\u0001\u0000\u0000\u0000\u03de\u03e3\u0001\u0000\u0000\u0000\u03df\u03e1"+
		"\u0005/\u0000\u0000\u03e0\u03e2\u0003\u0080@\u0000\u03e1\u03e0\u0001\u0000"+
		"\u0000\u0000\u03e1\u03e2\u0001\u0000\u0000\u0000\u03e2\u03e4\u0001\u0000"+
		"\u0000\u0000\u03e3\u03df\u0001\u0000\u0000\u0000\u03e3\u03e4\u0001\u0000"+
		"\u0000\u0000\u03e4\u03ea\u0001\u0000\u0000\u0000\u03e5\u03e7\u0005-\u0000"+
		"\u0000\u03e6\u03e8\u0003\u0080@\u0000\u03e7\u03e6\u0001\u0000\u0000\u0000"+
		"\u03e7\u03e8\u0001\u0000\u0000\u0000\u03e8\u03ea\u0001\u0000\u0000\u0000"+
		"\u03e9\u03db\u0001\u0000\u0000\u0000\u03e9\u03e5\u0001\u0000\u0000\u0000"+
		"\u03e9\u03ea\u0001\u0000\u0000\u0000\u03ea\u03fa\u0001\u0000\u0000\u0000"+
		"\u03eb\u03ed\u0005/\u0000\u0000\u03ec\u03ee\u0003\u0080@\u0000\u03ed\u03ec"+
		"\u0001\u0000\u0000\u0000\u03ed\u03ee\u0001\u0000\u0000\u0000\u03ee\u03f3"+
		"\u0001\u0000\u0000\u0000\u03ef\u03f1\u0005/\u0000\u0000\u03f0\u03f2\u0003"+
		"\u0080@\u0000\u03f1\u03f0\u0001\u0000\u0000\u0000\u03f1\u03f2\u0001\u0000"+
		"\u0000\u0000\u03f2\u03f4\u0001\u0000\u0000\u0000\u03f3\u03ef\u0001\u0000"+
		"\u0000\u0000\u03f3\u03f4\u0001\u0000\u0000\u0000\u03f4\u03fa\u0001\u0000"+
		"\u0000\u0000\u03f5\u03f7\u0005-\u0000\u0000\u03f6\u03f8\u0003\u0080@\u0000"+
		"\u03f7\u03f6\u0001\u0000\u0000\u0000\u03f7\u03f8\u0001\u0000\u0000\u0000"+
		"\u03f8\u03fa\u0001\u0000\u0000\u0000\u03f9\u03d3\u0001\u0000\u0000\u0000"+
		"\u03f9\u03d9\u0001\u0000\u0000\u0000\u03f9\u03da\u0001\u0000\u0000\u0000"+
		"\u03f9\u03eb\u0001\u0000\u0000\u0000\u03f9\u03f5\u0001\u0000\u0000\u0000"+
		"\u03fa\u008f\u0001\u0000\u0000\u0000\u03fb\u03fc\u0007\u0006\u0000\u0000"+
		"\u03fc\u0091\u0001\u0000\u0000\u0000\u03fd\u0406\u00055\u0000\u0000\u03fe"+
		"\u0403\u0003\u0094J\u0000\u03ff\u0400\u00050\u0000\u0000\u0400\u0402\u0003"+
		"\u0094J\u0000\u0401\u03ff\u0001\u0000\u0000\u0000\u0402\u0405\u0001\u0000"+
		"\u0000\u0000\u0403\u0401\u0001\u0000\u0000\u0000\u0403\u0404\u0001\u0000"+
		"\u0000\u0000\u0404\u0407\u0001\u0000\u0000\u0000\u0405\u0403\u0001\u0000"+
		"\u0000\u0000\u0406\u03fe\u0001\u0000\u0000\u0000\u0406\u0407\u0001\u0000"+
		"\u0000\u0000\u0407\u0408\u0001\u0000\u0000\u0000\u0408\u0409\u00056\u0000"+
		"\u0000\u0409\u0093\u0001\u0000\u0000\u0000\u040a\u040b\u00051\u0000\u0000"+
		"\u040b\u040c\u0003\u0080@\u0000\u040c\u040d\u00050\u0000\u0000\u040d\u040e"+
		"\u0003r9\u0000\u040e\u040f\u00052\u0000\u0000\u040f\u0412\u0001\u0000"+
		"\u0000\u0000\u0410\u0412\u0003\u0080@\u0000\u0411\u040a\u0001\u0000\u0000"+
		"\u0000\u0411\u0410\u0001\u0000\u0000\u0000\u0412\u0095\u0001\u0000\u0000"+
		"\u0000\u0413\u0414\u0007\u0007\u0000\u0000\u0414\u0097\u0001\u0000\u0000"+
		"\u0000\u008c\u009b\u009d\u00a8\u00ad\u00b0\u00b8\u00c0\u00c3\u00c6\u00c9"+
		"\u00d0\u00da\u00dd\u00e4\u00e6\u00eb\u00ef\u00fb\u00fe\u0104\u0110\u0115"+
		"\u011c\u0121\u0123\u012a\u0130\u0134\u013b\u0140\u0144\u0147\u014a\u014e"+
		"\u015c\u0161\u016b\u0175\u0177\u017b\u017f\u018b\u018f\u0194\u019d\u01ab"+
		"\u01ae\u01b1\u01b8\u01bc\u01be\u01c5\u01cd\u01d1\u01d7\u01db\u01e2\u01e7"+
		"\u01ee\u01f2\u01fc\u0200\u0203\u0208\u020f\u0212\u0219\u0224\u024c\u0254"+
		"\u025d\u0261\u0264\u0268\u026c\u0274\u027e\u0283\u028c\u0295\u029a\u029e"+
		"\u02a6\u02b8\u02bc\u02c4\u02c8\u02cc\u02cf\u02d6\u02e2\u02e6\u02ea\u02ee"+
		"\u02f2\u02f9\u0309\u030c\u0315\u0318\u031e\u0323\u0326\u032f\u0338\u0345"+
		"\u0347\u034c\u0356\u035b\u0362\u0369\u036d\u0374\u037e\u0385\u038d\u0394"+
		"\u03a4\u03a7\u03aa\u03b1\u03bf\u03c4\u03c7\u03d0\u03d7\u03dd\u03e1\u03e3"+
		"\u03e7\u03e9\u03ed\u03f1\u03f3\u03f7\u03f9\u0403\u0406\u0411";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}