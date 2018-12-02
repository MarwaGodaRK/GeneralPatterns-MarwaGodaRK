package com.aurea.testgenerator.generation.patterns.trycatch


import com.aurea.testgenerator.generation.AbstractMethodTestGenerator
import com.aurea.testgenerator.generation.TestGeneratorError
import com.aurea.testgenerator.generation.TestGeneratorResult
import com.aurea.testgenerator.generation.TestType
import com.aurea.testgenerator.generation.ast.DependableNode
import com.aurea.testgenerator.generation.merge.TestNodeMerger
import com.aurea.testgenerator.generation.methods.MethodsUtils
import com.aurea.testgenerator.generation.mock.util.MockitoUtils
import com.aurea.testgenerator.generation.names.NomenclatureFactory
import com.aurea.testgenerator.generation.names.TestMethodNomenclature
import com.aurea.testgenerator.generation.source.Imports
import com.aurea.testgenerator.reporting.CoverageReporter
import com.aurea.testgenerator.reporting.TestGeneratorResultReporter
import com.aurea.testgenerator.source.Unit
import com.aurea.testgenerator.value.ValueFactory
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.AccessSpecifier
import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.MethodCallExpr
import com.github.javaparser.ast.expr.NameExpr
import com.github.javaparser.ast.expr.VariableDeclarationExpr
import com.github.javaparser.ast.stmt.BlockStmt
import com.github.javaparser.ast.stmt.CatchClause
import com.github.javaparser.ast.stmt.ExpressionStmt
import com.github.javaparser.ast.stmt.ReturnStmt
import com.github.javaparser.ast.stmt.ThrowStmt
import com.github.javaparser.ast.stmt.TryStmt
import com.github.javaparser.ast.type.Type
import com.github.javaparser.ast.type.UnionType
import com.github.javaparser.resolution.types.ResolvedType
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import groovy.util.logging.Log4j2
import org.springframework.context.annotation.Profile
import org.springframework.stereotype.Component

import java.util.stream.Collectors


@Component
@Profile("trycatch")
@Log4j2
class TryCatchTestGenerator extends AbstractMethodTestGenerator {

    ValueFactory valueFactory
    private static final String EXPECTED_RESULT = "expectedResult"
    private static final List<ImportDeclaration> IMPORTS = [
            Imports.MOCKITO,
            Imports.STATIC_MOCKITO_ASTERISK,
            Imports.JUNIT_TEST
    ]

    TryCatchTestGenerator(JavaParserFacade solver, TestGeneratorResultReporter reporter,
                          CoverageReporter visitReporter, NomenclatureFactory nomenclatures,
                          ValueFactory valueFactory) {
        super(solver, reporter, visitReporter, nomenclatures)
        this.valueFactory = valueFactory
    }

    @Override
    protected TestGeneratorResult generate(MethodDeclaration callableDeclaration, Unit unitUnderTest) {

        TestGeneratorResult result = new TestGeneratorResult()

        List<NodeList<CatchClause>> catchClauses = callableDeclaration.findAll(TryStmt).catchClauses
        NodeList<CatchClause> catchExpr =
                catchClauses.size() > 0 && catchClauses.first().size() > 0 ? catchClauses.first() : null

        if (catchExpr != null) {
            try {
                DependableNode<MethodDeclaration> testMethod =
                        buildTestMethod(unitUnderTest, callableDeclaration, catchExpr)

                if (testMethod != null) {
                    result.tests = [testMethod]
                }

            } catch (
                    TestGeneratorError tge) {
                result.errors << tge
            }
        }

        result

    }

    DependableNode<MethodDeclaration> buildTestMethod(Unit unitUnderTest, MethodDeclaration method,
                                                      NodeList<CatchClause> catchExpr) {

        Collection<BlockStmt> tryStmts = method.findAll(TryStmt).tryBlock

        ArrayList<String> exceptionTypes = extractCaughtExceptionsList(catchExpr)

        Optional<MethodCallExpr> methodCallExpr = extractMethodExprThatThrowsCaughtException(tryStmts, exceptionTypes)

        if (methodCallExpr.isPresent()) {
            def (boolean catchClauseHasReturnStmt, Type newThrownExceptionType) = detectExceptionHandlingAction(catchExpr)

            String args = MockitoUtils.getArgs(method, methodCallExpr.get())

            String expectedClause = catchClauseHasReturnStmt ? "" : "(expected=" +
                    (newThrownExceptionType == null ?
                            (exceptionTypes.size() == 1 ?  exceptionTypes.get(0) : "Exception") :
                            newThrownExceptionType ) + ".class)"

            String catchedException = getTypeClassName(methodCallExpr.get().resolveInvokedMethod().getSpecifiedExceptions().get(0))

            String testName = getTestMethodName(unitUnderTest, method)
            ClassOrInterfaceDeclaration parentClass = method.getAncestorOfType(ClassOrInterfaceDeclaration).get()

            String testCode = """
            
            @Test${expectedClause}
            public void ${testName}() throws Exception {
                ${parentClass.name} object = new ${parentClass.name}();
                object = Mockito.spy(object);

                ${getVariableStatements(method)}

               doThrow(new ${catchedException}()).when(${getScope(methodCallExpr.get())}).${
                methodCallExpr.get().nameAsString
            }($args);     
         
               ${getVerifyCode(method)}
             }
            """
            return getTestMethod(method, testCode)

        }
    }

    private ArrayList<String> extractCaughtExceptionsList(NodeList<CatchClause> catchExpr) {
        Type catchedExceptions = catchExpr.first().parameter.type
        List<String> exceptionTypes = new ArrayList<>()
        if (catchedExceptions instanceof UnionType && catchedExceptions.elements.size() > 1) {
            exceptionTypes.addAll(catchedExceptions.elements.toList().stream().map({ t -> t.toString() })
                    .collect(Collectors.toList()))
        } else {
            exceptionTypes.add(catchedExceptions.toString())
        }
        exceptionTypes
    }

    private Optional<MethodCallExpr> extractMethodExprThatThrowsCaughtException(ArrayList<BlockStmt> tryStmts, exceptionTypes) {
        Optional<MethodCallExpr> methodCallExpr = tryStmts.first().findAll(MethodCallExpr).stream().filter {
            m ->
                m.resolveInvokedMethod().getSpecifiedExceptions().stream()
                        .filter({ e -> exceptionTypes.contains(getTypeClassName(e)) })
                        .collect(Collectors.toList()).size() > 0 &&
                        m.resolveInvokedMethod().accessSpecifier() != AccessSpecifier.PRIVATE
        }.findFirst()
        methodCallExpr
    }



    private String getTypeClassName(ResolvedType e) {
        JavaParser.parseClassOrInterfaceType(e.asReferenceType().qualifiedName).name.toString()
    }


    private List detectExceptionHandlingAction(NodeList<CatchClause> catchExpr) {

        Optional<BlockStmt> catchBlockStmt =
                catchExpr.childNodes.first().stream().filter { b -> b instanceof BlockStmt }.findFirst()

        Optional<BlockStmt> throwStmt =
                catchBlockStmt.get().statements.stream().filter { b -> b instanceof ThrowStmt }.findFirst()

        Optional<BlockStmt> returnStmt =
                catchBlockStmt.get().statements.stream().filter { b -> b instanceof ReturnStmt }.findFirst()

        Type newThrownExceptionType
        boolean catchClauseHasReturnStmt
        if (returnStmt.isPresent()) {
            catchClauseHasReturnStmt = true
        } else if (throwStmt.isPresent()) {
            if (!(throwStmt.get().expression instanceof NameExpr)) {
                newThrownExceptionType = throwStmt.get().expression.type
            }
        }
        [catchClauseHasReturnStmt, newThrownExceptionType]
    }

    private String getTestMethodName(Unit unit, MethodDeclaration method) {
        TestMethodNomenclature testMethodNomenclature = nomenclatures.getTestMethodNomenclature(unit.javaClass)
        testMethodNomenclature.requestTestMethodName(getType(), method)
    }

    private String getVariableStatements(MethodDeclaration method) {
        method.parameters.collect {
            new ExpressionStmt(valueFactory.getVariable(it.nameAsString, it.type).get().node)
        }.join(System.lineSeparator())
    }

    private Optional<DependableNode<VariableDeclarationExpr>> getExpectedResultDepNode(MethodDeclaration method) {
        if (MethodsUtils.returnsClassOrInterface(method)) {
            valueFactory.getVariable(EXPECTED_RESULT, method.type)
        } else {
            Optional.empty()
        }
    }

    private static String getScope(MethodCallExpr delegateExpression) {
        if (delegateExpression.scope.isPresent()) {
            delegateExpression.scope.get().asNameExpr().nameAsString
        } else {
            "object"
        }
    }

    private String getVerifyCode(MethodDeclaration method) {
        String params = ""
        if (method.parameters.size() > 0) {
            params = method.parameters.collect {
                "$it.name.identifier"
            }.join(",")
        }

        "object.${method.nameAsString}(${params});".toString()
    }

    private DependableNode<MethodDeclaration> getTestMethod(MethodDeclaration methodDeclaration, String testCode) {
        DependableNode<MethodDeclaration> testMethod = new DependableNode<>()

        getExpectedResultDepNode(methodDeclaration)
                .ifPresent { TestNodeMerger.appendDependencies(testMethod, it) }
        testMethod.node = JavaParser.parseBodyDeclaration(testCode).asMethodDeclaration()
        testMethod.dependency.imports << IMPORTS

        testMethod
    }

    @Override
    protected TestType getType() {
        return TryCatchTestTypes.CATCH_EXCEPTION_TEST
    }


    @Override
    protected boolean shouldBeVisited(Unit unit, MethodDeclaration method) {
        return super.shouldBeVisited(unit, method) && containsNonNestedTryCatchBlock(method)
    }

    private static boolean containsNonNestedTryCatchBlock(MethodDeclaration method) {
        method.findFirst(TryStmt).isPresent() &&
                method.findFirst(TryStmt).get().getParentNode().get().parentNode.get().equals(method)
    }

}

