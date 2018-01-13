package com.aurea.methobase.meta.purity

import com.aurea.methobase.meta.JavaParserFacadeFactory
import com.github.javaparser.ast.expr.ArrayAccessExpr
import com.github.javaparser.ast.expr.AssignExpr
import com.github.javaparser.ast.expr.NameExpr
import com.github.javaparser.resolution.declarations.ResolvedValueDeclaration
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import com.github.javaparser.symbolsolver.model.resolution.SymbolReference
import com.jasongoodwin.monads.Try

import java.util.function.BiPredicate

class ArrayAccessExprPureFunctionPredicate implements BiPredicate<ArrayAccessExpr, JavaParserFacade> {
    @Override
    boolean test(ArrayAccessExpr expr, JavaParserFacade context) {
        if (expr.name instanceof NameExpr) {
            NameExpr nameExpr = expr.name as NameExpr
            boolean isAssignment = expr.parentNode.map { it instanceof AssignExpr }.orElse(false)
            return !(isAssignment && !isVariable(nameExpr, context))
        }
        false
    }

    static boolean isVariable(NameExpr nameExpr, JavaParserFacade context) {
        SymbolReference<? extends ResolvedValueDeclaration> ref = Try.<SymbolReference<? extends ResolvedValueDeclaration>>ofFailable { context.solve(nameExpr) }
                                                                     .onFailure { JavaParserFacadeFactory.reportAsUnsolved(nameExpr)}
                                                                     .orElse(SymbolReference.unsolved(ResolvedValueDeclaration))
        ref.solved && ref.correspondingDeclaration.variable
    }
}
