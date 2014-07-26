// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.compile.transformers;

import org.kframework.compile.utils.MetaK;
import org.kframework.kil.ASTNode;
import org.kframework.kil.Attribute;
import org.kframework.kil.BoolBuiltin;
import org.kframework.kil.Configuration;
import org.kframework.kil.KApp;
import org.kframework.kil.KInjectedLabel;
import org.kframework.kil.KLabelConstant;
import org.kframework.kil.KSorts;
import org.kframework.kil.ListTerminator;
import org.kframework.kil.Module;
import org.kframework.kil.ModuleItem;
import org.kframework.kil.Production;
import org.kframework.kil.Rule;
import org.kframework.kil.Sort;
import org.kframework.kil.StringBuiltin;
import org.kframework.kil.Syntax;
import org.kframework.kil.Term;
import org.kframework.kil.Variable;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.BasicVisitor;
import org.kframework.kil.visitors.CopyOnWriteTransformer;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class AddPredicates extends CopyOnWriteTransformer {

    public static final KLabelConstant K2Sort = KLabelConstant.of("K2Sort");

    public class PredicatesVisitor extends BasicVisitor {

        public PredicatesVisitor(String name, Context context) {
            super(name, context);
        }

        private List<ModuleItem> result = new ArrayList<ModuleItem>();
        private Set<Sort> lists = new HashSet<>();

        @Override
        public Void visit(Module node, Void _) {
            lists.clear();
            super.visit(node, _);
            if (!lists.isEmpty()) {
                for (Sort listSort : lists) {
                    Rule rule = new Rule(
                            KApp.of(KLabelConstant.of(predicate(listSort.getName()), context), new ListTerminator(listSort, null)),
                            BoolBuiltin.TRUE, context);
                    rule.addAttribute(Attribute.PREDICATE);
                    result.add(rule);
                    rule = new Rule(
                            KApp.of(KLabelConstant.KRESULT_PREDICATE, new ListTerminator(listSort, null)),
                            BoolBuiltin.TRUE, context);
                    rule.addAttribute(Attribute.PREDICATE);
                    result.add(rule);
                }
            }
            return null;
        }

        @Override
        public Void visit(Syntax node, Void _) {
            Sort sort = node.getSort().getSort2();

            if (context.isListSort(sort))
                lists.add(sort);

            if (sort.isKSort())
                return null;
            else
                return super.visit(node, _);
        }

        @Override
        public Void visit(Production node, Void _) {
            if (node.containsAttribute("bracket"))
                return null;
            if (node.containsAttribute("predicate"))
                return null;
            if (node.isLexical()) {
                /* predicate definition for token sorts is deferred to each backend */
                return null;
            }

            if (context.getDataStructureSorts().containsKey(node.getSort())) {
                /* predicate definition for builtin collection sorts is deferred to each backend */
                return null;
            }

            Sort sort = node.getSort();
            Term term = MetaK.getTerm(node, context);

            Term rhs;
            if (node.containsAttribute("function") && node.getArity() > 0)
               rhs = KApp.of(KSymbolicPredicate, term);
            else
               rhs = BoolBuiltin.TRUE;
            Term lhs = KApp.of(KLabelConstant.of(syntaxPredicate(sort.getName()), context), term);
            Rule rule = new Rule(lhs, rhs, context);
            rule.addAttribute(Attribute.PREDICATE);
            result.add(rule);

            // define K2Sort for syntactic production (excluding subsorts)
            if (!node.isSubsort()) {
                lhs = KApp.of(K2Sort, term);
                rhs = StringBuiltin.kAppOf(sort.getName());
                rule = new Rule(lhs, rhs, context);
                rule.addAttribute(Attribute.FUNCTION);
                result.add(rule);
            }
            return null;
        }

        @Override
        public Void visit(Rule node, Void _) {
            return null;
        }

        @Override
        public Void visit(org.kframework.kil.Context node, Void _) {
            return null;
        }

        @Override
        public Void visit(Configuration node, Void _) {
            return null;
        }

        public List<ModuleItem> getResult() {
            return result;
        }
    }


    private static final String PredicatePrefix = "is";
    private static final String SymbolicPredicatePrefix = "Symbolic";
    public static final KLabelConstant BuiltinPredicate =
            KLabelConstant.of(predicate("Builtin"));
    public static final KLabelConstant VariablePredicate =
            KLabelConstant.of(predicate("Variable"));
    public static final KLabelConstant KSymbolicPredicate =
            KLabelConstant.of(symbolicPredicate(KSorts.K));


    public static final String predicate(String sort) {
        return PredicatePrefix + sort;
    }

    public static final String syntaxPredicate(String sort) {
        assert !Sort.of(sort).isKSort():
                "invalid syntactic predicate " + predicate(sort) + " for sort " + sort;

        return predicate(sort);
    }

    public static final String symbolicPredicate(String sort) {
        assert AddSymbolicK.allowSymbolic(sort):
                "invalid symbolic predicate " + predicate(SymbolicPredicatePrefix + sort)
                        + " for sort "+ sort;

        return predicate(SymbolicPredicatePrefix + sort);
    }

    @Override
    public ASTNode visit(Module node, Void _)  {
        Module retNode = node.shallowCopy();
        retNode.setItems(new ArrayList<ModuleItem>(node.getItems()));

        // declare isBuiltin predicate as KLabel
        retNode.addConstant(BuiltinPredicate);
        // declare isSymbolicK predicate as KLabel
        retNode.addConstant(KSymbolicPredicate);

        for (Sort sort : node.getAllSorts()) {
            if (!sort.isKSort()) {
                String pred = AddPredicates.syntaxPredicate(sort.getName());
                // declare isSort predicate as KLabel
                retNode.addConstant(Sort.KLABEL, pred);

                if (AddSymbolicK.allowKSymbolic(sort.getName())) {
                    String symPred = AddPredicates.symbolicPredicate(sort.getName());
                    // declare isSymbolicSort predicate as KLabel
                    retNode.addConstant(Sort.KLABEL, symPred);

                    // define isSymbolicSort predicate as the conjunction of isSort and isSymbolicK
                    Variable var = Variable.getFreshVar(Sort.K);
                    Term lhs = KApp.of(KLabelConstant.of(symPred, context), var);
                    Term rhs = KApp.of(
                            KLabelConstant.BOOL_ANDTHENBOOL_KLABEL,
                            KApp.of(KLabelConstant.of(pred, context), var),
                            KApp.of(KSymbolicPredicate, var));
                    Rule rule = new Rule(lhs, rhs, context);
                    rule.addAttribute(Attribute.PREDICATE);
                    retNode.appendModuleItem(rule);

                    String symCtor = AddSymbolicK.symbolicConstructor(sort.getName());
                    var = Variable.getFreshVar(Sort.KLIST);
                    Term symTerm = KApp.of(KLabelConstant.of(symCtor, context), var);

                    // define isSort for symbolic sort constructor symSort
                    lhs = KApp.of(KLabelConstant.of(pred, context), symTerm);
                    rule = new Rule(lhs, BoolBuiltin.TRUE, context);
                    rule.addAttribute(Attribute.PREDICATE);
                    retNode.appendModuleItem(rule);

                    // define isVariable predicate for symbolic sort constructor symSort
                    rule = getIsVariableRule(symTerm, context);
                    retNode.appendModuleItem(rule);

                    // define K2Sort function for symbolic sort constructor
                    // symSort
                    lhs = KApp.of(K2Sort, symTerm);
                    rhs = StringBuiltin.kAppOf(sort.getName());
                    rule = new Rule(lhs, rhs, context);
                    rule.addAttribute(Attribute.FUNCTION);
                    retNode.appendModuleItem(rule);
                } else if (sort.isBuiltinSort()) {
                    Variable var = Variable.getFreshVar(sort);
                    Term lhs = KApp.of(BuiltinPredicate, var);
                    Rule rule = new Rule(lhs, BoolBuiltin.TRUE, context);
                    rule.addAttribute(Attribute.PREDICATE);
                    retNode.appendModuleItem(rule);

                    /*
                     * definition for builtins moved in symbolic-k.k
                    lhs = new KApp(K2Sort, var);
                    Term rhs = new Constant("#String", "\"" + sort + "\"");
                    rule = new Rule();
                    rule.setBody(new Rewrite(lhs, rhs));
                    rule.getCellAttributes().getContents().add(Attribute.FUNCTION);
                    retNode.appendModuleItem(rule);
                     */
                } else if (context.getTokenSorts().contains(sort.getName())) {
                    /* defer membership predicate for lexical token to each backend */
                }
            }
        }

        /* add collection membership predicates */
        for (Sort sort : context.getDataStructureSorts().keySet()) {
            retNode.addConstant(Sort.KLABEL, AddPredicates.predicate(sort.getName()));
        }

        PredicatesVisitor mv = new PredicatesVisitor("PredicatesVisitor", context);
        mv.visitNode(node);
        retNode.getItems().addAll(mv.getResult());

        if (retNode.getItems().size() != node.getItems().size())
            return retNode;
        else
            return node;
    }

    public static Rule getIsVariableRule(Term symTerm, Context context) {
        Term lhs;
        Rule rule;
        if (!symTerm.getSort().isComputationSort()) {
            symTerm = KApp.of(new KInjectedLabel(symTerm));
        }

        lhs = KApp.of(VariablePredicate, symTerm);
        rule = new Rule(lhs, BoolBuiltin.TRUE, context);
        rule.addAttribute(Attribute.PREDICATE);
        return rule;
    }

    public AddPredicates(Context context) {
        super("Add syntax and symbolic predicates", context);
    }

}
