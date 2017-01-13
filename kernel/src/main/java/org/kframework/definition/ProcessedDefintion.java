package org.kframework.definition;

import org.kframework.kompile.KompileOptions;
import org.kframework.minikore.MiniKore;

/** Definition Contains the minikore Definition + Extras That are needed for the Backend to Function.
 * The Rewriter is a part of the backend. It needs two things to function - A Kore start term, and a Kore Definition (axioms for reachability logic/dynamic matching logic.
 * The Definition may contain other utilities needed by the backend, such as a parser/pretty printer, and hooks, that may be needed for the rewriter to function correctly.
 */

public class ProcessedDefintion {
    public KompileOptions kompileOptions;
    public MiniKore.Definition definition;

    public ProcessedDefintion(KompileOptions kompileOptions, MiniKore.Definition definition) {
        this.kompileOptions = kompileOptions;
        this.definition = definition;
    }
}