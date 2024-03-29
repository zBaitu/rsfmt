pub fn id_chain_exprs(&self) -> HashSet<Expr> {
    match self.kind {
        ExprKind::IdentifierChain(_) => { set.insert(self.clone()); },
    }
}
