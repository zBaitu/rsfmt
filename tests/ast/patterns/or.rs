fn f() {
    match a {
        a | b | c => true,
    }

    fn ff() {
        Ok(match self.op {
            Operator::Eq | Operator::Ne |
            Operator::Lt | Operator::Le |
            Operator::Gt | Operator::Ge => DataType::Boolean,
        })
    }

    fn fff() {
        Ok(match self.op {
            Operator::Eq | Operator::Ne | Operator::Lt | Operator::Le | Operator::Gt | Operator::Ge | Operator::And => DataType::Boolean,
        })
    }
}
