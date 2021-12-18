use jswt_common::Type;

pub fn check_assignment(lhs: &Type, rhs: &Type) -> Type {
    if lhs.is_unknown() || lhs == rhs {
        return rhs.clone();
    }
    Type::Unknown
}

pub fn check_comparison(lhs: &Type, rhs: &Type) -> Type {
    if lhs != rhs {
        return Type::Unknown;
    }
    Type::boolean()
}

pub fn check_arithmetic(lhs: &Type, rhs: &Type) -> Type {
    if lhs != rhs {
        return Type::Unknown;
    }
    lhs.clone()
}

pub fn check_bitwise(lhs: &Type, rhs: &Type) -> Type {
    if lhs != rhs {
        return Type::Unknown;
    }
    lhs.clone()
}