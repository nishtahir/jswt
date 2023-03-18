use std::borrow::Cow;

use jswt_symbols::SimpleSymbolTable as SymbolTable;
use jswt_wast::ValueType;

#[derive(Debug, PartialEq)]
pub enum WastSymbol {
    Param(usize, ValueType),
    Local(ValueType),
    Global(ValueType),
}

impl WastSymbol {
    /// Returns `true` if the wast symbol is [`Param`].
    ///
    /// [`Param`]: WastSymbol::Param
    pub fn is_param(&self) -> bool {
        matches!(self, Self::Param(..))
    }
}

#[derive(Debug)]
pub struct WastSymbolTable {
    inner: SymbolTable<Cow<'static, str>, WastSymbol>,
    locals: usize,
}

impl WastSymbolTable {
    pub fn new() -> Self {
        WastSymbolTable {
            inner: SymbolTable::new(vec![]),
            locals: 0,
        }
    }

    pub fn push_scope(&mut self) {
        self.inner.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.inner.pop_scope();
    }

    pub fn depth(&self) -> usize {
        self.inner.depth()
    }

    pub fn define<T: Into<Cow<'static, str>>>(&mut self, name: T, symbol: WastSymbol) {
        self.inner.define(name.into(), symbol);
    }

    pub fn define_synthetic_local(&mut self, ty: ValueType) -> Cow<'static, str> {
        self.locals += 1;
        // Synthetic locals should never collide with params
        let symbols = self.symbols_in_current_scope();
        let params = symbols.iter().filter(|(_, sym)| sym.is_param()).count();
        let label = Cow::from(format!("{}", params + self.locals));
        self.inner.define(label.clone(), WastSymbol::Local(ty));
        label
    }

    pub fn lookup<T: Into<Cow<'static, str>>>(&self, name: T) -> Option<&WastSymbol> {
        self.inner.lookup(&name.into())
    }

    pub fn lookup_current<T: Into<Cow<'static, str>>>(&self, name: T) -> Option<&WastSymbol> {
        self.inner.lookup_current(&name.into())
    }

    pub fn lookup_global<T: Into<Cow<'static, str>>>(&self, name: T) -> Option<&WastSymbol> {
        self.inner.lookup_global(&name.into())
    }

    pub fn symbols_in_current_scope(&self) -> Vec<(&Cow<'static, str>, &WastSymbol)> {
        self.inner.symbols_in_current_scope()
    }
}
