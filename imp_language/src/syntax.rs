use crate::{Expression, Scalar};
use log::*;
use regex::Regex;
use std::cell::Cell;
use std::str::FromStr;

struct Parser<'a> {
    source: &'a str,
    position: Cell<usize>,

    whitespace: Regex,
    name: Regex,
    string: Regex,
    number: Regex,
}

#[derive(Debug)]
pub struct Error {
    position: usize,
    expected: &'static str,
}

#[derive(Debug)]
enum BinaryOperator {
    Plus,
    Product,
    Union,
    Intersect,
    Equal,
}

impl BinaryOperator {
    fn precedence(&self) -> usize {
        use BinaryOperator::*;
        match self {
            Plus => 0,
            Product => 1,
            Union => 2,
            Intersect => 3,
            Equal => 4,
        }
    }
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Parser {
            source,
            position: Cell::new(0),

            whitespace: Regex::new(r"^\s+").unwrap(),
            name: Regex::new(r"^[[:alpha:]]([[:alnum:]]|[-_\?!])*").unwrap(),
            string: Regex::new(r#"^"(\\.|[^"])*""#).unwrap(),
            number: Regex::new(r"^[0-9]+").unwrap(),
        }
    }

    fn expected(&self, expected: &'static str) -> Error {
        Error {
            position: self.position.get(),
            expected,
        }
    }

    fn whitespace(&self) {
        if let Some(found) = self.whitespace.find(&self.source[self.position.get()..]) {
            self.position.set(self.position.get() + found.end());
        }
    }

    fn string(&self, pattern: &str) -> bool {
        if self.source[self.position.get()..].starts_with(pattern) {
            self.position.set(self.position.get() + pattern.len());
            self.whitespace();
            true
        } else {
            false
        }
    }

    fn regex(&self, pattern: &Regex) -> Option<&str> {
        pattern
            .find(&self.source[self.position.get()..])
            .map(|found| {
                self.position.set(self.position.get() + found.end());
                self.whitespace();
                found.as_str()
            })
    }

    fn try_binary_operator(&self, precedence: usize) -> Option<BinaryOperator> {
        let position = self.position.get();
        let operator = {
            if self.string("+") {
                BinaryOperator::Plus
            } else if self.string("x") {
                BinaryOperator::Product
            } else if self.string("|") {
                BinaryOperator::Union
            } else if self.string("&") {
                BinaryOperator::Intersect
            } else if self.string("=") {
                BinaryOperator::Equal
            } else {
                return None;
            }
        };
        if operator.precedence() < precedence {
            Some(operator)
        } else {
            self.position.set(position);
            return None;
        }
    }

    fn name(&self) -> Option<&str> {
        let position = self.position.get();
        if let Some(name) = self.regex(&self.name) {
            if name != "x"
                && name != "let"
                && name != "in"
                && name != "if"
                && name != "then"
                && name != "else"
                && name != "nothing"
                && name != "something"
            {
                return Some(name);
            }
        }
        self.position.set(position);
        None
    }

    fn try_abstract(&self) -> Result<Option<Expression>, Error> {
        let position = self.position.get();
        let mut names = vec![];
        while let Some(name) = self.name() {
            names.push(name);
        }
        if names.len() > 0 && self.string("->") {
            let mut expression = self.expression_outer()?;
            for name in names.into_iter().rev() {
                expression = Expression::Abstract(name.to_owned(), box expression);
            }
            Ok(Some(expression))
        } else {
            self.position.set(position);
            Ok(None)
        }
    }

    fn try_expression_inner(&self) -> Result<Option<Expression>, Error> {
        if self.string("(") {
            let expression = self.expression_outer()?;
            if !self.string(")") {
                return Err(self.expected(")"));
            }
            Ok(Some(expression))
        } else if self.string("{") {
            let expression = self.expression_outer()?;
            if !self.string("}") {
                return Err(self.expected("}"));
            }
            Ok(Some(Expression::Seal(box expression)))
        } else if self.string("?") {
            let expression = self.expression_inner()?;
            Ok(Some(Expression::Solve(box expression)))
        } else if self.string("!") {
            let expression = self.expression_inner()?;
            Ok(Some(Expression::Negate(box expression)))
        } else if self.string("$") {
            let expression = self.expression_inner()?;
            Ok(Some(Expression::Unseal(box expression)))
        } else if let Some(name) = self.name() {
            Ok(Some(Expression::Name(name.to_owned())))
        } else if let Some(string) = self.regex(&self.string) {
            Ok(Some(Expression::Scalar(Scalar::String(
                unescape::unescape(&string[1..(string.len() - 1)]).unwrap(),
            ))))
        } else if let Some(number) = self.regex(&self.number) {
            Ok(Some(Expression::Scalar(Scalar::Number(
                i64::from_str(number).unwrap(),
            ))))
        } else if self.string("something") {
            Ok(Some(Expression::Something))
        } else if self.string("nothing") {
            Ok(Some(Expression::Nothing))
        } else {
            Ok(None)
        }
    }

    fn expression_inner(&self) -> Result<Expression, Error> {
        match self.try_expression_inner()? {
            Some(expression) => Ok(expression),
            None => Err(self.expected("$expr_inner")),
        }
    }

    fn expression_apply(&self) -> Result<Expression, Error> {
        let mut expression = self.expression_inner()?;
        loop {
            if self.string("[") {
                let right = self.expression_outer()?;
                if !self.string("]") {
                    return Err(self.expected("]"));
                }
                expression = Expression::apply(
                    "permute",
                    vec![
                        Expression::Seal(box right),
                        Expression::Seal(box expression),
                    ],
                );
            } else if let Some(right) = self.try_expression_inner()? {
                expression = Expression::Apply(box expression, box right);
            } else {
                break;
            }
        }
        Ok(expression)
    }

    fn expression_binary(&self, precedence: usize) -> Result<Expression, Error> {
        let mut expression = self.expression_apply()?;
        while let Some(operator) = self.try_binary_operator(precedence) {
            let right = self.expression_binary(operator.precedence())?;
            expression = match operator {
                BinaryOperator::Plus => Expression::apply("+", vec![expression, right]),
                BinaryOperator::Product => Expression::Product(box expression, box right),
                BinaryOperator::Union => Expression::Union(box expression, box right),
                BinaryOperator::Intersect => Expression::Intersect(box expression, box right),
                BinaryOperator::Equal => Expression::Equal(box expression, box right),
            };
        }
        Ok(expression)
    }

    // TODO prevent self.string("let") from consuming strings starting with let
    fn expression_outer(&self) -> Result<Expression, Error> {
        if let Some(expression) = self.try_abstract()? {
            Ok(expression)
        } else if self.string("let") {
            let name = match self.name() {
                Some(name) => name,
                None => return Err(self.expected("let $name")),
            };
            if !self.string("=") {
                return Err(self.expected("let $name ="));
            }
            let value = self.expression_outer()?;
            if !self.string("in") {
                return Err(self.expected("let $name = in"));
            }
            let body = self.expression_outer()?;
            Ok(Expression::Let(name.to_owned(), box value, box body))
        } else if self.string("if") {
            let cond = self.expression_outer()?;
            if !self.string("then") {
                return Err(self.expected("if $expr then"));
            }
            let then = self.expression_outer()?;
            if !self.string("else") {
                return Err(self.expected("if $expr then $expr else"));
            }
            let els = self.expression_outer()?;
            Ok(Expression::If(box cond, box then, box els))
        } else {
            self.expression_binary(usize::max_value())
        }
    }
}

pub fn parse(source: &str) -> Result<Expression, Error> {
    let parser = Parser::new(source);
    parser.whitespace();
    let expression = parser.expression_outer()?;
    if parser.position.get() == parser.source.len() {
        Ok(expression)
    } else {
        Err(parser.expected("more"))
    }
}
