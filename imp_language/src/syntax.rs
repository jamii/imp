use crate::{Expression, Scalar};
use std::cell::Cell;
use std::str::FromStr;

struct Parser<'a> {
    source: &'a str,
    position: Cell<usize>,
}

#[derive(Debug)]
pub enum Error {
    Lex {
        position: usize,
    },
    Syntax {
        position: usize,
        expected: Vec<Token>,
        got: Token,
    },
    Other {
        position: usize,
        message: String,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token {
    OpenGroup,
    CloseGroup,
    OpenPermute,
    ClosePermute,
    OpenSeal,
    CloseSeal,
    Unseal,
    Negate,
    Solve,
    Intersect,
    Union,
    Product,
    Plus,
    Minus,
    LessThan,
    Equal,
    Abstract,
    String,
    Number,
    Name,
    Let,
    In,
    If,
    Then,
    Else,
    When,
    Reduce,
    Something,
    Nothing,
    Whitespace,
    EOF,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Parser {
            source,
            position: Cell::new(0),
        }
    }

    fn token(&self) -> Result<(Token, &str), Error> {
        use Token::*;
        let mut chars = self.source[self.position.get()..].chars();
        let mut len = 0;
        let token = match chars.next() {
            None => EOF,
            Some(char) => {
                len += char.len_utf8();
                match char {
                    '(' => OpenGroup,
                    ')' => CloseGroup,
                    '[' => OpenPermute,
                    ']' => ClosePermute,
                    '{' => OpenSeal,
                    '}' => CloseSeal,
                    '$' => Unseal,
                    '!' => Negate,
                    '?' => Solve,
                    '&' => Intersect,
                    '|' => Union,
                    'x' => Product,
                    '+' => Plus,
                    '=' => Equal,
                    '-' => match chars.next() {
                        Some('>') => {
                            len += 1;
                            Abstract
                        }
                        _ => Minus,
                    },
                    '<' => LessThan,
                    '"' => {
                        let mut escape = false;
                        loop {
                            match chars.next() {
                                None => {
                                    return Err(Error::Lex {
                                        position: self.position.get(),
                                    });
                                }
                                Some(char) => {
                                    len += char.len_utf8();
                                    match char {
                                        '\\' => {
                                            escape = true;
                                        }
                                        '"' => {
                                            if !escape {
                                                break;
                                            }
                                            escape = false;
                                        }
                                        _ => {
                                            if escape {
                                                return Err(Error::Lex {
                                                    position: self.position.get(),
                                                });
                                            }
                                            escape = false;
                                        }
                                    }
                                }
                            }
                        }
                        String
                    }
                    _ => {
                        if char.is_digit(10) {
                            loop {
                                match chars.next() {
                                    Some(char) if char.is_digit(10) => len += char.len_utf8(),
                                    _ => break,
                                }
                            }
                            Number
                        } else if char.is_alphabetic() {
                            loop {
                                match chars.next() {
                                    Some(char) if char.is_alphanumeric() || char == '_' => {
                                        len += char.len_utf8()
                                    }
                                    _ => break,
                                }
                            }
                            match &self.source[self.position.get()..self.position.get() + len] {
                                "let" => Let,
                                "in" => In,
                                "if" => If,
                                "then" => Then,
                                "else" => Else,
                                "when" => When,
                                "reduce" => Reduce,
                                "something" | "some" => Something,
                                "nothing" | "none" => Nothing,
                                _ => Name,
                            }
                        } else if char.is_ascii_whitespace() {
                            loop {
                                match chars.next() {
                                    Some(char) if char.is_whitespace() => len += char.len_utf8(),
                                    _ => break,
                                }
                            }
                            Whitespace
                        } else {
                            return Err(Error::Lex {
                                position: self.position.get(),
                            });
                        }
                    }
                }
            }
        };

        let token_start = self.position.get();
        let token_end = token_start + len;
        self.position.set(token_end);
        if let Whitespace = token {
            self.token()
        } else {
            Ok((token, &self.source[token_start..token_end]))
        }
    }

    fn expected(&self, expected: Vec<Token>, got: Token) -> Error {
        Error::Syntax {
            position: self.position.get(),
            expected,
            got,
        }
    }

    fn expect(&self, expected_token: Token) -> Result<&str, Error> {
        let (token, token_str) = self.token()?;
        if token == expected_token {
            Ok(token_str)
        } else {
            Err(self.expected(vec![expected_token], token))
        }
    }

    fn try_abstract(&self) -> Result<Option<Expression>, Error> {
        let position = self.position.get();
        let mut names = vec![];
        loop {
            match self.token()? {
                (Token::Name, name) => names.push(name.to_owned()),
                (Token::Abstract, _) if names.len() > 0 => {
                    let mut expression = self.expression_outer()?;
                    for name in names.into_iter().rev() {
                        expression = Expression::Abstract(name.to_owned(), box expression);
                    }
                    return Ok(Some(expression));
                }
                _ => {
                    self.position.set(position);
                    return Ok(None);
                }
            }
        }
    }

    fn try_expression_inner(&self) -> Result<Option<Expression>, Error> {
        use Token::*;
        let position = self.position.get();
        let (token, token_str) = self.token()?;
        Ok(Some(match token {
            OpenGroup => {
                let expression = self.expression_outer()?;
                self.expect(CloseGroup)?;
                expression
            }
            OpenSeal => {
                let expression = self.expression_outer()?;
                self.expect(CloseSeal)?;
                Expression::Seal(box expression)
            }
            Unseal => {
                let expression = self.expression_inner()?;
                Expression::Unseal(box expression)
            }
            Negate => {
                let expression = self.expression_inner()?;
                Expression::Negate(box expression)
            }
            Solve => {
                let expression = self.expression_inner()?;
                Expression::Solve(box expression)
            }
            Name => Expression::Name(token_str.to_owned()),
            String => Expression::Scalar(Scalar::String(
                unescape::unescape(&token_str[1..(token_str.len() - 1)]).unwrap(),
            )),
            Number => match i64::from_str(token_str) {
                Ok(number) => Expression::Scalar(Scalar::Number(number)),
                Err(error) => {
                    return Err(Error::Other {
                        position,
                        message: format!("{}", error),
                    });
                }
            },
            Something => Expression::Something,
            Nothing => Expression::Nothing,
            _ => {
                self.position.set(position);
                return Ok(None);
            }
        }))
    }

    fn expression_inner(&self) -> Result<Expression, Error> {
        match self.try_expression_inner()? {
            Some(expression) => Ok(expression),
            None => Err(Error::Other {
                position: self.position.get(),
                message: "Expected start of expression".to_owned(),
            }),
        }
    }

    fn expression_apply(&self) -> Result<Expression, Error> {
        let mut expression = self.expression_inner()?;
        loop {
            let position = self.position.get();
            if let Token::OpenPermute = self.token()?.0 {
                let right = self.expression_outer()?;
                self.expect(Token::ClosePermute)?;
                expression = Expression::apply(
                    "permute",
                    vec![
                        Expression::Seal(box right),
                        Expression::Seal(box expression),
                    ],
                );
            } else {
                self.position.set(position);
                if let Some(right) = self.try_expression_inner()? {
                    expression = Expression::Apply(box expression, box right);
                } else {
                    break;
                }
            }
        }
        Ok(expression)
    }

    fn expression_binary(&self, precedence: usize) -> Result<Expression, Error> {
        use Token::*;
        let mut expression = self.expression_apply()?;
        loop {
            let position = self.position.get();
            let token = self.token()?.0;
            let token_precedence = match token {
                Minus | Plus => 0,
                Product => 1,
                Union => 2,
                Intersect => 3,
                Equal | LessThan => 4,
                _ => usize::max_value(), // ie bail
            };
            if token_precedence >= precedence {
                self.position.set(position);
                break;
            }
            let right = self.expression_binary(token_precedence)?;
            expression = match token {
                Plus => Expression::apply("add", vec![expression, right]),
                Minus => Expression::apply("subtract", vec![expression, right]),
                Product => Expression::Product(box expression, box right),
                Union => Expression::Union(box expression, box right),
                Intersect => Expression::Intersect(box expression, box right),
                Equal => Expression::Equal(box expression, box right),
                LessThan => Expression::apply("less_than", vec![expression, right]),
                _ => unreachable!(),
            };
        }
        Ok(expression)
    }

    fn expression_outer(&self) -> Result<Expression, Error> {
        use Token::*;
        if let Some(expression) = self.try_abstract()? {
            Ok(expression)
        } else {
            let position = self.position.get();
            match self.token()?.0 {
                Let => {
                    let name = self.expect(Name)?;
                    self.expect(Equal)?;
                    let value = self.expression_outer()?;
                    self.expect(In)?;
                    let body = self.expression_outer()?;
                    Ok(Expression::Let(name.to_owned(), box value, box body))
                }
                If => {
                    let cond = self.expression_outer()?;
                    self.expect(Then)?;
                    let then = self.expression_outer()?;
                    self.expect(Else)?;
                    let els = self.expression_outer()?;
                    Ok(Expression::If(box cond, box then, box els))
                }
                When => {
                    let cond = self.expression_outer()?;
                    self.expect(Then)?;
                    let then = self.expression_outer()?;
                    Ok(Expression::If(box cond, box then, box Expression::Nothing))
                }
                Reduce => {
                    let init = self.expression_inner()?;
                    let vals = self.expression_inner()?;
                    let fun = self.expression_inner()?;
                    Ok(Expression::Reduce(box init, box vals, box fun))
                }
                _ => {
                    self.position.set(position);
                    self.expression_binary(usize::max_value())
                }
            }
        }
    }
}

pub fn parse(source: &str) -> Result<Expression, Error> {
    let parser = Parser::new(source);
    let expression = parser.expression_outer()?;
    parser.expect(Token::EOF)?;
    Ok(expression)
}
