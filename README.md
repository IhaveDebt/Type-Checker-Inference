use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

// === Lexer ===
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(i32),
    Plus,
    Star,
    LParen,
    RParen,
    Let,
    Equals,
    Ident(String),
    In,
    EOF,
}

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }
    
    fn next_token(&mut self) -> Token {
        while let Some(&c) = self.input.peek() {
            return match c {
                '0'..='9' => self.lex_number(),
                '+' => { self.input.next(); Token::Plus }
                '*' => { self.input.next(); Token::Star }
                '(' => { self.input.next(); Token::LParen }
                ')' => { self.input.next(); Token::RParen }
                '=' => { self.input.next(); Token::Equals }
                'a'..='z' | 'A'..='Z' | '_' => self.lex_ident_or_keyword(),
                ' ' | '\n' | '\t' => { self.input.next(); continue; }
                _ => panic!("Unknown character: {}", c),
            };
        }
        Token::EOF
    }
    
    fn lex_number(&mut self) -> Token {
        let mut num = 0;
        while let Some(&c) = self.input.peek() {
            if c.is_digit(10) {
                num = num * 10 + c.to_digit(10).unwrap() as i32;
                self.input.next();
            } else {
                break;
            }
        }
        Token::Number(num)
    }
    
    fn lex_ident_or_keyword(&mut self) -> Token {
        let mut ident = String::new();
        while let Some(&c) = self.input.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.input.next();
            } else {
                break;
            }
        }
        match ident.as_str() {
            "let" => Token::Let,
            "in" => Token::In,
            _ => Token::Ident(ident),
        }
    }
}

// === AST ===
#[derive(Debug, Clone)]
enum Expr {
    Number(i32),
    Var(String),
    BinaryOp {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Let {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
}

// === Parser ===
// Grammar:
// expr    = let_expr | add_expr
// let_expr= "let" ident "=" expr "in" expr
// add_expr= mul_expr ( "+" mul_expr)*
// mul_expr= factor ( "*" factor)*
// factor  = number | ident | "(" expr ")"

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let first_token = lexer.next_token();
        Parser {
            lexer,
            current_token: first_token,
        }
    }
    
    fn eat(&mut self, expected: Token) {
        if std::mem::discriminant(&self.current_token) == std::mem::discriminant(&expected) {
            self.current_token = self.lexer.next_token();
        } else {
            panic!("Expected {:?}, got {:?}", expected, self.current_token);
        }
    }
    
    fn parse(&mut self) -> Expr {
        self.expr()
    }
    
    fn expr(&mut self) -> Expr {
        match &self.current_token {
            Token::Let => self.let_expr(),
            _ => self.add_expr(),
        }
    }
    
    fn let_expr(&mut self) -> Expr {
        self.eat(Token::Let);
        let name = if let Token::Ident(n) = &self.current_token {
            n.clone()
        } else {
            panic!("Expected identifier after let");
        };
        self.eat(Token::Ident(name.clone()));
        self.eat(Token::Equals);
        let value = self.expr();
        self.eat(Token::In);
        let body = self.expr();
        
        Expr::Let {
            name,
            value: Box::new(value),
            body: Box::new(body),
        }
    }
    
    fn add_expr(&mut self) -> Expr {
        let mut node = self.mul_expr();
        
        while self.current_token == Token::Plus {
            let op = self.current_token.clone();
            self.eat(Token::Plus);
            let right = self.mul_expr();
            node = Expr::BinaryOp {
                left: Box::new(node),
                op,
                right: Box::new(right),
            };
        }
        
        node
    }
    
    fn mul_expr(&mut self) -> Expr {
        let mut node = self.factor();
        
        while self.current_token == Token::Star {
            let op = self.current_token.clone();
            self.eat(Token::Star);
            let right = self.factor();
            node = Expr::BinaryOp {
                left: Box::new(node),
                op,
                right: Box::new(right),
            };
        }
        
        node
    }
    
    fn factor(&mut self) -> Expr {
        match &self.current_token {
            Token::Number(n) => {
                let val = *n;
                self.eat(Token::Number(*n));
                Expr::Number(val)
            }
            Token::Ident(name) => {
                let var_name = name.clone();
                self.eat(Token::Ident(var_name.clone()));
                Expr::Var(var_name)
            }
            Token::LParen => {
                self.eat(Token::LParen);
                let node = self.expr();
                self.eat(Token::RParen);
                node
            }
            _ => panic!("Unexpected token in factor: {:?}", self.current_token),
        }
    }
}

// === Types ===
#[derive(Debug, PartialEq, Clone)]
enum Type {
    Int,
}

// === Type Checker / Inference ===
type TypeEnv = HashMap<String, Type>;

fn type_check(expr: &Expr, env: &mut TypeEnv) -> Result<Type, String> {
    match expr {
        Expr::Number(_) => Ok(Type::Int),
        Expr::Var(name) => {
            env.get(name)
                .cloned()
                .ok_or_else(|| format!("Undefined variable '{}'", name))
        }
        Expr::BinaryOp { left, op, right } => {
            let left_type = type_check(left, env)?;
            let right_type = type_check(right, env)?;
            
            if left_type != Type::Int || right_type != Type::Int {
                return Err("Operands must be integers".to_string());
            }
            
            match op {
                Token::Plus | Token::Star => Ok(Type::Int),
                _ => Err("Unsupported binary operator in type checker".to_string()),
            }
        }
        Expr::Let { name, value, body } => {
            let val_type = type_check(value, env)?;
            env.insert(name.clone(), val_type);
            let body_type = type_check(body, env)?;
            env.remove(name);
            Ok(body_type)
        }
    }
}

// === Main ===
fn main() {
    let inputs = [
        "2 + 3 * (4 + 5)",
        "let x = 10 in x + 20",
        "let x = 2 in let y = x * 3 in y + x",
        "let x = 1 in let y = x + true in y", // <-- this will fail at type checking
    ];
    
    for input in &inputs {
        println!("Input: {}", input);
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        println!("AST: {:?}", ast);
        
        let mut env = HashMap::new();
        match type_check(&ast, &mut env) {
            Ok(t) => println!("Type check succeeded: {:?}", t),
            Err(e) => println!("Type check error: {}", e),
        }
        
        println!("---");
    }
}
