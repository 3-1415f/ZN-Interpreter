use std::{fmt, io::{self,Write}};
use std::collections::HashMap;

#[derive(Clone,Debug,PartialEq)]
enum Atom{
  I64(i64),
  F64(f64),
  Str(String),
  Id(String),
  Lst(Vec<Atom>),
  Fn(fn(Vec<Atom>) -> Atom),
  Null,
}

impl fmt::Display for Atom{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Atom::I64(i) => write!(f, "{}", i),
      Atom::F64(d) => write!(f, "{}f", d),
      Atom::Str(s) => write!(f, "{:?}", s),
      Atom::Id(id) => write!(f, "${}", id),
      Atom::Lst(lst) => {
        write!(f, "[")?;
        for (i, item) in lst.iter().enumerate() {
          if i > 0 {
            write!(f, ",")?;
          }
          write!(f, "{}", item)?;
        }
        write!(f, "]")
      }
      Atom::Null => write!(f, "null"),
      Atom::Fn(_) => write!(f, "<fn>"),
    }
  }
}

#[derive(Clone)]
enum Op{
  Set,Add,Sub,Mul,Div,Mod,Pow,Lt,Gt,Le,Ge,Eq,Ne,In,Ni,And,Or,Xor,Not,BAnd,BOr,BXor,FnC,Lst,Ind,Loc,If,While,Blk,
}

#[derive(Debug)]
enum Token{
  Atom(Atom),
  Op(&'static str),
  Eof,
}

#[derive(Clone)]
enum S{
  Atom(Atom),
  Cons(Op, Vec<S>),
}

impl fmt::Display for S {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      S::Atom(atom) => write!(f, "{}", atom),
      S::Cons(op, args) => {
        write!(f, "{}(", match op {
          Op::Set => "Set",
          Op::Add => "Add",
          Op::Sub => "Sub",
          Op::Mul => "Mul",
          Op::Div => "Div",
          Op::Mod => "Mod",
          Op::Pow => "Pow",
          Op::Lt => "Lt",
          Op::Gt => "Gt",
          Op::Le => "Le",
          Op::Ge => "Ge",
          Op::Eq => "Eq",
          Op::Ne => "Ne",
          Op::In => "In",
          Op::Ni => "Ni",
          Op::And => "And",
          Op::Or => "Or",
          Op::Xor => "Xor",
          Op::Not => "Not",
          Op::BAnd => "BAnd",
          Op::BOr => "BOr",
          Op::BXor => "BXor",
          Op::FnC => "FnC",
          Op::Lst => "Lst",
          Op::Ind => "Ind",
          Op::Loc => "Loc",
          Op::If => "If",
          Op::While => "While",
          Op::Blk => "Blk",
        })?;
        for (i, arg) in args.iter().enumerate() {
          if i > 0 {
            write!(f, ",")?;
          }
          write!(f, "{}", arg)?;
        }
        write!(f, ")")
      }
    }
  }
}

enum Type{
  Brt,FnC,Lst,Ind,Blk,Null,
}

fn f_out(x:Vec<Atom>) -> Atom{
  for i in x{
    println!("{}",i);
  }
  Atom::Null
}

mod parser{
  use {Atom, Op, Token, S, Type};
  pub fn lex(input: &str)->Result<Vec<Token>, String>{
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
      if c.is_ascii_whitespace() {
        continue;
      }
      match c {
        '"' | '\'' => {
          let quote = c;
          let mut s = String::new();
          loop {
            match chars.next() {
              Some('\\') => {
                let escaped = match chars.next() {
                  Some('n') => '\n',
                  Some('t') => '\t',
                  Some('r') => '\r',
                  Some('\\') => '\\',
                  Some('\"') => '\"',
                  Some('\'') => '\'',
                  None => return Err(format!("Err:Escape:{}", c)),
                  Some(c) => return Err(format!("Err:Escape:{}", c)),
                };
                s.push(escaped);
              }
              Some(next_c) if next_c == quote => break,
              Some(next_c) => s.push(next_c),
              None => return Err(format!("Err:Quote:{}", s)),
            }
          }
          tokens.push(Token::Atom(Atom::Str(s)));
        }
        '0'..='9' => {
          let mut num = String::new();
          let mut radix = 10;
          if c == '0' {
            match chars.peek() {
              Some('x') | Some('X') => {
                chars.next();
                radix = 16;
              }
              Some('o') | Some('O') => {
                chars.next();
                radix = 8;
              }
              Some('b') | Some('B') => {
                chars.next();
                radix = 2;
              }
              _ => {}
            }
          }
          num.push(c);
          while let Some(&c) = chars.peek() {
            if !(c.is_ascii_alphanumeric() || c=='_' || c=='.') {
              break;
            }
            chars.next();
            if c!='_' {
              num.push(c);
            }
          }
          let parts: Vec<&str> = num.split('.').collect();
          if parts.len() == 1 {
            let integer = i64::from_str_radix(&num, radix)
              .map_err(|_| format!("Err:Num:{}", num))?;
            tokens.push(Token::Atom(Atom::I64(integer)));
          } else if parts.len() == 2 {
            let integer = i64::from_str_radix(parts[0], radix)
              .map_err(|_| format!("Err:Num:{}", num))?;
            let fraction = i64::from_str_radix(parts[1], radix)
              .map_err(|_| format!("Err:Num:{}", num))?;
            tokens.push(Token::Atom(Atom::F64((integer as f64) + (fraction as f64) / (radix as f64).powi(parts[1].len() as i32))));
          } else {
            return Err(format!("Err:Num:{}", num));
          }
        }
        'a'..='z' | 'A'..='Z' | '_' => {
          let mut ident = String::new();
          ident.push(c);
          while let Some(&next_c) = chars.peek() {
            if !(next_c.is_ascii_alphanumeric() || next_c == '_') {
              break;
            }
            ident.push(chars.next().unwrap());
          }
          match ident.as_str() {
            "true" => tokens.push(Token::Atom(Atom::I64(1))),
            "false" => tokens.push(Token::Atom(Atom::I64(0))),
            "null" => tokens.push(Token::Atom(Atom::Null)),
            "loc" => tokens.push(Token::Op("loc")),
            "if" => tokens.push(Token::Op("if")),
            "else" => tokens.push(Token::Op("else")),
            "while" => tokens.push(Token::Op("while")),
            _ => tokens.push(Token::Atom(Atom::Id(ident))),
          }
        }
        '/' => match chars.peek() {
          Some('/') => while chars.next_if(|&c| c != '\n').is_some() {},
          Some('*') => {
            chars.next();
            let mut prev = '\0';
            while let Some(c) = chars.next() {
              if prev == '*' && c == '/' { break; }
              prev = c;
            }
          }
          _ => tokens.push(Token::Op("/")),
        }
        '(' | ')' | '[' | ']' | '{' | '}' | '+' | '-' | '*' | '%' | '~' | '!' | '=' | '<' | '>' | '&' | '|' | '^' | '.' | ',' | ';' => {
          let op = match (c, chars.peek()) {
            ('=', Some('=')) => {chars.next();"=="},
            ('!', Some('=')) => {chars.next();"!="},
            ('<', Some('=')) => {chars.next();"<="},
            ('>', Some('=')) => {chars.next();">="},
            ('&', Some('&')) => {chars.next();"&&"},
            ('|', Some('|')) => {chars.next();"||"},
            ('^', Some('^')) => {chars.next();"^^"},
            ('*', Some('*')) => {chars.next();"**"},
            ('!', Some('~')) => {chars.next();"!~"},
            _ => match c {
              '(' => "(",
              ')' => ")",
              '[' => "[",
              ']' => "]",
              '{' => "{",
              '}' => "}",
              '+' => "+",
              '-' => "-",
              '*' => "*",
              '%' => "%",
              '~' => "~",
              '!' => "!",
              '=' => "=",
              '<' => "<",
              '>' => ">",
              '&' => "&",
              '|' => "|",
              '^' => "^",
              '.' => ".",
              ',' => ",",
              ';' => ";",
              _ => return Err("Err:NotReach".to_string()),
            }
          };
          tokens.push(Token::Op(op));
        }
        _ => return Err(format!("Err:Char:{}", c))
      }
    }
    tokens.reverse();
    Ok(tokens)
  }

  fn next(tokens:&mut Vec<Token>)->Token{
    tokens.pop().unwrap_or(Token::Eof)
  }

  fn peek(tokens:& Vec<Token>)->&Token{
    tokens.last().unwrap_or(&Token::Eof)
  }

  fn prefix(op:&str)->Option<((), u8)>{
    Some(match op {
      "+" | "-" | "!" => ((), 13),
      "loc" => ((), 15),
      _ => return None,
    })
  }

  fn postfix(op:&str)->Option<(u8, ())>{
    Some(match op {
      "(" | "[" | "{" => (16, ()),
      _ => return None,
    })
  }

  fn infix(op:&str)->Option<(u8, u8)>{
    Some(match op {
      "=" => (14,1),
      "." => (16,17),
      "+" | "-" => (8,9),
      "*" | "/" | "%" => (10,11),
      "**" => (12,11),
      "<" | ">" | "<=" | ">=" | "==" | "!=" =>(4,5),
      "~" | "!~" => (4,13),
      "&" | "|" | "^" => (6,7),
      "&&" | "||" | "^^" => (2,3),
      _ => return None
    })
  }

  fn expr_bp(tokens:&mut Vec<Token>, min_bp:u8, etype:Type)->Result<S, String>{
    let mut lhs = match next(tokens) {
      Token::Atom(it) => S::Atom(it),
      Token::Op("(") => expr_bp(tokens,0, Type::Brt)?,
      Token::Op("[") => {
        let mut vec_ = Vec::new();
        loop {
          match peek(tokens) {
            Token::Op("]") => {
              next(tokens);
              break;
            },
            Token::Eof => return Err("Err:Eof".to_string()),
            _ => vec_.push(expr_bp(tokens,0, Type::Lst)?),
          }
        }
        S::Cons(Op::Lst, vec_)
      },
      Token::Op("{") => {
        let mut vec_ = Vec::new();
        loop {
          match peek(tokens) {
            Token::Op("}") => {
              next(tokens);
              break;
            },
            Token::Eof => return Err("Err:Eof".to_string()),
            _ => vec_.push(expr_bp(tokens,0, Type::Blk)?),
          }
        }
        S::Cons(Op::Blk, vec_)
      },
      Token::Op("loc") => {
        let Token::Atom(Atom::Id(id)) = next(tokens) else {
          return Err("Err:LocId".to_string());
        };
        let Token::Op("=") = next(tokens) else {
          return Err("Err:LocSet".to_string());
        };
        let rhs = expr_bp(tokens, 0, Type::Null)?;
        S::Cons(Op::Set, vec![S::Cons(Op::Loc, vec![S::Atom(Atom::Id(id))]), rhs])
      }
      Token::Op("if") => {
        let Token::Op("(") = next(tokens) else {
          return Err("Err:IfCond".to_string());
        };
        let cond = expr_bp(tokens, 0, Type::Brt)?;
        let then = expr_bp(tokens, 0, Type::Null)?;
        if let Token::Op("else") = peek(tokens) {
          next(tokens);
          S::Cons(Op::If, vec![cond, then, expr_bp(tokens, 0, Type::Null)?])
        } else {
          S::Cons(Op::If, vec![cond, then])
        }
      },
      Token::Op("while") => {
        let Token::Op("(") = next(tokens) else {
          return Err("Err:WhileCond".to_string());
        };
        let cond = expr_bp(tokens, 0, Type::Brt)?;
        let body = expr_bp(tokens, 0, Type::Null)?;
        S::Cons(Op::While, vec![cond, body])
      },
      Token::Op(op) => {
        let Some(((), r_bp)) = prefix(op) else {
          return Err(format!("Err:Op:{}", op));
        };
        let rhs=expr_bp(tokens, r_bp, Type::Null)?;
        match op {
          "+" => S::Cons(Op::Add, vec![S::Atom(Atom::I64(0)), rhs]),
          "-" => S::Cons(Op::Sub, vec![S::Atom(Atom::I64(0)), rhs]),
          "!" => S::Cons(Op::Not, vec![rhs]),
          _ => return Err("Err:NotReach".to_string()),
        }
      },
      Token::Eof => return Err("Err:Eof at 1".to_string()),
    };
    loop {
      let op = match peek(tokens) {
        Token::Op(op) => *op,
        Token::Eof => break,
        _ => return Err("Err:Atom".to_string()),
      };
      if let Some((l_bp, ())) = postfix(op) {
        if l_bp < min_bp {break;}
        next(tokens);
        lhs = match op {
          "(" => {
            let mut vec_=vec![lhs];
            loop{
              match peek(tokens) {
                Token::Op(")") => {
                  next(tokens);
                  break;
                },
                Token::Eof => return Err("Err:Eof".to_string()),
                _ => vec_.push(expr_bp(tokens,0,Type::FnC)?),
              }
            }
            S::Cons(Op::FnC, vec_)
          }
          "[" => {
            S::Cons(Op::Ind, vec![lhs, expr_bp(tokens,0, Type::Ind)?])
          },
          _ => return Err(format!("Err:Op:{}", op)),
        };
        continue;
      }

      if let Some((l_bp, r_bp)) = infix(op) {
        if l_bp < min_bp {break;}
        next(tokens);
        let rhs = expr_bp(tokens, r_bp, Type::Null)?;
        lhs = S::Cons(match op {
          "=" => match lhs {
            S::Atom(Atom::Id(_))|S::Cons(Op::Ind, _) => Op::Set,
            _ => return Err("Err:LhsOfSet".to_string()),
          },
          "+" => Op::Add,
          "-" => Op::Sub,
          "*" => Op::Mul,
          "/" => Op::Div,
          "%" => Op::Mod,
          "**" => Op::Pow,
          "<" => Op::Lt,
          ">" => Op::Gt,
          "<=" => Op::Le,
          ">=" => Op::Ge,
          "==" => Op::Eq,
          "!=" => Op::Ne,
          "~" => Op::In,
          "!~" => Op::Ni,
          "&" => Op::BAnd,
          "|" => Op::BOr,
          "^" => Op::BXor,
          "&&" => Op::And,
          "||" => Op::Or,
          "^^" => Op::Xor,
          "." => match rhs {
            S::Atom(Atom::Id(_))|S::Atom(Atom::I64(_)) => Op::Ind,
            _ => return Err("Err:RhsOfDot".to_string()),
          },
          _ => return Err(format!("Err:Op:{}", op)),
        }, vec![lhs, rhs]);
        continue;
      }
      match (op, &etype) {
        (")", Type::Brt) | ("]", Type::Ind) | (",", Type::FnC) | (",", Type::Lst) | (";", Type::Blk) => {
          next(tokens);
          break;
        },
        _ => break,
      }
    }
    Ok(lhs)
  }
  pub fn expr(tokens:&mut Vec<Token>)->Result<S, String>{
    let res = expr_bp(tokens, 0, Type::Null)?;
    match next(tokens) {
      Token::Eof => Ok(res),
      _ => Err("Err:Token:".to_string()),
    }
  }
}

struct Interpreter {
  vars: HashMap<String, Atom>,
}
impl Interpreter {
  fn new() -> Self {
    let mut vars = HashMap::new();
    vars.insert("out".to_string(), Atom::Fn(f_out));
    Interpreter {
      vars,
    }
  }
  pub fn eval(&mut self, expr: S) -> Result<Atom, String> {
    match expr {
      S::Atom(Atom::Id(id)) => {
        match self.vars.get(&id) {
          Some(val) => Ok(val.clone()),
          None => Err(format!("Err:Name:{}", id)),
        }
      }
      S::Atom(atom) => Ok(atom),
      S::Cons(op, args) => match op {
        Op::Add => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64(x + y)),
            (Atom::F64(x), Atom::F64(y)) => Ok(Atom::F64(x + y)),
            (Atom::Str(x), Atom::Str(y)) => Ok(Atom::Str(x + &y)),
            _ => Err("Err:AddType".to_string()),
          }
        }
        Op::Sub => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64(x - y)),
            (Atom::F64(x), Atom::F64(y)) => Ok(Atom::F64(x - y)),
            _ => Err("Err:SubType".to_string()),
          }
        }
        Op::Mul => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64(x * y)),
            (Atom::F64(x), Atom::F64(y)) => Ok(Atom::F64(x * y)),
            (Atom::Str(x), Atom::I64(y)) => Ok(Atom::Str(x.repeat(y as usize))),
            _ => Err("Err:MulType".to_string()),
          }
        }
        Op::Div => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64(x / y)),
            (Atom::F64(x), Atom::F64(y)) => Ok(Atom::F64(x / y)),
            _ => Err("Err:DivType".to_string()),
          }
        }
        Op::Mod => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64(x % y)),
            _ => Err("Err:ModType".to_string()),
          }
        }
        Op::Pow => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64(x.pow(y as u32))),
            (Atom::F64(x), Atom::F64(y)) => Ok(Atom::F64(x.powf(y))),
            _ => Err("Err:PowType".to_string()),
          }
        }
        Op::Eq => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          Ok(Atom::I64((a == b) as i64))
        }
        Op::Ne => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          Ok(Atom::I64((a != b) as i64))
        }
        Op::Lt => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64((x < y) as i64)),
            (Atom::F64(x), Atom::F64(y)) => Ok(Atom::I64((x < y) as i64)),
            _ => Err("Err:LtType".to_string()),
          }
        }
        Op::Gt => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64((x > y) as i64)),
            (Atom::F64(x), Atom::F64(y)) => Ok(Atom::I64((x > y) as i64)),
            _ => Err("Err:GtType".to_string()),
          }
        }
        Op::Le => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64((x <= y) as i64)),
            (Atom::F64(x), Atom::F64(y)) => Ok(Atom::I64((x <= y) as i64)),
            _ => Err("Err:LeType".to_string()),
          }
        }
        Op::Ge => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64((x >= y) as i64)),
            (Atom::F64(x), Atom::F64(y)) => Ok(Atom::I64((x >= y) as i64)),
            _ => Err("Err:GeType".to_string()),
          }
        }
        Op::Not => {
          let a = self.eval(args[0].clone())?;
          match a {
            Atom::I64(x) => Ok(Atom::I64((x == 0) as i64)),
            _ => Err("Err:NotType".to_string()),
          }
        }
        Op::And => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64(((x != 0) && (y != 0)) as i64)),
            _ => Err("Err:AndType".to_string()),
          }
        }
        Op::Or => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::I64(x), Atom::I64(y)) => Ok(Atom::I64(((x != 0) || (y != 0)) as i64)),
            _ => Err("Err:OrType".to_string()),
          }
        }
        Op::Set => {
          match &args[0] {
            S::Atom(Atom::Id(id)) => {
              let val = self.eval(args[1].clone())?;
              self.vars.insert(id.clone(), val.clone());
              Ok(val)
            }
            S::Cons(Op::Ind, vec_) if vec_.len() == 2 => {
              let var_name = match &vec_[0] {
                S::Atom(Atom::Id(id)) => id.clone(),
                _ => return Err("Err:IndVarName".to_string()),
              };
              let Atom::Lst(mut lst) = self.vars.get(&var_name).cloned().unwrap_or(Atom::Null) else {
                return Err("Err:IndType".to_string());
              };
              let idx = match self.eval(vec_[1].clone())? {
                Atom::I64(i) => i,
                _ => return Err("Err:IndIdxType".to_string()),
              };
              let value = self.eval(args[1].clone())?;
              if idx < 0 || (idx as usize) >= lst.len() {
                Ok(Atom::Null)
              } else {
                lst[idx as usize] = value.clone();
                self.vars.insert(var_name, Atom::Lst(lst));
                Ok(value)
              }
            }
            S::Cons(Op::Loc, _) => Err("todo!".to_string()),
            _ => Err("Err:SetLhs".to_string()),
          }
        }
        Op::Blk => {
          for arg in args {
            self.eval(arg.clone())?;
          }
          Ok(Atom::Null)
        }
        Op::If => {
          let cond = self.eval(args[0].clone())?;
          match cond {
            Atom::I64(x) => {
              if x != 0 {
                self.eval(args[1].clone())
              } else if args.len() > 2 {
                self.eval(args[2].clone())
              } else {
                Ok(Atom::Null)
              }
            }
            _ => Err("Err:IfCondType".to_string()),
          }
        }
        Op::While => {
          loop {
            let cond = self.eval(args[0].clone())?;
            match cond {
              Atom::I64(x) => {
                if x == 0 {break;}
                self.eval(args[1].clone())?;
              }
              _ => return Err("Err:WhileCondType".to_string()),
            }
          }
          Ok(Atom::Null)
        }
        Op::Lst => {
          let mut vec_ = Vec::new();
          for arg in args {
            vec_.push(self.eval(arg)?);
          }
          Ok(Atom::Lst(vec_))
        }
        Op::Ind => {
          let a = self.eval(args[0].clone())?;
          let b = self.eval(args[1].clone())?;
          match (a, b) {
            (Atom::Lst(lst), Atom::I64(i)) => {
              if i < 0 || (i as usize) >= lst.len() {
                Ok(Atom::Null)
              } else {
                Ok(lst[i as usize].clone())
              }
            }
            _ => Err("Err:IndType".to_string()),
          }
        }
        Op::Loc => {
          Err("todo!".to_string())
        }
        Op::FnC => {
          let mut vec_ = Vec::new();
          for arg in args {
            vec_.push(self.eval(arg)?);
          }
          match vec_[0].clone() {
            Atom::Fn(f) => Ok(f(vec_[1..].to_vec())),
            _ => Err("Err:NotFn".to_string()),
          }
        }
        _ => Err("Err:OpNotImpl".to_string()),
      }
    }
  }
}

fn main() {
  let mut interpreter = Interpreter::new();
  println!("ZN Interpreter [v0.1]");
  loop {
    print!("> ");
    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let input = input.trim();
    match parser::lex(input) {
      Ok(mut tokens) => {
        if tokens.is_empty() {continue;}
        println!("% {:?}", tokens);
        match parser::expr(&mut tokens) {
          Ok(expr) => {
            println!("$ {}", expr);
            match interpreter.eval(expr) {
              Ok(result) => println!("= {}", result),
              Err(err) => println!("! {}", err),
            }
          },
          Err(err) => println!("! {:?}\n& {}", tokens, err),
        }
      },
      Err(err) => println!("! {}", err),
    }
  }
}
