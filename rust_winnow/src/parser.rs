use std::char;
use std::fmt::format;
use std::str::FromStr;

use winnow::ascii::digit1;
use winnow::combinator::{alt, delimited, iterator, opt, peek, preceded, repeat, terminated};
use winnow::error::{self, ContextError, ErrMode, ParserError, StrContext, StrContextValue};
use winnow::prelude::*;
use winnow::stream::Stream;
use winnow::token::{any, literal, none_of, one_of, take, take_till};

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Boolean(bool),
    Accessor(Vec<Identifier>),
    Function {
        identifier: Identifier,
        argument: Box<Expression>,
    },
    String(String),
    Number(usize), // TODO: implement float too
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier(pub String);

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier(value.to_owned())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TemplateArgument {
    Named {
        name: Identifier,
        argument: Expression,
    },
    Positional(Expression),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Text(String),
    ValueRender(Expression),
    TemplateRender {
        name: Identifier,
        arguments: Vec<TemplateArgument>,
    },
    Conditional {
        // everything is a case when
        condition: Expression,
        matching: Vec<(Expression, Vec<Statement>)>,
        otherwise: Option<Vec<Statement>>,
    },
    Iterator {
        index: Option<Identifier>,
        value: Identifier,
        iterable: Expression,
        repeat: Vec<Statement>,
        delimeter: Option<Vec<Statement>>,
        otherwise: Option<Vec<Statement>>,
    },
    Raw(String),
}

impl Statement {
    pub fn text(text: &str) -> Self {
        return Self::Text(text.to_owned());
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Template {
    pub name: Identifier,
    pub arguments: Vec<Identifier>,
    pub body: Vec<Statement>,
}

// <uppercase> <= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
pub fn parse_uppercase(input: &mut &str) -> PResult<char> {
    one_of('A'..='Z').parse_next(input)
}

// <lowercase> <= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
pub fn parse_lowercase(input: &mut &str) -> PResult<char> {
    one_of('a'..='z').parse_next(input)
}

// <underscore> <= "_"
pub fn parse_underscore(input: &mut &str) -> PResult<char> {
    one_of('_').parse_next(input)
}

// <digit> <= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
pub fn parse_digit(input: &mut &str) -> PResult<char> {
    one_of('0'..='9').parse_next(input)
}

//<escape> <= "\"
pub fn parse_escaped(input: &mut &str) -> PResult<char> {
    preceded(one_of('\\'), any).parse_next(input)
}

// <letter> <= <uppercase> | <lowercase> | "_"
pub fn parse_letter(input: &mut &str) -> PResult<char> {
    alt((parse_lowercase, parse_uppercase, parse_underscore)).parse_next(input)
}

//<w_s_s> <=  " " | "\t" | "\n" | "\r"
pub fn parse_multiple_whitespaces(input: &mut &str) -> PResult<()> {
    one_of(('\t', ' ', '\n', '\r'))
        .map(|_| ())
        .parse_next(input)
}

//<w_s> <=  (" " | "\t" | "\n" | "\r")*
pub fn parse_whitespace(input: &mut &str) -> PResult<()> {
    repeat(0.., parse_multiple_whitespaces)
        .map(|_: Vec<()>| ())
        .parse_next(input)
}

//<identifier> <= <letter> (<letter> | <digit>)*
pub fn parse_identifier(input: &mut &str) -> PResult<Identifier> {
    let x = parse_letter(input)?;
    let y: String = repeat(0.., alt((parse_letter, parse_digit))).parse_next(input)?;
    return PResult::Ok(Identifier(format!("{x}{y}")));
}

// <w_identifier> <= <w_s> <identifier> <w_s>
pub fn parse_w_identifier(input: &mut &str) -> PResult<Identifier> {
    delimited(parse_whitespace, parse_identifier, parse_whitespace).parse_next(input)
}

// <boolean> <= "true" | "false"
pub fn parse_boolean(input: &mut &str) -> PResult<Expression> {
    alt((
        "true".map(|_| Expression::Boolean(true)),
        "false".map(|_| Expression::Boolean(false)),
    ))
    .parse_next(input)
}

// <text> <= (<any_character>)*
pub fn parse_text(input: &mut &str) -> PResult<String> {
    let until_brackets = take_till(0.., ['<', '{', '[', '$']).parse_next(input)?;

    //println!("Parsed so far: \"{}\"", until_brackets);

    let parsed_until_brackets = input.checkpoint();

    let ending_strings: Result<&str, ErrMode<ContextError>> =
        alt(("<<", "{{", "[[", "${")).parse_next(input);

    //println!("Parsed next 2 characters...");

    //if ending strings were there, backtrack and return
    if let Ok(s) = ending_strings {
        //println!("There was a special string: '{s}' Exitting...");
        input.reset(&parsed_until_brackets);

        if until_brackets == "" {
            return Err(ErrMode::Backtrack(ContextError::new()));
        }
        return Ok(until_brackets.to_owned());
    }
    //println!("There was no special string... continuing...");

    // else parse the next character
    let bracket: Result<&str, ErrMode<ContextError>> = take(1usize).parse_next(input);
    //if it is the end of string, then we return the string parsed until now
    if let Err(_) = bracket {
        //println!("couldnt take an extra string, going to return the one until now");
        if until_brackets == "" {
            return Err(ErrMode::Backtrack(ContextError::new()));
        }

        return Ok(until_brackets.to_owned());
    }

    //and try to continue with the text parser
    let rest = parse_text.parse_next(input)?;
    //println!("Returned from previous call: {rest}");
    let result = format!("{}{}{}", until_brackets, bracket.unwrap(), rest);
    return Ok(result);
}

// <q_text> <= (<any_character>)*
pub fn parse_q_text(input: &mut &str) -> PResult<String> {
    repeat(1.., alt((parse_escaped, none_of('\"')))).parse_next(input)
}

// <text> <= (<any_character>)*
pub fn parse_rr_text(input: &mut &str) -> PResult<String> {
    let until_brackets = take_till(1.., ['<']).parse_next(input)?;
    let parsed_until_brackets = input.checkpoint();
    let x: Result<&str, ErrMode<ContextError>> = "<</raw>>".parse_next(input);
    let bracket = take(1usize).parse_next(input)?;
    if let Ok(_) = x {
        input.reset(&parsed_until_brackets);
        return Ok(until_brackets.to_owned());
    }
    let rest = parse_rr_text.parse_next(input)?;
    return Ok(format!("{}{}{}", until_brackets, bracket, rest));
}

// <string> <= "\"" <text> "\""
pub fn parse_quoted_string(input: &mut &str) -> PResult<Expression> {
    delimited("\"", parse_q_text, "\"")
        .map(|x| Expression::String(x))
        .parse_next(input)
}

// <raw> <= "<<raw>>" <text> "<</raw>>"
pub fn parse_raw(input: &mut &str) -> PResult<Statement> {
    delimited(
        (
            parse_whitespace,
            "<<raw>>".context(StrContext::Label("first ignore")),
        ),
        parse_rr_text.context(StrContext::Label("first ignore")),
        "<</raw>>".context(StrContext::Label("first ignore")),
    )
    .map(|s| Statement::Raw(s))
    .parse_next(input)
}

// <number> <= (<digit>)+
pub fn parse_number(input: &mut &str) -> PResult<usize> {
    digit1.parse_to().parse_next(input)
}

//pub fn concat((a, b): (String, String)) -> String {
//    a + &b
//}

// ("." <identifier>)
pub fn parse_dot_identifier(input: &mut &str) -> PResult<Identifier> {
    (".".map(String::from), parse_identifier)
        .map(|(_, x)| x)
        .parse_next(input)
}

// <accessor> <= <identifier> ("." <identifier>)*
pub fn parse_accessor(input: &mut &str) -> PResult<Expression> {
    let mut iden = vec![parse_identifier(input)?];
    let rep_dotted_idents: Vec<Identifier> = repeat(0.., parse_dot_identifier).parse_next(input)?;

    for i in rep_dotted_idents {
        iden.push(i);
    }
    return PResult::Ok(Expression::Accessor(iden));
}

// <function_call> <= <identifier> "("  <w_expression>  ")"
pub fn parse_function_call(input: &mut &str) -> PResult<Expression> {
    (parse_identifier, delimited("(", parse_w_expression, ")"))
        .map(|(ident, expr)| Expression::Function {
            identifier: ident,
            argument: Box::new(expr),
        })
        .parse_next(input)
}

// <w_expression> <= <w_s> <expression> <w_s>
pub fn parse_w_expression(input: &mut &str) -> PResult<Expression> {
    delimited(parse_whitespace, parse_expression, parse_whitespace).parse_next(input)
}

// <expression> <=  ("!")?  <boolean> | <accessor> | <function_call> | <string> | <number>
pub fn parse_expression(input: &mut &str) -> PResult<Expression> {
    alt((
        parse_boolean,
        parse_function_call,
        parse_accessor,
        parse_quoted_string,
        parse_number.map(|x| Expression::Number(x)),
    ))
    .parse_next(input)
}

// <value_render> <= "${"  <w_expression>  "}"
pub fn parse_value_render(input: &mut &str) -> PResult<Statement> {
    delimited("${", parse_w_expression, "}")
        .map(|e| Statement::ValueRender(e))
        .parse_next(input)
}

// <template_argument> <= named | positional
pub fn parse_template_argument(input: &mut &str) -> PResult<TemplateArgument> {
    let result = alt((
        (parse_identifier, "=", parse_expression)
            .map(|(name, _, argument)| TemplateArgument::Named { name, argument }),
        parse_w_expression.map(|x| TemplateArgument::Positional(x)),
    ))
    .map(|x| x)
    .parse_next(input);

    //println!("Parsed template argument: {result:?}");
    return result;
}

// <template_render> <= "<<"  <w_identifier> (<expression>)* ">>"
pub fn parse_template_render(input: &mut &str) -> PResult<Statement> {
    let result = delimited(
        "<<".context(StrContext::Label("open br")),
        (parse_w_identifier, repeat(0.., parse_template_argument))
            .context(StrContext::Label("inside")),
        ">>".context(StrContext::Label("close br")),
    )
    .map(|(name, arguments)| Statement::TemplateRender { name, arguments })
    .parse_next(input);

    //println!("Template render attempted: {result:?}");

    return result;
}

// <conditional> <= <if> <w_statement> (<else>)? <end_condition>
pub fn parse_conditional(input: &mut &str) -> PResult<Statement> {
    (
        parse_if_partial.context(StrContext::Label("if partial")),
        parse_statements.context(StrContext::Label("w_statements")),
        opt(parse_else_partial).context(StrContext::Label("optional else_partial")),
        parse_end_condition,
    )
        .map(|(con, th, el, _)| {
            return Statement::Conditional {
                condition: con,
                matching: vec![(Expression::Boolean(true), th)],
                otherwise: el,
            };
        })
        .parse_next(input)
}

//<if> <= "{{" <w_s> "if "  <w_expression>  "}}"
pub fn parse_if_partial(input: &mut &str) -> PResult<Expression> {
    delimited(("{{", parse_whitespace, "if "), parse_w_expression, "}}").parse_next(input)
}

// <else> <= "{{" <w_s> "else" <w_s> "}}" <w_statement>
pub fn parse_else_partial(input: &mut &str) -> PResult<Vec<Statement>> {
    preceded(
        ("{{", parse_whitespace, "else", parse_whitespace, "}}"),
        parse_statements,
    )
    .parse_next(input)
}

// <else> <= "{{" <w_s> "else" <w_s> "}}" <w_statement>
pub fn parse_list_else_partial(input: &mut &str) -> PResult<Vec<Statement>> {
    preceded(
        ("[[", parse_whitespace, "else", parse_whitespace, "]]"),
        parse_statements,
    )
    .parse_next(input)
}

//<end_condition> <= "{{" <w_s> "end" <w_s> "}}"
pub fn parse_end_condition(input: &mut &str) -> PResult<()> {
    ("{{", parse_whitespace, "end", parse_whitespace, "}}")
        .map(|_| ())
        .parse_next(input)
}

//<w_statement> <= <w_s> <statement> <w_s>
pub fn parse_statement(input: &mut &str) -> PResult<Statement> {
    let result = alt((
        parse_value_render,
        parse_template_render,
        parse_conditional,
        multi_conditional,
        parse_iterator,
        parse_raw,
        parse_text.map(|s| Statement::Text(s)),
    ))
    .parse_next(input);

    if let Ok(_) = &result {
        println!("parsed statement: {:?}", &result);
    }

    return result;
}

pub fn parse_statements(input: &mut &str) -> PResult<Vec<Statement>> {
    repeat(0.., parse_statement).parse_next(input)
}

// <multicondition> <= <case_when> (<when>)* (<else>)? <end_condition>
pub fn multi_conditional(input: &mut &str) -> PResult<Statement> {
    (
        parse_case_partial,
        repeat(0.., parse_when_partial).map(|x: Vec<_>| x),
        opt(parse_else_partial),
        parse_end_condition,
    )
        .map(|((condition, when), d, el, _)| {
            let mut matching = vec![];
            match when {
                Some((ex, stat)) => matching.push((ex, stat)),
                None => (),
            };

            for (e, s) in d {
                matching.push((e, s));
            }

            return Statement::Conditional {
                condition,
                matching,
                otherwise: el,
            };
        })
        .parse_next(input)
}

// <case_when> <= "{{" <w_s> "case " <w_expression> ((" when " <w_expression> "}}" <w_statement>) | "}}")
pub fn parse_case_partial(
    input: &mut &str,
) -> PResult<(Expression, Option<(Expression, Vec<Statement>)>)> {
    let optional_when: _ = alt((
        (
            delimited("when", parse_w_expression, "}}"),
            parse_statements,
        )
            .map(Some),
        "}}".map(|_| None),
    ));

    return preceded(
        ("{{", parse_whitespace, "case "),
        (parse_w_expression, optional_when),
    )
    .parse_next(input);
}

// <when> <= "{{" <w_s> "when" <w_expression> "}}" <w_statement>
pub fn parse_when_partial(input: &mut &str) -> PResult<(Expression, Vec<Statement>)> {
    (
        delimited(("{{", parse_whitespace, "when"), parse_w_expression, "}}"),
        parse_statements,
    )
        .parse_next(input)
}

// <iterator> <= <list_comprehension> <w_statement> (<delimeter>)? <end_iter>
pub fn parse_iterator(input: &mut &str) -> PResult<Statement> {
    (
        parse_list_comprehension.context(StrContext::Label("list_comprehension")),
        parse_statements.context(StrContext::Label("w_statements")),
        opt(parse_delimeter).context(StrContext::Label("optional delimeter")),
        opt(parse_list_else_partial).context(StrContext::Label("optional delimeter")),
        (
            "[[".context(StrContext::Expected(StrContextValue::StringLiteral("[["))),
            parse_whitespace,
            "end",
            parse_whitespace,
            "]]",
        )
            .context(StrContext::Label("end of iterator")),
    )
        .map(
            |((index, value, iterable), statement, delimeter, otherwise, _)| Statement::Iterator {
                index,
                value,
                iterable,
                delimeter,
                repeat: statement,
                otherwise,
            },
        )
        .parse_next(input)
}

// <delimeter> <= "[[" <w_s> "separated by" <w_s> "]]" <statements>
pub fn parse_delimeter(input: &mut &str) -> PResult<Vec<Statement>> {
    let sep_by = (
        "[[".context(StrContext::Label("opening brachets")),
        parse_whitespace.context(StrContext::Label("first whitespace")),
        "separated by".context(StrContext::Label("sep by")),
        parse_whitespace.context(StrContext::Label("second whitespace")),
        "]]".context(StrContext::Label("closing brackets")),
    );

    let result = preceded(sep_by, parse_statements).parse_next(input);

    //if let Ok(_) = result {
    println!("Test: {:?}", result);
    //}
    return result;
}

// <list_comprehension> <= "[[" <w_s> "for " (<w_identifier> ", ")? <w_identifier>) " in " <w_expression> "]]"
pub fn parse_list_comprehension(
    input: &mut &str,
) -> PResult<(Option<Identifier>, Identifier, Expression)> {
    return (
        preceded(
            ("[[ ", parse_whitespace, "for ").context(StrContext::Label("for boilerplate")),
            opt(terminated(parse_w_identifier, ", ")).context(StrContext::Label("index")),
        ),
        parse_w_identifier.context(StrContext::Label("value")),
        delimited(
            "in ".context(StrContext::Label("in")),
            parse_w_expression.context(StrContext::Label("expression")),
            "]]".context(StrContext::Label("closing brackets")),
        )
        .context(StrContext::Label("iterable")),
    )
        .parse_next(input);
}

// <template_definition> <= "template <<" <w_identifier> " " (<w_expression>)* ">>" (<w_statement>)* <template_end>
pub fn parse_template_definition(input: &mut &str) -> PResult<Template> {
    let args: _ = repeat(0.., parse_w_identifier);
    (
        preceded(
            "template <<".context(StrContext::Label("template boilerplate")),
            parse_w_identifier.context(StrContext::Label("template name")),
        ),
        terminated(args, ">>"),
        terminated(
            repeat(1.., parse_statement).context(StrContext::Label("many statements")),
            (
                "<</".context(StrContext::Label("first slash")),
                parse_w_identifier,
                ">>",
            )
                .context(StrContext::Label("End of template")),
        ),
    )
        .map(|(name, arguments, statements)| Template {
            name,
            arguments,
            body: statements,
        })
        .parse_next(input)
}

// <document> <= <w_s> (<template_definition> <w_s>)+
pub fn parse_document(input: &mut &str) -> PResult<Vec<Template>> {
    (
        parse_whitespace,
        repeat(
            1..,
            (parse_template_definition, parse_whitespace).map(|(templ, _)| templ),
        ),
    )
        .map(|(_, templates)| templates)
        .parse_next(input)
}
