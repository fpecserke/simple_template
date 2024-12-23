use winnow::ascii::digit1;
use winnow::combinator::{alt, delimited, iterator, opt, preceded, repeat, terminated};
use winnow::prelude::*;
use winnow::token::{any, literal, one_of};

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Identifier(String);

#[derive(Debug)]
pub enum Statement {
    Text(String),
    ValueRender(Expression),
    TemplateRender(Identifier),
    Conditional {
        // everything is a case when
        condition: Expression,
        matching: Vec<(Expression, Box<Statement>)>,
        otherwise: Option<Box<Statement>>,
    },
    Iterator {
        index: Option<Identifier>,
        value: Identifier,
        iterable: Expression,
        repeat: Box<Statement>,
        delimeter: Option<Box<Statement>>,
    },
    Raw(String),
}

#[derive(Debug)]
pub struct Template {
    name: Identifier,
    arguments: Vec<Identifier>,
    body: Vec<Statement>,
}

// <uppercase> <= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
pub fn uppercase(input: &mut &str) -> PResult<char> {
    one_of('A'..='Z').parse_next(input)
}

// <lowercase> <= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
pub fn lowercase(input: &mut &str) -> PResult<char> {
    one_of('a'..='z').parse_next(input)
}

// <lowercase> <= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
pub fn underscore(input: &mut &str) -> PResult<char> {
    one_of('_').parse_next(input)
}

// <digit> <= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
pub fn digit(input: &mut &str) -> PResult<char> {
    one_of('0'..='9').parse_next(input)
}

// <special_character> <= "[[" | "]]" | "{{" | "}}" | "<<" | ">>"
pub fn special_character(input: &mut &str) -> PResult<String> {
    alt((
        literal("[[").map(String::from),
        literal("]]").map(String::from),
        literal("{{").map(String::from),
        literal("}}").map(String::from),
        literal("<<").map(String::from),
        literal(">>").map(String::from),
    ))
    .parse_next(input)
}

//<escape> <= "\"
pub fn escape(input: &mut &str) -> PResult<char> {
    one_of('\\').parse_next(input)
}

// <back_slash> <= "\\"
pub fn back_slash(input: &mut &str) -> PResult<String> {
    literal("\\\\").map(String::from).parse_next(input)
}

// <letter> <= <uppercase> | <lowercase> | "_"
pub fn letter(input: &mut &str) -> PResult<char> {
    alt((lowercase, uppercase)).parse_next(input)
}

//<w_s> <=  (" " | "\t" | "\n" | "\r")*
pub fn w_s(input: &mut &str) -> PResult<char> {
    one_of(('\t', ' ', '\n', '\r')).parse_next(input)
}

//<any_character> <= <letter> | <digit> | <w_s>
pub fn any_character(input: &mut &str) -> PResult<char> {
    any(input)
}

//<identifier> <= <letter> (<letter> | <digit>)*
pub fn identifier(input: &mut &str) -> PResult<Identifier> {
    let x = letter(input)?;
    let y: String = repeat(0.., alt((letter, digit))).parse_next(input)?;
    return PResult::Ok(Identifier(format!("{x}{y}")));
}

// <w_identifier> <= <w_s> <identifier> <w_s>
pub fn w_identifier(input: &mut &str) -> PResult<Identifier> {
    delimited(w_s, identifier, w_s).parse_next(input)
}

// <boolean> <= "true" | "false"
pub fn boolean(input: &mut &str) -> PResult<Expression> {
    alt((
        "true".map(|_| Expression::Boolean(true)),
        "false".map(|_| Expression::Boolean(false)),
    ))
    .parse_next(input)
}

// <text> <= (<any_character>)*
pub fn text(input: &mut &str) -> PResult<String> {
    repeat(0.., any_character).parse_next(input)
}

// <string> <= "\"" <text> "\""
pub fn quoted_string(input: &mut &str) -> PResult<Expression> {
    delimited("\"", text, "\"")
        .map(|x| Expression::String(x))
        .parse_next(input)
}

// <raw> <= "<<raw>>" <text> "<</raw>>"
pub fn raw(input: &mut &str) -> PResult<Statement> {
    delimited("<<raw>>", text, "<<raw>>")
        .map(|s| Statement::Raw(s))
        .parse_next(input)
}

// <number> <= (<digit>)+
pub fn number(input: &mut &str) -> PResult<usize> {
    digit1.parse_to().parse_next(input)
}

//pub fn concat((a, b): (String, String)) -> String {
//    a + &b
//}

// ("." <identifier>)
pub fn dot_identifier(input: &mut &str) -> PResult<Identifier> {
    (".".map(String::from), identifier)
        .map(|(_, x)| x)
        .parse_next(input)
}

// <accessor> <= <identifier> ("." <identifier>)*
pub fn accessor(input: &mut &str) -> PResult<Expression> {
    let mut iden = vec![identifier(input)?];
    let rep_dotted_idents: Vec<Identifier> = repeat(0.., dot_identifier).parse_next(input)?;

    for i in rep_dotted_idents {
        iden.push(i);
    }
    return PResult::Ok(Expression::Accessor(iden));
}

// <function_call> <= <identifier> "("  <w_expression>  ")"
pub fn function_call(input: &mut &str) -> PResult<Expression> {
    (identifier, delimited("(", w_expression, ")"))
        .map(|(ident, expr)| Expression::Function {
            identifier: ident,
            argument: Box::new(expr),
        })
        .parse_next(input)
}

// <w_expression> <= <w_s> <expression> <w_s>
pub fn w_expression(input: &mut &str) -> PResult<Expression> {
    delimited(w_s, expression, w_s).parse_next(input)
}

// <expression> <=  ("!")?  <boolean> | <accessor> | <function_call> | <string> | <number>
pub fn expression(input: &mut &str) -> PResult<Expression> {
    alt((
        boolean,
        accessor,
        function_call,
        quoted_string,
        number.map(|x| Expression::Number(x)),
    ))
    .parse_next(input)
}

// <value_render> <= "${"  <w_expression>  "}"
pub fn value_render(input: &mut &str) -> PResult<Statement> {
    delimited("${", w_expression, "}")
        .map(|e| Statement::ValueRender(e))
        .parse_next(input)
}

// <template_render> <= "<<"  <w_identifier> ">>"
pub fn template_render(input: &mut &str) -> PResult<Statement> {
    delimited("<<", w_identifier, ">>")
        .map(|x| Statement::TemplateRender(x))
        .parse_next(input)
}

// <conditional> <= <if> <w_statement> (<else>)? <end_condition>
pub fn conditional(input: &mut &str) -> PResult<Statement> {
    (if_partial, w_statement, opt(else_partial), end_condition)
        .map(|(con, th, el, _)| {
            return Statement::Conditional {
                condition: con,
                matching: vec![(Expression::Boolean(true), Box::new(th))],
                otherwise: el.map(|x| Box::new(x)),
            };
        })
        .parse_next(input)
}

//<if> <= "{{" <w_s> "if "  <w_expression>  "}}"
pub fn if_partial(input: &mut &str) -> PResult<Expression> {
    delimited(("{{", w_s, "if "), w_expression, "}}").parse_next(input)
}

// <else> <= "{{" <w_s> "else" <w_s> "}}" <w_statement>
pub fn else_partial(input: &mut &str) -> PResult<Statement> {
    preceded(("{{", w_s, "else", w_s, "}}"), w_statement).parse_next(input)
}

//<end_condition> <= "{{" <w_s> "end" <w_s> "}}"
pub fn end_condition(input: &mut &str) -> PResult<()> {
    ("{{", w_s, "end", w_s, "}}").map(|_| ()).parse_next(input)
}

// <statement> <=  <text> | <value_render> | <template_render> | <conditional> | <multicondition> | <iterator> | <raw_string>
pub fn statement(input: &mut &str) -> PResult<Statement> {
    alt((
        text.map(|s| Statement::Text(s)),
        value_render,
        template_render,
        conditional,
        multi_conditional,
        iterator_2,
        raw,
    ))
    .parse_next(input)
}

//<w_statement> <= <w_s> <statement> <w_s>
pub fn w_statement(input: &mut &str) -> PResult<Statement> {
    delimited(w_s, statement, w_s).parse_next(input)
}

// <multicondition> <= <case_when> (<when>)* (<else>)? <end_condition>
pub fn multi_conditional(input: &mut &str) -> PResult<Statement> {
    (
        case_partial,
        repeat(0.., when_partial).map(|x: Vec<(Expression, Statement)>| x),
        opt(else_partial),
        end_condition,
    )
        .map(|((condition, when), d, el, _)| {
            let mut matching = vec![];
            match when {
                Some((ex, stat)) => matching.push((ex, Box::new(stat))),
                None => (),
            };

            for (e, s) in d {
                matching.push((e, Box::new(s)));
            }

            return Statement::Conditional {
                condition,
                matching,
                otherwise: el.map(|x| Box::new(x)),
            };
        })
        .parse_next(input)
}

// <case_when> <= "{{" <w_s> "case " <w_expression> ((" when " <w_expression> "}}" <w_statement>) | "}}")
pub fn case_partial(input: &mut &str) -> PResult<(Expression, Option<(Expression, Statement)>)> {
    let optional_when: _ = alt((
        (delimited("when", w_expression, "}}"), w_statement).map(Some),
        "}}".map(|_| None),
    ));

    return preceded(("{{", w_s, "case "), (w_expression, optional_when)).parse_next(input);
}

// <when> <= "{{" <w_s> "when" <w_expression> "}}" <w_statement>
pub fn when_partial(input: &mut &str) -> PResult<(Expression, Statement)> {
    (
        delimited(("{{", w_s, "when"), w_expression, "}}"),
        w_statement,
    )
        .parse_next(input)
}

// <iterator> <= <list_comprehension> <w_statement> (<delimeter>)? <end_iter>
pub fn iterator_2(input: &mut &str) -> PResult<Statement> {
    (
        list_comprehension,
        w_statement,
        opt(delimeter),
        ("[[", w_s, "end", w_s, "]]"),
    )
        .map(
            |((index, value, iterable), statement, delimeter, _)| Statement::Iterator {
                index,
                value,
                iterable,
                delimeter: delimeter.map(Box::new),
                repeat: Box::new(statement),
            },
        )
        .parse_next(input)
}

// <delimeter> <= "[[" <w_s> "separated by" <w_s> "]]" <text>
pub fn delimeter(input: &mut &str) -> PResult<Statement> {
    let sep_by = ("[[", w_s, "separated by", w_s, "]]");

    return preceded(sep_by, text)
        .map(|s| Statement::Text(s))
        .parse_next(input);
}

// <list_comprehension> <= "[[" <w_s> "for " (<w_identifier> ", ")? <w_identifier>) " in " <w_expression> "]]"
pub fn list_comprehension(
    input: &mut &str,
) -> PResult<(Option<Identifier>, Identifier, Expression)> {
    return (
        preceded(("[[", w_s, "for "), opt(terminated(w_identifier, ", "))),
        w_identifier,
        delimited(" in ", w_expression, "]]"),
    )
        .parse_next(input);
}

// <template_definition> <= "template <<" <w_identifier> " " (<w_expression>)* ">>" (<w_statement>)* <template_end>
pub fn template_definition(input: &mut &str) -> PResult<Template> {
    let args: _ = repeat(0.., w_identifier);
    (
        delimited("template <<", w_identifier, " "),
        terminated(args, ">>"),
        terminated(repeat(1.., w_statement), ("<</", w_identifier, ">>")),
    )
        .map(|(name, arguments, statements)| Template {
            name,
            arguments,
            body: statements,
        })
        .parse_next(input)
}

// <document> <= <w_s> (<template_definition> <w_s>)+
pub fn document(input: &mut &str) -> PResult<Vec<Template>> {
    (
        w_s,
        repeat(1.., (template_definition, w_s).map(|(templ, _)| templ)),
    )
        .map(|(_, templates)| templates)
        .parse_next(input)
}
