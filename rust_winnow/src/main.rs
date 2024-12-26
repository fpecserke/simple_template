use core::panic;
use std::{
    fs::{self, File},
    io::Read,
};

use parser::{
    multi_conditional, parse_accessor, parse_boolean, parse_conditional, parse_delimeter,
    parse_digit, parse_document, parse_escaped, parse_function_call, parse_identifier,
    parse_iterator, parse_letter, parse_list_comprehension, parse_lowercase, parse_number,
    parse_quoted_string, parse_raw, parse_statements, parse_template_definition,
    parse_template_render, parse_text, parse_underscore, parse_uppercase, parse_value_render,
    parse_w_identifier, parse_whitespace, Expression, Identifier, Statement, Template,
    TemplateArgument,
};
use winnow::{combinator::opt, Parser};

mod parser;

fn main() -> Result<(), String> {
    let mut strng = r#"
        template <<Hashmap map>>
        {
            [[ for key, val in map ]]
                "${key}":  <<JsonValue val>>
            [[ separated by ] ,
            [[ end ]]
        }
        <<end>>


        template <<Array array>>
        [
            [[ for element in array ]]
                <<JsonValue element>>
            [[ separated by ]] ,
            [[ end ]]

        ]
        <<end>>


        template <<JsonValue val>>
        {{ case type(val) when "number" }}
            ${val}
        {{ when "bool" }}
            ${val}

        {{ when "string" }}
            "${val}"

        {{ when "array" }}
            <<Array val>>

        {{ when "map" }}
            <<Hashmap val>>

        {{ end }}
        <<end>
"#;

    let doc = parse_document(&mut strng);
    match doc {
        Ok(x) => {
            println!("Templates: {:#?}", x);
        }
        Err(e) => println!("Error: {:#?}", e),
    };

    Ok(())
}

#[test]
pub fn test_uppercase() {
    let mut input = "Ahoj";
    let parsed = parse_uppercase(&mut input).expect("should parse uppercase");
    assert_eq!(parsed, 'A');
    assert_eq!(input, "hoj");
}

#[test]
pub fn test_lowercase() {
    let mut input = "ahoj";
    let parsed = parse_lowercase(&mut input).expect("should parse lowecase");
    assert_eq!(parsed, 'a');
    assert_eq!(input, "hoj");
}

#[test]
pub fn test_underscore() {
    let mut input = "_ahoj";
    let parsed = parse_underscore(&mut input).expect("should parse underscore");
    assert_eq!(parsed, '_');
    assert_eq!(input, "ahoj");
}

#[test]
fn test_digit() {
    let mut input = "559";
    let parsed = parse_digit(&mut input).expect("should parse digit");
    assert_eq!(parsed, '5');
    assert_eq!(input, "59");
}

#[test]
fn test_escape() {
    let mut input = r#"\ntest"#;
    let parsed = parse_escaped(&mut input).expect("should parse escape");
    assert_eq!(parsed, 'n');
    assert_eq!(input, "test");
}

#[test]
fn test_letter() {
    let mut input = "test";
    let parsed = parse_letter(&mut input).expect("should parse letter");
    assert_eq!(parsed, 't');
    assert_eq!(input, "est");
}

#[test]
fn test_w_s() {
    let mut input = " \n  test ";
    let parsed = parse_whitespace(&mut input).expect("should parse whitespace");
    assert_eq!(parsed, ());
    assert_eq!(input, "test ");
}

#[test]
fn test_identifier() {
    let mut input = "map";
    let parsed = parse_identifier(&mut input).expect("should parse identifier");
    assert_eq!(parsed.0, "map".to_owned());
    assert_eq!(input, "");
}

#[test]
fn test_w_identifier() {
    let mut input = " map  ";
    let parsed = parse_w_identifier(&mut input).expect("should white space surrounded identifier");
    assert_eq!(parsed.0, "map".to_owned());
    assert_eq!(input, "");
}

#[test]
fn test_boolean() {
    let mut input = "true";
    let parsed = parse_boolean(&mut input).expect("should parse a boolean");
    assert_eq!(parsed, Expression::Boolean(true));
    assert_eq!(input, "");
}

#[test]
fn test_text() {
    let mut input = "This is a random text with a lot of weird signs like # < > { } [ ] but when this [[ follows, it is cut short";
    let parsed = parse_text(&mut input).expect("should parse a text");
    println!("Parsed input: \n{}", parsed);
    assert_eq!(
        parsed,
        "This is a random text with a lot of weird signs like # < > { } [ ] but when this "
            .to_owned()
    );
    assert_eq!(input, "[[ follows, it is cut short");
}

#[test]
fn test_string() {
    let mut input =
        r#""A quoted string, here, I can do whatever I want , even this <<test>>, or this \" ""#;
    let parsed = parse_quoted_string(&mut input).expect("should parse a quoted_string");
    assert_eq!(
        parsed,
        Expression::String(
            "A quoted string, here, I can do whatever I want , even this <<test>>, or this \" "
                .to_owned()
        )
    );
}

#[test]
fn test_raw_string() {
    let mut input = r#"
        <<raw>>
            here is some raw text, here I can even write this: [[for x in b]] repeat ${x} [[separated by ]] , [[end]]
            and it still is finem it is still part of the raw string
        <</raw>>
        "#;
    let parsed = parse_raw(&mut input).expect("should parse a raw string");
    assert_eq!(
        parsed,
        Statement::Raw(
            r#"
            here is some raw text, here I can even write this: [[for x in b]] repeat ${x} [[separated by ]] , [[end]]
            and it still is finem it is still part of the raw string
        "#.to_owned()
        )
    );
}

#[test]
fn test_number() {
    let mut input = "1450";
    let parsed = parse_number(&mut input).expect("should parse a number");
    assert_eq!(parsed, 1450)
}

#[test]
fn test_accessor() {
    let mut input = "hello.world";
    let parsed = parse_accessor(&mut input).expect("should parse an accessor");
    assert_eq!(
        parsed,
        Expression::Accessor(vec!["hello".into(), "world".into()])
    )
}

#[test]
fn test_function_call() {
    let mut input = "upper(hello.world)";
    let parsed = parse_function_call(&mut input).expect("should parse an accessor");

    assert_eq!(
        parsed,
        Expression::Function {
            identifier: "upper".into(),
            argument: Box::new(Expression::Accessor(vec!["hello".into(), "world".into()]))
        }
    );

    let mut input2 = "upper(true)";
    let parsed2 = parse_function_call(&mut input2).expect("should parse an accessor");

    assert_eq!(
        parsed2,
        Expression::Function {
            identifier: "upper".into(),
            argument: Box::new(Expression::Boolean(true))
        }
    );

    let mut input3 = "upper(lower(x))";
    let parsed3 = parse_function_call(&mut input3).expect("should parse an accessor");

    assert_eq!(
        parsed3,
        Expression::Function {
            identifier: "upper".into(),
            argument: Box::new(Expression::Function {
                identifier: "lower".into(),
                argument: Box::new(Expression::Accessor(vec!["x".into()]))
            })
        }
    );

    let mut input4 = "upper(\"test\")";
    let parsed4 = parse_function_call(&mut input4).expect("should parse an accessor");
    assert_eq!(
        parsed4,
        Expression::Function {
            identifier: "upper".into(),
            argument: Box::new(Expression::String("test".into()))
        }
    );

    let mut input5 = "upper(5)";
    let parsed5 = parse_function_call(&mut input5).expect("should parse an accessor");
    assert_eq!(
        parsed5,
        Expression::Function {
            identifier: "upper".into(),
            argument: Box::new(Expression::Number(5))
        }
    )
}

#[test]
fn test_value_render() {
    let mut input = "${test}";
    let parsed = parse_value_render(&mut input).expect("should parse a value render");
    assert_eq!(
        parsed,
        Statement::ValueRender(Expression::Accessor(vec!["test".into()]))
    )
}

#[test]
fn test_template_render() {
    let mut input = "<<test x>>";
    let parsed = parse_template_render(&mut input).expect("should parse a template render");
    assert_eq!(
        parsed,
        Statement::TemplateRender {
            name: "test".into(),
            arguments: vec![TemplateArgument::Positional(Expression::Accessor(vec![
                "x".into()
            ]))]
        }
    )
}

#[test]
fn test_conditional() {
    let mut input = "{{ if x }} show if true {{ else }} show if false {{end}}";
    let parsed = parse_conditional(&mut input).expect("should parse a conditional");
    assert_eq!(
        parsed,
        Statement::Conditional {
            condition: Expression::Accessor(vec!["x".into()]),
            matching: vec![(
                Expression::Boolean(true),
                vec![Statement::text(" show if true ")]
            )],
            otherwise: Some(vec![Statement::text(" show if false ")])
        }
    )
}

#[test]
fn test_multiconditional() {
    let mut input =
        "{{ case x when 1 }} show if 1 {{when 5}} show if 5 {{ else }} else: ${x} {{end}}";
    let parsed = multi_conditional(&mut input).expect("should parse a conditional");
    assert_eq!(
        parsed,
        Statement::Conditional {
            condition: Expression::Accessor(vec!["x".into()]),
            matching: vec![
                (Expression::Number(1), vec![Statement::text(" show if 1 ")]),
                (Expression::Number(5), vec![Statement::text(" show if 5 ")])
            ],
            otherwise: Some(vec![
                Statement::text(" else: "),
                Statement::ValueRender(Expression::Accessor(vec!["x".into()])),
                Statement::text(" ")
            ])
        }
    )
}

#[test]
fn test_iterator() {
    let mut input =
        "[[ for i, v in list(5) ]] <li> ${i} - ${v} </li> [[ separated by ]] <br> [[else]] empty list :/ [[end]]";
    let parsed = parse_iterator
        .parse(&mut input)
        .expect("should parse an iterator");
    assert_eq!(
        parsed,
        Statement::Iterator {
            index: Some("i".into()),
            value: "v".into(),
            iterable: Expression::Function {
                identifier: "list".into(),
                argument: Box::new(Expression::Number(5)),
            },
            repeat: vec![
                Statement::text(" <li> "),
                Statement::ValueRender(Expression::Accessor(vec!["i".into()])),
                Statement::text(" - "),
                Statement::ValueRender(Expression::Accessor(vec!["v".into()])),
                Statement::text(" </li> ")
            ],
            delimeter: Some(vec![Statement::text(" <br> ")]),
            otherwise: Some(vec![Statement::text(" empty list :/ ")])
        }
    )
}

#[test]
fn test_template_definition() {
    let mut input =
        "template <<Test x>>[[ for i, v in list(x) ]] <li> ${i} - ${v} </li> [[ separated by ]] <br> [[else]] empty list :/ [[end]]<</Test>>";
    let parsed = parse_template_definition
        .parse(&mut input)
        .expect("should parse a template definition");
    assert_eq!(
        parsed,
        Template {
            name: "Test".into(),
            arguments: vec!["x".into()],
            body: vec![Statement::Iterator {
                index: Some("i".into()),
                value: "v".into(),
                iterable: Expression::Function {
                    identifier: "list".into(),
                    argument: Box::new(Expression::Accessor(vec!["x".into()])),
                },
                repeat: vec![
                    Statement::text(" <li> "),
                    Statement::ValueRender(Expression::Accessor(vec!["i".into()])),
                    Statement::text(" - "),
                    Statement::ValueRender(Expression::Accessor(vec!["v".into()])),
                    Statement::text(" </li> ")
                ],
                delimeter: Some(vec![Statement::text(" <br> ")]),
                otherwise: Some(vec![Statement::text(" empty list :/ ")])
            }]
        }
    )
}

#[test]
fn test_document() {
    let mut input =
        "template <<Test x>>[[ for i, v in list(x) ]] <li> ${i} - ${v} </li> [[ separated by ]] <br> [[else]] empty list :/ [[end]]<</Test>>";
    let parsed = parse_document
        .parse(&mut input)
        .expect("should parse a template definition");
    assert_eq!(
        parsed,
        vec![Template {
            name: "Test".into(),
            arguments: vec!["x".into()],
            body: vec![Statement::Iterator {
                index: Some("i".into()),
                value: "v".into(),
                iterable: Expression::Function {
                    identifier: "list".into(),
                    argument: Box::new(Expression::Accessor(vec!["x".into()])),
                },
                repeat: vec![
                    Statement::text(" <li> "),
                    Statement::ValueRender(Expression::Accessor(vec!["i".into()])),
                    Statement::text(" - "),
                    Statement::ValueRender(Expression::Accessor(vec!["v".into()])),
                    Statement::text(" </li> ")
                ],
                delimeter: Some(vec![Statement::text(" <br> ")]),
                otherwise: Some(vec![Statement::text(" empty list :/ ")])
            }]
        }]
    )
}

#[test]
fn test_example_json() {
    let mut file = File::open("../examples/json.html").unwrap();
    let mut s = String::new();
    let _bytes_read = file.read_to_string(&mut s).unwrap();
    let parsed = parse_document(&mut s.as_str()).expect("should read the document");
}

#[test]
fn test_example_table() {
    let mut file = File::open("../examples/table_template.html").unwrap();
    let mut s = String::new();
    let _bytes_read = file.read_to_string(&mut s).unwrap();
    let parsed = parse_document(&mut s.as_str()).expect("should read the document");
}
