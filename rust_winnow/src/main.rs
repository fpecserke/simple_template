use parser::{document, identifier};

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

    let doc = document(&mut strng);
    match doc {
        Ok(x) => {
            println!("Templates: {:#?}", x);
        }
        Err(e) => println!("Error: {:#?}", e),
    };

    Ok(())
}

#[test]
fn test_identifier() {
    let mut i = "map";

    let parsed = identifier(&mut i).expect("should return 'map'");
}
