# Simple template

This is a templating language and engine for rendering Hypertemplates.

Hypertemplate is a template that:

- is logicless
- can traverse arbitrary JSON and render it
- can contain data that can be extracted and exposed to the application layer
- can be parsed and the input structure inferred

Example:

```sql
{{ metadata(JSON) }}
    {
        "template": "data/table.html" 
        "actions": [
            { "get": "query/get_sales.sql", "name": "Sales" }
            { "get": "query/get_purchases.sql", "name": "Purchases" }
        ]
    }
{{ end }}

SELECT
    {{ for key, col in columns }}
        {{ col }} --{{ key }}
    {{ end }}
FROM {{ table_name }}

```

We can infer from this example that the accepted schema is:

```JSON
{
  "table_name": string,
  "columns": []string | map<key, value>
}
```

If we then pass an object with this structure, it will be correctly rendered
by the engine. Notice that the columns can be an array of objects, or an
hashmap/object.

- if it is array, then the `key` will be the index if that array
- if it is object, then the `key` will be the key to look in the object

Then we can traverse an object like this:

```JSON
{
  "table_name": "countries",
  "columns": {
    "Country Name": "country_name",
    "Country Code":  "country_code",
    "Id": "id"
  }
}
```

Or like this:

```JSON
{
  "table_name": "countries",
  "columns": ["country_name", "country_code", "id"]
}
```

Also there is the `metadata(JSON)` that exposes some data that can be relevant
to this template to my application. I can use this data to find another
template (this time HTML for example) and use the `actions` as a part of an
input passed to it, along with the result of the query.

Also you can specify any format for the metadata that supports nesting arrays
and hashmaps/objects/dictionaries, as long as the engine supports it.


The testing EBNF spec is in `hypertempl.ebnf`
