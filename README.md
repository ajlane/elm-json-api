# Elm JSON API

An Elm code generator for JSON APIs.

Generates types and functions for working with a service defined by a simple interface spec.

## Getting Started

### Install from Git

```
git clone https://github.com/ajlane/elm-json-api.git
cd elm-json-api
npm run build
npm link -g
```

### Run example

```
cd example
elm-json-api
elm reactor
```

## What does it do?

It allows you to declaratively define a JSON-based API for a service, using a spec that looks something like this:

```yaml
description: An example interface
types:
  CalculatorUrl:
    is: Url
    description:
      A link to a service that adds or subtracts from an integer stored in memory.
    methods:
      get: Int
      post:
        accept: Operation
        expect: Int
  Operation:
    is: Enum
    description:
      An operation for the calculator to apply
    oneOf:
      Clear:
        description:
          Resets the result to zero.
      Add:
        is: Int
        description:
          Adds the given value to the current result
      Subtract:
        is: Int
        description:
          Subtracts the given value from the current result
```

And use that spec to generate code and documentation:

```bash
elm-json-api --spec example.spec.yaml --namespace ExampleApi --out src
```

That you can then import and use in an Elm application:

```elm
import ExampleApi.Model exposing (CalculatorUrl, Operation(..))
import ExampleApi.Http exposing (getCalculatorUrl, postCalculatorUrl, request)

url = CalculatorUrl "http://localhost/calculator"
    
type Model
    = Loading
    | Ok Int
    | Err
    
type Msg
    = Send Operation
    | ShowNumber Int
    | ShowError Http.Error

init _ = (Loading, url |> getCalculatorUrl ShowNumber ShowError |> request)

update msg result =
    case msg of
        Send operation ->
            (Loading, url |> postCalculatorUrl operation ShowNumber ShowError |> request)
        ShowNumber number ->
            (Ok number, Nothing)
        ShowError err ->
            (Err, Nothing)

view result =
    case result of
        Loading ->
            text "Please wait"
        Err ->
            text "Error"
        Ok number ->
            div [] [
                h1 [ text (number |> String.fromInt) ]
                button [onClick (Add 1 |> Send)] [ text "Increment" ]
                button [onClick (Subtract 1 |> Send)] [ text "Decrement" ]
                button [onClick (Clear |> Send)] [ text "Reset" ]
            ]
```

## Is it finished?

It works, but it isn't finished yet. Outstanding work includes:

- [ ] Make sure the generated functions for POST and PUT requests send the body correctly
- [ ] Increase test coverage for generated files (by expanding the example app's spec to use more features and test them)
- [ ] Contribute a fix upstream to make uri templates conform to the spec for missing params
- [ ] Contribute a fix upstream to remove excess whitespace in generated docs
- [ ] Add descriptions of parameters and fields to generated docs
- [ ] Add examples to generated docs
- [ ] Remove unused imports
- [ ] Optionally install necessary dependencies in elm.json
- [ ] Fill out the set of constraints you can add to primitive types, such as setting a minimum length for lists, or matching a regular expression against a string
- [ ] Add support for a key-value type (or decide that lists of key-value pairs are good enough)
- [ ] Add a feature to check whether two different versions of a spec are compatible
- [ ] Rename the tool to avoid confusion with elm-jsonapi or the json:api project

## What about the server-side?

If you use something like [elm serverless](https://github.com/the-sett/elm-serverless), you can use the same generated .elm files to implement the API from the server side.

There isn't much that is elm-specific about the spec files. They're just json objects, formatted with yaml for readability. Tools could be written to generate equivalent types, stubs, and documentation for the server-side implementation in any language.
