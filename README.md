# Elm JSON API

An Elm code generator for JSON APIs.

Generates types and functions for working with a service defined by a simple interface spec.

## Example

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

```bash
elm-json-api --spec example.spec.yaml --namespace ExampleApi --out src
```

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
