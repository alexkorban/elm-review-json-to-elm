# elm-review-json-to-elm

Provides an [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to generate JSON decoders and encoders from a JSON sample in a `Debug.todo` string. 


## Provided rules

- [`JsonToElm`](https://package.elm-lang.org/packages/alexkorban/elm-review-json-to-elm/1.0.1/JsonToElm/) - Finds JSON samples in `Debug.todo` strings and offers to generate JSON decoders and encoders from them. The JSON part of the string needs to be prefixed with `@json` to allow the rule to detect it. 


## Configuration

```elm
module ReviewConfig exposing (config)

import JsonToElm
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ JsonToElm.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template alexkorban/elm-review-json-to-elm/example
```
