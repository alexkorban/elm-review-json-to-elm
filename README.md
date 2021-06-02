# elm-review-json2elm

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`Json2Elm`](https://package.elm-lang.org/packages/alexkorban/elm-review-json2elm/1.0.0/Json2Elm) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import Json2Elm
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Json2Elm.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template alexkorban/elm-review-json2elm/example
```
