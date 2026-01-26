The [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule
[`NoCatchAllForSpecificRemainingPatterns.rule`](https://package.elm-lang.org/packages/lue-bird/elm-review-no-catch-all-for-specific-remaining-patterns/1.0.0/NoCatchAllForSpecificRemainingPatterns#rule) enforces that all specific cases are listed if possible.

Below would be reported for example

```elm
type Resource
    = Loaded String
    | FailedToLoad String
    | Loading

displayResource : Resource -> String
displayResource resource =
    case resource of
        Loaded text ->
            Ui.text text

        FailedToLoad error ->
            Ui.error error
        
        -- will be fixed to Loading ->
        _ ->
            Ui.spinner
```

Try it:

```bash
elm-review --template lue-bird/elm-review-no-catch-all-for-specific-remaining-patterns/example
```

If you like it, add it, add it to your configuration

```elm
module ReviewConfig exposing (config)

import NoCatchAllForSpecificRemainingPatterns
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoCatchAllForSpecificRemainingPatterns.rule
        { onlyReportCatchAllIfEquivalentToSinglePattern = True }
    ]
```

## why?

  - no accidentally missed cases:
    ```elm
    patternIsZero expression =
        case expression of
            Elm.Syntax.Pattern.IntPattern int ->
                int == 0
            
            _ ->
                -- accidentally missed HexPattern!
                False
    ```

  - a reminder if another variant is added in the future:
    ```elm
    type Resource
        = Loaded String
        | FailedToLoad String
        | Loading
        -- added
        | PartiallyLoaded String

    displayResource : Resource -> String
    displayResource resource =
        case resource of
            Loaded text ->
                Ui.text text
            
            _ ->
                -- we actually should display partially loaded
                -- but the compiler doesn't remind us
                Ui.spinner
    ```

  - less jumping around to understand the code
    ```elm
    case result of
        Ok geoLocation ->
            let
                ...
            in
            a
                bunchOf
                code

        _ ->
            -- what is _ ?
            0
    ```


## not implemented (yet?)
  - nested patterns like in tuples, variants or lists
