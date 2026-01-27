#### 2.0.1
  - correct documentation

## 2.0.0
  - add configuration `onlyReportCatchAllIfEquivalentToSinglePattern`
    to `NoCatchAllForSpecificRemainingPatterns.rule`.
    Thanks for [suggesting it, @jfmengels](https://github.com/lue-bird/elm-review-no-catch-all-for-specific-remaining-patterns/issues/4)
  - catch-all patterns with variables are now fixed to `(..specific..) as yourVariable -> yourResult`
    instead of `..specific.. -> let yourVariable = yourBetweenCaseAndOf in yourResult`.
    This avoids unnecessary duplicate computation.
    Thanks for [suggesting it, @jfmengels](https://github.com/lue-bird/elm-review-no-catch-all-for-specific-remaining-patterns/issues/3)
  - support when earlier case patterns use record patterns
  - go through `as` and parenthesized patterns when checking for list patterns

#### 1.0.2
  - fix bug where modules exposing multiple `type`s could "randomly"
    lead to the catch-all branch being removed instead of replaced with the remaining variants

#### 1.0.1
  - fix bug where variant and list patterns including narrowing arguments would lead to invalid fix. Thanks [Simon Lydell](https://github.com/lydell) for [reporting](https://github.com/lue-bird/elm-review-no-catch-all-for-specific-remaining-patterns/issues/1)!
