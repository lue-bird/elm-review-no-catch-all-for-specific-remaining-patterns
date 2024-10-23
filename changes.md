#### 1.0.2
  - fix bug where modules exposing multiple `type`s could "randomly"
    lead to the catch-all branch being removed instead of replaced with the remaining variants

#### 1.0.1
  - fix bug where variant and list patterns including narrowing arguments would lead to invalid fix. Thanks [Simon Lydell](https://github.com/lydell) for [reporting](https://github.com/lue-bird/elm-review-no-catch-all-for-specific-remaining-patterns/issues/1)!
