module NoCatchAllForSpecificRemainingPatternsTest exposing (all)

import Expect
import NoCatchAllForSpecificRemainingPatterns
import Review.Project
import Review.Test
import Review.Test.Dependencies
import Test


all : Test.Test
all =
    Test.describe "NoCatchAllForSpecificRemainingPatterns"
        [ Test.test "allow: cases are all imported variants" <|
            \() ->
                """module A exposing (..)
a =
    case Nothing of
        Nothing ->
            0
        
        Just _ ->
            1
"""
                    |> runWithAnyConfiguration
                        Review.Test.expectNoErrors
        , Test.test "allow: catch-all catches infinite cases" <|
            \() ->
                """module A exposing (..)
a =
    case 0 of
        0 ->
            0
        
        _ ->
            1
"""
                    |> runWithAnyConfiguration
                        Review.Test.expectNoErrors
        , Test.test "allow: single-case" <|
            \() ->
                """module A exposing (..)
type Wrap filling
    = Yum filling
a =
    case Wrap 0 of
        Wrap filling ->
            filling
"""
                    |> runWithAnyConfiguration
                        Review.Test.expectNoErrors
        , Test.test "allow: imported variant with non-catch-all attachment" <|
            \() ->
                """module A exposing (..)
a =
    case Nothing of
        Just 1 ->
            1
        
        _ ->
            0
"""
                    |> runWithAnyConfiguration
                        Review.Test.expectNoErrors
        , Test.test "allow: list with non-catch-all element" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        [ 1 ] ->
            1
        
        _ ->
            0
"""
                    |> runWithAnyConfiguration
                        Review.Test.expectNoErrors
        , Test.test "allow: cases are [] and _::_" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        [] ->
            0
        
        _ :: _ ->
            1
"""
                    |> runWithAnyConfiguration
                        Review.Test.expectNoErrors
        , Test.test "allow: cases are [] and [_] and _::_::_" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        [] ->
            0
        
        [ _ ] ->
            1
        
        _ :: _ :: _ ->
            2
"""
                    |> runWithAnyConfiguration
                        Review.Test.expectNoErrors
        , Test.test "allow: cases are [] and _::[] and _::_::_" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        [] ->
            0
        
        _ :: [] ->
            1
        
        _ :: _ :: _ ->
            2
"""
                    |> runWithAnyConfiguration
                        Review.Test.expectNoErrors
        , Test.test "allow: cases are [] and [_] and _::_" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        [] ->
            0
        
        [ _ ] ->
            1
        
        _ :: _ ->
            2
"""
                    |> runWithAnyConfiguration
                        Review.Test.expectNoErrors
        , Test.test "report _ case with imported choice type" <|
            \() ->
                """module A exposing (..)
a =
    case Nothing of
        Just n ->
            n
        
        _ ->
            0
"""
                    |> runWithAnyConfiguration
                        (Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "catch-all can be replaced by more specific patterns"
                                , details =
                                    [ "The last case in this case-of covers a finite number of specific patterns."
                                    , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                    ]
                                , under = "_"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
a =
    case Nothing of
        Just n ->
            n
        
        Nothing ->
            0
"""
                            ]
                        )
        , Test.test "report _ case with imported choice type, non-trivial variant that matches all sub-values" <|
            \() ->
                """module A exposing (..)
type SingleVariant = SingleVariant ( { n : () }, () )
a =
    case Nothing of
        Just (SingleVariant ( ({ n }) as record, () )) ->
            n
        
        _ ->
            0
"""
                    |> runWithAnyConfiguration
                        (Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "catch-all can be replaced by more specific patterns"
                                , details =
                                    [ "The last case in this case-of covers a finite number of specific patterns."
                                    , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                    ]
                                , under = "_"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
type SingleVariant = SingleVariant ( { n : () }, () )
a =
    case Nothing of
        Just (SingleVariant ( ({ n }) as record, () )) ->
            n
        
        Nothing ->
            0
"""
                            ]
                        )
        , Test.test "report _ case with imported choice type fully qualified" <|
            \() ->
                """module A exposing (..)
a =
    case Maybe.Nothing of
        Maybe.Just n ->
            n
        
        _ ->
            0
"""
                    |> runWithAnyConfiguration
                        (Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "catch-all can be replaced by more specific patterns"
                                , details =
                                    [ "The last case in this case-of covers a finite number of specific patterns."
                                    , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                    ]
                                , under = "_"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
a =
    case Maybe.Nothing of
        Maybe.Just n ->
            n
        
        Maybe.Nothing ->
            0
"""
                            ]
                        )
        , Test.test "report _ case with dependency imported choice type fully qualified" <|
            \() ->
                """module A exposing (..)
import Parser.Advanced
continueLoop step =
    case step of
        Parser.Advanced.Done _ ->
            Debug.todo ""

        _ ->
            Debug.todo ""
"""
                    |> Review.Test.runWithProjectData
                        (Review.Project.new
                            |> Review.Project.addDependency Review.Test.Dependencies.elmParser
                        )
                        (NoCatchAllForSpecificRemainingPatterns.rule
                            { onlyReportCatchAllIfEquivalentToSinglePattern = False }
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "catch-all can be replaced by more specific patterns"
                            , details =
                                [ "The last case in this case-of covers a finite number of specific patterns."
                                , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                ]
                            , under = "_"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 8, column = 9 }, end = { row = 8, column = 10 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Parser.Advanced
continueLoop step =
    case step of
        Parser.Advanced.Done _ ->
            Debug.todo ""

        Parser.Advanced.Loop _ ->
            Debug.todo ""
"""
                        ]
        , Test.test "should not report _ case with module declared choice type when onlyReportCatchAllIfEquivalentToSinglePattern = True" <|
            \() ->
                """module A exposing (..)
type Resource
    = Loaded String
    | FailedToLoad String
    | Loading
a =
    case Loading of
        Loaded text ->
            not (String.isEmpty text)

        _ ->
            False
"""
                    |> Review.Test.run
                        (NoCatchAllForSpecificRemainingPatterns.rule
                            { onlyReportCatchAllIfEquivalentToSinglePattern = True }
                        )
                    |> Review.Test.expectNoErrors
        , Test.test "report _ case with module declared choice type when onlyReportCatchAllIfEquivalentToSinglePattern = False" <|
            \() ->
                """module A exposing (..)
type Resource
    = Loaded String
    | FailedToLoad String
    | Loading
a =
    case Loading of
        Loaded text ->
            not (String.isEmpty text)

        _ ->
            False
"""
                    |> Review.Test.run
                        (NoCatchAllForSpecificRemainingPatterns.rule
                            { onlyReportCatchAllIfEquivalentToSinglePattern = False }
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "catch-all can be replaced by more specific patterns"
                            , details =
                                [ "The last case in this case-of covers a finite number of specific patterns."
                                , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
type Resource
    = Loaded String
    | FailedToLoad String
    | Loading
a =
    case Loading of
        Loaded text ->
            not (String.isEmpty text)

        FailedToLoad _ ->
            False

        Loading ->
            False
"""
                        ]
        , Test.test "report _ case with _::_"
            (\() ->
                """module A exposing (..)
a =
    case [] of
        e0 :: e1Up ->
            e0 :: e1Up
        
        _ ->
            []
"""
                    |> runWithAnyConfiguration
                        (Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "catch-all can be replaced by more specific patterns"
                                , details =
                                    [ "The last case in this case-of covers a finite number of specific patterns."
                                    , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                    ]
                                , under = "_"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
a =
    case [] of
        e0 :: e1Up ->
            e0 :: e1Up
        
        [] ->
            []
"""
                            ]
                        )
            )
        , Test.test "report not _ case with _::_::_ when onlyReportCatchAllIfEquivalentToSinglePattern = True" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        e0 :: e1 :: el2Up ->
            Just (e0 :: e1 :: el2Up)
        
        _ ->
            Nothing
"""
                    |> Review.Test.run
                        (NoCatchAllForSpecificRemainingPatterns.rule
                            { onlyReportCatchAllIfEquivalentToSinglePattern = True }
                        )
                    |> Review.Test.expectNoErrors
        , Test.test "report _ case with _::_ when onlyReportCatchAllIfEquivalentToSinglePattern = False"
            (\() ->
                """module A exposing (..)
a =
    case [] of
        e0 :: e1Up ->
            e0 :: e1Up
        
        _ ->
            []
"""
                    |> runWithAnyConfiguration
                        (Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "catch-all can be replaced by more specific patterns"
                                , details =
                                    [ "The last case in this case-of covers a finite number of specific patterns."
                                    , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                    ]
                                , under = "_"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
a =
    case [] of
        e0 :: e1Up ->
            e0 :: e1Up
        
        [] ->
            []
"""
                            ]
                        )
            )
        , Test.test "should not report _ case with _::_::_ when onlyReportCatchAllIfEquivalentToSinglePattern = True" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        e0 :: e1 :: el2Up ->
            Just (e0 :: e1 :: el2Up)
        
        _ ->
            Nothing
"""
                    |> Review.Test.run
                        (NoCatchAllForSpecificRemainingPatterns.rule
                            { onlyReportCatchAllIfEquivalentToSinglePattern = True }
                        )
                    |> Review.Test.expectNoErrors
        , Test.test "report _ case with _::_::_ when onlyReportCatchAllIfEquivalentToSinglePattern = False" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        e0 :: e1 :: el2Up ->
            Just (e0 :: e1 :: el2Up)
        
        _ ->
            Nothing
"""
                    |> Review.Test.run
                        (NoCatchAllForSpecificRemainingPatterns.rule
                            { onlyReportCatchAllIfEquivalentToSinglePattern = False }
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "catch-all can be replaced by more specific patterns"
                            , details =
                                [ "The last case in this case-of covers a finite number of specific patterns."
                                , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    case [] of
        e0 :: e1 :: el2Up ->
            Just (e0 :: e1 :: el2Up)
        
        [] ->
            Nothing

        [ _ ] ->
            Nothing
"""
                        ]
        , Test.test "report _ case with _::(_::_) when onlyReportCatchAllIfEquivalentToSinglePattern = False" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        e0 :: (e1 :: el2Up) ->
            Just (e0 :: e1 :: el2Up)
        
        _ ->
            Nothing
"""
                    |> Review.Test.run
                        (NoCatchAllForSpecificRemainingPatterns.rule
                            { onlyReportCatchAllIfEquivalentToSinglePattern = False }
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "catch-all can be replaced by more specific patterns"
                            , details =
                                [ "The last case in this case-of covers a finite number of specific patterns."
                                , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    case [] of
        e0 :: (e1 :: el2Up) ->
            Just (e0 :: e1 :: el2Up)
        
        [] ->
            Nothing

        [ _ ] ->
            Nothing
"""
                        ]
        , Test.test "report _ case with _::((_::_) as variable) when onlyReportCatchAllIfEquivalentToSinglePattern = False" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        e0 :: ((e1 :: el2Up) as el1Up) ->
            Just (e0 :: e1 :: el2Up)
        
        _ ->
            Nothing
"""
                    |> Review.Test.run
                        (NoCatchAllForSpecificRemainingPatterns.rule
                            { onlyReportCatchAllIfEquivalentToSinglePattern = False }
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "catch-all can be replaced by more specific patterns"
                            , details =
                                [ "The last case in this case-of covers a finite number of specific patterns."
                                , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    case [] of
        e0 :: ((e1 :: el2Up) as el1Up) ->
            Just (e0 :: e1 :: el2Up)
        
        [] ->
            Nothing

        [ _ ] ->
            Nothing
"""
                        ]
        , Test.test "report variable pattern case with []"
            (\() ->
                """module A exposing (..)
a list =
    case list of
        [] ->
            Nothing
        
        listFilled ->
            Just listFilled
"""
                    |> runWithAnyConfiguration
                        (Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "catch-all can be replaced by more specific patterns"
                                , details =
                                    [ "The last case in this case-of covers a finite number of specific patterns."
                                    , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                    ]
                                , under = "listFilled"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 7, column = 9 }, end = { row = 7, column = 19 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
a list =
    case list of
        [] ->
            Nothing
        
        (_ :: _) as listFilled ->
            Just listFilled
"""
                            ]
                        )
            )
        , Test.test "report variable pattern in as pattern case with []"
            (\() ->
                """module A exposing (..)
a list =
    case list of
        [] ->
            Nothing
        
        listFilled as list ->
            Just listFilled
"""
                    |> runWithAnyConfiguration
                        (Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "catch-all can be replaced by more specific patterns"
                                , details =
                                    [ "The last case in this case-of covers a finite number of specific patterns."
                                    , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                    ]
                                , under = "listFilled as list"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
a list =
    case list of
        [] ->
            Nothing
        
        ((_ :: _) as listFilled) as list ->
            Just listFilled
"""
                            ]
                        )
            )
        , Test.test "report parenthesized all pattern in as pattern in as pattern case with []"
            (\() ->
                """module A exposing (..)
a list =
    case list of
        [] ->
            Nothing
        
        ((_ as listFilled)) as list ->
            Just listFilled
"""
                    |> runWithAnyConfiguration
                        (Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "catch-all can be replaced by more specific patterns"
                                , details =
                                    [ "The last case in this case-of covers a finite number of specific patterns."
                                    , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                                    ]
                                , under = "((_ as listFilled)) as list"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
a list =
    case list of
        [] ->
            Nothing
        
        ((_ :: _) as listFilled) as list ->
            Just listFilled
"""
                            ]
                        )
            )
        ]


runWithAnyConfiguration :
    (Review.Test.ReviewResult -> Expect.Expectation)
    -> String
    -> Expect.Expectation
runWithAnyConfiguration expect test =
    Expect.all
        [ \() ->
            test
                |> Review.Test.run
                    (NoCatchAllForSpecificRemainingPatterns.rule
                        { onlyReportCatchAllIfEquivalentToSinglePattern = True }
                    )
                |> expect
        , \() ->
            test
                |> Review.Test.run
                    (NoCatchAllForSpecificRemainingPatterns.rule
                        { onlyReportCatchAllIfEquivalentToSinglePattern = False }
                    )
                |> expect
        ]
        ()
