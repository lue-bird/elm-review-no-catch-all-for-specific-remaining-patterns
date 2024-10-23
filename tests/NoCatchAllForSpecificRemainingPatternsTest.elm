module NoCatchAllForSpecificRemainingPatternsTest exposing (all)

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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectNoErrors
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
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
    case Nothing of
        Just n ->
            n
        
        Nothing ->
            0
"""
                        ]
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
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
    case Maybe.Nothing of
        Maybe.Just n ->
            n
        
        Maybe.Nothing ->
            0
"""
                        ]
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
                        NoCatchAllForSpecificRemainingPatterns.rule
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
        , Test.test "report _ case with module declared choice type" <|
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
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
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
        , Test.test "report _ case with _::_" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        e0 :: e1Up ->
            e0 :: e1Up
        
        _ ->
            []
"""
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
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
        e0 :: e1Up ->
            e0 :: e1Up
        
        [] ->
            []
"""
                        ]
        , Test.test "report _ case with _::_::_" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        e0 :: e1 :: el2Up ->
            Just (e0 :: e1 :: el2Up)
        
        _ ->
            Nothing
"""
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
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
        , Test.test "report variable case with []" <|
            \() ->
                """module A exposing (..)
a list =
    case list of
        [] ->
            Nothing
        
        listFilled ->
            Just listFilled
"""
                    |> Review.Test.run NoCatchAllForSpecificRemainingPatterns.rule
                    |> Review.Test.expectErrors
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
        
        _ :: _ ->
            let
                listFilled =
                    list
            in
            Just listFilled
"""
                        ]
        ]
