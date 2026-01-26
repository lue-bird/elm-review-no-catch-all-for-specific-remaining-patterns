module NoCatchAllForSpecificRemainingPatterns exposing (rule)

{-|

@docs rule

-}

import Dict
import Elm.Docs
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import FastDict
import FastSet
import Review.Fix
import Review.ModuleNameLookupTable
import Review.Project.Dependency
import Review.Rule


{-| Enforces listing all specific cases if possible

    config =
        [ NoCatchAllForSpecificRemainingPatterns.rule
            { onlyReportCatchAllIfEquivalentToSinglePattern = True }
        ]


### reported

    type Resource
        = Loaded String
        | FailedToLoad String
        | Loading

    -- with any configuration
    displayResource : Resource -> Ui
    displayResource resource =
        case resource of
            Loaded text ->
                Ui.text text

            FailedToLoad error ->
                Ui.error error

            _ ->
                Ui.spinner

    -- only with { onlyReportCatchAllIfEquivalentToSinglePattern = False }
    resourceIsLoaded : Resource -> Bool
    resourceIsLoaded resource =
        case resource of
            Loaded _ ->
                True

            _ ->
                False


## allowed

    -- with any configuration
    resourceIsLoaded : Resource -> Bool
    resourceIsLoaded resource =
        case resource of
            Loaded _ ->
                True

            FailedToLoad _ ->
                False

            Loading ->
                False

    -- only with { onlyReportCatchAllIfEquivalentToSinglePattern = True }
    displayResource : Resource -> Ui
    displayResource resource =
        case resource of
            Loaded text ->
                Ui.text text

            FailedToLoad error ->
                Ui.error error

            _ ->
                Ui.spinner

This works for variant, list and cons patterns.

-}
rule :
    { onlyReportCatchAllIfEquivalentToSinglePattern : Bool }
    -> Review.Rule.Rule
rule config =
    Review.Rule.newProjectRuleSchema "NoCatchAllForSpecificRemainingPatterns" initialContext
        |> Review.Rule.providesFixesForProjectRule
        |> Review.Rule.withDependenciesProjectVisitor
            (\deps ctx -> ( [], visitDependencies deps ctx ))
        |> Review.Rule.withModuleVisitor
            (\moduleVisitor ->
                moduleVisitor
                    |> Review.Rule.withDeclarationListVisitor
                        (\decls ctx -> ( [], visitDeclarations decls ctx ))
                    |> Review.Rule.withExpressionEnterVisitor
                        (\(Elm.Syntax.Node.Node _ expr) ctx -> ( visitExpression config expr ctx, ctx ))
            )
        |> Review.Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = projectToModuleContext
            , fromModuleToProject = moduleToProjectContext
            , foldProjectContexts = projectContextMerge
            }
        |> Review.Rule.withContextFromImportedModules
        |> Review.Rule.fromProjectRuleSchema


type alias ProjectContext =
    { choiceTypes :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            (List
                { name : String
                , variants : List { name : String, parameterCount : Int }
                }
            )
    }


type alias ModuleContext =
    { importedChoiceTypes :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            (List
                { name : String
                , variants : List { name : String, parameterCount : Int }
                }
            )
    , moduleDeclaredChoiceTypes :
        List
            { name : String
            , variants : List { name : String, parameterCount : Int }
            }
    , moduleOriginLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
    , sourceInRange : Elm.Syntax.Range.Range -> String
    }


initialContext : ProjectContext
initialContext =
    { choiceTypes = FastDict.empty
    }


projectContextMerge : ProjectContext -> ProjectContext -> ProjectContext
projectContextMerge a b =
    { choiceTypes = FastDict.union a.choiceTypes b.choiceTypes
    }


projectToModuleContext : Review.Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContext =
    Review.Rule.initContextCreator
        (\moduleOriginLookup sourceInRange projectContext ->
            { importedChoiceTypes = projectContext.choiceTypes
            , moduleDeclaredChoiceTypes = []
            , moduleOriginLookup = moduleOriginLookup
            , sourceInRange = sourceInRange
            }
        )
        |> Review.Rule.withModuleNameLookupTable
        |> Review.Rule.withSourceCodeExtractor


moduleToProjectContext : Review.Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContext =
    Review.Rule.initContextCreator
        (\moduleName moduleContext ->
            { choiceTypes =
                FastDict.singleton moduleName moduleContext.moduleDeclaredChoiceTypes
            }
        )
        |> Review.Rule.withModuleName


visitDependencies :
    Dict.Dict String Review.Project.Dependency.Dependency
    -> ProjectContext
    -> ProjectContext
visitDependencies dependenciesByName context =
    { choiceTypes =
        FastDict.union context.choiceTypes
            (dependenciesByName
                |> Dict.foldl
                    (\_ dependency soFar ->
                        FastDict.union
                            soFar
                            (dependency |> dependencyInterfaceChoiceTypes)
                    )
                    FastDict.empty
            )
    }


dependencyInterfaceChoiceTypes :
    Review.Project.Dependency.Dependency
    ->
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            (List
                { name : String
                , variants : List { name : String, parameterCount : Int }
                }
            )
dependencyInterfaceChoiceTypes dependency =
    dependency
        |> Review.Project.Dependency.modules
        |> List.foldl
            (\moduleInterface soFarInDependency ->
                case moduleInterface |> moduleInterfaceChoiceTypes of
                    [] ->
                        soFarInDependency

                    (_ :: _) as choiceTypes ->
                        FastDict.insert (moduleInterface.name |> String.split ".")
                            choiceTypes
                            soFarInDependency
            )
            FastDict.empty


moduleInterfaceChoiceTypes :
    Elm.Docs.Module
    ->
        List
            { name : String
            , variants : List { name : String, parameterCount : Int }
            }
moduleInterfaceChoiceTypes moduleInterface =
    moduleInterface.unions
        |> List.filterMap
            (\choiceTypeInterface ->
                case choiceTypeInterface.tags of
                    [] ->
                        -- opaque type
                        Nothing

                    variant0 :: variant1Up ->
                        Just
                            { name = choiceTypeInterface.name
                            , variants =
                                (variant0 :: variant1Up)
                                    |> List.map
                                        (\( variantName, variantParameters ) ->
                                            { name = variantName
                                            , parameterCount = variantParameters |> List.length
                                            }
                                        )
                            }
            )


visitDeclarations :
    List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration)
    -> ModuleContext
    -> ModuleContext
visitDeclarations declarations context =
    { moduleOriginLookup = context.moduleOriginLookup
    , sourceInRange = context.sourceInRange
    , importedChoiceTypes = context.importedChoiceTypes
    , moduleDeclaredChoiceTypes =
        declarations
            |> List.filterMap
                (\(Elm.Syntax.Node.Node _ declaration) ->
                    case declaration of
                        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                            Just
                                { name = choiceTypeDeclaration.name |> Elm.Syntax.Node.value
                                , variants =
                                    choiceTypeDeclaration.constructors
                                        |> List.map
                                            (\(Elm.Syntax.Node.Node _ variantDeclaration) ->
                                                { name = variantDeclaration.name |> Elm.Syntax.Node.value
                                                , parameterCount =
                                                    variantDeclaration.arguments |> List.length
                                                }
                                            )
                                }

                        Elm.Syntax.Declaration.FunctionDeclaration _ ->
                            Nothing

                        Elm.Syntax.Declaration.AliasDeclaration _ ->
                            Nothing

                        Elm.Syntax.Declaration.PortDeclaration _ ->
                            Nothing

                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                            Nothing

                        Elm.Syntax.Declaration.Destructuring _ _ ->
                            Nothing
                )
    }


visitExpression :
    { onlyReportCatchAllIfEquivalentToSinglePattern : Bool }
    -> Elm.Syntax.Expression.Expression
    -> ModuleContext
    -> List (Review.Rule.Error {})
visitExpression config expression context =
    case expression of
        Elm.Syntax.Expression.CaseExpression caseOf ->
            case caseOf.cases of
                [] ->
                    []

                [ _ ] ->
                    []

                case0 :: case1 :: case2Up ->
                    let
                        ( case1UpBeforeLast, ( lastCasePattern, Elm.Syntax.Node.Node lastCaseExpressionRange _ ) ) =
                            listFilledSplitOffLast ( case1, case2Up )
                    in
                    if lastCasePattern |> patternCatchesAll context then
                        let
                            previousCasesMaybeCatchFiniteNarrow : Maybe CatchFiniteNarrow
                            previousCasesMaybeCatchFiniteNarrow =
                                (case0 :: case1UpBeforeLast)
                                    |> listMapAndAllJust
                                        (\( casePattern, _ ) ->
                                            casePattern |> patternToFiniteNarrow context
                                        )
                                    |> Maybe.andThen patternFiniteNarrowsCombine
                        in
                        case previousCasesMaybeCatchFiniteNarrow of
                            Nothing ->
                                []

                            Just previousCasesCatchFiniteNarrow ->
                                badCaseOfToError config
                                    context
                                    { previousCasesCatchFiniteNarrow = previousCasesCatchFiniteNarrow
                                    , casedExpressionRange = caseOf.expression |> Elm.Syntax.Node.range
                                    , lastCasePattern = lastCasePattern
                                    , lastCaseExpressionRange = lastCaseExpressionRange
                                    }

                    else
                        []

        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.Application _ ->
            []

        Elm.Syntax.Expression.OperatorApplication _ _ _ _ ->
            []

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.IfBlock _ _ _ ->
            []

        Elm.Syntax.Expression.PrefixOperator _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Negation _ ->
            []

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.TupledExpression _ ->
            []

        Elm.Syntax.Expression.ParenthesizedExpression _ ->
            []

        Elm.Syntax.Expression.LetExpression _ ->
            []

        Elm.Syntax.Expression.LambdaExpression _ ->
            []

        Elm.Syntax.Expression.RecordExpr _ ->
            []

        Elm.Syntax.Expression.ListExpr _ ->
            []

        Elm.Syntax.Expression.RecordAccess _ _ ->
            []

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.RecordUpdateExpression _ _ ->
            []

        Elm.Syntax.Expression.GLSLExpression _ ->
            []


badCaseOfToError :
    { onlyReportCatchAllIfEquivalentToSinglePattern : Bool }
    -> ModuleContext
    ->
        { previousCasesCatchFiniteNarrow : CatchFiniteNarrow
        , lastCasePattern : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        , casedExpressionRange : Elm.Syntax.Range.Range
        , lastCaseExpressionRange : Elm.Syntax.Range.Range
        }
    -> List (Review.Rule.Error {})
badCaseOfToError config context caseOf =
    let
        casePatternsToReplaceLastWith : List String
        casePatternsToReplaceLastWith =
            case caseOf.previousCasesCatchFiniteNarrow of
                CatchList catchList ->
                    case catchList.allAfterElementCount of
                        Nothing ->
                            let
                                greatestSpecificElementCount : Int
                                greatestSpecificElementCount =
                                    catchList.specificElementCounts
                                        |> FastSet.getMax
                                        |> Maybe.withDefault 0

                                specificElementCountCasePatternsToAdd : List String
                                specificElementCountCasePatternsToAdd =
                                    FastSet.diff
                                        (FastSet.fromList (List.range 0 greatestSpecificElementCount))
                                        catchList.specificElementCounts
                                        |> FastSet.toList
                                        |> List.map printListPatternIgnoringElementsWithCount
                            in
                            specificElementCountCasePatternsToAdd
                                ++ [ String.repeat greatestSpecificElementCount "_ :: " ++ "_ :: _"
                                   ]

                        Just allAfterElementCount ->
                            FastSet.diff
                                (FastSet.fromList (List.range 0 (allAfterElementCount - 1)))
                                catchList.specificElementCounts
                                |> FastSet.toList
                                |> List.map printListPatternIgnoringElementsWithCount

                CatchChoiceType catchChoiceType ->
                    let
                        allVariantsToCatch : Maybe (List { parameterCount : Int, name : String })
                        allVariantsToCatch =
                            case catchChoiceType.variantNames |> FastSet.getMin of
                                Nothing ->
                                    Nothing

                                Just someCaughtUnqualifiedVariantName ->
                                    case catchChoiceType.moduleOrigin of
                                        [] ->
                                            context.moduleDeclaredChoiceTypes
                                                |> listMapAndFirstJust
                                                    (\choiceTypeDeclaration ->
                                                        if choiceTypeDeclaration.variants |> List.any (\variant -> variant.name == someCaughtUnqualifiedVariantName) then
                                                            Just choiceTypeDeclaration.variants

                                                        else
                                                            Nothing
                                                    )

                                        (_ :: _) as importedModuleOrigin ->
                                            case context.importedChoiceTypes |> FastDict.get importedModuleOrigin of
                                                Nothing ->
                                                    Nothing

                                                Just choiceTypesFromReferencedModule ->
                                                    choiceTypesFromReferencedModule
                                                        |> listMapAndFirstJust
                                                            (\choiceTypeDeclaration ->
                                                                if choiceTypeDeclaration.variants |> List.any (\variant -> variant.name == someCaughtUnqualifiedVariantName) then
                                                                    Just choiceTypeDeclaration.variants

                                                                else
                                                                    Nothing
                                                            )
                    in
                    allVariantsToCatch
                        |> Maybe.withDefault []
                        |> List.filterMap
                            (\variantToCatch ->
                                if catchChoiceType.variantNames |> FastSet.member variantToCatch.name then
                                    Nothing

                                else
                                    Just
                                        (referenceToString { qualification = catchChoiceType.qualification, name = variantToCatch.name }
                                            ++ String.repeat variantToCatch.parameterCount " _"
                                        )
                            )

        shouldNotBeReported : Bool
        shouldNotBeReported =
            config.onlyReportCatchAllIfEquivalentToSinglePattern
                && (case casePatternsToReplaceLastWith of
                        [ _ ] ->
                            False

                        _ :: _ :: _ ->
                            True

                        -- inexhaustive cases or other non-compiling code
                        [] ->
                            True
                   )
    in
    if shouldNotBeReported then
        []

    else
        let
            (Elm.Syntax.Node.Node lastCasePatternRange _) =
                caseOf.lastCasePattern

            caseIndentation : Int
            caseIndentation =
                lastCasePatternRange.start.column - 1

            expressionForEachAddedCasePrinted : String
            expressionForEachAddedCasePrinted =
                if caseOf.lastCasePattern |> Elm.Syntax.Node.value |> patternContainsVariables then
                    String.repeat (caseIndentation + 4) " "
                        ++ "let\n"
                        ++ String.repeat (caseIndentation + 8) " "
                        ++ context.sourceInRange lastCasePatternRange
                        ++ " =\n"
                        ++ String.repeat (caseIndentation + 12) " "
                        ++ stringIndentBy
                            8
                            (context.sourceInRange caseOf.casedExpressionRange)
                        ++ "\n"
                        ++ String.repeat (caseIndentation + 4) " "
                        ++ "in\n"
                        ++ String.repeat (caseIndentation + 4) " "
                        ++ context.sourceInRange caseOf.lastCaseExpressionRange

                else
                    String.repeat (caseIndentation + 4) " "
                        ++ context.sourceInRange caseOf.lastCaseExpressionRange
        in
        [ Review.Rule.errorWithFix
            { message = "catch-all can be replaced by more specific patterns"
            , details =
                [ "The last case in this case-of covers a finite number of specific patterns."
                , "Listing these explicitly might let you recognize cases you've missed now or in the future, so make sure to check each one (after applying the suggested fix)!"
                ]
            }
            lastCasePatternRange
            [ Review.Fix.replaceRangeBy
                { start = { row = lastCasePatternRange.start.row, column = 1 }
                , end = caseOf.lastCaseExpressionRange.end
                }
                (casePatternsToReplaceLastWith
                    |> List.map
                        (\casePatternToReplaceLastWith ->
                            String.repeat caseIndentation " "
                                ++ casePatternToReplaceLastWith
                                ++ " ->\n"
                                ++ expressionForEachAddedCasePrinted
                        )
                    |> String.join "\n\n"
                )
            ]
        ]


printListPatternIgnoringElementsWithCount : Int -> String
printListPatternIgnoringElementsWithCount elementCount =
    case elementCount of
        0 ->
            "[]"

        elementCountAtLeast1 ->
            "[ " ++ String.join ", " (List.repeat elementCountAtLeast1 "_") ++ " ]"


patternCatchesAll : ModuleContext -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> Bool
patternCatchesAll context (Elm.Syntax.Node.Node patternRange pattern) =
    case pattern of
        Elm.Syntax.Pattern.AllPattern ->
            True

        Elm.Syntax.Pattern.VarPattern _ ->
            True

        Elm.Syntax.Pattern.UnitPattern ->
            True

        Elm.Syntax.Pattern.RecordPattern _ ->
            True

        Elm.Syntax.Pattern.CharPattern _ ->
            False

        Elm.Syntax.Pattern.StringPattern _ ->
            False

        Elm.Syntax.Pattern.IntPattern _ ->
            False

        Elm.Syntax.Pattern.HexPattern _ ->
            False

        Elm.Syntax.Pattern.FloatPattern _ ->
            False

        Elm.Syntax.Pattern.UnConsPattern _ _ ->
            False

        Elm.Syntax.Pattern.ListPattern _ ->
            False

        Elm.Syntax.Pattern.AsPattern aliasedPattern _ ->
            patternCatchesAll context aliasedPattern

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            patternCatchesAll context inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            parts |> List.all (\part -> part |> patternCatchesAll context)

        Elm.Syntax.Pattern.NamedPattern variantName attachmentPatterns ->
            -- && but TCO
            let
                isSingleVariant : Bool
                isSingleVariant =
                    case Review.ModuleNameLookupTable.moduleNameAt context.moduleOriginLookup patternRange of
                        Nothing ->
                            False

                        Just [] ->
                            context.moduleDeclaredChoiceTypes
                                |> List.any
                                    (\choiceTypeDeclaration ->
                                        case choiceTypeDeclaration.variants of
                                            [ onlyVariant ] ->
                                                onlyVariant.name == variantName.name

                                            _ :: _ :: _ ->
                                                False

                                            -- invalid syntax
                                            [] ->
                                                False
                                    )

                        Just (moduleNamePart0 :: moduleNamePart1Up) ->
                            case context.importedChoiceTypes |> FastDict.get (moduleNamePart0 :: moduleNamePart1Up) of
                                Nothing ->
                                    False

                                Just variantOriginModuleDeclaredChoiceTypes ->
                                    variantOriginModuleDeclaredChoiceTypes
                                        |> List.any
                                            (\choiceTypeDeclaration ->
                                                case choiceTypeDeclaration.variants of
                                                    [ onlyVariant ] ->
                                                        onlyVariant.name == variantName.name

                                                    _ :: _ :: _ ->
                                                        False

                                                    -- invalid syntax
                                                    [] ->
                                                        False
                                            )
            in
            if isSingleVariant then
                attachmentPatterns |> List.all (\attachmentPattern -> attachmentPattern |> patternCatchesAll context)

            else
                False


patternContainsVariables : Elm.Syntax.Pattern.Pattern -> Bool
patternContainsVariables pattern =
    case pattern of
        Elm.Syntax.Pattern.VarPattern _ ->
            True

        Elm.Syntax.Pattern.AsPattern _ _ ->
            True

        Elm.Syntax.Pattern.RecordPattern _ ->
            True

        Elm.Syntax.Pattern.AllPattern ->
            False

        Elm.Syntax.Pattern.UnitPattern ->
            False

        Elm.Syntax.Pattern.CharPattern _ ->
            False

        Elm.Syntax.Pattern.StringPattern _ ->
            False

        Elm.Syntax.Pattern.IntPattern _ ->
            False

        Elm.Syntax.Pattern.HexPattern _ ->
            False

        Elm.Syntax.Pattern.FloatPattern _ ->
            False

        Elm.Syntax.Pattern.UnConsPattern (Elm.Syntax.Node.Node _ headPattern) (Elm.Syntax.Node.Node _ tailPattern) ->
            -- || but TCO
            if patternContainsVariables headPattern then
                True

            else
                patternContainsVariables tailPattern

        Elm.Syntax.Pattern.ListPattern elements ->
            elements |> List.any (\(Elm.Syntax.Node.Node _ elementPattern) -> elementPattern |> patternContainsVariables)

        Elm.Syntax.Pattern.TuplePattern partPatterns ->
            partPatterns |> List.any (\(Elm.Syntax.Node.Node _ partPattern) -> partPattern |> patternContainsVariables)

        Elm.Syntax.Pattern.NamedPattern _ attachmentPatterns ->
            attachmentPatterns |> List.any (\(Elm.Syntax.Node.Node _ part) -> part |> patternContainsVariables)

        Elm.Syntax.Pattern.ParenthesizedPattern (Elm.Syntax.Node.Node _ inParens) ->
            patternContainsVariables inParens


type PatternFiniteNarrow
    = VariantPattern
        { qualification : Elm.Syntax.ModuleName.ModuleName
        , moduleOrigin : Elm.Syntax.ModuleName.ModuleName
        , unqualifiedName : String
        }
    | ListPattern { elementCount : Int, hasTail : Bool }


patternToFiniteNarrow :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> Maybe PatternFiniteNarrow
patternToFiniteNarrow context (Elm.Syntax.Node.Node patternRange pattern) =
    case pattern of
        Elm.Syntax.Pattern.AllPattern ->
            Nothing

        Elm.Syntax.Pattern.VarPattern _ ->
            Nothing

        Elm.Syntax.Pattern.UnitPattern ->
            Nothing

        Elm.Syntax.Pattern.CharPattern _ ->
            Nothing

        Elm.Syntax.Pattern.StringPattern _ ->
            Nothing

        Elm.Syntax.Pattern.IntPattern _ ->
            Nothing

        Elm.Syntax.Pattern.HexPattern _ ->
            Nothing

        Elm.Syntax.Pattern.FloatPattern _ ->
            Nothing

        Elm.Syntax.Pattern.RecordPattern _ ->
            Nothing

        Elm.Syntax.Pattern.NamedPattern variantName attachmentPatterns ->
            if attachmentPatterns |> List.all (\attachmentPattern -> attachmentPattern |> patternCatchesAll context) then
                case Review.ModuleNameLookupTable.moduleNameAt context.moduleOriginLookup patternRange of
                    Nothing ->
                        Nothing

                    Just moduleOrigin ->
                        Just
                            (VariantPattern
                                { qualification = variantName.moduleName
                                , moduleOrigin = moduleOrigin
                                , unqualifiedName = variantName.name
                                }
                            )

            else
                Nothing

        Elm.Syntax.Pattern.TuplePattern _ ->
            -- TODO for now
            Nothing

        Elm.Syntax.Pattern.AsPattern aliasedPattern _ ->
            patternToFiniteNarrow context aliasedPattern

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            patternToFiniteNarrow context inParens

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            if elementPatterns |> List.all (\elementPattern -> elementPattern |> patternCatchesAll context) then
                Just
                    (ListPattern
                        { elementCount = elementPatterns |> List.length
                        , hasTail = False
                        }
                    )

            else
                Nothing

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            let
                expanded :
                    { elements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                    , tail : Maybe (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                    }
                expanded =
                    consPatternExpand { head = headPattern, tail = tailPattern }
            in
            if expanded.elements |> List.all (\elementPattern -> elementPattern |> patternCatchesAll context) then
                Just
                    (ListPattern
                        { elementCount = expanded.elements |> List.length
                        , hasTail =
                            case expanded.tail of
                                Nothing ->
                                    False

                                Just _ ->
                                    True
                        }
                    )

            else
                Nothing


consPatternExpand :
    { head : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    }
    ->
        { elements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , tail : Maybe (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        }
consPatternExpand consPattern =
    let
        tailExpanded :
            { elements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
            , tail : Maybe (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
            }
        tailExpanded =
            tailPatternExpand consPattern.tail
    in
    { elements = consPattern.head :: tailExpanded.elements
    , tail = tailExpanded.tail
    }


tailPatternExpand :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        { elements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , tail : Maybe (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        }
tailPatternExpand patternNode =
    case patternNode |> Elm.Syntax.Node.value of
        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            { elements = elementPatterns, tail = Nothing }

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            consPatternExpand { head = headPattern, tail = tailPattern }

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            tailPatternExpand inParens

        Elm.Syntax.Pattern.AsPattern aliasedPatternNode _ ->
            tailPatternExpand aliasedPatternNode

        Elm.Syntax.Pattern.AllPattern ->
            { elements = [], tail = Just patternNode }

        Elm.Syntax.Pattern.VarPattern _ ->
            { elements = [], tail = Just patternNode }

        -- all patterns below won't type check
        Elm.Syntax.Pattern.UnitPattern ->
            { elements = [], tail = Just patternNode }

        Elm.Syntax.Pattern.CharPattern _ ->
            { elements = [], tail = Just patternNode }

        Elm.Syntax.Pattern.StringPattern _ ->
            { elements = [], tail = Just patternNode }

        Elm.Syntax.Pattern.IntPattern _ ->
            { elements = [], tail = Just patternNode }

        Elm.Syntax.Pattern.HexPattern _ ->
            { elements = [], tail = Just patternNode }

        Elm.Syntax.Pattern.FloatPattern _ ->
            { elements = [], tail = Just patternNode }

        Elm.Syntax.Pattern.TuplePattern _ ->
            { elements = [], tail = Just patternNode }

        Elm.Syntax.Pattern.RecordPattern _ ->
            { elements = [], tail = Just patternNode }

        Elm.Syntax.Pattern.NamedPattern _ _ ->
            { elements = [], tail = Just patternNode }


type CatchFiniteNarrow
    = CatchChoiceType
        { qualification : Elm.Syntax.ModuleName.ModuleName
        , moduleOrigin : Elm.Syntax.ModuleName.ModuleName
        , variantNames : FastSet.Set String
        }
    | CatchList
        { specificElementCounts : FastSet.Set Int
        , allAfterElementCount : Maybe Int
        }


patternFiniteNarrowsCombine : List PatternFiniteNarrow -> Maybe CatchFiniteNarrow
patternFiniteNarrowsCombine patternFiniteNarrows =
    case patternFiniteNarrows of
        [] ->
            Nothing

        patternFiniteNarrow0 :: patternFiniteNarrow1Up ->
            patternFiniteNarrow1Up
                |> List.foldl
                    (\patternFiniteNarrow soFar ->
                        case soFar of
                            Nothing ->
                                Nothing

                            Just soFarCatchFiniteNarrow ->
                                catchFiniteNarrowMerge soFarCatchFiniteNarrow
                                    (patternFiniteNarrow |> patternFiniteNarrowCatch)
                    )
                    (Just (patternFiniteNarrow0 |> patternFiniteNarrowCatch))


catchFiniteNarrowMerge : CatchFiniteNarrow -> CatchFiniteNarrow -> Maybe CatchFiniteNarrow
catchFiniteNarrowMerge a b =
    case a of
        CatchChoiceType aCatchChoiceType ->
            case b of
                CatchList _ ->
                    Nothing

                CatchChoiceType bCatchChoiceType ->
                    if aCatchChoiceType.moduleOrigin == bCatchChoiceType.moduleOrigin then
                        Just
                            (CatchChoiceType
                                { qualification = aCatchChoiceType.qualification
                                , moduleOrigin = aCatchChoiceType.moduleOrigin
                                , variantNames =
                                    FastSet.union
                                        aCatchChoiceType.variantNames
                                        bCatchChoiceType.variantNames
                                }
                            )

                    else
                        Nothing

        CatchList aCatchList ->
            case b of
                CatchChoiceType _ ->
                    Nothing

                CatchList bCatchList ->
                    Just
                        (CatchList
                            { specificElementCounts =
                                FastSet.union
                                    aCatchList.specificElementCounts
                                    bCatchList.specificElementCounts
                            , allAfterElementCount =
                                case bCatchList.allAfterElementCount of
                                    Nothing ->
                                        aCatchList.allAfterElementCount

                                    Just bAllAfterElementCount ->
                                        case aCatchList.allAfterElementCount of
                                            Nothing ->
                                                Just bAllAfterElementCount

                                            Just aAllAfterElementCount ->
                                                Just (Basics.min aAllAfterElementCount bAllAfterElementCount)
                            }
                        )


patternFiniteNarrowCatch : PatternFiniteNarrow -> CatchFiniteNarrow
patternFiniteNarrowCatch patternFiniteNarrow =
    case patternFiniteNarrow of
        VariantPattern variantPattern ->
            CatchChoiceType
                { qualification = variantPattern.qualification
                , moduleOrigin = variantPattern.moduleOrigin
                , variantNames = FastSet.singleton variantPattern.unqualifiedName
                }

        ListPattern listPattern ->
            if listPattern.hasTail then
                CatchList
                    { specificElementCounts = FastSet.empty
                    , allAfterElementCount = Just listPattern.elementCount
                    }

            else
                CatchList
                    { specificElementCounts =
                        FastSet.singleton listPattern.elementCount
                    , allAfterElementCount = Nothing
                    }


referenceToString : { qualification : Elm.Syntax.ModuleName.ModuleName, name : String } -> String
referenceToString reference =
    case reference.qualification of
        [] ->
            reference.name

        moduleNamePart0 :: moduleNamePart1 ->
            ((moduleNamePart0 :: moduleNamePart1) |> String.join ".")
                ++ "."
                ++ reference.name


stringIndentBy : Int -> String -> String
stringIndentBy additionalIndentation string =
    case string |> String.lines of
        [] ->
            ""

        line0 :: line1Up ->
            let
                additionalIndentationString : String
                additionalIndentationString =
                    String.repeat additionalIndentation " "
            in
            line0
                ++ (line1Up
                        |> List.map (\line -> "\n" ++ additionalIndentationString ++ line)
                        |> String.concat
                   )


listMapAndFirstJust : (a -> Maybe b) -> List a -> Maybe b
listMapAndFirstJust elementToMaybeFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case elementToMaybeFound head of
                (Just _) as justFound ->
                    justFound

                Nothing ->
                    listMapAndFirstJust elementToMaybeFound tail


listMapAndAllJust : (a -> Maybe b) -> List a -> Maybe (List b)
listMapAndAllJust elementToMaybe list =
    case list of
        [] ->
            Just []

        head :: tail ->
            case elementToMaybe head of
                Nothing ->
                    Nothing

                Just headValue ->
                    case listMapAndAllJust elementToMaybe tail of
                        Nothing ->
                            Nothing

                        Just tailValues ->
                            Just (headValue :: tailValues)


listFilledSplitOffLast : ( a, List a ) -> ( List a, a )
listFilledSplitOffLast ( head, tail ) =
    case tail of
        [] ->
            ( [], head )

        tailHead :: tailTail ->
            let
                ( tailBeforeLast, last ) =
                    listFilledSplitOffLast ( tailHead, tailTail )
            in
            ( head :: tailBeforeLast, last )
