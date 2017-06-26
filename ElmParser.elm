module ElmParser exposing (..)



import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Parser.LowLevel exposing (..)
import Char
import Set exposing (Set)





type alias ModuleDeclaration =
    { range : Range
    , sourceTag : SourceTag
    , moduleName : ModuleName
    , exposing_ : ExposingStatement
    }


type ModuleName
    = ModuleName PreWSSep (Range, UpperCaseIdentifier)


moduleDeclaration : Parser ModuleDeclaration
moduleDeclaration =
    withRange (\range (sourceTag, moduleName, exposing_) -> ModuleDeclaration range sourceTag moduleName exposing_) <|
        succeed (\sourceTag moduleName exposing_ -> (sourceTag, moduleName, exposing_))
            |= sourceTag
            |. symbol "module"
            |= moduleName
            |= exposingStatement


moduleDeclarationToSource : ModuleDeclaration -> String
moduleDeclarationToSource {sourceTag, moduleName, exposing_} =
    sourceTagToSource sourceTag
        ++ moduleNameToSource moduleName
        ++ exposingStatementToSource exposing_



type SourceTag
  = Normal
  | Effect PostWSSep
  | Port PostWSSep



sourceTag : Parser SourceTag
sourceTag =
    oneOf
        [ succeed Effect
            |. symbol "effect"
            |= map PostWSSep mandatoryWSSep
        , succeed Port
            |. symbol "port"
            |= map PostWSSep mandatoryWSSep
        , succeed Normal
        ]

sourceTagToSource : SourceTag -> String
sourceTagToSource sourceTag =
    case sourceTag of
        Normal ->
            "module"

        Effect (PostWSSep wsList) ->
            "effect" ++ wsListToSource wsList ++ "module"

        Port (PostWSSep wsList) ->
            "port" ++ wsListToSource wsList ++ "module"




moduleName : Parser ModuleName
moduleName =
    succeed ModuleName
        |= map PreWSSep mandatoryWSSep
        |= (withRange (,) moduleNameHelper)


moduleNameHelper : Parser UpperCaseIdentifier
moduleNameHelper =
    succeed joinUppers
        |= capVar
        |= oneOf
            [ keep (Exactly 1) (\c -> c == '.')
                |> map UpperCaseIdentifier
                |> andThen (\c -> (map (joinUppers c) moduleNameHelper))
            , succeed <| UpperCaseIdentifier ""
            ]


joinUppers : UpperCaseIdentifier -> UpperCaseIdentifier -> UpperCaseIdentifier
joinUppers (UpperCaseIdentifier val1) (UpperCaseIdentifier val2) =
    UpperCaseIdentifier <| val1 ++ val2



moduleNameToSource : ModuleName -> String
moduleNameToSource (ModuleName (PreWSSep wsList) (_, (UpperCaseIdentifier name)) ) =
    wsListToSource wsList ++ name



-- Exposing
type ExposingStatement
    = ExposingStatement ExposingKeyword Exposing

{-| Everything within the exposing parens |-}
type Exposing
    = ExposingAll PreWSSep PostWSSep
    | ExposingListing Range (List ExportItem)

type ExportItem
    = ExportItem PreWSSep (Range, Export) PostWSSep

type Export
    = TypeExport UpperCaseIdentifier (Maybe (PreWSSep, AdtExposing))
    | FnExport LowerCaseIdentifier
    | OpExport PreWSSep (Range, SymbolIdentifier) PostWSSep

{-| The stuff in parens in an ADT exposing |-}
type AdtExposing
    = AdtExposingAll PreWSSep PostWSSep
    | AdtExposingListing Range (List AdtExportItem)


type AdtExportItem
    = AdtExportItem PreWSSep (Range, UpperCaseIdentifier) PostWSSep



type ExposingKeyword
    = ExposingKeyword PreWSSep PostWSSep



exposingStatement : Parser ExposingStatement
exposingStatement =
    succeed ExposingStatement
        |= exposingKeyword
        |. symbol "("
        |= exposing_
        |. symbol ")"


exposingStatementToSource : ExposingStatement -> String
exposingStatementToSource (ExposingStatement (ExposingKeyword (PreWSSep preWs) (PostWSSep postWs)) exp) =
    wsListToSource preWs
        ++ "exposing"
        ++ wsListToSource postWs
        ++ exposingToSource exp


exposing_ : Parser Exposing
exposing_ =
    oneOf
        [ delayedCommitMap (\preWs postWs -> ExposingAll preWs postWs)
            (map PreWSSep optionalWSSep)
            (succeed PostWSSep
                |. symbol ".."
                |= optionalWSSep
            )
        , exposingListing
        ]


exposingToSource : Exposing -> String
exposingToSource exp =
    case exp of
        ExposingAll (PreWSSep preWs) (PostWSSep postWs) ->
            "(" ++ wsListToSource preWs ++ ".." ++ wsListToSource postWs ++ ")"

        ExposingListing _ lst ->
            "(" ++ exposingListingToSource lst ++ ")"



exposingListing : Parser Exposing
exposingListing =
    withRange ExposingListing <|
        (export
            |> andThen (\e -> exposingListingHelp [e]) )


exposingListingHelp : List ExportItem -> Parser (List ExportItem)
exposingListingHelp revExports =
    oneOf
        [ nextExport
            |> andThen (\e -> exposingListingHelp (e :: revExports))
        , succeed (List.reverse revExports)
        ]

exposingListingToSource : List ExportItem -> String
exposingListingToSource lst =
    List.map exportItemToSource lst |> String.join ","


exportItemToSource : ExportItem -> String
exportItemToSource (ExportItem (PreWSSep preWs) (_, export) (PostWSSep postWs)) =
    wsListToSource preWs ++ exportToSource export ++ wsListToSource postWs


exportToSource : Export -> String
exportToSource exp =
    case exp of
        FnExport (LowerCaseIdentifier fn) ->
            fn
        OpExport (PreWSSep preWs) (_, (SymbolIdentifier sym )) (PostWSSep postWs) ->
            "(" ++ wsListToSource preWs ++ sym ++ wsListToSource postWs ++ ")"

        TypeExport (UpperCaseIdentifier name) _ ->
            name


   --TypeExport UpperCaseIdentifier (Maybe (PreWSSep, AdtExposing))




nextExport : Parser ExportItem
nextExport =
    delayedCommit (symbol ",") export


export : Parser ExportItem
export =
    succeed ExportItem
        |= map PreWSSep optionalWSSep
        |= (withRange (,) <|
            oneOf
                [ typeExport
                , opExport
                , map FnExport lowVar
                ]
        )
        |= map PostWSSep optionalWSSep


typeExport : Parser Export
typeExport =
    succeed TypeExport
        |= capVar
        |= oneOf
            [ succeed (\preWs adt -> Just (preWs, adt))
                |= map PreWSSep optionalWSSep
                |. symbol "("
                |= adtExposing
                |. symbol ")"
            , succeed Nothing
            ]


opExport : Parser Export
opExport =
    succeed OpExport
        |. symbol "("
        |= map PreWSSep optionalWSSep
        |= withRange (,) (map SymbolIdentifier <| keep oneOrMore isSymbol)
        |= map PostWSSep optionalWSSep
        |. symbol ")"



adtExposing : Parser AdtExposing
adtExposing =
    inContext "Adt exposing" <|
        oneOf
            [ delayedCommitMap (\preWs postWs -> AdtExposingAll preWs postWs)
                (map PreWSSep optionalWSSep)
                (succeed PostWSSep
                    |. symbol ".."
                    |= optionalWSSep
                )
            , adtExposingListing
            ]


adtExposingListing : Parser AdtExposing
adtExposingListing =
    withRange AdtExposingListing <|
        (adtExportItem
            |> andThen (\e -> adtExposingListingHelp [e]) )


adtExposingListingHelp : List AdtExportItem -> Parser (List AdtExportItem)
adtExposingListingHelp revExports =
    oneOf
        [ adtNextExport
            |> andThen (\e -> adtExposingListingHelp (e :: revExports))
        , succeed (List.reverse revExports)
        ]


adtNextExport : Parser AdtExportItem
adtNextExport =
    delayedCommit (symbol ",") adtExportItem



adtExportItem : Parser AdtExportItem
adtExportItem =
    succeed AdtExportItem
        |= map PreWSSep optionalWSSep
        |= (withRange (,) capVar)
        |= map PostWSSep optionalWSSep


exposingKeyword : Parser ExposingKeyword
exposingKeyword =
    succeed ExposingKeyword
        |= map PreWSSep mandatoryWSSep
        |. symbol "exposing"
        |= map PostWSSep optionalWSSep




-- VARS

type UpperCaseIdentifier =
    UpperCaseIdentifier String

type LowerCaseIdentifier =
    LowerCaseIdentifier String

type SymbolIdentifier =
    SymbolIdentifier String


lowVar : Parser LowerCaseIdentifier
lowVar =
    map LowerCaseIdentifier <| variable Char.isLower isVarChar keywords


capVar : Parser UpperCaseIdentifier
capVar =
    inContext "UpperCase identifier" <|
        map UpperCaseIdentifier <|
            variable Char.isUpper isVarChar keywords





-- WHITESPACE RELATED

type PreWSSep =
    PreWSSep (List WS)


type PostWSSep =
    PostWSSep (List WS)



mandatoryWSSep : Parser (List WS)
mandatoryWSSep =
    mandatoryWS
        |> inContext "Mandatory whitespace between symbols"
        |> andThen (\ws -> wsListHelp [ws])
        |> andThen
            (\wsList ->
                case (List.reverse wsList |> List.head) of
                    Just (Spaces _) ->
                        succeed wsList
                    _ ->
                        fail "Did you forget a space perhaps ?"
            )

optionalWSSep : Parser (List WS)
optionalWSSep =
    oneOf [ mandatoryWSSep, succeed []]





wsListHelp : List WS -> Parser (List WS)
wsListHelp revList =
    oneOf
        [ mandatoryWS
            |> andThen (\ws -> wsListHelp (ws :: revList))
        , succeed (List.reverse revList)
        ]


mandatoryWS : Parser WS
mandatoryWS =
    oneOf [spaces, newlines]




spaces : Parser WS
spaces =
    source (ignore oneOrMore isSpace)
        |> andThen (\s -> succeed ( Spaces <| String.length s))

newlines : Parser WS
newlines =
    newline |> andThen (\ws -> newlinesHelp ws)


newlinesHelp : WS -> Parser WS
newlinesHelp ws =
    oneOf
        [ newline |> andThen (\moreWs ->
            case (ws, moreWs ) of
                (Newlines x, Newlines y ) ->
                    newlinesHelp (Newlines <| x + y)
                _ ->
                    fail "Can't really merge anything but newlines"
        )
        , succeed ws
        ]


newline : Parser WS
newline =
    oneOf
        [ keep oneOrMore isNewline
            |> andThen (\s -> succeed (Newlines <| String.length s))
        , symbol "\x0D\n"
            |> andThen (\_ -> succeed <| Newlines 1)
        ]






type WS
    = Spaces Int
    | MultiLineComment String
    | Newlines Int



wsListToSource : List WS -> String
wsListToSource lst =
    List.map wsToSource lst
        |> String.join ""



wsToSource : WS -> String
wsToSource ws =
    case ws of
        Spaces count ->
            List.repeat count " " |> String.join ""

        Newlines count ->
            List.repeat count "\n" |> String.join ""

        MultiLineComment val ->
            "{- " ++ val ++ " -}"



isSpace : Char -> Bool
isSpace char =
    char == ' '

isNewline : Char -> Bool
isNewline char =
    char == '\n'


isCarriageReturn : Char -> Bool
isCarriageReturn char =
    char == '\x0D'



-- Primitives


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'


isChar : Char -> Bool
isChar char =
    True


keywords : Set String
keywords =
    Set.fromList [ "let", "in", "case", "of", "if", "then", "else" ]


isOp : String -> Bool
isOp name =
    String.toList name
        |> List.all isSymbol


isSymbol : Char -> Bool
isSymbol c =
    Set.member c validSymbols


validSymbols : Set.Set Char
validSymbols =
    String.toList "+-/*=.<>:&|^?%~!"
        |> Set.fromList



-- Range
withRange : (Range -> a -> b) -> Parser a -> Parser b
withRange mapFn someParser =
    getPosition
        |> andThen
            (\start ->
                someParser
                    |> andThen
                        (\res ->
                            getPosition
                                |> andThen
                                    (\end ->
                                        succeed <| mapFn (Range start end) res
                                    )
                        )
            )


type Range =
    Range (Int, Int) (Int, Int)

