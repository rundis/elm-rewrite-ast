module{-dill-}Main exposing ( .. )

import Parser exposing (..)
import Parser.LanguageKit exposing (..)
import Parser.LowLevel exposing (..)
import Char
import Set exposing (Set)




-- Module declaration



type alias ModuleHeader =
    { range : Range
    , moduleDeclaration : ModuleDeclaration
    , doc : Maybe String
    , imports : List ImportDeclaration
    }


type alias ModuleDeclaration =
    { range : Range
    , sourceTag : SourceTag
    , name : ModuleName
    , exports : ExposingDecl
    }


type SourceTag
  = Normal
  | Effect
  | Port


type alias ModuleName =
    { range : Range
    , value : UpperCaseIdentifier
    }




type alias ExposingDecl =
    { range : Range
    , exposing_ : Exposing
    }


type Exposing
    = ExposeAll
    | ExposingListing (List ExportItem)

type alias ExportItem =
    { range : Range
    , export : Export
    }

type Export
    = TypeExport UpperCaseIdentifier
    | FnExport LowerCaseIdentifier
    | OpExport SymbolIdentifier
    | AdtExport AdtExportDecl


type alias AdtExportDecl =
    { value : UpperCaseIdentifier
    , exposing_ : AdtExposing
    }


type AdtExposing
    = AdtExposeAll
    | AdtExposingListing (List AdtExportItem)


type alias AdtExportItem =
    { range : Range
    , value : UpperCaseIdentifier
    }


moduleHeader : Parser ModuleHeader
moduleHeader =
    inContext "Module header" <|
        withRange (\range ( decl, doc, imports ) -> ModuleHeader range decl doc imports) <|
            succeed (\decl doc imports -> ( decl, doc, imports ))
                |= moduleDeclaration
                |= moduleDocs
                |= importDeclarations



moduleDeclaration : Parser ModuleDeclaration
moduleDeclaration =
    inContext "Module declaration" <|
        withRange (\range ( tag, name, exports ) -> ModuleDeclaration range tag name exports) <|
            succeed (\tag name exports -> ( tag, name, exports ))
                |= sourceTag
                |. symbol "module"
                |. mandatoryWS
                |= moduleName
                |. mandatoryWS
                |. symbol "exposing"
                |. optionalSpace
                |. symbol "("
                |= moduleExports
                |. symbol ")"



moduleDocs : Parser (Maybe String)
moduleDocs =
    succeed identity
        |. naiveWS
        |= oneOf [map Just docComment, map (\p -> Just <| toString p) getPosition]



naiveWS : Parser ()
naiveWS =
    naiveWSHelp <|
        oneOf
            [ lineComment
            , delayedCommit multilineComment (succeed ())
            ]


naiveWSHelp : Parser a -> Parser ()
naiveWSHelp parser =
    (|.) chompSpaces <|
        oneOf [ andThen (\_ -> naiveWSHelp parser) parser, succeed () ]



sourceTag : Parser SourceTag
sourceTag =
    oneOf
        [ succeed Effect
            |. symbol "effect"
            |. mandatoryWS
        , succeed Port
            |. symbol "port"
            |. mandatoryWS
        , succeed Normal
        ]




moduleName : Parser ModuleName
moduleName =
    withRange ModuleName moduleNameHelper


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


moduleExports : Parser ExposingDecl
moduleExports =
    inContext "Module exports" <|
        withRange ExposingDecl <|
            succeed identity
                |. optionalSpace
                |= oneOf [exposeAll, exposingListing]
                |. optionalSpace



exposeAll : Parser Exposing
exposeAll =
    inContext "Expose All" <|
        succeed ExposeAll
            |. symbol ".."



exposingListing : Parser Exposing
exposingListing =
    inContext "Exposing listing" <|
        succeed ExposingListing
            |. optionalWS
            |= andThen (\e -> exportListHelp [e]) export




exportListHelp : List ExportItem -> Parser (List ExportItem)
exportListHelp revExports =
  oneOf
    [ nextExport
        |> andThen (\n -> exportListHelp (n :: revExports))
    , succeed (List.reverse revExports)
    ]

nextExport : Parser ExportItem
nextExport =
    delayedCommit optionalWS <|
        succeed identity
            |. symbol ","
            |. optionalWS
            |= export


export : Parser ExportItem
export =
    inContext  "Expecting export item" <|
        withRange ExportItem <|
            succeed identity
                |= oneOf
                    [ adtOrType
                    , map FnExport lowVar
                    , map OpExport opExport
                    ]




opExport : Parser SymbolIdentifier
opExport =
    inContext "BinOp export" <|
        succeed SymbolIdentifier
            |. symbol "("
            |. optionalWS
            |= keep oneOrMore isSymbol
            |. optionalWS
            |. symbol ")"





adtOrType : Parser Export
adtOrType =
    succeed identity
        |= andThen (\n -> adtOrTypeHelp n) capVar


adtOrTypeHelp : UpperCaseIdentifier -> Parser Export
adtOrTypeHelp  idf =
    let
        adtPre =
            succeed identity
                |. optionalWS
                |. symbol "("
                |. optionalWS
    in
        oneOf
            [ delayedCommit adtPre <|
                succeed AdtExport
                    |= map (AdtExportDecl idf) (oneOf [ adtExposeAll, adtExposingListing ])
                    |. optionalWS
                    |. symbol ")"
            , succeed <| TypeExport idf
            ]


adtExposeAll : Parser AdtExposing
adtExposeAll =
    inContext "ADT expose all (..)" <|
        succeed AdtExposeAll |. symbol ".."


adtExposingListing : Parser AdtExposing
adtExposingListing =
    inContext "ADT Exposing List" <|
        succeed AdtExposingListing
            |. optionalWS
            |= andThen (\e -> adtExportListHelp [e]) adtExport



adtExportListHelp : List AdtExportItem -> Parser (List AdtExportItem)
adtExportListHelp revExports =
  oneOf
    [ nextAdtExport
        |> andThen (\n -> adtExportListHelp (n :: revExports))
    , succeed (List.reverse revExports)
    ]

nextAdtExport : Parser AdtExportItem
nextAdtExport =
    delayedCommit optionalWS <|
        succeed identity
            |. symbol ","
            |. optionalWS
            |= adtExport



adtExport : Parser AdtExportItem
adtExport =
    withRange AdtExportItem capVar




-- IMPORTS
type alias ImportDeclaration =
    { range : Range
    , module_ : ModuleName
    , alias_ : Maybe Alias
    , exports : Maybe ExposingDecl
    }


type alias Alias =
    { range : Range
    , value : UpperCaseIdentifier
    }



importDeclarations : Parser (List ImportDeclaration)
importDeclarations =
    inContext "User imports" <|
        succeed identity
            |. naiveWS
            |= andThen (\i -> importsHelp [i]) importDeclaration

importsHelp : List ImportDeclaration -> Parser (List ImportDeclaration)
importsHelp revImports =
    oneOf
        [ nextImport
            |> andThen (\i -> importsHelp (i :: revImports))
        , succeed (List.reverse revImports)
        ]


nextImport : Parser ImportDeclaration
nextImport =
    delayedCommit naiveWS <|
        succeed identity
            |= importDeclaration

importDeclaration : Parser ImportDeclaration
importDeclaration =
    inContext "Import declaration" <|
        withRange (\range ( module_, alias_, exports ) -> ImportDeclaration range module_ alias_ exports) <|
            succeed (\module_ alias_ exports -> ( module_, alias_, exports ))
                |. symbol "import"
                |. mandatoryWS
                |= moduleName
                |= importAlias
                |= importExports




importAlias : Parser (Maybe Alias)
importAlias =
    inContext "Import alias" <|
        oneOf
            [ map Just <|
                withRange Alias <|
                    delayedCommit mandatoryWS <|
                        succeed identity
                            |. symbol "as"
                            |. mandatoryWS
                            |= capVar
            , succeed Nothing
            ]


importExports : Parser (Maybe ExposingDecl)
importExports =
    let
        toExposing range maybeExposing =
            case maybeExposing of
                Just x ->
                    Just <| ExposingDecl range x

                Nothing ->
                    Nothing
    in
        inContext "Import exports" <|
            withRange toExposing <|
                oneOf
                    [ delayedCommit mandatoryWS <|
                        succeed identity
                            |. symbol "exposing"
                            |. optionalSpace
                            |. symbol "("
                            |. optionalSpace
                            |= oneOf
                                [ map Just exposeAll
                                , map Just exposingListing
                                ]
                            |. optionalSpace
                            |. symbol ")"
                    , succeed Nothing
                    ]



-- WHITESPACE


mandatoryWS : Parser ()
mandatoryWS =
    inContext "Expecting a space or a multiline comment" <|
        oneOf [ mandatorySpace, multilineComment ]


mandatorySpace : Parser ()
mandatorySpace =
    inContext "Expecting one or more spaces" <|
        (keep oneOrMore isSpace
            |> andThen
                (\s ->
                    if not (String.contains " " s) then
                        fail "Expecting atleast one space"
                    else if String.contains "\n" s && not (String.right 1 s == " ") then
                        fail "Missing indent"
                    else
                        succeed ()
                )
        )

optionalWS : Parser ()
optionalWS =
    oneOf [ optionalSpace, multilineComment, succeed ()]


optionalSpace : Parser ()
optionalSpace =
    (keep zeroOrMore isSpace)
        |> andThen
            (\s ->
                if String.contains "\n" s && not (String.right 1 s == " ") then
                    fail "Missing indent"
                else
                    succeed ()
            )



chompSpaces : Parser ()
chompSpaces =
  Parser.ignore zeroOrMore isSpace


isSpace : Char -> Bool
isSpace char =
    char == ' ' || char == '\n' || char == '\x0D'



-- Vars
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
    inContext "capVar" <|
        map UpperCaseIdentifier <|
            variable Char.isUpper isVarChar keywords



-- COMMENTS
lineComment : Parser ()
lineComment =
    symbol "--"
        |. ignoreUntil "\n"


multilineComment : Parser ()
multilineComment =
    delayedCommit (symbol "{-") <|
        oneOf
            [ symbol "|" |> andThen (\_ -> fail "Not expecting doc comment here")
            , nestableComment "{-" "-}"
            ]




nestableComment : String -> String -> Parser ()
nestableComment start end =
    inContext "Multiline comment" <|
        case ( String.uncons start, String.uncons end ) of
            ( Nothing, _ ) ->
                fail "Trying to parse a multi-line comment, but the start token cannot be the empty string!"

            ( _, Nothing ) ->
                fail "Trying to parse a multi-line comment, but the end token cannot be the empty string!"

            ( Just ( startChar, _ ), Just ( endChar, _ ) ) ->
                let
                    isNotRelevant char =
                        char /= startChar && char /= endChar
                in
                    --symbol start
                    nestableCommentHelp isNotRelevant start end 1


nestableCommentHelp : (Char -> Bool) -> String -> String -> Int -> Parser ()
nestableCommentHelp isNotRelevant start end nestLevel =
    lazy <|
        \_ ->
            (|.) (Parser.ignore zeroOrMore isNotRelevant) <|
                oneOf
                    [ (|.) (symbol end) <|
                        if nestLevel == 1 then
                            succeed ()
                        else
                            nestableCommentHelp isNotRelevant start end (nestLevel - 1)
                    , (|.) (symbol start) <|
                        nestableCommentHelp isNotRelevant start end (nestLevel + 1)
                    , (|.) (Parser.ignore (Exactly 1) isChar) <|
                        nestableCommentHelp isNotRelevant start end nestLevel
                    ]


docComment : Parser String
docComment =
    let
        isNotRelevant char =
            char /= '|'
    in
        succeed identity
            |. symbol "{-|"
            |= docCommentHelp isNotRelevant

docCommentHelp : (Char -> Bool) -> Parser String
docCommentHelp isNotRelevant =
    lazy <|
        \_ ->
            succeed (++)
                |= (source <| Parser.ignore zeroOrMore isNotRelevant)
                |= oneOf
                    [ map (always "") (symbol "|-}")
                    , (Parser.ignore (Exactly 1) isChar
                        |> andThen (\_ -> source <| docCommentHelp isNotRelevant)
                      )
                    ]




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


-- META / CONTEXT


type alias Range =
    { start : Pos
    , end : Pos
    }


withRange : (Range -> a -> b) -> Parser a -> Parser b
withRange mapFn someParser =
    posParser
        |> andThen
            (\start ->
                someParser
                    |> andThen
                        (\res ->
                            posParser
                                |> andThen
                                    (\end ->
                                        succeed <| mapFn (Range start end) res
                                    )
                        )
            )


posParser : Parser Pos
posParser =
    map (\( row, col ) -> Pos row col) getPosition


type alias Pos =
    { row : Int, col : Int }
