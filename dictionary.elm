import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Dict exposing (Dict(..))
import Dict
import Http
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Task
import String

main = App.program
       { init          = init
       , view          = view
       , update        = update
       , subscriptions = subscriptions
       }

-- Model

type alias DictEntry = 
    { pinyin     : String
    , definition : String
    }

type alias CEDict = Dict String (List DictEntry)

type alias Model =
    { query      : String
    , result     : List DictEntry
    , related    : Maybe CEDict
    , dictionary : CEDict
    }

init : (Model, Cmd Msg)
init = (Model "" [] Nothing Dict.empty, loadDict "cedict.json")

loadDict : String -> Cmd Msg
loadDict s = Task.perform LoadFail LoadDict (Http.get decodeDict s)

decodeDict : Json.Decoder CEDict
decodeDict = Json.dict (Json.list decodeDictEntry)

decodeDictEntry : Json.Decoder DictEntry
decodeDictEntry = Json.object2 DictEntry
                  ("pinyin"     := Json.string)
                  ("definition" := Json.string)

-- Update

type Msg = UserInput String
         | LoadDict CEDict
         | LoadFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        UserInput s -> 
            let result  = 
                    case Dict.get s model.dictionary of
                        Just a  -> a
                        Nothing -> []

                related = Nothing
                -- related = let (rdict, _) = (Dict.partition (\k _ -> String.contains s k) model.dictionary)
                --           in if Dict.isEmpty rdict 
                --              then Nothing 
                --              else Just rdict

            in ( { model | query   = s, result  = result, related = related }
               , Cmd.none)

        LoadDict d -> ( { model | dictionary = d }
                      , Cmd.none)

        LoadFail e -> (model, Cmd.none)

-- View

view : Model -> Html Msg
view model = 
    div [ style [("margin", "1em")] ]
    [ Html.form [] 
        [ input [ type' "text"
                , placeholder "Search in 中文"
                , onInput UserInput ] []
        ]
    , div [] (List.map (showEntry model.query) model.result)
    , case model.related of
          Just a -> showRelated a
          Nothing -> div [] []
    ]

showEntry : String -> DictEntry -> Html Msg
showEntry query entry = 
    div []
        [ p [ style [("font-size", "3em")] ] 
              [ text (String.join " " [query, entry.pinyin])]
        , showDefinition entry.definition
        ]

showDefinition : String -> Html Msg
showDefinition s = 
    ul [] 
    (List.foldr (\a acc -> li [] [ text a ] :: acc) [] (String.split "; " s))

showRelated : CEDict -> Html Msg
showRelated d = 
    div [] 
    [ h3 [] [ text "Related" ]
    , div [] (List.concat <| Dict.foldl (\k v acc -> List.map (showEntry k) v :: acc) [] d)
    ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
