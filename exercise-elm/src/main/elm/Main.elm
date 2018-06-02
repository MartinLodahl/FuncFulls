import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode

main : Program Never Model Msg
main =
  Html.program
    { init = start
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

url : String -> String
url action = "http://localhost:9000/member/" ++ action

-- MODEL*

type alias Model =
  { count : Int
  , message : String
  , member : Member
  }

type alias Member =
  { id : Int
  , name : String
  , email : String
  }

start : (Model, Cmd Msg)
start =
  ( Model 0 "No message" (Member -1 "Not here" "Not here")
  , Cmd.none
  )

-- UPDATE

type Msg
  = GetMemberCount
  | MemberCountReceived (Result Http.Error Int)
  | GetMember
  | MemberReceived (Result Http.Error Member)
  | IdChanged String
  | EmailChanged String
  | NameChanged String
  | PostMember
  | MemberPosted (Result Http.Error Member)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMemberCount ->
      (model, getMemberCount)

    MemberCountReceived (Ok newCount) ->
      ( { model | count = newCount }, Cmd.none)

    MemberCountReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    GetMember ->
      (model, getMember model.member.id)

    MemberReceived (Ok member)->
      ( { model | member = member }, Cmd.none)

    MemberReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    NameChanged name ->
      let
        member = model.member
        newMember = {member | name = name}
      in
        ( {model | member = {member | name = name}}, Cmd.none)

    EmailChanged email ->
      let
        member = model.member
      in
        ({model | member = {member | email = email}}, Cmd.none)

    IdChanged idText ->
      case String.toInt idText of
        Ok num ->
          ({model | member = (Member num model.member.name model.member.email)}, Cmd.none)
        Err _ ->
          ({model | member = (Member 0 "No one" "No one@NoOne.no1"),
                              message = "Failed to parse ID, setting it to 0"}, Cmd.none)

    PostMember ->
      (model, postMember model.member)

    MemberPosted (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    MemberPosted (Ok member) ->
      ({model | member = member}, getMemberCount)
-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Member Count = " ++ toString model.count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , button [ onClick GetMember ] [ text "Get Member" ]
    , button [ onClick PostMember ] [ text "Post Member" ]
    , hr [] []
    , div[]
    [ input [ type_ "text", value (toString model.member.id), onInput IdChanged  ] []
    , input [ type_ "text", value model.member.name, onInput NameChanged ] []
    , input [ type_ "text", value model.member.email, onInput EmailChanged ] []]
    , text model.message
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP
postMember : Member -> Cmd Msg
postMember member =
  Http.send MemberPosted (Http.post (url "") (memberJsonBody member) decodeMember)

memberJsonBody : Member -> Http.Body
memberJsonBody member =
  Http.jsonBody <| encodeMember member

getMember : Int -> Cmd Msg
getMember id=
  Http.send MemberReceived (Http.get (url (toString id)) decodeMember)

getMemberCount : Cmd Msg
getMemberCount =
    Http.send MemberCountReceived (Http.get (url "count") Decode.int)

decodeMember : Decode.Decoder Member
decodeMember =
  Decode.map3 Member
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "email" Decode.string)

encodeMember : Member -> Encode.Value
encodeMember member =
  Encode.object
    [ ("id", Encode.int member.id)
    , ("name", Encode.string member.name)
    , ("email", Encode.string member.email)
    ]
