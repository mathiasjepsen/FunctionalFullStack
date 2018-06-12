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


-- MODEL


type alias Model =
  { count : Int
  , message : String
  , member : Member
  , members : List Member
  }

start : (Model, Cmd Msg)
start =
  ( Model 0 "No message" (Member 0 "Dummy Member" "dummy@dumdum.com") []
  , getMemberCount
  )

type alias Member =
  { id : Int
  , name : String
  , email : String
  }


-- UPDATE


type Msg
  = GetMemberCount
  | MemberCountReceived (Result Http.Error Int)
  | GetMember Int
  | MemberReceived (Result Http.Error Member)
  | ID String
  | Name String
  | Email String
  | PostMember
  | MemberPosted (Result Http.Error String)
  | MembersReceived (Result Http.Error (List Member))
  | UpdateMemberForm Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMemberCount ->
      (model, getMemberCount)

    MemberCountReceived (Ok newCount) ->
      ( { model | count = newCount }, getMembers)

    MemberCountReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    GetMember id ->
      (model, getMember id)

    MemberReceived (Ok newMember) ->
      ( { model | member = newMember }, Cmd.none)

    MemberReceived (Err err) ->
      ( { model | message = toString err }, Cmd.none)

    ID id ->
      case String.toInt id of
        Ok num ->
          ( { model | member = (Member num model.member.name model.member.email) }, Cmd.none)
        Err _ ->
          ( { model | member = (Member 0 model.member.name model.member.email),
                      message = "Failed to parse ID, setting it to 0" }, Cmd.none)

    Name name ->
      ( { model | member = (Member model.member.id name model.member.email) }, Cmd.none)

    Email email ->
      ( { model | member = (Member model.member.id model.member.name email) }, Cmd.none)

    PostMember ->
      (model, postMember model.member)

    MemberPosted (Ok msg) ->
      ( { model | message = msg }, getMemberCount)

    MemberPosted (Err err) ->
      ( { model | message = toString err }, Cmd.none)

    MembersReceived (Ok members) ->
      ( { model | members = members }, Cmd.none)

    MembersReceived (Err err) ->
      ( { model | message = "Failed to retrieve the list of members" }, Cmd.none)

    UpdateMemberForm id ->
      (model, getMember id)


-- VIEW


view : Model -> Html Msg
view {count, message, member, members} =
  div []
    [ h2 [] [text ("Member Count = " ++ toString count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , button [ onClick (GetMember member.id) ] [ text "Get Member" ]
    , hr [] []
    , text message
    , hr [] []
    , input [ type_ "text", placeholder "ID", onInput ID, value <| toString member.id ] []
    , input [ type_ "text", placeholder "Name", onInput Name, value member.name ] []
    , input [ type_ "text", placeholder "E-mail", onInput Email, value member.email ] []
    , button [ onClick PostMember ] [ text "Post Member" ]
    , hr [] []
    , ul [] <| List.map createMemberList members
    ]

createMemberList : Member -> Html Msg
createMemberList member =
  li []
    [ text (member.name ++ " - " ++ member.email)
    , button [ onClick (UpdateMemberForm member.id) ] [ text <| toString member.id ]
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- HTTP


getMemberCount : Cmd Msg
getMemberCount =
  Http.send MemberCountReceived (Http.get (url "count") Decode.int)

getMember : Int -> Cmd Msg
getMember id =
  Http.send MemberReceived (Http.get (url <| toString id) decodeMember)

memberJsonBody : Member -> Http.Body
memberJsonBody member =
  Http.jsonBody <| encodeMember member

postMember : Member -> Cmd Msg
postMember member =
  Http.send MemberPosted (Http.post (url "") (memberJsonBody member) Decode.string)

getMembers : Cmd Msg
getMembers =
  Http.send MembersReceived (Http.get (url "") decodeMembers)


-- DECODER


decodeMember : Decode.Decoder Member
decodeMember =
  Decode.map3 Member
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "email" Decode.string)

decodeMembers : Decode.Decoder (List Member)
decodeMembers =
  Decode.list decodeMember


-- ENCODER


encodeMember : Member -> Encode.Value
encodeMember member =
  Encode.object
    [ ("id", Encode.int member.id)
    , ("name", Encode.string member.name)
    , ("email", Encode.string member.email)
    ]
