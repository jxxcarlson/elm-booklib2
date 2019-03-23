module User.Post exposing (Post, PostRecord, encodePost, postDecoder, postListDecoder, postRecordDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Post =
    { id : Int
    , title : String
    , content : String
    , authorName : String
    , groupId : Int
    , tags : List String
    }


type alias PostRecord =
    { data : Post }


postListDecoder : Decoder (List Post)
postListDecoder =
    Decode.field "data" (Decode.list postDecoder)


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" Decode.int
        |> required "title" Decode.string
        |> required "content" Decode.string
        |> required "author_name" Decode.string
        |> required "group_id" Decode.int
        |> required "tags" (Decode.list Decode.string)


postRecordDecoder : Decoder PostRecord
postRecordDecoder =
    Decode.field "data" postDecoder
        |> Decode.map (\p -> { data = p })


encodePost : Post -> Encode.Value
encodePost post =
    Encode.object
        [ ( "title", Encode.string post.title )
        , ( "content", Encode.string post.content )
        , ( "author_name", Encode.string post.authorName )
        , ( "group_id", Encode.int post.groupId )
        , ( "tags", Encode.list Encode.string post.tags )
        ]
