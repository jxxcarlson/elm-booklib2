module Types exposing (..)

import Url exposing(Url)
import Browser.Navigation
import Browser exposing(UrlRequest)

type alias FrontendModel =
  {   url : Url
    , key : Browser.Navigation.Key
    , message : String
  }

type alias BackendModel =
  { message : String
  }

type FrontendMsg
    = NoOpFrontendMsg
    | UrlChanged Url
    | LinkClicked UrlRequest

type ToBackend
    = NoOpToBackend

type BackendMsg
    = NoOpBackendMsg

type ToFrontend
    = NoOpToFrontend