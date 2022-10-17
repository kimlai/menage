module Icons exposing (..)

import Svg
import VirtualDom exposing (Attribute, attribute)


arrowPath : List (Attribute msg) -> Svg.Svg msg
arrowPath attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke-width" "1.5", attribute "stroke" "currentColor", attribute "class" "w-6 h-6"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0l3.181 3.183a8.25 8.25 0 0013.803-3.7M4.031 9.865a8.25 8.25 0 0113.803-3.7l3.181 3.182m0-4.991v4.99"]) []]