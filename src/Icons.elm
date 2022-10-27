module Icons exposing (..)

import Svg
import VirtualDom exposing (Attribute, attribute)


arrowPath : List (Attribute msg) -> Svg.Svg msg
arrowPath attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke-width" "1.5", attribute "stroke" "currentColor", attribute "class" "w-6 h-6"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0l3.181 3.183a8.25 8.25 0 0013.803-3.7M4.031 9.865a8.25 8.25 0 0113.803-3.7l3.181 3.182m0-4.991v4.99"]) []]

bellSlash : List (Attribute msg) -> Svg.Svg msg
bellSlash attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke-width" "1.5", attribute "stroke" "currentColor", attribute "class" "w-6 h-6"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M9.143 17.082a24.248 24.248 0 003.844.148m-3.844-.148a23.856 23.856 0 01-5.455-1.31 8.964 8.964 0 002.3-5.542m3.155 6.852a3 3 0 005.667 1.97m1.965-2.277L21 21m-4.225-4.225a23.81 23.81 0 003.536-1.003A8.967 8.967 0 0118 9.75V9A6 6 0 006.53 6.53m10.245 10.245L6.53 6.53M3 3l3.53 3.53"]) []]

chartBar : List (Attribute msg) -> Svg.Svg msg
chartBar attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke-width" "1.5", attribute "stroke" "currentColor", attribute "class" "w-6 h-6"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M3 13.125C3 12.504 3.504 12 4.125 12h2.25c.621 0 1.125.504 1.125 1.125v6.75C7.5 20.496 6.996 21 6.375 21h-2.25A1.125 1.125 0 013 19.875v-6.75zM9.75 8.625c0-.621.504-1.125 1.125-1.125h2.25c.621 0 1.125.504 1.125 1.125v11.25c0 .621-.504 1.125-1.125 1.125h-2.25a1.125 1.125 0 01-1.125-1.125V8.625zM16.5 4.125c0-.621.504-1.125 1.125-1.125h2.25C20.496 3 21 3.504 21 4.125v15.75c0 .621-.504 1.125-1.125 1.125h-2.25a1.125 1.125 0 01-1.125-1.125V4.125z"]) []]

listBullet : List (Attribute msg) -> Svg.Svg msg
listBullet attrs = Svg.node "svg" ([attribute "xmlns" "http://www.w3.org/2000/svg", attribute "fill" "none", attribute "viewBox" "0 0 24 24", attribute "stroke-width" "1.5", attribute "stroke" "currentColor", attribute "class" "w-6 h-6"] ++ attrs) [ Svg.node "path" ([attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "d" "M8.25 6.75h12M8.25 12h12m-12 5.25h12M3.75 6.75h.007v.008H3.75V6.75zm.375 0a.375.375 0 11-.75 0 .375.375 0 01.75 0zM3.75 12h.007v.008H3.75V12zm.375 0a.375.375 0 11-.75 0 .375.375 0 01.75 0zm-.375 5.25h.007v.008H3.75v-.008zm.375 0a.375.375 0 11-.75 0 .375.375 0 01.75 0z"]) []]