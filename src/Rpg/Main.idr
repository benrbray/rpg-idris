module Rpg.Main

import Text.HTML
import Text.HTML.Select
import Web.MVC

%default total
%language ElabReflection

------------------------------------------------------------

||| ID of the `<body>` element. The page content will
||| be placed here.
export
appDiv : Ref Tag.Body
appDiv = Id "app"

||| ID of a `<style>` element in the page header.
||| The generated CSS rules will go here.
export
appStyle : Ref Style
appStyle = Id "appstyle"

------------------------------------------------------------

-- application events
data Event : Type where
  Init : Event

-- application state
record ST where
  constructor S
  example : ()

init : ST
init = S ()

update : Event -> ST -> ST
update Init = id

-- content : Node ()
-- content =
--   div
--     [ class ("content"), Id contentDiv ]
--     [ label [] [Text "content"] ]

content : Node Event
content = 
  div
    [ class "content" ]
    [ Text "hello, world!" ]

display : Event -> ST -> Cmd Event
display Init s = child appDiv content

------------------------------------------------------------

covering
ui : IO ()
ui = runMVC update display (putStrLn . dispErr) Init init

------------------------------------------------------------

covering
main : IO ()
main = ui