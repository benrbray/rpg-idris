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
contentDiv : Ref Tag.Body
contentDiv = Id "content"

||| The page consists of a static heading with a title an
||| (eventually) a short description of the project.
||| This is followed by a selection box, where visitors can
||| choose an example application.
|||
||| The example application will be dynamicall generated and
||| placed in to a `<div>` with ID `"example"`.
export
exampleDiv : Ref Div
exampleDiv = Id "example"

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

display : Event -> ST -> Cmd Event
display Init s = child contentDiv (Text "hello, world!")

covering
ui : IO ()
ui = runMVC update display (putStrLn . dispErr) Init init

covering
main : IO ()
main = ui