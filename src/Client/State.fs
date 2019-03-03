module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types

let pageParser: Parser<Page->Page,Page> =
    oneOf [
        map About (s "about")
        map ListWorkflows (s "workflows")
    ]

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init result =
    let (counter, counterCmd) = Counter.State.init()
    let (home, homeCmd) = Home.State.init()
    let (listWorkflows, wfCmd) = ListWorkflows.State.init()
    let (model, cmd) =
        urlUpdate result
          { CurrentPage = ListWorkflows
            ListWorkflows = listWorkflows }

    model, Cmd.batch [ cmd
                       Cmd.map ListWFMsg wfCmd ]

let update msg model =
    match msg with
    | ListWFMsg msg ->
        let (listWF, listWFCmd) = ListWorkflows.State.update msg model.ListWorkflows
        { model with ListWorkflows = listWF }, Cmd.map ListWFMsg listWFCmd