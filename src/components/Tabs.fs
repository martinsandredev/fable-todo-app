module Tabs

open Fable.React
open Fable.React.Props
open ClassList

type Tab<'T> =
    { Id: 'T
      Title: string
      Content: ReactElement
      Active: bool }

type TabsProps<'T> =
    { Tabs: Tab<'T> list
      OnChangeTab: 'T -> Browser.Types.MouseEvent -> unit }

let Tabs<'T> (props: TabsProps<'T>) =
    div [] [
        div [ ClassName "tabs-nav"
              Data
                  ("tab-active",
                   props.Tabs
                   |> List.findIndex (fun tab -> tab.Active)) ] [
            props.Tabs
            |> List.map (fun tab ->
                button [ classList [ ("tab-nav", true)
                                     ("tab-nav--active", tab.Active) ]
                         OnClick(props.OnChangeTab tab.Id) ] [
                    str tab.Title
                ])
            |> ofList
        ]
        div [ ClassName "tabs-content" ] [
            props.Tabs
            |> List.filter (fun tab -> tab.Active)
            |> List.map (fun tab -> tab.Content)
            |> ofList
        ]
    ]
