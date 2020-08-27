module TodoApp

open Browser.Dom
open Fable.React
open Fable.React.Props
open Elmish
open CommonTypes
open Button
open Input
open Tabs
open IconClose
open CheckBox
open IconAngleDown
open ClassList
open Elmish.React

Fable.Core.JsInterop.importAll "./styles/main.scss"

type ShowEnum =
    | All
    | Active
    | Completed

type Msg =
    | Failure of string
    | Add
    | Delete of int
    | Complete of int * bool
    | CompleteAll
    | UpdateField of string
    | Remove of int
    | ChangeVisibility of ShowEnum


type Entry =
    { description: string
      completed: bool
      editing: bool
      id: int }

type Model =
    { entries: Entry list
      field: string
      uid: int
      visibility: ShowEnum }

let newEntry description id =
    { description = description
      completed = false
      editing = false
      id = id }


let emptyModel =
    { entries = []
      visibility = All
      field = ""
      uid = 0 }

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Failure err ->
        console.error (err)
        model, []

    | Add ->
        let entries =
            if System.String.IsNullOrEmpty model.field
            then model.entries
            else model.entries @ [ newEntry model.field model.uid ]

        { model with
              uid = model.uid + 1
              field = ""
              entries = entries },
        []

    | Delete id ->
        { model with
              entries = model.entries |> List.filter (fun t -> t.id <> id) },
        []

    | Complete (id, isCompleted) ->
        let updateEntry entry =
            if entry.id = id then { entry with completed = isCompleted } else entry

        { model with
              entries = model.entries |> List.map updateEntry },
        []

    | CompleteAll ->
        let allCompleted =
            List.forall (fun t -> t.completed) model.entries

        let updateEntry entry =
            { entry with
                  completed = not allCompleted }

        { model with
              entries = model.entries |> List.map updateEntry },
        []

    | UpdateField str -> { model with field = str }, []

    | Remove id ->
        { model with
              entries = model.entries |> List.filter (fun t -> t.id <> id) },
        []

    | ChangeVisibility visibility -> { model with visibility = visibility }, []



let viewForm field dispatch =
    div [ ClassName "todo__form" ] [
        Button
            { OnClick = fun _ -> CompleteAll |> dispatch
              Appearance = Default
              ClassName = None }
            [ IconAngleDown { Size = Middle } ]
        div [ ClassName "todo__field" ] [
            Input
                { Value = field
                  OnChange = (fun event -> event.Value |> UpdateField |> dispatch)
                  Placeholder = "What needs to be done?"
                  AutoFocus = true
                  OnEnter = fun () -> Add |> dispatch }
        ]
    ]

let viewList list dispatch =
    section [ ClassName "todo__content" ] [
        ul [ ClassName "todo__list" ] [
            list
            |> List.sortByDescending (fun item -> item.id)
            |> List.map (fun item ->
                li [ classList [ ("todo__item", true)
                                 ("todo__item--completed", item.completed) ]
                     Key("todo-" + (string item.id)) ] [
                    CheckBox
                        { Checked = item.completed
                          OnChange = fun _ -> Complete(item.id, not item.completed) |> dispatch }
                    span [] [ str item.description ]
                    Button
                        { OnClick = fun _ -> (Remove(item.id) |> dispatch)
                          Appearance = Icon
                          ClassName = None }
                        [ IconClose { Size = Small } ]
                ])
            |> ofList
        ]
    ]


// let App =
//     FunctionComponent.Of(fun () ->
//         let field = Hooks.useState ("")
//         let list = Hooks.useState ([])
//         let activeTab = Hooks.useState (All)
//         let uid = Hooks.useState (0)

//         let onChange =
//             (fun (event: Browser.Types.Event) -> field.update (event.Value))

//         let onChangeTab =
//             fun (show: ShowEnum) _ -> activeTab.update (show)

//         let onAdd =
//             (fun _ ->
//                 if field.current <> System.String.Empty then
//                     (list.update (fun list -> list @ [ newEntry field.current uid.current ])
//                      field.update ("")
//                      uid.update (fun uid -> uid + 1)))


//         let onRemove id =
//             (fun _ -> list.update (fun list -> list |> List.filter (fun item -> item.id <> id)))

//         let onCompleted id =
//             (fun _ ->
//                 list.update (fun list ->
//                     list
//                     |> List.map (fun item -> if item.id = id then checkEntry item (not item.completed) else item)))

//         let newTab title idTab =
//             { Id = id
//               Title = title
//               Active = (activeTab.current = idTab)
//               Content = (viewList (list.current |> List.filter (isVisible idTab)) onRemove onCompleted) }

//         let isCompletedAll =
//             (list.current
//              |> List.filter (fun item -> item.completed)
//              |> List.length) = list.current.Length

//         let onCompleteAll =
//             fun _ ->
//                 list.update (fun list ->
//                     list
//                     |> List.map (fun item ->
//                         { item with
//                               completed = not isCompletedAll }))



//         div [ classList [ ("todo", true)
//                           ("todo--completed-all", isCompletedAll) ] ] [
//             viewForm field.current onChange onAdd onCompleteAll
//             Tabs
//                 { Tabs =
//                       [ (newTab "All" All)
//                         (newTab "Active" Active)
//                         (newTab "Completed" Completed) ]
//                   OnChangeTab = fun id _ -> activeTab.update (id) }
//         ])




// let init () =
//     ReactDom.render (App(), document.getElementById ("root"))

// init ()

let view model dispatch =
    let isAllCompleted =
        model.entries
        |> List.forall (fun t -> t.completed)

    let isVisible visibility todo =
        match visibility with
        | Completed -> todo.completed
        | Active -> not todo.completed
        | _ -> true

    let newTab title idTab =
        { Id = idTab
          Title = title
          Active = (model.visibility = idTab)
          Content = (lazyView2 viewList (model.entries |> List.filter (isVisible idTab)) dispatch) }

    div [ classList [ ("todo", true)
                      ("todo--completed-all", isAllCompleted) ] ] [
        lazyView2 viewForm model.field dispatch
        Tabs
            { Tabs =
                  [ (newTab "All" All)
                    (newTab "Active" Active)
                    (newTab "Completed" Completed) ]
              OnChangeTab = fun id _ -> ChangeVisibility(id) |> dispatch }
    ]


let init =
    function
    | Some savedModel -> savedModel, []
    | _ -> emptyModel, []

module S =
    let private STORAGE_KEY = "todo-app"

    let private decoder =
        Thoth.Json.Decode.Auto.generateDecoder<Model> ()

    let load (): Model option =
        Browser.WebStorage.localStorage.getItem (STORAGE_KEY)
        |> unbox
        |> Option.bind
            (Thoth.Json.Decode.fromString decoder
             >> function
             | Ok r -> Some r
             | _ -> None)

    let save (model: Model) =
        Browser.WebStorage.localStorage.setItem (STORAGE_KEY, Thoth.Json.Encode.Auto.toString (1, model))


let setStorage (model: Model): Cmd<Msg> =
    Cmd.OfFunc.attempt S.save model (string >> Failure)

let updateWithStorage (msg: Msg) (model: Model) =
    match msg with
    | Failure _ -> model, []
    | _ ->
        let (newModel, cmds) = update msg model
        newModel, Cmd.batch [ setStorage newModel; cmds ]

Program.mkProgram (S.load >> init) updateWithStorage view
|> Program.withReactBatched "root"
|> Program.run
