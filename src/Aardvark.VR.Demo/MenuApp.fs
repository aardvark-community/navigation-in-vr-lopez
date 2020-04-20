namespace Demo

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Trafos
open Aardvark.Vr

open OpcViewer.Base
open Demo.Menu

type MenuAction = 
| CreateMenu of ControllerKind * bool
| HoverIn of string
| HoverOut
| UpdateControllerPose of ControllerKind * Pose
| Select of ControllerKind * bool 
| CloseMenu

module MenuApp = 
    open FShade
    

    let rec update (controllers : hmap<ControllerKind, ControllerInfo>) (state : VrState) (vr : VrActions) (model : MenuModel) (msg : MenuAction)  : MenuModel = 
        match msg with
        | CreateMenu (kind, buttonPressed) ->                
            let model = 
                if not(model.initialMenuPositionBool) then 
                    let controllerPos = controllers |> HMap.tryFind kind
                    match controllerPos with
                    | Some id -> 
                        {model with initialMenuPosition = id.pose; initialMenuPositionBool = true}
                    | None -> model
                else model

            if buttonPressed then 
                let hmdPos = controllers |> HMap.values |> Seq.item 0
                //match model.menu with
                //| PlaceLandmarks ->
                let newMenuBoxes = UtilitiesMenu.mkBoxesMenu model.initialMenuPosition hmdPos.pose 9 //number of menu possibilities should be the number of boxes. So far 2
                let box0id = newMenuBoxes |> Seq.item 0
                let box1id = newMenuBoxes |> Seq.item 1
                let box2id = newMenuBoxes |> Seq.item 2
                let box3id = newMenuBoxes |> Seq.item 3
                let box4id = newMenuBoxes |> Seq.item 4
                let box5id = newMenuBoxes |> Seq.item 5
                let box6id = newMenuBoxes |> Seq.item 6
                let box7id = newMenuBoxes |> Seq.item 7

                let newMenuBoxes = 
                    newMenuBoxes 
                    |> PList.map (fun idx -> 
                        if idx.id.Equals(box0id.id) then {idx with id = "Place Landmarks"}
                        else if idx.id.Equals(box1id.id) then {idx with id = "WIM Place Landmarks "}
                        else if idx.id.Equals(box2id.id) then {idx with id = "WIM"}
                        else if idx.id.Equals(box3id.id) then {idx with id = "Scale"}
                        else if idx.id.Equals(box4id.id) then {idx with id = "Reset"}
                        else if idx.id.Equals(box5id.id) then {idx with id = "Teleportation"}
                        else if idx.id.Equals(box6id.id) then {idx with id = "Drone mode"}
                        else if idx.id.Equals(box7id.id) then {idx with id = "Drone mode On Controller"}
                        else {idx with id = "Cyllinder"}
                        )
                {model with mainMenuBoxes = newMenuBoxes; menuButtonPressed = buttonPressed}
    
            else 
                {model with 
                    mainMenuBoxes           = PList.empty; 
                    menuButtonPressed       = buttonPressed; 
                    initialMenuPositionBool = false
                }
        | HoverIn id -> 
            match model.boxHovered with 
            | Some oldID when id = oldID -> model
            | _ ->
                { model with boxHovered = Some id}
        | HoverOut -> 
            if model.boxHovered.IsSome then
                { model with boxHovered = None}
            else 
                model
        | UpdateControllerPose (kind, p) -> 
            let newModel =
                 let controllerA = controllers |> HMap.tryFind ControllerKind.ControllerA
                 let controllerB = controllers |> HMap.tryFind ControllerKind.ControllerB
                 
                 match controllerA, controllerB with
                 | Some a, Some b -> 
                    let mayHoverMenu = UtilitiesMenu.mayHover model.mainMenuBoxes a b
                    match mayHoverMenu with
                     | Some id  -> //SELECT
                        if (a.joystickPressed || b.joystickPressed) then
                            let box0ID = model.mainMenuBoxes |> Seq.item 0
                            let box1ID = model.mainMenuBoxes |> Seq.item 1
                            let box2ID = model.mainMenuBoxes |> Seq.item 2
                            let box3ID = model.mainMenuBoxes |> Seq.item 3
                            let box4ID = model.mainMenuBoxes |> Seq.item 4
                            let box5ID = model.mainMenuBoxes |> Seq.item 5
                            let box6ID = model.mainMenuBoxes |> Seq.item 6
                            let box7ID = model.mainMenuBoxes |> Seq.item 7

                            let menuSelector = if a.joystickHold then a else b
                                
                            if box0ID.id = id then 
                                {   model with menu = MenuState.PlaceLandmarks; controllerMenuSelector = menuSelector; initialMenuState = MenuState.PlaceLandmarks}
                            else if box1ID.id = id then 
                                {   model with menu = MenuState.WIMLandmarks; controllerMenuSelector = menuSelector; initialMenuState = MenuState.WIMLandmarks}
                            else if box2ID.id = id then 
                                {   model with menu = MenuState.WIM; controllerMenuSelector = menuSelector; initialMenuState = MenuState.WIM}
                            else if box3ID.id = id then 
                                {   model with menu = MenuState.Scale; controllerMenuSelector = menuSelector; initialMenuState = MenuState.Scale}
                            else if box4ID.id = id then 
                                {   model with menu = MenuState.Reset; controllerMenuSelector = menuSelector; initialMenuState = MenuState.Reset}
                            else if box5ID.id = id then 
                                {   model with menu = MenuState.Teleportation; controllerMenuSelector = menuSelector; initialMenuState = MenuState.Teleportation}
                            else if box6ID.id = id then 
                                {   model with menu = MenuState.DroneMode; controllerMenuSelector = menuSelector; initialMenuState = MenuState.DroneMode}
                            else if box7ID.id = id then 
                                {   model with menu = MenuState.DroneModeController; controllerMenuSelector = menuSelector; initialMenuState = MenuState.DroneModeController}
                            else 
                                {
                                    model with 
                                        menu = MenuState.Cyllinder
                                        controllerMenuSelector = menuSelector; 
                                        initialMenuState = MenuState.Cyllinder
                                        mainMenuBoxes = PList.empty
                                }
                        else //HOVER
                            update controllers state vr model (HoverIn id)
                     | _ -> //HOVEROUT
                         update controllers state vr model HoverOut
                 | _ -> //DEFAULT
                    model

            newModel
        | Select (kind, buttonPressed) -> 
            match model.menuButtonPressed with 
            | true -> 
                if not(buttonPressed) then 
                    printfn "button unpressed, going to new mode %s" (model.menu.ToString())
                    update controllers state vr model (CreateMenu (model.controllerMenuSelector.kind, true))
                else model
            | false -> model
        | CloseMenu -> model
            
    let input (msg : VrMessage) =
        match msg with
        // buttons identifications: sensitive = 0, backButton = 1, sideButtons = 2
        | VrMessage.Touch(con,button) -> 
            match button with 
            | 0 -> [CreateMenu(con |> ControllerKind.fromInt, true)]
            | _ -> []
        | VrMessage.Untouch(con,button) -> 
            match button with 
            | 0 -> [CreateMenu(con |> ControllerKind.fromInt, false)]
            | _ -> []
        | VrMessage.UpdatePose(cn,p) -> 
            if p.isValid then 
                [UpdateControllerPose(cn |> ControllerKind.fromInt ,p)]
            else []
        | VrMessage.Press(cn,button) -> 
            match button with
            | 0 -> [Select(cn |> ControllerKind.fromInt, true)]
            | _ -> []//Select?
        | VrMessage.Unpress(cn,button) -> 
            match button with 
            | 0 -> [Select(cn |> ControllerKind.fromInt, false)]
            | _ -> []//UnSelect?
        | _ -> 
            []

    
    let ui (info : VrSystemInfo) (m : MMenuModel) : DomNode<MenuAction> = DomNode.Empty()

    let mkColor (model : MMenuModel) (box : MVisibleBox) =
        let id = box.id

        let color = 
            id
            |> Mod.bind (fun s ->
                let hoverColor =
                    model.boxHovered 
                    |> Mod.bind (function 
                        | Some k -> if k = s then Mod.constant C4b.Blue else box.color
                        | None -> box.color
                    )
                hoverColor
            )
        color
    
    let mkISg (model : MMenuModel) (box : MVisibleBox) =
        let color = mkColor model box
        let pos = box.trafo
        let font = Font.create "Consolas" FontStyle.Regular

        let menuText = 
            box.geometry |> Mod.map ( fun box1 -> 
                Sg.text font C4b.White box.id
                    |> Sg.noEvents
                    |> Sg.trafo(Mod.constant(Trafo3d.RotationInDegrees(V3d(90.0,0.0,90.0))))
                    |> Sg.scale 0.05
                    |> Sg.trafo(pos)
                    |> Sg.pickable (PickShape.Box (box1))
            )
                |> Sg.dynamic 
        
        let menuBox = 
            Sg.box color box.geometry
                |> Sg.noEvents
                |> Sg.trafo(pos)
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.vertexColor
                    }                    
                |> Sg.fillMode (Mod.constant FillMode.Line)

        menuText
        |> Sg.andAlso menuBox

    let vr (info : VrSystemInfo) (m : MMenuModel) : ISg<'a> = 

        let menuBox = 
            m.mainMenuBoxes
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkISg m b 
               )
            |> Sg.set
            |> Sg.effect [
                toEffect DefaultSurfaces.trafo
                toEffect DefaultSurfaces.vertexColor
                toEffect DefaultSurfaces.simpleLighting                              
            ]
            |> Sg.noEvents

        menuBox

    let threads (model : MenuModel) = 
        ThreadPool.empty

    let pause (info : VrSystemInfo) (m : MMenuModel) =
        Sg.box' C4b.Red Box3d.Unit
        |> Sg.noEvents
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }

    let initial =
        {
            menu                    = MenuState.Scale
            controllerMenuSelector  = ControllerInfo.initial
            initialMenuState        = MenuState.Scale
            menuButtonPressed       = false
            initialMenuPosition     = Pose.none
            initialMenuPositionBool = false
            mainMenuBoxes           = PList.empty
            boxHovered              = None
        }
    let app =
        {
            unpersist = Unpersist.instance
            initial = initial
            update = update (HMap.empty)
            threads = threads
            input = input 
            ui = ui
            vr = vr
            pauseScene = Some pause
        }