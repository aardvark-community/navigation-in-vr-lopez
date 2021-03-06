namespace Demo.Main

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Opc
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Trafos
open Aardvark.UI.Generic
open FShade
open Aardvark.Application.OpenVR
open Aardvark.Vr

open OpcViewer.Base
open OpcViewer.Base.Picking
open OpcViewer.Base.Attributes
open Demo.Menu
open Demo

module Shaders = 
    open FShade
    open Aardvark.Base.Rendering.Effects

    let blendColor (c : C4f) (v : Vertex) = 
        let c = c.ToV4d()
        fragment {
            return c * v.c
        }

type DemoAction =
| SetText of string 
| ToggleVR
| MenuMessage of MenuAction * ControllerKind * bool
| CameraMessage         of FreeFlyController.Message    
| SetControllerPosition of ControllerKind *  Pose
| Select            of ControllerKind * ControllerButtons * bool
| OpcViewerMsg of PickingAction
| GetTrackpadPosition of ControllerKind * int * V2d

module Demo =
    open Aardvark.Application //TODO ML: avoid using module opens ... just put them up in one place
    open Aardvark.VRVis.Opc
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    open Model
    open OpenTK
    open Valve.VR
    open OpenTK.Input
    open Aardvark.UI.Extensions
    open OpcViewer.Base.Shader
    open Aardvark.Base
    open Aardvark.Base.MultimethodTest
    open Aardvark.Base.IndexedGeometryPrimitives
    
    let show  (att : list<string * AttributeValue<_>>) (sg : ISg<_>) =

        let view (m : MCameraControllerState) =
            let frustum = Frustum.perspective 60.0 0.1 1000.0 1.0 |> Mod.constant
            FreeFlyController.controlledControl m id frustum (AttributeMap.ofList att) sg
            
        let app =
            {
                initial = FreeFlyController.initial
                update = FreeFlyController.update
                view = view
                threads = FreeFlyController.threads
                unpersist = Unpersist.instance
            }

        subApp' (fun _ _ -> Seq.empty) (fun _ _ -> Seq.empty) [] app

    let rec update (state : VrState) (vr : VrActions) (model : Model) (msg : DemoAction) : Model =
        match msg with
        | OpcViewerMsg m -> 
            let newOpcModel = OpcViewer.Base.Picking.PickingApp.update OpcViewer.Base.Picking.PickingModel.initial m
            {model with pickingModel = newOpcModel}
        | SetText t -> 
            { model with text = t }
        | ToggleVR ->
            if model.vr then vr.stop()
            else vr.start()
            { model with vr = not model.vr }
        | MenuMessage (a, kind, buttonTouched) ->   
            
            let updateCont = 
                model.controllerInfos 
                |> HMap.alter kind (fun old -> 
                    match old, buttonTouched with 
                    | Some x, true -> 
                        Some { x with joystickHold = true }   // update / overwrite
                    | Some x, false -> 
                        Some { x with joystickHold = false }   // update / overwrite
                    | None, true -> 
                        Some 
                            {ControllerInfo.initial with 
                                kind            = kind
                                buttonKind      = ControllerButtons.Joystick
                                joystickHold    = true 
                            } //create
                    | None, false -> 
                        Some
                            {ControllerInfo.initial with 
                                kind        = kind
                                buttonKind  = ControllerButtons.Joystick 
                            } //create
                )

            let newModel = {model with controllerInfos = updateCont}

            let controllers = newModel.controllerInfos

            let newModel = 
                match buttonTouched with 
                | true -> 
                    let newMenuModel = {newModel.menuModel with menu = Menu.MenuState.InMenu}
                    {newModel with menuModel = newMenuModel}
                | false -> 
                    let newMenuModel = {newModel.menuModel with menu = newModel.menuModel.initialMenuState}
                    {model with menuModel = newMenuModel}

            let newMenuModel = 
                let checkCon = controllers |> HMap.tryFind kind
                match checkCon with 
                | Some controller -> 
                    if not (controller.sideButtonPressed) then 
                        MenuApp.update controllers state vr newModel.menuModel a
                    else newModel.menuModel
                | None -> newModel.menuModel
            
            { newModel with 
                menuModel = newMenuModel; 
            }
        | CameraMessage m -> 
            { model with cameraState = FreeFlyController.update model.cameraState m }   
        | SetControllerPosition (kind, p) ->    
            let newModel = 
                match model.menuModel.menu with 
                | Menu.MenuState.PlaceLandmarks ->
                    model 
                    |> PlaceLandmark.placing kind p
                | Menu.MenuState.Scale -> 
                    let model = 
                        model 
                        |> NavigationOpc.currentSceneInfo kind p 

                    model
                | Menu.MenuState.Cyllinder -> 
                    let model = 
                        model
                        |> WIMOpc.checkHoverUserWIM kind p

                    let model = 
                        model
                        |> CyllinderCenter.checkInside kind p

                    model
                    |> CyllinderCenter.controlCenter kind p
                | Menu.MenuState.WIM -> 
                    let newModel = 
                        model 
                        |> WIMOpc.updateMiniMap kind p

                    let newModel = 
                        newModel
                        |> WIMOpc.editMiniMap kind p

                    newModel 
                | Menu.MenuState.WIMLandmarks -> 
                    let model = 
                        model
                        |> WIMOpc.editMiniMap kind p

                    let model = 
                        model
                        |> WIMOpc.checkHoverUserWIM kind p
                    
                    let model = 
                        model
                        |> PlaceLandmark.placingOnWIM kind p

                    model 
                    |> PlaceLandmark.hoverEvaluationLandmarks kind p 
                | Menu.MenuState.Teleportation -> 
                    let model = 
                        model 
                        |> Teleport.hitRay kind p

                    let model = 
                        model 
                        |> Teleport.rayIntersection kind p

                    model 
                    |> PlaceLandmark.hoverEvaluationLandmarks kind p
                | Menu.MenuState.DroneMode -> 
                    let controllerPos = model.menuModel.controllerMenuSelector
                    let userHMD = model.controllerInfos |> HMap.tryFind ControllerKind.HMD
                    let conPos = model.controllerInfos |> HMap.tryFind controllerPos.kind
                    let newHMDTrafo = 
                        match userHMD, conPos with 
                        | Some hmdPos, Some cPos ->     
                            let hmdDir = hmdPos.pose.deviceToWorld.Forward.C1

                            //Trafo3d.Translation(hmdPos.pose.deviceToWorld.GetModelOrigin() + hmdDir.XYZ) * Trafo3d.Translation(V3d(2.0, -1.5, -1.5))// * Trafo3d.Translation(HMDpos.pose.deviceToWorld.Forward.TransformDir V3d.YAxis) 
                            Trafo3d.Translation(cPos.pose.deviceToWorld.GetModelOrigin()) * Trafo3d.Translation(V3d(1.5, -1.5, -1.5))// * Trafo3d.Translation(HMDpos.pose.deviceToWorld.Forward.TransformDir V3d.YAxis) 
                        | _, _ -> Trafo3d.Identity

                    let model = 
                        model 
                        |> DroneControlCenter.checkHoverScreen kind p 

                    let model = 
                        model 
                        |> DroneControlCenter.moveDrone kind p
                        
                    model 
                    |> PlaceLandmark.hoverEvaluationLandmarks kind p
                | Menu.MenuState.HoverDroneScreen ->  
                    let model = 
                        model 
                        |> DroneControlCenter.moveScreenPos kind p
                    
                    model 
                    |> DroneControlCenter.checkHoverScreen kind p 
                | Menu.MenuState.DroneModeController -> 
                    let model = 
                        model 
                        |> DroneControlCenter.moveDrone kind p

                    let model = 
                        model
                        |> DroneControlCenter.moveScreenAttachedController kind p

                    model 
                    |> PlaceLandmark.hoverEvaluationLandmarks kind p
                | Menu.MenuState.HoverChangeUserWIM -> 
                    let model = 
                        model
                        |> WIMOpc.editMiniMap kind p
                    let model = 
                        model 
                        |> WIMOpc.changeUserPosWIM kind p
                    
                    let model = 
                        model
                        |> WIMOpc.checkHoverUserWIM kind p

                    model 
                    |> PlaceLandmark.hoverEvaluationLandmarks kind p
                | _ -> 
                    let newControllersPosition = 
                        model 
                        |> OpcUtilities.updateControllersInfo kind p
        
                    {model with controllerInfos = newControllersPosition}

            let controllerMenuUpdate = MenuApp.update model.controllerInfos state vr newModel.menuModel (MenuAction.UpdateControllerPose (kind, p))
            {newModel with 
                menuModel = controllerMenuUpdate; 
            }
        | Select (kind, buttonKind, buttonPress)-> 
            printfn "Menu mode is: %s when buttonpress is: %s" (model.menuModel.menu.ToString()) (buttonPress.ToString())

            let updateControllerButtons = 
                model.controllerInfos
                |> HMap.alter kind (fun but ->  
                match but with
                | Some x -> 
                    match buttonKind with 
                    | ControllerButtons.Joystick -> Some {x with joystickPressed = buttonPress}
                    | ControllerButtons.Back -> Some {x with backButtonPressed = buttonPress}
                    | ControllerButtons.Side -> Some {x with sideButtonPressed = buttonPress}
                    | ControllerButtons.Home -> Some {x with homeButtonPressed = buttonPress}
                    | _ -> None
                    
                | None -> 
                    match buttonKind with 
                    | ControllerButtons.Side -> 
                        Some 
                            {
                                ControllerInfo.initial with 
                                    kind                = kind
                                    buttonKind          = buttonKind
                                    sideButtonPressed   = buttonPress
                            }
                    | ControllerButtons.Back -> 
                        Some 
                            {
                                ControllerInfo.initial with 
                                    kind                = kind 
                                    buttonKind          = buttonKind
                                    backButtonPressed   = buttonPress
                            }
                    | ControllerButtons.Joystick -> 
                        Some 
                            {
                                ControllerInfo.initial with 
                                    kind                = kind 
                                    buttonKind          = buttonKind
                                    joystickPressed     = buttonPress
                            }
                    | ControllerButtons.Home -> 
                        Some 
                            {
                                ControllerInfo.initial with
                                    kind                = kind
                                    buttonKind          = buttonKind
                                    homeButtonPressed   = buttonPress
                            }
                    | _ -> None
                )
            
            let newModel = {model with controllerInfos = updateControllerButtons}
            
            let newModel = 
                match buttonKind with 
                | ControllerButtons.Home -> 
                    let homeButtonPressed = {newModel.menuModel with menu = MenuState.PlaceLandmarks}
                    {newModel with 
                        menuModel           = homeButtonPressed
                    }
                | _ -> newModel
                
            let controllerMenuUpdate = MenuApp.update newModel.controllerInfos state vr newModel.menuModel (MenuAction.Select (kind, buttonPress))

            let newModel = {newModel with menuModel = controllerMenuUpdate}

            let controllerPos = newModel.controllerInfos |> HMap.tryFind kind
            match controllerPos with 
            | Some id -> 
                match newModel.menuModel.menu with 
                | Menu.MenuState.WIM -> 
                    let newUserPosWIM = OpcUtilities.mkFlagsUser Trafo3d.Identity 1 
                    
                    let newUserPosWIMcone = OpcUtilities.mkCone Trafo3d.Identity 1

                    let newModel = {newModel with WIMuserPos = newUserPosWIM; WIMuserPosCone = newUserPosWIMcone}

                    let newModel = newModel |> WIMOpc.showMiniMap

                    let newLandmarkList = OpcUtilities.mkEvalFlags (Trafo3d.Translation(V3d.One * 100000.0)) 5
                    let newLandmarkListLook = OpcUtilities.mkCone (Trafo3d.Translation(V3d.One * 100000.0)) 5
                    
                    let newModel = 
                        {newModel with 
                            droneControl = Drone.initial; 
                            evaluationLandmarksWIM2RealWorld = newLandmarkList
                            evaluationLandmarksWIM = newLandmarkList 
                            evaluationLandmarksWIMLook = newLandmarkListLook
                        }

                    newModel 
                    |> PlaceLandmark.updateLandmarksPosition
                | Menu.MenuState.WIMLandmarks ->
                    let newUserPos = 
                        if newModel.userPosOnAnnotationSpace.Count.Equals(0) then 
                            OpcUtilities.mkFlags id.pose.deviceToWorld 1
                        else newModel.userPosOnAnnotationSpace
                    let newInitialUserPosWIM = 
                        OpcUtilities.mkFlags (Trafo3d.Translation(V3d.One * 1000000.0)) 1
                    let newInitialUserPosWIMcone = 
                        OpcUtilities.mkCone Trafo3d.Identity 1

                    let newModel = 
                        newModel 
                        |> PlaceLandmark.updateLandmarksPosition

                    {newModel with 
                        userPosOnAnnotationSpace= newUserPos
                        WIMinitialUserPos       = newInitialUserPosWIM
                        WIMinitialUserPosCone   = newInitialUserPosWIMcone
                        //droneControl            = Drone.initial;
                        //evaluationLandmarksWIM2RealWorld = newLandmarkList
                        //evaluationLandmarksWIM = newLandmarkList
                    }
                | Menu.MenuState.Teleportation -> 
                    let controllTrafo = id.pose.deviceToWorld
                    
                    let origin = controllTrafo.Forward.TransformPos V3d.Zero
                    let controllDir = controllTrafo.Forward.TransformDir V3d.YAxis
                    
                    //the next lines are the same as the previous ones
                    //let controllDir = controllTrafo.GetViewDirectionLH()
                    //let origin = controllTrafo.GetModelOrigin()

                    let testRay = Ray3d(origin, controllDir) 

                    let newTeleportCone = 
                        let coneList = 
                            if newModel.teleportCone.Count.Equals(0) then 
                                OpcUtilities.mkCone id.pose.deviceToWorld 1
                            else newModel.teleportCone
                        let firstCone = coneList |> PList.tryFirst
                        match firstCone with 
                        | Some cone -> {cone with geometry = Cone.solidCone (cone.trafo.GetModelOrigin()) V3d.Zero 500.0 500.0 20 C4b.Red} |> PList.single
                        | None -> newModel.teleportCone
                    
                    let newBox = 
                        let boxList = 
                            if newModel.teleportBox.Count.Equals(0) then 
                                OpcUtilities.mkFlags id.pose.deviceToWorld 1
                            else newModel.teleportBox
                        let firstBox = boxList |> PList.tryFirst
                        match firstBox with 
                        | Some box -> {box with geometry = Box3d.FromSize(V3d(5.0, 1.0, 1.0))} |> PList.single
                        | None -> newModel.teleportBox

                    
                    let newModel = 
                        {newModel with 
                            teleportRay = testRay; 
                            droneControl = Drone.initial; 
                            teleportBox = newBox;
                            teleportCone = newTeleportCone
                        }

                    let newModel = 
                        newModel 
                        |> PlaceLandmark.updateLandmarksPosition

                    newModel 
                    |> Teleport.teleportUser
                | Menu.MenuState.DroneModeController -> 
                    let newDrone = 
                        if newModel.droneControl.drone.Count.Equals(0) then 
                            let newTrafo = Trafo3d.Translation(V3d(id.pose.deviceToWorld.GetModelOrigin().X + 2.0, id.pose.deviceToWorld.GetModelOrigin().Y, id.pose.deviceToWorld.GetModelOrigin().Z))
                            OpcUtilities.mkDrone newTrafo 1
                        else newModel.droneControl.drone

                    let newDroneScreen = 
                        if newModel.droneControl.screen.Count.Equals(0) then 
                            //VisibleBox.createDroneScreenOnController C4b.Red (id.pose.deviceToWorld.GetModelOrigin())
                            VisibleBox.createDroneScreenOnController C4b.Red (newModel.droneControl.cameraPosition.GetModelOrigin() + V3d(0.1, 0.0, 0.0))
                            |> PList.single
                        else newModel.droneControl.screen

                    let updateDrones = 
                        {newModel.droneControl with 
                            drone = newDrone; 
                            screen = newDroneScreen
                        }

                    let newModel = 
                        {newModel with 
                            WIMopcSpaceTrafo                = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0)); 
                            WIMuserPos                      = PList.empty;
                            droneControl                    = updateDrones
                        }

                    let newModel = 
                        newModel 
                        |> PlaceLandmark.updateLandmarksPosition

                    newModel
                    |> DroneControlCenter.moveUserToDronePos
                | Menu.MenuState.Reset -> 
                    initial
                | Menu.MenuState.PlaceLandmarks -> newModel
                | Menu.MenuState.Scale -> 
                    let newModel = newModel |> NavigationOpc.initialSceneInfo
                    {newModel with 
                        WIMopcSpaceTrafo = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0)); 
                        droneControl = Drone.initial;
                        WIMuserPos = PList.empty
                    }
                | Menu.MenuState.Cyllinder -> 
                    let newUserPosWIM = 
                        if newModel.WIMuserPos.Count.Equals(0) then 
                            OpcUtilities.mkFlagsUser Trafo3d.Identity 1 
                        else newModel.WIMuserPos
                    let newInitialUserPosWIM = 
                        OpcUtilities.mkFlags (Trafo3d.Translation(V3d.One * 1000000.0)) 1
                    let newUserPos = 
                        if newModel.userPosOnAnnotationSpace.Count.Equals(0) then 
                            OpcUtilities.mkFlags id.pose.deviceToWorld 1
                        else newModel.userPosOnAnnotationSpace
                    
                    {newModel with 
                        WIMuserPos                  = newUserPosWIM;
                        userPosOnAnnotationSpace    = newUserPos;
                        WIMinitialUserPos           = newInitialUserPosWIM
                    }
                | Menu.MenuState.DroneMode -> 
                    let newDrone = 
                        if newModel.droneControl.drone.Count.Equals(0) then 
                            let newTrafo = Trafo3d.Translation(V3d(id.pose.deviceToWorld.GetModelOrigin().X + 2.0, id.pose.deviceToWorld.GetModelOrigin().Y, id.pose.deviceToWorld.GetModelOrigin().Z))
                            OpcUtilities.mkDrone newTrafo 1
                        else newModel.droneControl.drone
                    
                    let newDroneScreen = 
                        if newModel.droneControl.screen.Count.Equals(0) then 
                            VisibleBox.createDroneScreen C4b.Red (newModel.droneControl.cameraPosition.GetModelOrigin() + V3d(0.2, -0.25, -0.25))
                            |> PList.single
                        else newModel.droneControl.screen

                    let updateDrones = 
                        {newModel.droneControl with 
                            drone = newDrone; 
                            screen = newDroneScreen
                        }

                    let newModel = 
                        {newModel with 
                            WIMopcSpaceTrafo                = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0)); 
                            WIMuserPos                      = PList.empty;
                            droneControl                    = updateDrones
                        }

                    newModel
                    |> DroneControlCenter.moveUserToDronePos
                | Menu.MenuState.HoverChangeUserWIM -> 
                    let newUserPos1 = 
                        if newModel.userPosOnAnnotationSpace.Count.Equals(0) then 
                            OpcUtilities.mkFlags id.pose.deviceToWorld 1
                        else newModel.userPosOnAnnotationSpace
                    let newInitialUserPosWIM1 = OpcUtilities.mkFlags (Trafo3d.Translation(V3d.One * 1000000.0)) 1
                    let newInitialUserPosWIMcone = OpcUtilities.mkCone Trafo3d.Identity 1

                    let newModel = 
                        {newModel with 
                            userPosOnAnnotationSpace= newUserPos1;
                            WIMinitialUserPos       = newInitialUserPosWIM1;
                            WIMinitialUserPosCone   = newInitialUserPosWIMcone;
                            droneControl            = Drone.initial;
                        }

                    let newModel = 
                        newModel 
                        |> PlaceLandmark.updateLandmarksPosition   

                    newModel 
                    |> WIMOpc.moveUserToAnnotationSpaceFromWIM 
                | Menu.MenuState.HoverDroneScreen -> 
                    let newInitConTrafo = 
                        newModel 
                        |> OpcUtilities.getWorldTrafoIfBackPressed kind
                    let updateDrones = 
                        {newModel.droneControl with 
                            initControlTrafo = Trafo3d.Translation(newInitConTrafo.GetModelOrigin())
                            initCameraPosition = newModel.droneControl.cameraPosition
                        }
                    
                    {newModel with droneControl = updateDrones}
                | _ -> newModel
            | None -> newModel
        | GetTrackpadPosition (con, axis, pos) -> 
            model 
                
    let mkColor (model : MModel) (box : MVisibleBox) =
        let id = box.id

        let color = 
            id
            |> Mod.bind (fun s ->

                let hoverColor =
                    model.menuModel.boxHovered 
                    |> Mod.bind (function 
                        | Some k -> if k = s then Mod.constant C4b.Blue else box.color
                        | None -> box.color
                    )
                hoverColor
            )
        color
    
    let mkISg (model : MModel) (box : MVisibleBox) =
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
                    //do! DefaultSurfaces.simpleLighting
                    }                
                |> Sg.withEvents [
                    Sg.onEnter (fun _  -> HoverIn  (box.id.ToString()))
                    Sg.onLeave (fun _ -> HoverOut)
                ]     
                |> Sg.fillMode (Mod.constant FillMode.Line)

        menuText
        |> Sg.andAlso menuBox

    let mkDrawingBox (model : MModel) (box : MVisibleBox) =
        let color = box.color
        let pos = box.trafo
        Sg.box color box.geometry
            |> Sg.noEvents
            |> Sg.trafo(pos)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
                }                

    let threads (model : Model) =
        ThreadPool.empty
        
    let input (state : VrState) (msg : VrMessage) =
        match msg with
        // buttons identifications: sensitive = 0, backButton = 1, sideButtons = 2
        | VrMessage.Touch(con,button) -> 
            printfn "touchin button: %d" button
            match button with 
            | 0 -> 
                [MenuMessage (Demo.MenuAction.CreateMenu(con |> ControllerKind.fromInt, true), con |> ControllerKind.fromInt, true)]
            | _ -> []
        | VrMessage.Untouch(con,button) -> 
            match button with 
            | 0 -> 
                printfn "UNtouchin"
                [MenuMessage (Demo.MenuAction.CreateMenu(con |> ControllerKind.fromInt, false), con |> ControllerKind.fromInt, false)]
            | _ -> []
        | VrMessage.ValueChange(con, axis, pos) ->
            match axis with 
            | 0 -> 
                [GetTrackpadPosition(con |> ControllerKind.fromInt, axis, pos)]
            | _ -> []
        | VrMessage.PressButton(con,button) ->
            printfn "button pressed: %d" button
            match button with 
            | 2 -> [Select(con |> ControllerKind.fromInt, button |> ControllerButtons.fromInt, true)]
            | 1 -> [Select(con |> ControllerKind.fromInt, 3 |> ControllerButtons.fromInt, true)]
            | _ -> []
        | VrMessage.UnpressButton(con, button) -> 
            match button with 
            | 2 -> [Select(con |> ControllerKind.fromInt, button |> ControllerButtons.fromInt, false)]
            | _ -> []
        | VrMessage.UpdatePose(cn,p) -> 
            
            // old way: 
            if p.isValid then 
                [SetControllerPosition (cn |> ControllerKind.fromInt, p)]
            else []
            // new way:
            //if p.isValid then 
            //    let kind  =
            //        match HMap.tryFind cn state.devices with
            //        | None -> []
            //        | Some d -> 
            //            [SetControllerPosition (d.kind, p)]
            //else []
        | VrMessage.Press(con,button) -> 
            printfn "%d Touch identification %d" con button
            match button with
            | _ -> [Select(con |> ControllerKind.fromInt, button |> ControllerButtons.fromInt, true)]
        | VrMessage.Unpress(con,button) -> 
            printfn "Touch unpressed by %d" con
            match button with 
            | _ -> [Select (con |> ControllerKind.fromInt, button |> ControllerButtons.fromInt, false)]
        | _ -> 
            []

    let mkCone (cp : MVisibleCone) (color : IMod<C4b>) =
        Sg.cone 20 color (Mod.constant 0.5) (Mod.constant 5.0) 
            |> Sg.noEvents
            |> Sg.scale 0.01
            |> Sg.trafo (Mod.constant (Trafo3d.RotationInDegrees(V3d(-90.0,90.0,0.0))))
            |> Sg.trafo cp.trafo
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
                }

    let mkConeLook (cp : MVisibleCone) (color : IMod<C4b>) =
        Sg.cone 20 color (Mod.constant 0.5) (Mod.constant 5.0) 
            |> Sg.noEvents
            |> Sg.scale 5.0
            |> Sg.trafo cp.trafo
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
                }
    
    let mkConeLookWIM (cp : MVisibleCone) (color : IMod<C4b>) =
        Sg.cone 20 color (Mod.constant 0.25) (Mod.constant 5.0) 
            |> Sg.noEvents
            |> Sg.scale 4.25
            |> Sg.trafo cp.trafo
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
                }
    
    let mkConeTeleport (cp : MVisibleCone) (color : IMod<C4b>) =
        Sg.cone 20 color (Mod.constant 0.5) (Mod.constant 5.0) 
            |> Sg.noEvents
            |> Sg.trafo (Mod.constant (Trafo3d.RotationInDegrees(V3d(-90.0,90.0,0.0))))
            |> Sg.trafo cp.trafo
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
                }     
    
    let mkFlag (model : MModel) (box : MVisibleBox) =
        let color = mkColor model box
        let pos = box.trafo

        Sg.box color box.geometry
            |> Sg.noEvents
            |> Sg.trafo(pos)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
                } 

    let mkFlagCullMode (model : MModel) (box : MVisibleBox) =
        let color = //mkColor model box
            box.color |> Mod.map (fun yy -> C4f(yy.ToC3f(), 0.5f).ToC4b())
            
        let pos = box.trafo

        Sg.box color box.geometry
            |> Sg.noEvents
            |> Sg.trafo(pos)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
                } 
            |> Sg.blendMode (Mod.constant BlendMode.Blend)
            |> Sg.cullMode (Mod.constant CullMode.Front)
            |> Sg.pass (RenderPass.after "" RenderPassOrder.Arbitrary RenderPass.main)
                               
    let mkSphere (model : MModel) (sphere : MVisibleSphere) =
        let color = sphere.color
        let pos = sphere.trafo

        Sg.sphere 10 color sphere.radius
            |> Sg.noEvents
            |> Sg.trafo(pos)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
                }  

    let mkCylinder (model : MModel) (cylinder : MVisibleCylinder) = 
        let pos = cylinder.trafo
        let color = 
            let hsv = HSVf((1.0 - 60.0) * 0.625, 1.0, 1.0)
            let col = C4f(hsv.ToC3f(), 0.5f).ToC4b()
            Mod.constant col
        let rad = cylinder.radius

        Sg.cylinder 50 color rad (Mod.constant 100.0)
            |> Sg.noEvents
            |> Sg.trafo(pos)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
            }

    let mkControllerBox (cp : MControllerInfo) (model : MModel) =
        let mkDisappear = 
            let conKind = cp.kind
                
            adaptive {
                let! ck = conKind
                match ck with 
                | ControllerKind.ControllerA -> return true 
                | _ -> return false
            }
        Sg.cone' 20 C4b.Cyan 0.5 5.0 
            |> Sg.noEvents
            |> Sg.scale 0.01
            |> Sg.trafo (Mod.constant (Trafo3d.RotationInDegrees(V3d(-90.0,90.0,0.0))))
            |> Sg.trafo cp.pose.deviceToWorld
            |> Sg.onOff mkDisappear

    let ui' (info : VrSystemInfo) (m : MModel) = 
        let text = m.vr |> Mod.map (function true -> "Stop VR" | false -> "Start VR")

        let opcs = 
            m.opcInfos
              |> AMap.toASet
              |> ASet.map(fun info -> Sg.createSingleOpcSg m.opcAttributes.selectedScalar (Mod.constant false) m.cameraState.view info)
              |> Sg.set
              |> Sg.effect [ 
                toEffect Shader.stableTrafo
                toEffect DefaultSurfaces.diffuseTexture  
                toEffect Shader.AttributeShader.falseColorLegend //falseColorLegendGray
                ]
      
        let frustum =
            Mod.constant (Frustum.perspective 60.0 0.1 100.0 1.0)

        div [ style "width: 100%; height: 100%" ] [
            FreeFlyController.controlledControl m.cameraState CameraMessage m.mainFrustum
                (AttributeMap.ofList [
                    style "width: 100%; height:100%"; 
                    attribute "showFPS" "true";       // optional, default is false
                    attribute "useMapping" "true"
                    attribute "data-renderalways" "false"
                    attribute "data-samples" "4"
                ])
                (
                    opcs
                    |> Sg.map OpcViewerMsg
                    |> Sg.noEvents
                )
            button [ style "position: fixed; bottom: 5px; right: 5px"; onClick (fun () -> ToggleVR) ] text
        ]
    
    let vr' (runtime : IRuntime) (info : VrSystemInfo) (m : MModel) = 
        let defaultEffect (msg : ISg<_>)=    
            msg
            |> Sg.effect [
                toEffect DefaultSurfaces.trafo
                toEffect DefaultSurfaces.vertexColor
                toEffect DefaultSurfaces.simpleLighting                              
            ]       

        let showControllerCones = 
            m.controllerInfos
            |> AMap.toASet
            |> ASet.map (fun boxController -> 
                let ci_pose = snd boxController
                mkControllerBox ci_pose m)
            |> Sg.set
            |> defaultEffect
        
        let drones = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.DroneMode | MenuState.HoverDroneScreen | MenuState.DroneModeController -> return true 
                    | _ -> return false
                }
            m.droneControl.drone
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents
            |> Sg.onOff mkDisappear

        let userPosOnWIM = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.WIM | MenuState.WIMLandmarks | MenuState.HoverChangeUserWIM -> return true 
                    | _ -> return false
                }
            m.WIMuserPos
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents
            |> Sg.onOff mkDisappear

        let userConeOnWim = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.WIM | MenuState.WIMLandmarks | MenuState.HoverChangeUserWIM -> return true 
                    | _ -> return false
                }
            m.WIMuserPosCone
            |> AList.toASet
            |> ASet.map (fun b -> 
                mkCone b b.color
            )
            |> Sg.set
            |> defaultEffect
            |> Sg.onOff mkDisappear

        let initialUserPosOnWIM = 
            m.WIMinitialUserPos
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents

        let initialUserConeOnWim = 
            m.WIMinitialUserPosCone
            |> AList.toASet
            |> ASet.map (fun b -> 
                mkCone b b.color
            )
            |> Sg.set
            |> defaultEffect

        let userPosOnAnnotationSpace =  
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                let controllerPos = m.menuModel.controllerMenuSelector
                let con = m.controllerInfos |> AMap.tryFind (controllerPos.kind.GetValue())
                let conBackButton = 
                    con
                    |> Mod.bind (fun c -> 
                        match c with 
                        | Some cc -> cc.backButtonPressed
                        | None -> Mod.constant false
                    )

                adaptive {
                    let! conBB = conBackButton
                    let! newMenuMode = menuMode
                    
                    match newMenuMode, conBB with 
                    | MenuState.WIMLandmarks, true -> 
                        return true
                    | MenuState.HoverChangeUserWIM, true -> return true
                    |  _, _ -> return false
                }

            m.userPosOnAnnotationSpace
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents
            |> Sg.onOff mkDisappear

        let evaluationLands = 
            m.evaluationLandmarks
            |> AList.toASet 
            |> ASet.map (fun b ->
                mkFlagCullMode m b
            )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents

        let evaluationLandsLook = 
            m.evaluationLandmarksLook
            |> AList.toASet 
            |> ASet.map (fun b ->
                mkConeLook b b.color
            )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents

        let evaluationLandsOnWIM = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.WIM | MenuState.WIMLandmarks | MenuState.HoverChangeUserWIM -> return true 
                    | _ -> return false
                }
            m.evaluationLandmarksWIM
            |> AList.toASet 
            |> ASet.map (fun b ->
                mkFlag m b
            )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents
            |> Sg.onOff mkDisappear

        let evaluationLandsOnWIMLook = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.WIM | MenuState.WIMLandmarks | MenuState.HoverChangeUserWIM -> return true 
                    | _ -> return false
                }
            m.evaluationLandmarksWIMLook
            |> AList.toASet 
            |> ASet.map (fun b ->
                mkConeLookWIM b b.color
            )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents
            |> Sg.onOff mkDisappear

        let evaluationLandsOnWIM2RealWorld = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.WIM | MenuState.WIMLandmarks | MenuState.HoverChangeUserWIM -> return true 
                    | _ -> return false
                }
            m.evaluationLandmarksWIM2RealWorld
            |> AList.toASet 
            |> ASet.map (fun b ->
                mkFlag m b
            )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents
            |> Sg.onOff mkDisappear

        let deviceSgs = 
            info.state.devices |> AMap.toASet |> ASet.chooseM (fun (_,d) ->
                d.Model |> Mod.map (fun m ->
                    match m with
                    | Some sg -> 
                        sg 
                        |> Sg.noEvents 
                        |> Sg.trafo d.pose.deviceToWorld
                        |> Sg.onOff d.pose.isValid
                        |> Sg.shader {
                            do! DefaultSurfaces.trafo
                            do! DefaultSurfaces.diffuseTexture
                            do! Shaders.blendColor (if d.id % 2 = 0 then C4f.Red else C4f.Green)
                        }
                        |> Some
                    | None -> 
                        None 
                )
            )
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.simpleLighting
            }
    
        let menuApp = 
            MenuApp.vr info m.menuModel
            |> Sg.map MenuMessage

        let opcs = 
            m.opcInfos
                |> AMap.toASet
                |> ASet.map(fun info -> 
                    Sg.createSingleOpcSg m.opcAttributes.selectedScalar (Mod.constant false) m.cameraState.view info
                    )
                |> Sg.set
                |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.diffuseTexture  
                    toEffect Shader.AttributeShader.falseColorLegend
                ]
                |> Sg.noEvents
                |> Sg.map OpcViewerMsg
                |> Sg.noEvents     
                |> Sg.trafo m.opcSpaceTrafo

        let opcsCamera = 
            let startOpcTrafo = Trafo3d.FromBasis(V3d(0.0138907544072255, 0.0370928394273679, 0.410690910035505), V3d(0.11636514267386, 0.393870197365478, -0.0395094556451799), V3d(-0.395603213079913, 0.117157783795495, 0.0027988969790869), V3d(-57141.4217354136, 16979.9987604353, -1399135.09579421))
            m.opcInfos
                |> AMap.toASet
                |> ASet.map(fun info -> 
                    Sg.createSingleOpcSg m.opcAttributes.selectedScalar (Mod.constant false) m.cameraState.view info
                    )
                |> Sg.set
                |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.diffuseTexture  
                    toEffect Shader.AttributeShader.falseColorLegend
                ]
                |> Sg.noEvents
                |> Sg.map OpcViewerMsg
                |> Sg.noEvents     
                |> Sg.trafo (Mod.constant startOpcTrafo)

        let WIMopcs = 
            m.WIMopcInfos
                |> AMap.toASet
                |> ASet.map(fun info -> 
                    Sg.createSingleOpcSg m.opcAttributes.selectedScalar (Mod.constant false) m.cameraState.view info
                    )
                |> Sg.set
                |> Sg.effect [ 
                    toEffect Shader.stableTrafo
                    toEffect DefaultSurfaces.diffuseTexture  
                    toEffect Shader.AttributeShader.falseColorLegend
                ]
                |> Sg.noEvents
                |> Sg.map OpcViewerMsg
                |> Sg.noEvents     
                |> Sg.trafo m.WIMopcSpaceTrafo

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = 1 }
                DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
            ]
        
        let size = V2i(1024,1024) |> Mod.init 

        let dronetrafo  =
            m.droneControl.drone
            |> AList.toMod
            |> Mod.bind 
                (fun s -> 
                    s |> Seq.tryHead 
                        |> Option.map (fun d -> d.trafo) 
                        |> (Option.defaultValue (Mod.constant Trafo3d.Identity))
                )
        
        let dronePos = dronetrafo |> Mod.map (fun t -> t.GetModelOrigin())
            
        let droneDir = dronetrafo |> Mod.map (fun t -> t.Forward.C3.XYZ)
        
        let dirHMD = 
            let findHMD = 
                m.controllerInfos
                |> AMap.toMod
                |> Mod.bind (fun ci -> 
                    let test = ci |> HMap.tryFind ControllerKind.HMD
                    match test with 
                    | Some id -> id.pose.deviceToWorld
                    | None -> Mod.constant Trafo3d.Identity
                )
            adaptive {
                let! hmd = findHMD
                return hmd.Forward.C1.XYZ    
            }

        let dirController = 
            let controllerPos = m.menuModel.controllerMenuSelector 
            let controllerPosKind = controllerPos.kind
            let con = m.controllerInfos |> AMap.tryFind (controllerPos.kind.GetValue()) //check if the reason the vulkan error is this, because when i use the controller contrary to the line attached to controller in teleportation, it works, when i use the same cnotroller as teh line projected in teleportation it never works. 
            let secondCon = 
                if controllerPos.kind.Equals(ControllerKind.ControllerA) then
                    m.controllerInfos |> AMap.tryFind ControllerKind.ControllerA
                else m.controllerInfos |> AMap.tryFind ControllerKind.ControllerB
            let findHMD = 
                con
                |> Mod.bind (fun x -> 
                    match x with 
                    | Some id -> id.pose.deviceToWorld
                    | None -> Mod.constant Trafo3d.Identity
                )
                
            adaptive {
                let! conKind = controllerPosKind
                let! hmd = findHMD
                return hmd.Forward.C1.XYZ    
            }
            
        let offscreenTask = 
            opcsCamera
            |> Sg.andAlso evaluationLands //landmarksOnAnnotationSpace
            |> Sg.noEvents
            // attach a constant view trafo (which makes our box visible)
            |> Sg.viewTrafo (
                Mod.map2 (fun p (d : V3d) -> 
                let loc = p
                let cen = p + d
                CameraView.look p d.Normalized V3d.OOI
                //CameraView.lookAt loc cen V3d.OOI 
                    |> CameraView.viewTrafo 
                ) dronePos dirController
            )
            // since our render target size is dynamic, we compute a proj trafo using standard techniques
            |> Sg.projTrafo (size |> Mod.map (fun actualSize -> 
                Frustum.perspective 110.0 0.1 1000.0 (float actualSize.X / float actualSize.Y) |> Frustum.projTrafo
                )
            )
            // next, we use Sg.compile in order to turn a scene graph into a render task (a nice composable alias for runtime.CompileRender)
            |> Sg.compile runtime signature 

        let offscreenTexture =
            RenderTask.renderToColor size offscreenTask

        let showSecondCamera = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.DroneMode | MenuState.HoverDroneScreen -> return true 
                    | _ -> return false
                }
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d.FromSize(V3d(0.1, 3.0, 3.0))))
            |> Sg.diffuseTexture offscreenTexture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                //do! DefaultSurfaces.simpleLighting
            }
            |> Sg.trafo m.droneControl.cameraPosition
            |> Sg.onOff mkDisappear

        let showSecondCameraOnController = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.DroneModeController -> return true 
                    | _ -> return false
                }

            let boxCenter = V3d(m.droneControl.cameraPosition.GetValue().GetModelOrigin().X, m.droneControl.cameraPosition.GetValue().GetModelOrigin().Y + 0.2, m.droneControl.cameraPosition.GetValue().GetModelOrigin().Z + 0.2)
            //* Trafo3d.Translation(V3d(0.0, 0.2, 0.025))
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d.FromCenterAndSize(boxCenter, V3d(0.35, 0.01, 0.35)))) //V3d(0.1, 0.5, 0.5)
            |> Sg.diffuseTexture offscreenTexture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                //do! DefaultSurfaces.simpleLighting
            }
            |> Sg.trafo m.droneControl.cameraPosition
            |> Sg.onOff mkDisappear

        let borderSecondCamera = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.DroneMode | MenuState.HoverDroneScreen -> return true 
                    | _ -> return false
                }
            
            m.droneControl.screen
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents
            |> Sg.onOff mkDisappear

        let borderSecondCameracontrollerTest = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.DroneModeController -> return true 
                    | _ -> return false
                }

            let boxCenter = V3d(m.droneControl.cameraPosition.GetValue().GetModelOrigin().X, m.droneControl.cameraPosition.GetValue().GetModelOrigin().Y  + 0.21, m.droneControl.cameraPosition.GetValue().GetModelOrigin().Z + 0.20)
            
            Sg.box (Mod.constant C4b.Red) (Mod.constant (Box3d.FromCenterAndSize(boxCenter, V3d(0.40, 0.012, 0.40))))
            |> Sg.noEvents
            |> defaultEffect
            |> Sg.trafo m.droneControl.screenPosition
            |> Sg.onOff mkDisappear

        let droneCylinder = 
            let color = 
                let hsv = HSVf((1.0 - 60.0) * 0.625, 1.0, 1.0)
                let col = C4f(hsv.ToC3f(), 0.5f).ToC4b()
                Mod.constant col

            let rad = Mod.constant 0.45

            let dTrafo = 
                dronetrafo
                |> Mod.map (fun t -> 
                    Trafo3d.Translation(V3d(t.GetModelOrigin().X + 0.22, t.GetModelOrigin().Y + 0.22, t.GetModelOrigin().Z))
                )

            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.DroneMode | MenuState.HoverDroneScreen | MenuState.DroneModeController -> return true 
                    | _ -> return false
                }

            Sg.cylinder 50 color rad (Mod.constant 100.0)
            |> Sg.noEvents
            |> Sg.trafo(dTrafo)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
            }
            |> defaultEffect
            |> Sg.blendMode (Mod.constant BlendMode.Blend)
            |> Sg.cullMode (Mod.constant CullMode.Front)
            |> Sg.pass (RenderPass.after "" RenderPassOrder.Arbitrary RenderPass.main)
            |> Sg.onOff mkDisappear
        
        let showDroneHeight = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.DroneModeController | MenuState.HoverDroneScreen -> return true 
                    | _ -> return false
                }
            Sg.textWithConfig { TextConfig.Default with renderStyle = RenderStyle.Billboard; align = TextAlignment.Center; flipViewDependent = true } m.droneHeight.text
            |> Sg.noEvents
            |> Sg.scale 0.05
            |> Sg.trafo(m.droneHeight.trafo)
            |> Sg.onOff mkDisappear

        let showDroneDist2Landmark = 
            Sg.textWithConfig { TextConfig.Default with renderStyle = RenderStyle.Billboard; align = TextAlignment.Center; flipViewDependent = true } m.droneDistanceToLandmark.text
            |> Sg.noEvents
            |> Sg.scale 0.05
            |> Sg.trafo(m.droneDistanceToLandmark.trafo)

        let teleport2intersection = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.Teleportation -> return true 
                    | _ -> return false
                }
            m.teleportBox
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents
            |> Sg.trafo m.opcSpaceTrafo
            |> Sg.onOff mkDisappear
        
        let teleportationCone = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.Teleportation -> return true 
                    | _ -> return false
                }
            m.teleportCone
            |> AList.toASet
            |> ASet.map (fun b -> 
                mkConeTeleport b b.color
            )
            |> Sg.set
            |> defaultEffect
            |> Sg.trafo m.opcSpaceTrafo
            |> Sg.onOff mkDisappear

        let rayLine = 
            let intersectionVector = m.hitPoint
            let controller = m.menuModel.controllerMenuSelector
            let newCP = m.controllerInfos |> AMap.tryFind (controller.kind.GetValue())
            let conPos = 
                newCP 
                |> Mod.bind (fun cp -> 
                    match cp with 
                    | Some c -> c.pose.deviceToWorld 
                    | None -> Mod.constant Trafo3d.Identity
                )
            let opcSpace = m.opcSpaceTrafo
            adaptive {
                let! con = conPos 
                let! opc = opcSpace 
                let! intersectPoint = intersectionVector 

                return [|Line3d((con * opc.Inverse).GetModelOrigin(), intersectPoint)|]
            }

        let showDynamicLine = 
            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.Teleportation -> return true 
                    | _ -> return false
                }
            rayLine
                |> Sg.lines (Mod.constant C4b.Red)
                |> Sg.noEvents
                |> Sg.uniform "LineWidth" (Mod.constant 5) 
                |> Sg.effect [
                    toEffect DefaultSurfaces.trafo
                    toEffect DefaultSurfaces.vertexColor
                    toEffect DefaultSurfaces.thickLine
                    ]
                |> Sg.pass (RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main)
                |> Sg.depthTest (Mod.constant DepthTestMode.None)
                |> Sg.trafo m.opcSpaceTrafo
                |> Sg.onOff mkDisappear

        let texturedQuad = 
            Sg.quad
            |> Sg.billboard
            |> Sg.noEvents
            //|> Sg.diffuseFileTexture' "path to texture" true
            |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.constantColor C4f.White
                    //do! DefaultSurfaces.diffuseTexture
               }
            |> Sg.trafo(m.evaluationLookAtLand.trafo)       
            |> Sg.scale 10.0
        
        let transformedSgs = 
            [
                evaluationLands
                evaluationLandsLook
                //texturedQuad
                drones
                droneCylinder
            ]
            |> Sg.ofList
            |> Sg.trafo m.annotationSpaceTrafo

        let WIMtransformedSgs = 
            [
                userPosOnWIM 
                userPosOnAnnotationSpace |> Sg.trafo m.annotationSpaceTrafo
                userConeOnWim
                initialUserPosOnWIM
                initialUserConeOnWim
                evaluationLandsOnWIM
                evaluationLandsOnWIMLook
                evaluationLandsOnWIM2RealWorld |> Sg.trafo m.annotationSpaceTrafo
            ]
            |> Sg.ofList

        let notTransformedSgs =
            [
                deviceSgs
                menuApp
                teleport2intersection
                showDynamicLine
                teleportationCone
                showSecondCamera
                borderSecondCamera
                borderSecondCameracontrollerTest
                showSecondCameraOnController
                //showDroneHeight
                showControllerCones
                showDroneDist2Landmark
            ] |> Sg.ofList

        Sg.ofList [transformedSgs; WIMtransformedSgs; notTransformedSgs; opcs; WIMopcs]

    let pause (info : VrSystemInfo) (m : MModel) =
        Sg.box' C4b.Red Box3d.Unit
        |> Sg.noEvents
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }

    let initial =
        let rotateBoxInit = false
        let marcPath = @"C:\Users\lopez\Desktop\VictoriaCrater\HiRISE_VictoriaCrater_SuperResolution"
        let publishPath = @"..\data"
        let path = 
            if System.IO.Directory.Exists publishPath then publishPath
            elif System.IO.Directory.Exists marcPath then marcPath
            elif System.IO.Directory.Exists "./data" then "./data"
            else 
                failwithf "could not find data dir. current directory i: %s" System.Environment.CurrentDirectory
        let patchHierarchiesInit = 
            OpcViewerFunc.patchHierarchiesImport path //"..\bin\data" //"C:\Users\lopez\Desktop\VictoriaCrater\HiRISE_VictoriaCrater_SuperResolution"

        let boundingBoxInit = 
            OpcViewerFunc.boxImport (patchHierarchiesInit)

        let opcInfosInit = 
            OpcViewerFunc.opcInfosImport (patchHierarchiesInit)

        let up =
            OpcViewerFunc.getUpVector boundingBoxInit rotateBoxInit

        let upRotationTrafo = 
            Trafo3d.RotateInto(boundingBoxInit.Center.Normalized, V3d.OOI)

        let cameraStateInit = 
            OpcViewerFunc.restoreCamStateImport boundingBoxInit V3d.OOI

        let newEvalLandmarks = OpcUtilities.mkEvalFlags (Trafo3d.Translation(V3d.One * 100000.0)) 15

        let newEvalLandmarksLook = OpcUtilities.mkCone (Trafo3d.Translation(V3d.One * 100000.0)) 15

        Log.line "using path: %s" path
        //C:\Users\lopez\Desktop\VictoriaCrater\HiRISE_VictoriaCrater_SuperResolution
        let startOpcTrafo = Trafo3d.FromBasis(V3d(0.0138907544072255, 0.0370928394273679, 0.410690910035505), V3d(0.11636514267386, 0.393870197365478, -0.0395094556451799), V3d(-0.395603213079913, 0.117157783795495, 0.0027988969790869), V3d(-57141.4217354136, 16979.9987604353, -1399135.09579421))
        
        let loadKdTrees = 
            let kdTreesPerHierarchy =
                [| 
                    for h in patchHierarchiesInit do
                        if true then   
                            yield KdTrees.loadKdTrees h Trafo3d.Identity ViewerModality.XYZ SerializationOpc.binarySerializer                    
                        else 
                            yield HMap.empty
                |]

            let totalKdTrees = kdTreesPerHierarchy.Length
            Log.line "creating %d kdTrees" totalKdTrees

            let kdTrees = 
                kdTreesPerHierarchy                     
                |> Array.Parallel.mapi (fun i e ->
                    Log.start "creating kdtree #%d of %d" i totalKdTrees
                    let r = e
                    Log.stop()
                    r
                )
                |> Array.fold (fun a b -> HMap.union a b) HMap.empty
            kdTrees
        
        {
            text                        = "some text"
            vr                          = false

            ControllerPosition          = V3d.OOO
            controllerInfos             = HMap.empty
            offsetToCenter              = V3d.One
            cameraState                 = cameraStateInit
            
            patchHierarchies            = patchHierarchiesInit
            boundingBox                 = boundingBoxInit
            opcInfos                    = opcInfosInit
            opcAttributes               = SurfaceAttributes.initModel path //"C:\Users\lopez\Desktop\VictoriaCrater\HiRISE_VictoriaCrater_SuperResolution"
            mainFrustum                 = Frustum.perspective 60.0 0.01 1000.0 1.0
            rotateBox                   = rotateBoxInit
            pickingModel                = OpcViewer.Base.Picking.PickingModel.initial

            kdTree                      = loadKdTrees

            offsetControllerDistance    = 1.0

            opcSpaceTrafo               = startOpcTrafo//Trafo3d.Translation(-boundingBoxInit.Center) * upRotationTrafo
            workSpaceTrafo              = Trafo3d.Identity
            annotationSpaceTrafo        = Trafo3d.Identity

            initOpcSpaceTrafo           = startOpcTrafo //Trafo3d.Translation(-boundingBoxInit.Center) * upRotationTrafo
            initWorkSpaceTrafo          = Trafo3d.Identity
            initAnnotationSpaceTrafo    = Trafo3d.Identity

            initControlTrafo            = Trafo3d.Identity
            init2ControlTrafo           = Trafo3d.Identity
            rotationAxis                = Trafo3d.Identity

            menuModel                   = Menu.MenuModel.init

            WIMopcSpaceTrafo            = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0)) * upRotationTrafo
            WIMworkSpaceTrafo           = Trafo3d.Identity
            WIMannotationSpaceTrafo     = Trafo3d.Identity
            WIMopcInfos                 = opcInfosInit
            WIMuserPos                  = PList.empty
            WIMuserPosCone              = PList.empty
            WIMinitialUserPos           = PList.empty
            WIMinitialUserPosCone       = PList.empty
            userPosOnAnnotationSpace    = PList.empty

            evaluationLandmarks         = newEvalLandmarks
            evaluationLandmarksWIM      = PList.empty
            evaluationLandmarksWIM2RealWorld= PList.empty

            evaluationLandmarksLook     = newEvalLandmarksLook 
            evaluationLandmarksWIMLook  = PList.empty
            evaluationLandmarksWIM2RealWorldLook = PList.empty
            evaluationLookAtLand        = StringInfo.initial

            evaluationCounter           = 0
            
            droneControl                = Drone.initial
            droneDistanceToLandmark     = StringInfo.initial
            droneHeight                 = StringInfo.initial
            
            teleportRay                 = Ray3d.Invalid
            teleportBox                 = PList.empty
            teleportCone                = PList.empty
            hitPoint                    = V3d.One * 5000.0

        }
    let app (runtime : IRuntime) =
        {
            unpersist = Unpersist.instance
            initial = initial
            update = update
            threads = threads
            input = input 
            ui = ui'
            vr = vr' runtime
            pauseScene = Some pause
        }

