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
                    model 
                    |> NavigationOpc.currentSceneInfo kind p 
                | Menu.MenuState.NodeBased -> 
                    model
                | Menu.MenuState.WIM -> 
                    let newModel = 
                        model 
                        |> WIMOpc.updateMiniMap kind p

                    newModel
                    |> WIMOpc.editMiniMap kind p
                    
                | Menu.MenuState.WIMLandmarks -> 
                    model
                    |> PlaceLandmark.placingOnWIM kind p
                | Menu.MenuState.Teleportation -> 
                    model 
                    |> Teleport.hitRay kind p
                | Menu.MenuState.DroneMode -> 
                    model 
                    |> DroneControlCenter.moveDrone kind p
                | _ -> model
            
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
                | Menu.MenuState.PlaceLandmarks -> 
                    let newLandmark = OpcUtilities.mkFlags id.pose.deviceToWorld 1
                    //printfn "flag scale: %s" (newModel.landmarkOnAnnotationSpace.ToString())
                        
                    {newModel with 
                        landmarkOnController = newLandmark
                        //WIMopcSpaceTrafo = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0))
                        //WIMlandmarkOnAnnotationSpace = PList.empty
                        //droneControl = Drone.initial;
                        //WIMuserPos = PList.empty
                    }
                | Menu.MenuState.Scale -> 
                    let newModel = newModel |> NavigationOpc.initialSceneInfo
                    {newModel with 
                        landmarkOnController = PList.empty;
                        WIMopcSpaceTrafo = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0)); 
                        WIMlandmarkOnAnnotationSpace = PList.empty;
                        droneControl = Drone.initial;
                        WIMuserPos = PList.empty
                    }
                | Menu.MenuState.NodeBased -> 
                    newModel
                | Menu.MenuState.WIM -> 
                    let newUserPosWIM = OpcUtilities.mkFlagsUser Trafo3d.Identity 1 
                    
                    let newModel = {newModel with WIMuserPos = newUserPosWIM}

                    let newModel = newModel |> WIMOpc.showMiniMap
                    
                    {newModel with 
                        droneControl = Drone.initial;
                    }
                | Menu.MenuState.WIMLandmarks ->
                    let newLandmark = OpcUtilities.mkFlags id.pose.deviceToWorld 1
                    let newUserPos = 
                        if newModel.userPosOnAnnotationSpace.Count.Equals(0) then 
                            OpcUtilities.mkFlags id.pose.deviceToWorld 1
                        else newModel.userPosOnAnnotationSpace
                    let newInitialUserPosWIM = 
                        OpcUtilities.mkFlags (Trafo3d.Translation(V3d.One * 1000000.0)) 1

                    let newModel = 
                        {newModel with 
                            landmarkOnController = newLandmark
                            userPosOnAnnotationSpace = newUserPos
                            WIMinitialUserPos = newInitialUserPosWIM
                        }
                    newModel 
                    |> PlaceLandmark.moveUserToNewPosOnAnnotationSpace
                    
                | Menu.MenuState.Reset -> 
                    {newModel with 
                        landmarkOnController         = PList.empty;
                        landmarkOnAnnotationSpace    = PList.empty;
                        WIMopcSpaceTrafo             = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0));
                        WIMlandmarkOnAnnotationSpace = PList.empty;
                        WIMuserPos                   = PList.empty;
                        opcSpaceTrafo                = Trafo3d.FromBasis(V3d(0.0138907544072255, 0.0370928394273679, 0.410690910035505), V3d(0.11636514267386, 0.393870197365478, -0.0395094556451799), V3d(-0.395603213079913, 0.117157783795495, 0.0027988969790869), V3d(-57141.4217354136, 16979.9987604353, -1399135.09579421));
                        annotationSpaceTrafo         = Trafo3d.Identity;
                        workSpaceTrafo               = Trafo3d.Identity;
                        droneControl                 = Drone.initial
                    }
                | Menu.MenuState.Teleportation -> 
                    let controllTrafo = id.pose.deviceToWorld

                    let origin = controllTrafo.Forward.TransformPos V3d.Zero
                    let controllDir = controllTrafo.Forward.TransformDir V3d.YAxis
                    
                    //the next lines are the same as the previous ones
                    //let controllDir = controllTrafo.GetViewDirectionLH()
                    //let origin = controllTrafo.GetModelOrigin()

                    let testRay = Ray3d(origin, controllDir)
                    {newModel with teleportRay = testRay}
                | Menu.MenuState.DroneMode -> 
                    let newDrone = 
                        if newModel.droneControl.drone.Count.Equals(0) then 
                            OpcUtilities.mkDrone id.pose.deviceToWorld 1
                        else newModel.droneControl.drone
                    let updateDrones = {newModel.droneControl with drone = newDrone}

                    let newModel = 
                        {newModel with 
                            landmarkOnController = PList.empty;
                            WIMopcSpaceTrafo = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0)); 
                            WIMlandmarkOnAnnotationSpace = PList.empty;
                            WIMuserPos = PList.empty;
                            droneControl = updateDrones
                        }

                    newModel
                    |> DroneControlCenter.moveUserToDronePos
                    
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
        
    let input (msg : VrMessage) =
        match msg with
        // buttons identifications: sensitive = 0, backButton = 1, sideButtons = 2
        | VrMessage.Touch(con,button) -> 
            match button with 
            | 0 -> [MenuMessage (Demo.MenuAction.CreateMenu(con |> ControllerKind.fromInt, true), con |> ControllerKind.fromInt, true)]
            | _ -> []
        | VrMessage.Untouch(con,button) -> 
            match button with 
            | 0 -> [MenuMessage (Demo.MenuAction.CreateMenu(con |> ControllerKind.fromInt, false), con |> ControllerKind.fromInt, false)]
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
            if p.isValid then 
                [SetControllerPosition (cn |> ControllerKind.fromInt, p)]
            else []
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

    let mkControllerBox (cp : IMod<Trafo3d>) (color : C4b) =
        let newTrafo = 
            cp
            |> Mod.map (fun t -> 
                let tt = V3d(t.GetModelOrigin().X, t.GetModelOrigin().Y, t.GetModelOrigin().Z + 0.07)
                let r = t.GetOrthoNormalOrientation()
                let rr = Rot3d.FromFrame(r.Forward.C0.XYZ, r.Forward.C1.XYZ, r.Forward.C2.XYZ)
                let rrr = rr.GetEulerAngles()
                let s = V3d.One
                Trafo3d.FromComponents(s, rrr, tt)
            )
        Sg.cone' 20 color 0.5 5.0 
            |> Sg.noEvents
            |> Sg.scale 0.01
            |> Sg.trafo (Mod.constant (Trafo3d.RotationInDegrees(V3d(-90.0,90.0,0.0))))
            |> Sg.trafo newTrafo
    
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
        let color = Mod.constant C4b.Yellow
        let rad = cylinder.radius

        Sg.cylinder 50 color rad (Mod.constant 100.0)
            |> Sg.noEvents
            |> Sg.trafo(pos)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                //do! DefaultSurfaces.simpleLighting
            }

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

        let landmarks = 
            m.landmarkOnController
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents

        let drones = 
            m.droneControl.drone
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents

        let userPosOnWIM = 
            m.WIMuserPos
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents

        let userConeOnWim = 
            m.WIMuserPos
            |> AList.toASet
            |> ASet.map (fun b -> 
                mkControllerBox b.trafo C4b.Red
            )
            |> Sg.set
            |> defaultEffect

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
            m.WIMinitialUserPos
            |> AList.toASet
            |> ASet.map (fun b -> 
                mkControllerBox b.trafo C4b.Cyan
            )
            |> Sg.set
            |> defaultEffect

        let userPosOnAnnotationSpace =  
            m.userPosOnAnnotationSpace
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents

        let landmarksOnAnnotationSpace = 
            m.landmarkOnAnnotationSpace
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents

        let landmarksOnWIM = 
            m.WIMlandmarkOnAnnotationSpace
            |> AList.toASet 
            |> ASet.map (fun b -> 
                mkFlag m b 
               )
            |> Sg.set
            |> defaultEffect
            |> Sg.noEvents
            
        //let landmarksFromStart = 
        //    m.landmarkFromStart
        //    |> AList.toASet 
        //    |> ASet.map (fun b -> 
        //        mkFlag m b 
        //       )
        //    |> Sg.set
        //    |> defaultEffect
        //    |> Sg.noEvents

        let deviceSgs = 
            info.state.devices |> AMap.toASet |> ASet.chooseM (fun (_,d) ->
                d.Model |> Mod.map (fun m ->
                    match m with
                    | Some sg -> 
                        sg 
                        |> Sg.noEvents 
                        |> Sg.trafo d.pose.deviceToWorld
                        |> Sg.onOff d.pose.isValid
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

        let throwRay = 
            //m.teleportRay
            //|> Mod.map(fun ray -> 
            //    [|ray.Line3d|]
            //)
            let ttt = m.teleportRay
            adaptive{
                let! rrr = ttt
                return [|rrr.Line3d|]
            }
            
        let throwRayLine = 
            throwRay 
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
        
        let compass = 
            let north = "N"
            Sg.textWithConfig { TextConfig.Default with renderStyle = RenderStyle.Billboard; align = TextAlignment.Center; flipViewDependent = true } (Mod.constant(north))
            |> Sg.noEvents
            |> Sg.scale 0.05
            |> Sg.scale 50.0
            |> Sg.trafo(Mod.constant(Trafo3d.Identity))

        let compass1 = 
            m.totalCompass
            |> AList.toASet
            |> ASet.map (fun c -> 
                Sg.textWithConfig { TextConfig.Default with renderStyle = RenderStyle.Billboard; align = TextAlignment.Center; flipViewDependent = true } c.text
                |> Sg.noEvents
                |> Sg.scale 0.05
                |> Sg.scale 50.0
                |> Sg.trafo c.trafo
            )
            |> Sg.set

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

        let offscreenTask = 
            opcs
            |> Sg.noEvents
            // attach a constant view trafo (which makes our box visible)
            |> Sg.viewTrafo (
                Mod.map2 (fun p d -> 
                let loc = p
                let cen = p + d
                CameraView.lookAt loc cen V3d.OOI 
                    |> CameraView.viewTrafo 
                ) dronePos droneDir
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

        let secondCameraTrafo = 
            m.menuModel.menu 
            |> Mod.map (fun ms -> 
                match ms with 
                | Menu.MenuState.DroneMode -> Trafo3d.Identity //change this camera position to be something close to the user even if they move their position
                | _ -> Trafo3d.Translation(V3d.One * 1000000.0)
            )

        let showSecondCamera = 
            Sg.box (Mod.constant C4b.White) (Mod.constant (Box3d.FromSize(V3d(0.1, 3.0, 3.0))))
            |> Sg.diffuseTexture offscreenTexture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.trafo secondCameraTrafo
        
        let droneCylinder = 
            let color = 
                let hsv = HSVf((1.0 - 60.0) * 0.625, 1.0, 1.0)
                let col = C4f(hsv.ToC3f(), 0.5f).ToC4b()
                Mod.constant col

            let rad = Mod.constant 0.20

            let dTrafo = 
                dronetrafo
                |> Mod.map (fun t -> 
                    Trafo3d.Translation(V3d(t.GetModelOrigin().X, t.GetModelOrigin().Y, t.GetModelOrigin().Z - 50.0))
                )

            let mkDisappear = 
                let menuMode = m.menuModel.menu
                
                adaptive {
                    let! newMenuMode = menuMode
                    match newMenuMode with 
                    | MenuState.DroneMode -> return true 
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

        let transformedSgs = 
            [
                landmarksOnAnnotationSpace
                drones
                droneCylinder
                //landmarksFromStart
            ]
            |> Sg.ofList
            |> Sg.trafo m.annotationSpaceTrafo

        let WIMtransformedSgs = 
            [
                landmarksOnWIM 
                userPosOnWIM 
                userPosOnAnnotationSpace |> Sg.trafo m.annotationSpaceTrafo
                userConeOnWim
                initialUserPosOnWIM
                //initialUserConeOnWim
            ]
            |> Sg.ofList

        let notTransformedSgs =
            [
                deviceSgs
                menuApp
                landmarks
                //throwRayLine
                showSecondCamera
                //compass
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
        
        let startLandmark = 
            let ttt = OpcUtilities.mkFlags (Trafo3d.Translation(V3d(-1.14622000066486, 0.188908132549816, 0.456057644407983))) 1
            ttt
            
        let rotateBoxInit = false
        let patchHierarchiesInit = 
            OpcViewerFunc.patchHierarchiesImport "C:\Users\lopez\Desktop\VictoriaCrater\HiRISE_VictoriaCrater_SuperResolution"

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

        let startOpcTrafo = Trafo3d.FromBasis(V3d(0.0138907544072255, 0.0370928394273679, 0.410690910035505), V3d(0.11636514267386, 0.393870197365478, -0.0395094556451799), V3d(-0.395603213079913, 0.117157783795495, 0.0027988969790869), V3d(-57141.4217354136, 16979.9987604353, -1399135.09579421))
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
            opcAttributes               = SurfaceAttributes.initModel "C:\Users\lopez\Desktop\VictoriaCrater\HiRISE_VictoriaCrater_SuperResolution"
            mainFrustum                 = Frustum.perspective 60.0 0.01 1000.0 1.0
            rotateBox                   = rotateBoxInit
            pickingModel                = OpcViewer.Base.Picking.PickingModel.initial

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

            landmarkOnController        = PList.empty
            landmarkOnAnnotationSpace   = PList.empty
            //landmarkFromStart           = startLandmark
            WIMopcSpaceTrafo            = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0)) * upRotationTrafo
            WIMworkSpaceTrafo           = Trafo3d.Identity
            WIMannotationSpaceTrafo     = Trafo3d.Identity
            WIMopcInfos                 = opcInfosInit
            WIMlandmarkOnController     = PList.empty
            WIMlandmarkOnAnnotationSpace= PList.empty
            WIMuserPos                  = PList.empty
            WIMinitialUserPos           = PList.empty
            userPosOnAnnotationSpace    = PList.empty
            teleportRay                 = Ray3d.Invalid
            droneControl                = Drone.initial

            totalCompass                = PList.empty
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

