namespace Demo.Main

open Aardvark.Base
open Aardvark.Base.Incremental

module DroneControlCenter = 
    open Aardvark.Vr
    open Aardvark.Application
    open Aardvark.VRVis.Opc
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    open Model
    open OpenTK
    open Aardvark.Base.MapExtImplementation
    open Demo
    open Demo.Menu
    open System

    let moveDrone kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let newModel = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = newModel.menuModel.controllerMenuSelector
        let newCP = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
        let userHMd = newModel.controllerInfos |> HMap.tryFind ControllerKind.HMD
        match newCP, userHMd with 
        | Some id, Some hmd -> 
            //let newModel = 
            match id.backButtonPressed with 
            | true -> 
                let controllDir = id.pose.deviceToWorld.Forward.C1
                
                let moveDrone = 
                    newModel.droneControl.drone
                    |> PList.map (fun drone -> 
                        let newTrafo = drone.trafo.GetModelOrigin() + controllDir.XYZ * 0.05
                        {drone with trafo = Trafo3d.Translation(newTrafo)}
                        
                    )

                let newModel = 
                    let droneFirst = newModel.droneControl.drone |> PList.tryFirst
                    match droneFirst with 
                    | Some dd -> 
                        let newDroneHeightTrafo = Trafo3d.Translation(V3d(newModel.droneControl.cameraPosition.GetModelOrigin().X, newModel.droneControl.cameraPosition.GetModelOrigin().Y - 0.75, newModel.droneControl.cameraPosition.GetModelOrigin().Z))
                        let updateDroneHeight = 
                            {newModel.droneHeight with text = "Drone height: " + dd.trafo.GetModelOrigin().Z.ToString(); trafo = newDroneHeightTrafo}
                        {newModel with droneHeight = updateDroneHeight}
                    | None -> newModel

                let updateDrones = 
                    {newModel.droneControl with drone = moveDrone}
                
                {newModel with droneControl = updateDrones}
            | false -> newModel
        | _, _ -> newModel 

    let moveUserToDronePos model : Model =
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with 
        | Some id -> 
            match id.sideButtonPressed with 
            | true -> 
                let dronePos = model.droneControl.drone |> PList.tryFirst
                match dronePos with 
                | Some dPos -> 
                    //let newDronePos = dPos.trafo * model.workSpaceTrafo.Inverse //* model.annotationSpaceTrafo //drone pos in annotation space
                    let newDronePos = Trafo3d.Translation(dPos.trafo.GetModelOrigin()).Inverse
                    //update Real World space trafos
                    let newWorkSpace = model.initWorkSpaceTrafo * newDronePos
                    let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace
                    let newFlagSpace = model.initAnnotationSpaceTrafo * newWorkSpace

                    { model with 
                        workSpaceTrafo              = newWorkSpace
                        opcSpaceTrafo               = newOpcSpace
                        annotationSpaceTrafo        = newFlagSpace
                    }
                    
                | None -> model 
            | false -> model 
        | None -> model 
        
    let checkHoverScreen kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        let screenVector = 
            model.droneControl.screen
            |> PList.tryFirst
        match newCP, screenVector with 
        | Some con, Some screen -> 
            match con.backButtonPressed with 
            | true -> model 
            | false -> 
                let dist = V3d.Distance(con.pose.deviceToWorld.GetModelOrigin(), screen.trafo.GetModelOrigin())
                printfn "dist: %A" dist
                if dist <= 1.15 then 
                    let newMode = {model.menuModel with menu = MenuState.HoverDroneScreen}
                    {model with menuModel = newMode}
                else 
                    let newMode1 = {model.menuModel with menu = MenuState.DroneMode}
                    {model with menuModel = newMode1}
        | _, _ -> 
            model

    let moveScreenPos kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = model.menuModel.controllerMenuSelector

        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
 
        match newCP with 
        | Some con -> 
            match con.backButtonPressed with 
            | true -> 
                let shiftTrafo = model.droneControl.initCameraPosition * Trafo3d.Translation(model.droneControl.initControlTrafo.GetModelOrigin()).Inverse 
                let newScreenPos = 
                    model.droneControl.screen 
                    |> PList.map (fun screen -> 
                        let newTrafo = shiftTrafo * Trafo3d.Translation(V3d(con.pose.deviceToWorld.GetModelOrigin().X + 0.2, con.pose.deviceToWorld.GetModelOrigin().Y - 0.1, con.pose.deviceToWorld.GetModelOrigin().Z - 0.2))
                        {screen with trafo = newTrafo}
                    )
                    
                let newDroneCameraPos = 
                    {model.droneControl with 
                        cameraPosition = shiftTrafo * Trafo3d.Translation(con.pose.deviceToWorld.GetModelOrigin())
                        screen = newScreenPos    
                    }
                {model  with droneControl = newDroneCameraPos}
            | false -> model 
        | None -> model
    
    let moveScreenAttachedController kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = model.menuModel.controllerMenuSelector
        
        let secondCon = 
            if controllerPos.kind.Equals(ControllerKind.ControllerA) then
                model.controllerInfos |> HMap.tryFind ControllerKind.ControllerB
            else model.controllerInfos |> HMap.tryFind ControllerKind.ControllerA
        
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
 
        match newCP, secondCon with 
        | Some con, Some con2 -> 
            let newLmkC = con2.pose.deviceToWorld 
            let rtLmkC = newLmkC.GetOrthoNormalOrientation()
            let rotLmkC = Rot3d.FromFrame(rtLmkC.Forward.C0.XYZ, rtLmkC.Forward.C1.XYZ, rtLmkC.Forward.C2.XYZ)
            let rotation = rotLmkC.GetEulerAngles()
            let rotation1 = V3d(0.0, 0.0, rotation.Z - Math.PI / 2.0)

            let translation = V3d(newLmkC.GetModelOrigin().X + 0.1, newLmkC.GetModelOrigin().Y + 0.6, newLmkC.GetModelOrigin().Z )
            let translationCameraPosition = V3d(newLmkC.GetModelOrigin().X, newLmkC.GetModelOrigin().Y, newLmkC.GetModelOrigin().Z)
            let translationScreenPosition = V3d(newLmkC.GetModelOrigin().X + 0.12, newLmkC.GetModelOrigin().Y, newLmkC.GetModelOrigin().Z)

            let scale = V3d.One

            let newTrafo = Trafo3d.FromComponents(scale, rotation1, translation) //* Trafo3d.RotationInDegrees(V3d(0.0,0.0,90.0))
            let newTrafoCameraPosition = Trafo3d.FromComponents(scale, rotation1, translationCameraPosition) //* Trafo3d.RotationInDegrees(V3d(0.0,0.0,90.0))
            let newTrafoScreenPosition = Trafo3d.FromComponents(scale, rotation1, translationScreenPosition) //* Trafo3d.RotationInDegrees(V3d(0.0,0.0,90.0))

            let newScreenPos = 
                model.droneControl.screen 
                |> PList.map (fun screen -> 
                    {screen with trafo = newTrafo}
                )
                    
            let newDroneCameraPos = 
                {model.droneControl with 
                    cameraPosition = newTrafoCameraPosition
                    screen = newScreenPos 
                    screenPosition = newTrafoScreenPosition
                }
            {model  with droneControl = newDroneCameraPos}
        | _, _ -> model

