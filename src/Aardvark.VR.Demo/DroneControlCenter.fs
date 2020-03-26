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
                        {drone with trafo = Trafo3d.Translation(newTrafo) }
                        
                    )

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
                    let newDronePos = dPos.trafo * model.workSpaceTrafo.Inverse * model.annotationSpaceTrafo //drone pos in annotation space
                    //update Real World space trafos
                    let newWorkSpace = model.initWorkSpaceTrafo * newDronePos.Inverse
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
                let newScreenPos = 
                    model.droneControl.screen 
                    |> PList.map (fun screen -> 
                        let newTrafo = Trafo3d.Translation(V3d(con.pose.deviceToWorld.GetModelOrigin().X + 0.2, con.pose.deviceToWorld.GetModelOrigin().Y, con.pose.deviceToWorld.GetModelOrigin().Z))
                        {screen with trafo = newTrafo}
                    )
                    
                let newDroneCameraPos = 
                    {model.droneControl with 
                        cameraPosition = Trafo3d.Translation(con.pose.deviceToWorld.GetModelOrigin())
                        screen = newScreenPos    
                    }
                {model  with droneControl = newDroneCameraPos}
            | false -> model 
        | None -> model

