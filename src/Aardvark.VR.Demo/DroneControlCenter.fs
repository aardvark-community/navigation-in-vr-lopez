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

    let moveDrone kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let newModel = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = newModel.menuModel.controllerMenuSelector
        let newCP = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with 
        | Some id -> 
            let newModel = 
                match id.backButtonPressed with 
                | true -> 
                    let controllDir = id.pose.deviceToWorld.Forward.C1
                    printfn "orientation? %s" (controllDir.ToString())
                    let moveDrone = 
                        newModel.droneControl.drone
                        |> PList.map (fun drone -> 
                            let newTrafo = drone.trafo.GetModelOrigin() + V3d(controllDir.X, controllDir.Y, controllDir.Z) * 0.05
                            {drone with trafo = Trafo3d.Translation(newTrafo) }
                        )
                
                    let updateDrones = 
                        {newModel.droneControl with drone = moveDrone}
                
                    {newModel with droneControl = updateDrones}
                | false -> newModel
            match id.sideButtonPressed with 
            | true -> 
                let hmdPos = newModel.controllerInfos |> HMap.tryFind ControllerKind.HMD
                let dronePos = newModel.droneControl.drone |> PList.tryFirst
                match hmdPos, dronePos with 
                | Some hmd, Some dPos -> 
                    //let newPose : Pose = {Aardvark.Vr.Pose with deviceToWorld = dPos.trafo}
                    //let newHmdPose : Hmd = {Hmd with pose = newPose}
                    //let newVrState : VrState = {VrState with display = newHmdPose}
                    let newCameraPos = {newModel.cameraState with moveVec = dPos.trafo.GetModelOrigin()}
                    let updateControllerInfo = 
                        newModel.controllerInfos
                        |> HMap.update ControllerKind.HMD (fun h -> 
                            match h with 
                            | Some x -> 
                                let updateHmdPose = 
                                    {hmd.pose with deviceToWorld = dPos.trafo}
                                //let updateHmd = {hmd with pose = updateHmdPose}
                                //updateHmd
                                {x with pose = updateHmdPose}
                            | None -> ControllerInfo.initial
                        ) 
                    {newModel with 
                        controllerInfos = updateControllerInfo; 
                        cameraState     = newCameraPos;
                        //vrStateCamera   = newVrState
                    }
                | _, _ -> newModel 
            | false -> newModel 
        | None -> newModel 

