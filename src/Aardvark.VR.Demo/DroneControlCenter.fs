namespace Demo.Main

open Aardvark.Base
open Aardvark.Base.Incremental

module DroneControlCenter = 
    open Aardvark.Application
    open Aardvark.VRVis.Opc
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    open Model
    open OpenTK
    open Aardvark.Base.MapExtImplementation

    let moveDrone kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let newModel = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = newModel.menuModel.controllerMenuSelector
        let newCP = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with 
        | Some id -> 
            match id.backButtonPressed with 
            | true -> 
                let controllDir = id.pose.deviceToWorld.Forward.C1
                printfn "orientation? %s" (controllDir.ToString())
                let moveDrone = 
                    newModel.droneControl.drone
                    |> PList.map (fun drone -> 
                        let newTrafo = drone.trafo.GetModelOrigin() + V3d(controllDir.X, controllDir.Y, controllDir.Z) * 0.005
                        {drone with trafo = Trafo3d.Translation(newTrafo) }
                    )
                
                let updateDrones = 
                    {newModel.droneControl with drone = moveDrone}
                
                {newModel with droneControl = updateDrones}
            | false -> newModel
        | None -> newModel 

