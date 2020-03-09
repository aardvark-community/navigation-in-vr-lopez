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
            //let newModel = 
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
        | None -> newModel 


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
                    //model
                    //update WIM space trafos
                    //TO CONTINUE FROM HERE CHECK WHY THE CONTROLLER IS NOT PLACED IN THE RIGHT POSITION OF THE WIM
                    //let newDronePosWIM = id.pose.deviceToWorld * model.workSpaceTrafo.Inverse * model.WIMworkSpaceTrafo
                    //let minimapRt = newDronePosWIM.GetOrthoNormalOrientation() 
                    //let minimapRot = Rot3d.FromFrame(minimapRt.Forward.C0.XYZ, minimapRt.Forward.C1.XYZ, minimapRt.Forward.C2.XYZ)
                    //let minimapRotation = minimapRot.GetEulerAngles()
                    //let minimapTrans = newDronePosWIM.GetModelOrigin() 
                    //let minimapScale = V3d(0.0025, 0.0025, 0.0025)

                    //let minimapPos = //minimapCoordinateSystem
                    //    Trafo3d.FromComponents(minimapScale, V3d.Zero, minimapTrans)
        
                    //let newWorkSpaceWIM = minimapPos * newDronePosWIM.Inverse
                    //let newAnnotationSpaceWIM = model.initAnnotationSpaceTrafo * newWorkSpaceWIM
                    //let newOpcSpaceWIM = model.initOpcSpaceTrafo * newWorkSpaceWIM

                    //{model with 
                    //    WIMworkSpaceTrafo           = newWorkSpaceWIM
                    //    WIMannotationSpaceTrafo     = newAnnotationSpaceWIM
                    //    WIMopcSpaceTrafo            = newOpcSpaceWIM
                    //}
                | None -> model 
            | false -> model 
        | None -> model 
        

