namespace Demo.Main

open Aardvark.Base
open Aardvark.Base.Incremental

module Teleport = 
    open Aardvark.Application
    open Aardvark.VRVis.Opc
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    open Model
    open OpenTK
    open Aardvark.Base.MapExtImplementation

    let hitRay kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with
        | Some id -> 
            let controllTrafo = id.pose.deviceToWorld
            let origin = controllTrafo.Forward.TransformPos V3d.Zero
            let controllDir = controllTrafo.Forward.TransformDir V3d.YAxis

            let testRay = Ray3d(origin, controllDir)
            {model with teleportRay = testRay}
        | None -> model

    let teleportUser model : Model = 
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with 
        | Some id -> 
            let controllDir = id.pose.deviceToWorld.Forward.C1
            match id.backButtonPressed with 
            | true -> 
                let moveBox = 
                        model.teleportBox
                        |> PList.map (fun tBox -> 
                            let newTrafo = Trafo3d.Translation(tBox.trafo.GetModelOrigin() + controllDir.XYZ * 50.0)
                            {tBox with trafo = newTrafo}
                        )

                let model = {model with teleportBox = moveBox}
                let teleportPos = model.teleportBox |> PList.tryFirst
                match teleportPos with 
                | Some tPos -> 
                    let newTeleportPos = Trafo3d.Translation(tPos.trafo.GetModelOrigin()).Inverse
                    let newWorkSpace = model.initWorkSpaceTrafo * newTeleportPos
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

        
        

