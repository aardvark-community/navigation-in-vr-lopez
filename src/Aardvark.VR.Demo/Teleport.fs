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
            //let dir = V3d.IOO //V3d.Subtract(V3d(id.pose.deviceToWorld.GetModelOrigin().X + 1000000000.0, id.pose.deviceToWorld.GetModelOrigin().Y, id.pose.deviceToWorld.GetModelOrigin().Z), id.pose.deviceToWorld.GetModelOrigin())
            let testRay = Ray3d(origin, controllDir)
            {model with teleportRay = testRay}
        | None -> model

        
        

