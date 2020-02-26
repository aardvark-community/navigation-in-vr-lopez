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
        
        let newModel = { model with controllerInfos = newControllersPosition}
        
        let ci = newModel.controllerInfos |> HMap.tryFind kind
        
        let controllerPos = newModel.menuModel.controllerMenuSelector
        let newCP = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with
        | Some id -> 
            let dir = V3d.Subtract(V3d(id.pose.deviceToWorld.GetModelOrigin().X + 10.0, id.pose.deviceToWorld.GetModelOrigin().Y, id.pose.deviceToWorld.GetModelOrigin().Z), id.pose.deviceToWorld.GetModelOrigin())
            let testRay = Ray3d(id.pose.deviceToWorld.GetModelOrigin(), dir.Normalized)
            {model with teleportRay = testRay}
        | None -> model

        
        

