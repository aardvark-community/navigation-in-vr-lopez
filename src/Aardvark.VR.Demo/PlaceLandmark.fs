namespace Demo.Main

open Aardvark.Base
open Aardvark.Base.Incremental

module PlaceLandmark = 
    open Aardvark.Application
    open Aardvark.VRVis.Opc
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    open Model
    open OpenTK
    open Aardvark.Base.MapExtImplementation

    
    
    let placing kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let newModel = { model with controllerInfos = newControllersPosition}
        
        let ci = newModel.controllerInfos |> HMap.tryFind kind
        
        let controllerPos = newModel.menuModel.controllerMenuSelector
        let newCP = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
                    
        match newCP with 
        | Some id -> 
            let newModel = 
                let updateFlagPos = 
                    newModel.landmarkOnController
                    |> PList.map (fun landmark -> {landmark with trafo = id.pose.deviceToWorld})
                
                match id.backButtonPressed with 
                | true -> 
                    let landMarkOnController = 
                        newModel.landmarkOnController
                        |> PList.tryFirst

                    match landMarkOnController with 
                    | Some landmark ->
                        let updateLandmark = {landmark with trafo = id.pose.deviceToWorld * newModel.workSpaceTrafo.Inverse}
                        printfn "landmark pos: %s" (updateLandmark.trafo.GetModelOrigin().ToString())
                        let newlandMarkOnAnnotationSpace = 
                            newModel.landmarkOnAnnotationSpace
                            |> PList.prepend updateLandmark

                        {newModel with 
                            landmarkOnController = PList.empty; 
                            landmarkOnAnnotationSpace = newlandMarkOnAnnotationSpace
                        }
                    | None -> newModel
                | false -> {newModel with landmarkOnController = updateFlagPos}

            newModel
        | None -> newModel

    let placingOnWIM kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let newModel = { model with controllerInfos = newControllersPosition}
        
        let ci = newModel.controllerInfos |> HMap.tryFind kind
        
        let controllerPos = newModel.menuModel.controllerMenuSelector
        let newCP = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with 
        | Some id -> 
            let newModel = 
                let updateFlagPos = 
                    newModel.landmarkOnController
                    |> PList.map (fun landmark -> {landmark with trafo = id.pose.deviceToWorld})

                match id.backButtonPressed with 
                    | true -> 
                        let landMarkOnController = 
                            newModel.landmarkOnController
                            |> PList.tryFirst

                        match landMarkOnController with 
                        | Some landmark ->
                            let updateLandmark = 
                                {landmark with 
                                    trafo = id.pose.deviceToWorld * newModel.workSpaceTrafo.Inverse
                                }
                            let newlandMarkOnAnnotationSpace = 
                                newModel.landmarkOnAnnotationSpace
                                |> PList.prepend updateLandmark
                            
                            let updateWIMLandmark = 
                                {landmark with 
                                    trafo = id.pose.deviceToWorld * newModel.WIMworkSpaceTrafo.Inverse
                                }
                            let newlandMarkOnWIMAnnotationSpace = 
                                newModel.WIMlandmarkOnAnnotationSpace
                                |> PList.prepend updateWIMLandmark

                            {newModel with 
                                landmarkOnController = PList.empty; 
                                WIMlandmarkOnAnnotationSpace = newlandMarkOnAnnotationSpace;
                                landmarkOnAnnotationSpace = newlandMarkOnWIMAnnotationSpace
                            }
                        | None -> newModel
                    | false -> 
                        {newModel with 
                            landmarkOnController = updateFlagPos
                        }
            newModel
        | None -> newModel
        
        