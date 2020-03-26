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
    open Demo
    open Demo.Menu
    open ProviderImplementation.ProvidedTypes.AssemblyReader

    let createNewTrafo con : Trafo3d = 
        let con2Pos = con.pose.deviceToWorld
        let r = con2Pos.GetOrthoNormalOrientation()
        let rot = Rot3d.FromFrame(r.Forward.C0.XYZ, r.Forward.C1.XYZ, r.Forward.C2.XYZ)
        let rotation = rot.GetEulerAngles()
        let rotation1 = V3d(0.0, 0.0, rotation.Z)

        let translation = con2Pos.GetModelOrigin()

        let scale = V3d(0.5, 0.5, 0.5)
                        
        Trafo3d.FromComponents(scale, rotation1, translation)

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
        
        let controllerPos = newModel.menuModel.controllerMenuSelector
        let newCP = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with 
        | Some id -> 
            let newModel = 
                let updateLandmarkPos = 
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
                                trafo = id.pose.deviceToWorld * newModel.WIMworkSpaceTrafo.Inverse
                            }
                        let newlandMarkOnAnnotationSpace = 
                            newModel.landmarkOnAnnotationSpace
                            |> PList.prepend updateLandmark
                            
                        let updateWIMLandmark = 
                            {landmark with 
                                trafo = id.pose.deviceToWorld 
                            }
                        let newlandMarkOnWIMAnnotationSpace = 
                            newModel.WIMlandmarkOnAnnotationSpace
                            |> PList.prepend updateWIMLandmark

                        {newModel with 
                            landmarkOnController = PList.empty; 
                            WIMlandmarkOnAnnotationSpace = newlandMarkOnWIMAnnotationSpace;
                            landmarkOnAnnotationSpace = newlandMarkOnAnnotationSpace
                        }
                    | None -> newModel
                | false -> 
                    {newModel with 
                        landmarkOnController = updateLandmarkPos
                    }
            newModel
        | None -> newModel

        