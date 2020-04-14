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
                    |> PList.map (fun landmark -> 
                        {landmark with 
                            trafo = id.pose.deviceToWorld
                        }
                    )

                match id.backButtonPressed with 
                | true -> 
                    let landMarkOnController = 
                        newModel.landmarkOnController
                        |> PList.tryFirst

                    match landMarkOnController with 
                    | Some landmark ->
                        let updateLandmark = 
                            let newTrafo = Trafo3d.Translation(id.pose.deviceToWorld.GetModelOrigin())
                            {landmark with 
                                trafo = newTrafo * newModel.WIMworkSpaceTrafo.Inverse //id.pose.deviceToWorld * newModel.WIMworkSpaceTrafo.Inverse
                            }
                        let newlandMarkOnAnnotationSpace = 
                            newModel.landmarkOnAnnotationSpace
                            |> PList.prepend updateLandmark
                            
                        let updateWIMLandmark = 
                            let newTrafo = Trafo3d.Translation(id.pose.deviceToWorld.GetModelOrigin())
                            {landmark with 
                                trafo = newTrafo //id.pose.deviceToWorld 
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

    let hoverEvaluationLandmarks kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        let evalLandmark = 
            model.evaluationLandmarks
            |> PList.tryAt model.evaluationCounter
        
        match newCP, evalLandmark with
        | Some con, Some evalLand ->
            
            let newBoxPos = 
                model.evaluationLandmarks
                |> PList.updateAt model.evaluationCounter (fun el -> 
                    let newPositions = 
                        [V3d(52.288818359375, -7.18469619750977, -12.9421234130859);
                        V3d(-66.6853427886963, 103.339958190918, 20.5557823181152); 
                        V3d(46.2436199188232, -119.581413269043, -5.77807426452637);
                        V3d(61.1997127532959, -58.692741394043, -13.3692264556885);
                        V3d(189.576578140259, 37.2320175170898, 8.15391540527344)]
                    
                    {el with 
                        trafo = Trafo3d.Translation(newPositions.Item model.evaluationCounter);
                        geometry = Box3d.FromSize(V3d(10.0, 10.0, 1.0))
                    })
                    //{el with trafo = Trafo3d.Translation(V3d(Trafo3d.Identity.GetModelOrigin().X + float(model.evaluationCounter), Trafo3d.Identity.GetModelOrigin().Y, Trafo3d.Identity.GetModelOrigin().Z))})
                // place the new position of the next landmark here!!!! for testing purposes it is set to id+1
            let model = {model with evaluationLandmarks = newBoxPos}
            let controllerOnAnnotationSpace = con.pose.deviceToWorld * model.workSpaceTrafo.Inverse
            let dist = V3d.Distance(controllerOnAnnotationSpace.GetModelOrigin(), evalLand.trafo.GetModelOrigin())
            printfn "dist: %A" dist
            if dist <= 8.0 then 
                let newBoxColor = 
                    model.evaluationLandmarks
                    |> PList.updateAt model.evaluationCounter (fun el -> {el with color = C4b.Green})
                {model with evaluationCounter = model.evaluationCounter + 1; evaluationLandmarks = newBoxColor}
            else model
        | _, _ -> model

    



        
        
        
        



        