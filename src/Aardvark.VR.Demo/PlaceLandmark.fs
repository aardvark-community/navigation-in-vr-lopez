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
                                trafo = newTrafo * newModel.WIMworkSpaceTrafo.Inverse 
                            }
                        let newlandMarkOnAnnotationSpace = 
                            newModel.landmarkOnAnnotationSpace
                            |> PList.prepend updateLandmark
                            
                        let updateWIMLandmark = 
                            let newTrafo = Trafo3d.Translation(id.pose.deviceToWorld.GetModelOrigin())
                            {landmark with 
                                trafo = newTrafo 
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

    let updateLandmarksPosition model : Model = 
        let newBoxPos = 
            model.evaluationLandmarks
            |> PList.updateAt model.evaluationCounter (fun el -> 
                let newPositions = 
                    [V3d(151.104545593262, 17.6482200622559, -1.07669830322266);
                    V3d(41.4958953857422, -86.480712890625, 9.59806442260742); 
                    V3d(-28.3977508544922, 82.3419570922852, 20.4675674438477);
                    V3d(47.2373962402344, -146.375179290771, 3.89385223388672);
                    V3d(158.316850662231, -14.4028663635254, -2.29530334472656);
                    V3d(26.3594150543213, -195.874500274658, 18.9978122711182);
                    V3d(52.1530628204346, 29.5162200927734, -5.88483810424805);
                    V3d(25.4886627197266, -58.6039543151855, -4.27331924438477);
                    V3d(181.050395965576, -126.30672454834, 8.62436294555664);
                    V3d(-39.9592399597168, -44.0988540649414, -1.48630142211914);
                    V3d(109.532499313354, 35.3672981262207, -2.02255249023438);
                    V3d(25.2892017364502, -97.76611328125, 7.16843605041504);
                    V3d(-39.0450477600098, -19.5907592773438, 1.12066268920898);
                    V3d(55.6381225585938, -57.164478302002, -1.92041397094727);
                    V3d(187.817573547363, 34.8072052001953, 7.72666931152344);]
                    
                {el with 
                    trafo = Trafo3d.Translation(newPositions.Item model.evaluationCounter);
                    geometry = Box3d.FromSize(V3d(1.0, 1.0, 15.0))
                })

        let newBoxPos1 = 
            model.evaluationLandmarksLook //look for different position for the look landmarks
            |> PList.updateAt model.evaluationCounter (fun el -> 
                let newPositions = 
                    [V3d(-7.3760986328125, -41.402530670166, -7.04555511474609);
                    V3d(88.4960174560547, -52.3723602294922, -11.848258972168); 
                    V3d(-86.4656448364258, -78.271484375, 23.7221240997314);
                    V3d(34.4533920288086, -24.4295120239258, -8.52031707763672);
                    V3d(110.958623886108, -79.7956466674805, -6.08882904052734);
                    V3d(-16.7708873748779, -83.1976890563965, -3.19957733154297);
                    V3d(74.9305248260498, -33.1958770751953, -12.7483367919922);
                    V3d(10.8861923217773, 97.8855133056641, 19.6725845336914);
                    V3d(87.2105121612549, -139.066791534424, 1.42250061035156);
                    V3d(120.385646820068, -49.5013236999512, -9.09461975097656);
                    V3d(59.9913120269775, 96.1291313171387, 16.9708728790283);
                    V3d(78.129243850708, -189.288806915283, 13.8190746307373);
                    V3d(-69.0516948699951, 39.6745681762695, 24.7023105621338);
                    V3d(213.29870223999, -40.9213066101074, 8.6432933807373);
                    V3d(87.7236366271973, -0.589466094970703, -2.62374877929688);]
                    
                {el with 
                    trafo = Trafo3d.Translation(newPositions.Item model.evaluationCounter);
                    geometry = Box3d.FromSize(V3d(1.0, 1.0, 15.0));
                    color = C4b.Blue
                })

        let model = {model with evaluationLandmarks = newBoxPos; evaluationLandmarksLook = newBoxPos1}
        model
    
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
            
            let controllerOnAnnotationSpace = con.pose.deviceToWorld * model.workSpaceTrafo.Inverse
            let dist = System.Math.Round(V3d.Distance(controllerOnAnnotationSpace.GetModelOrigin(), evalLand.trafo.GetModelOrigin()), 3)
            //printfn "dist: %A" dist
            let model = 
                let newTrafo = Trafo3d.Translation(V3d(model.droneControl.cameraPosition.GetModelOrigin().X + 0.30, model.droneControl.cameraPosition.GetModelOrigin().Y - 0.75, model.droneControl.cameraPosition.GetModelOrigin().Z))
                let updateDrone = {model.droneDistanceToLandmark with text = "distance to next landmark: " + dist.ToString(); trafo = newTrafo}
                {model with droneDistanceToLandmark = updateDrone}

            let model = 
                if dist <= 8.0 then 
                    let newBoxColor = 
                        model.evaluationLandmarks
                        |> PList.updateAt model.evaluationCounter (fun el -> {el with color = C4b.Green})
                    
                    let newBoxColor1 = 
                        model.evaluationLandmarksLook
                        |> PList.updateAt model.evaluationCounter (fun el -> {el with color = C4b.Green})
                    
                    let model = {model with evaluationCounter = model.evaluationCounter + 1; evaluationLandmarks = newBoxColor; evaluationLandmarksLook = newBoxColor1}
                    
                    model |> updateLandmarksPosition

                else model

            //let model = 
            //    match model.evaluationCounter with 
            //    | x when x < 5 -> 
            //        let newMenuMode = {model.menuModel with menu = MenuState.DroneModeController}
            //        {model with menuModel = newMenuMode}
            //    | x when x > = 5 && x < 10 -> 
            //        let newMenuMode = {model.menuModel with menu = MenuState.WIM}
            //        {model with menuModel = newMenuMode}
            //    | x when x > = 10 && x < 15 -> 
            //        let newMenuMode = {model.menuModel with menu = MenuState.Teleportation}
            //        {model with menuModel = newMenuMode}
            //    | _ -> model

            model
        | _, _ -> model

   