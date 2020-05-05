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
    open Aardvark.Base.IndexedGeometryPrimitives

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
        | Some id -> newModel
        | None -> newModel

    let placingOnWIM kind p model : Model =         
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let newModel = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = newModel.menuModel.controllerMenuSelector
        let newCP = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with 
        | Some id -> newModel
        | None -> newModel

    let updateLandmarksPosition model : Model = 
        let randomIndex = 
            let count = model.evaluationLandmarks |> PList.toArray |> Array.length
            let rand = Aardvark.Base.RandomSystem(0)
            let randomOrderIndex = rand.CreatePermutationArray(count)
            let takeRandomInt = 
                randomOrderIndex 
                |> Array.item model.evaluationCounter
            takeRandomInt

        let newList = 
            let newPos = 
                match model.menuModel.menu with 
                | MenuState.Teleportation -> 
                    [
                    V3d(2.0909309387207, -142.078304290771, 3.15451622009277);
                    V3d(57.3034286499023, -146.660900115967, -0.212621688842773);
                    V3d(22.669792175293, -48.552417755127, -19.4751739501953);
                    V3d(129.562330245972, 7.34128952026367, -6.46204948425293);
                    V3d(90.2481079101563, -110.047054290771, -11.1869812011719); 
                    V3d(162.833166122437, -70.1943397521973, -11.143970489502);
                    V3d(44.392204284668, -57.7020645141602, -15.8747673034668);
                    V3d(-3.53236198425293, -106.497097015381, -7.25884437561035);
                    V3d(113.606834411621, -15.682315826416, -14.9408340454102);
                    V3d(171.59743309021, -36.6165161132813, -7.13167190551758);
                    V3d(38.8544082641602, -90.4772758483887, -0.00252723693847656);
                    V3d(111.649608612061, -83.9065551757813, -15.6451225280762);
                    V3d(61.7051124572754, -7.85865783691406, -14.4855976104736);
                    V3d(-1.60675048828125, -67.8957939147949, -13.7360572814941);
                    V3d(135.869789123535, -128.028678894043, -6.07657432556152);]
                | _ ->
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
            let newRandomPos = 
                newPos |> List.item randomIndex 

            model.evaluationLandmarks
            |> PList.updateAt model.evaluationCounter (fun el -> 
                {el with 
                    trafo = Trafo3d.Translation newRandomPos; 
                    geometry = Box3d.FromSize(V3d(1.0, 1.0, 15.0));
                    color = C4b.Red
                }
            )

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
                    geometry = Box3d.FromSize(V3d(1.0, 1.0, 15.0));
                    color = C4b.Red
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
                    
                let newTrafo = Trafo3d.Translation(newPositions.Item model.evaluationCounter)
                {el with 
                    trafo = newTrafo;
                    geometry = Cone.solidCone (newTrafo.GetModelOrigin()) V3d.Zero 5.0 0.5 20 C4b.Red //Box3d.FromSize(V3d(1.0, 1.0, 15.0));
                    color = C4b.Blue
                })

        let model = {model with evaluationLandmarks = newList; evaluationLandmarksLook = newBoxPos1}
        model
    
    let hoverEvaluationLandmarks kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = model.menuModel.controllerMenuSelector
        
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind

        let secondCon = 
            if controllerPos.kind.Equals(ControllerKind.ControllerA) then
                model.controllerInfos |> HMap.tryFind ControllerKind.ControllerB
            else model.controllerInfos |> HMap.tryFind ControllerKind.ControllerA

        let evalLandmark = 
            model.evaluationLandmarks
            |> PList.tryAt model.evaluationCounter

        match newCP, evalLandmark, secondCon with
        | Some con, Some evalLand, Some con2 ->
            let controllerOnAnnotationSpace = con.pose.deviceToWorld * model.workSpaceTrafo.Inverse
            let dist = System.Math.Round((evalLand.trafo.GetModelOrigin().XY - controllerOnAnnotationSpace.GetModelOrigin().XY).Length, 3)
            
            //printfn "minimum dist: %A" dist
            
            let model = 
                let newTrafo = Trafo3d.Translation(V3d(con.pose.deviceToWorld.GetModelOrigin().X + 0.30, con.pose.deviceToWorld.GetModelOrigin().Y - 0.75, con.pose.deviceToWorld.GetModelOrigin().Z))
                let updateDrone = {model.droneDistanceToLandmark with text = "distance to next landmark: " + dist.ToString() + " meters"; trafo = newTrafo}
                {model with droneDistanceToLandmark = updateDrone}

            let model = 
                if dist <= 8.0 then 
                    let newBoxColor = 
                        model.evaluationLandmarks
                        |> PList.updateAt model.evaluationCounter (fun el -> {el with color = C4b.Green}) //i can make the boxes that are already seen smaller so that it is not that confusing here
                    
                    let model = {model with evaluationLandmarks = newBoxColor}
                    
                    let model = 
                        match con2.backButtonPressed with 
                        | true -> 
                            let newBoxColor1 = 
                                model.evaluationLandmarksLook
                                |> PList.updateAt (model.evaluationCounter) (fun el -> {el with color = C4b.Green})

                            let model = 
                                {model with 
                                    evaluationCounter = model.evaluationCounter + 1; 
                                    evaluationLandmarks = newBoxColor; 
                                    evaluationLandmarksLook = newBoxColor1;
                                }
                    
                            model |> updateLandmarksPosition
                        | false -> model
                    model 
                else model
            model
        | _, _, _ -> model


   