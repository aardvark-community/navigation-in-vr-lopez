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
                    [V3d(38.8926982879639, -29.7210693359375, -8.70199203491211);
                    V3d(110.17427444458, -14.1537666320801, -8.29739570617676); 
                    V3d(14.0573501586914, -133.96692276001, 5.29561042785645);
                    V3d(35.921049118042, 53.1082153320313, -2.07629203796387);
                    V3d(146.564960479736, -66.0524368286133, -5.71193695068359);
                    V3d(41.5493488311768, -87.373161315918, 6.32567405700684);
                    V3d(54.5666694641113, -133.329486846924, -2.17437744140625);
                    V3d(136.545467376709, 30.7690620422363, -0.152873992919922);
                    V3d(73.2680320739746, 15.2651786804199, -8.22319984436035);
                    V3d(18.0526256561279, -63.6599540710449, -9.76648330688477);
                    V3d(155.813646316528, -20.6089973449707, -1.05986595153809);
                    V3d(62.2969150543213, -40.8635139465332, -13.5091781616211);
                    V3d(115.009212493896, -106.939125061035, -5.96075057983398);
                    V3d(-5.00621795654297, -81.6075325012207, -5.85775375366211);
                    V3d(66.914176940918, 58.1603050231934, 0.438928604125977);]
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


   