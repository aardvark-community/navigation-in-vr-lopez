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
        
        let newModel = 
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

        let secondCon = 
            if controllerPos.kind.Equals(ControllerKind.ControllerA) then
                newModel.controllerInfos |> HMap.tryFind ControllerKind.ControllerB
            else newModel.controllerInfos |> HMap.tryFind ControllerKind.ControllerA
        
        let checkWIMuserHover = 
            match secondCon with 
            | Some con2 -> 
                newModel.WIMuserPos
                |> PList.choosei (fun _ u -> 
                    let dist = V3d.Distance(u.trafo.GetModelOrigin(), con2.pose.deviceToWorld.GetModelOrigin())
                    if (dist <= 0.1) then 
                        Some {u with color = C4b.Blue; isHovered = true}
                    else Some {u with color = C4b.Red; isHovered = false}
                )
            | None -> newModel.WIMuserPos 
        
        let newModel = {newModel with WIMuserPos = checkWIMuserHover}

        let changeWIMuserPosWithCon2 = 
            match secondCon with
            | Some con2 -> 
                match con2.backButtonPressed with 
                | true -> 
                    newModel.WIMuserPos 
                    |> PList.map (fun uPos -> {uPos with trafo = con2.pose.deviceToWorld}//Trafo3d.FromComponents(scaleUPos, rotationUPos1, translationUPos)}
                    )
                | false -> newModel.WIMuserPos
            | None -> newModel.WIMuserPos
            
        {newModel with WIMuserPos = changeWIMuserPosWithCon2}

    let moveUserToNewPosOnAnnotationSpace model : Model = 
        let controllerPos = model.menuModel.controllerMenuSelector

        let secondCon = 
            if controllerPos.kind.Equals(ControllerKind.ControllerA) then
                model.controllerInfos |> HMap.tryFind ControllerKind.ControllerB
            else model.controllerInfos |> HMap.tryFind ControllerKind.ControllerA

        match secondCon with 
        | Some con2 -> 
            let upWIM = model.WIMuserPos |> PList.tryFirst
            let newRealWorldPos = model.userPosOnAnnotationSpace |> PList.tryFirst
            let model = 
                match upWIM with 
                | Some pos -> 
                    let updatePosAnnSpace = 
                        {pos with trafo = con2.pose.deviceToWorld * model.WIMworkSpaceTrafo.Inverse}
                    match model.menuModel.menu with
                    | MenuState.WIMLandmarks -> 

                        let newPosAnnSpaceList = 
                            model.userPosOnAnnotationSpace
                            |> PList.prepend updatePosAnnSpace

                        {model with userPosOnAnnotationSpace = newPosAnnSpaceList}
                    
                    | _ -> model
                | None -> model 
            match newRealWorldPos with 
            | Some pos -> 
                let updatePosAnnSpace = 
                    {pos with trafo = pos.trafo * model.WIMworkSpaceTrafo.Inverse}
                match model.menuModel.menu with
                | MenuState.Scale -> 
                    match con2.sideButtonPressed with 
                    | true -> 
                        ////
                        //let newLmkC = landmark.trafo * newModel.workSpaceTrafo.Inverse * newModel.WIMworkSpaceTrafo
                        //let rtLmkC = newLmkC.GetOrthoNormalOrientation()
                        //let rotLmkC = Rot3d.FromFrame(rtLmkC.Forward.C0.XYZ, rtLmkC.Forward.C1.XYZ, rtLmkC.Forward.C2.XYZ)
                        //let rotationLmkC = rotLmkC.GetEulerAngles()
                        //let translationLmkC = newLmkC.GetModelOrigin()
                        //let scaleLmkC = V3d(0.5, 0.5, 0.5)
                        ////

                        let transportUserPos = updatePosAnnSpace.trafo * model.workSpaceTrafo.Inverse * model.annotationSpaceTrafo

                        let newWorkSpace = model.initWorkSpaceTrafo * transportUserPos.Inverse
                        let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace
                        let newFlagSpace = model.initAnnotationSpaceTrafo * newWorkSpace
                    
                        {model with 
                            workSpaceTrafo              = newWorkSpace
                            opcSpaceTrafo               = newOpcSpace
                            annotationSpaceTrafo        = newFlagSpace
                        }
                    | false -> model
                | _ -> model
            | None -> model 
        | None -> model
         
        
        