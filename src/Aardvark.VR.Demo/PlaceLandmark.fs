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
        let con2Pos = con.pose.deviceToWorld// * newModel.workSpaceTrafo.Inverse * newModel.WIMworkSpaceTrafo
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
                        Some {u with isHovered = true}
                    else Some {u with isHovered = false}
                )
            | None -> newModel.WIMuserPos 
        
        let newModel = {newModel with WIMuserPos = checkWIMuserHover}

        let changeWIMuserPosWithCon2 = 
            match secondCon with
            | Some con2 -> 
                match con2.backButtonPressed with 
                | true -> 
                    newModel.WIMuserPos 
                    |> PList.map (fun uPos -> 
                        let newTrafo = createNewTrafo con2
                        {uPos with trafo = newTrafo}//Trafo3d.FromComponents(scaleUPos, rotationUPos1, translationUPos)}
                    )
                | false -> newModel.WIMuserPos
            | None -> newModel.WIMuserPos

        let changeUserPosWithCon2OnAnnotationSpace = 
            match secondCon with 
            | Some con2 -> 
                match con2.backButtonPressed with 
                | true -> 
                    newModel.userPosOnAnnotationSpace
                    |> PList.map (fun uPosAS -> 
                        let newTrafo = createNewTrafo con2
                        {uPosAS with 
                            trafo = newTrafo * newModel.WIMworkSpaceTrafo.Inverse
                            color = C4b.Yellow
                        }
                    )
                | false -> newModel.userPosOnAnnotationSpace
            | None -> newModel.userPosOnAnnotationSpace
            
        {newModel with 
            WIMuserPos = changeWIMuserPosWithCon2
            userPosOnAnnotationSpace = changeUserPosWithCon2OnAnnotationSpace
        }

    let moveUserToNewPosOnAnnotationSpace model : Model = 
        let controllerPos = model.menuModel.controllerMenuSelector

        let secondCon = 
            if controllerPos.kind.Equals(ControllerKind.ControllerA) then
                model.controllerInfos |> HMap.tryFind ControllerKind.ControllerB
            else model.controllerInfos |> HMap.tryFind ControllerKind.ControllerA

        match secondCon with 
        | Some con2 -> 
            match con2.backButtonPressed with 
            | true -> 
                let upInitialWIM = model.WIMinitialUserPos |> PList.tryFirst
                match upInitialWIM with
                | Some p -> 
                    let newTrafo = createNewTrafo con2
                    let updateInitialWIM = {p with trafo = newTrafo; color = C4b.Cyan}
                    let newInitialList = 
                        model.WIMinitialUserPos 
                        |> PList.prepend updateInitialWIM
                
                    {model with WIMinitialUserPos = newInitialList}
                | None -> model
                
            | false -> 
                let upWIM = model.WIMuserPos |> PList.tryFirst
                let upAnn = model.userPosOnAnnotationSpace |> PList.tryFirst
                match upWIM, upAnn with 
                | Some pos, Some annPos -> 
                    match pos.isHovered with //this part is still not taken into account
                    | true -> 
                        match model.menuModel.menu with
                        | MenuState.WIMLandmarks | MenuState.Cyllinder -> 
                            let updatePosWIMspace =                                 
                                let newTrafo = createNewTrafo con2
                                {pos with trafo = newTrafo}
                                |> PList.single
                                //user position in wim

                            let conRotation = con2.pose.deviceToWorld.GetOrthoNormalOrientation()

                            let shiftTrafo = Trafo3d.Translation(annPos.trafo.GetModelOrigin()).Inverse * conRotation.Inverse 
                            //is takes all possible rotation of controller into account, we only want the z rotation

                            let newWorkSpace = model.initWorkSpaceTrafo * shiftTrafo
                            let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace
                            let newFlagSpace = model.initAnnotationSpaceTrafo * newWorkSpace

                            {model with 
                                WIMuserPos                  = updatePosWIMspace
                                workSpaceTrafo              = newWorkSpace
                                opcSpaceTrafo               = newOpcSpace
                                annotationSpaceTrafo        = newFlagSpace
                            }
                        
                        | _ -> model
                    | false -> model
                | _, _ -> model 
        
        | None -> model
        