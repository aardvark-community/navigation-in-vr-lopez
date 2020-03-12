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
                    |> PList.map (fun uPos -> {uPos with trafo = con2.pose.deviceToWorld}//Trafo3d.FromComponents(scaleUPos, rotationUPos1, translationUPos)}
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
                        //let newTrafo = con2.pose.deviceToWorld.GetModelOrigin() + model.wim
                        {uPosAS with 
                            trafo = con2.pose.deviceToWorld * newModel.WIMworkSpaceTrafo.Inverse
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
                model
            | false -> 
                let upWIM = model.WIMuserPos |> PList.tryFirst
                let upAnn = model.userPosOnAnnotationSpace |> PList.tryFirst
                match upWIM, upAnn with 
                | Some pos, Some annPos -> 
                    match pos.isHovered with //this part is still not taken into account
                    | true -> 
                        match model.menuModel.menu with
                        | MenuState.WIMLandmarks -> 
                            let vbPosAnnSpace = annPos//{pos with trafo = con2.pose.deviceToWorld * model.WIMworkSpaceTrafo.Inverse}
                            let updatePosAnnSpace = 
                                {annPos with trafo = vbPosAnnSpace.trafo; color = C4b.Yellow}
                                |> PList.single
                                //user position in real world
                            
                            let updatePosWIMspace = 
                                {pos with trafo = con2.pose.deviceToWorld }
                                |> PList.single
                                //user position in wim

                            let newPos = annPos.trafo.GetModelOrigin()
                            let oldPos = model.workSpaceTrafo.GetModelOrigin()
                            let shift = newPos// - oldPos
                            let shiftTrafo = Trafo3d.Translation(shift).Inverse

                            printfn "old pos: %A new pos: %A with shift: %A" oldPos newPos shift

                            //let transportUserPos = annPos.trafo.Inverse * model.workSpaceTrafo //* model.annotationSpaceTrafo 
                            let newWorkSpace = model.initWorkSpaceTrafo * shiftTrafo
                            let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace
                            let newFlagSpace = model.initAnnotationSpaceTrafo * newWorkSpace

                            {model with 
                                //userPosOnAnnotationSpace    = updatePosAnnSpace;
                                WIMuserPos                  = updatePosWIMspace
                                workSpaceTrafo              = newWorkSpace
                                opcSpaceTrafo               = newOpcSpace
                                annotationSpaceTrafo        = newFlagSpace
                            }
                    
                        | _ -> model
                    | false -> model
                | _, _ -> model 
        
        | None -> model
        
        
        
        //match secondCon with 
        //| Some con2 -> 
        //    let upWIM = model.WIMuserPos |> PList.tryFirst
        //    let model = 
        //        match upWIM with 
        //        | Some pos -> 
        //            let updatePosAnnSpace = 
        //                {pos with trafo = con2.pose.deviceToWorld * model.workSpaceTrafo.Inverse}
        //                //user position in real world
        //            match model.menuModel.menu with
        //            | MenuState.WIMLandmarks -> 
        //                let newPosAnnSpaceList = 
        //                    model.userPosOnAnnotationSpace
        //                    |> PList.prepend updatePosAnnSpace
        //                {model with userPosOnAnnotationSpace = newPosAnnSpaceList}
        //            | _ -> model
        //        | None -> model 
        //    let newRealWorldPos = model.userPosOnAnnotationSpace |> PList.tryFirst
        //    match newRealWorldPos with 
        //    | Some pos -> 
        //        let updatePosAnnSpace = 
        //            {pos with trafo = pos.trafo}
        //        match model.menuModel.menu with
        //        | MenuState.Scale -> 
        //            match con2.sideButtonPressed with 
        //            | true -> 

        //                let s, r, t = updatePosAnnSpace.trafo.Decompose()
        //                let ss, rr, tt = pos.trafo.Decompose()
        //                printfn "t: %s" (t.ToString())
        //                printfn " tt: %s" (tt.ToString())
        //                printfn "pos: %s" (pos.trafo.GetModelOrigin().ToString())
        //                let tttttt = pos.trafo * model.workSpaceTrafo.Inverse * model.annotationSpaceTrafo
        //                printfn "ttttttt: %s" (tttttt.GetModelOrigin().ToString())
        //                let transportUserPos = updatePosAnnSpace.trafo * model.workSpaceTrafo.Inverse * model.annotationSpaceTrafo
        //                let transportUserPosWIMInv = updatePosAnnSpace.trafo * model.WIMworkSpaceTrafo.Inverse * model.annotationSpaceTrafo
        //                let transportUserPosNoAnn = updatePosAnnSpace.trafo * model.workSpaceTrafo.Inverse 
        //                let testttt = updatePosAnnSpace.trafo * model.workSpaceTrafo * model.WIMworkSpaceTrafo.Inverse
        //                let test1 = updatePosAnnSpace.trafo.Inverse * model.workSpaceTrafo.Inverse * model.annotationSpaceTrafo
        //                let test2 = updatePosAnnSpace.trafo.Inverse * model.workSpaceTrafo * model.WIMworkSpaceTrafo.Inverse
        //                let test3 = updatePosAnnSpace.trafo.Inverse * model.workSpaceTrafo.Inverse * model.WIMworkSpaceTrafo.Inverse
        //                let newworkspacewim = model.initWorkSpaceTrafo * transportUserPosWIMInv.Inverse
        //                printfn "workspace trafo: %s" (model.workSpaceTrafo.GetModelOrigin().ToString())
        //                printfn "update pos ann trafo: %s" (updatePosAnnSpace.trafo.GetModelOrigin().ToString())
        //                printfn "update pos ann trafo wim inverse: %s" (transportUserPosWIMInv.GetModelOrigin().ToString())
        //                printfn "transport trafo: %s" (transportUserPos.GetModelOrigin().ToString())
        //                printfn "transport trafo No Ann: %s" (transportUserPosNoAnn.GetModelOrigin().ToString())
        //                printfn "controller pos: %s" (con2.pose.deviceToWorld.GetModelOrigin().ToString())
        //                printfn "test: %s" (testttt.GetModelOrigin().ToString())
        //                printfn "test1: %s" (test1.GetModelOrigin().ToString())
        //                printfn "test2: %s" (test2.GetModelOrigin().ToString())
        //                printfn "test3: %s" (test3.GetModelOrigin().ToString())
                        
        //                let newWorkSpace = model.initWorkSpaceTrafo * transportUserPos.Inverse
        //                let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace
        //                let newFlagSpace = model.initAnnotationSpaceTrafo * newWorkSpace
        //                printfn "newowrkspace trafo: %s" (newWorkSpace.GetModelOrigin().ToString())
        //                printfn "newowrkspace wim trafo: %s" (newworkspacewim.GetModelOrigin().ToString())
        //                printfn "newopcspace trafo: %s" (newOpcSpace.GetModelOrigin().ToString())
        //                printfn "newflagspace trafo: %s" (newFlagSpace.GetModelOrigin().ToString())
                    
        //                {model with 
        //                    workSpaceTrafo              = newWorkSpace
        //                    opcSpaceTrafo               = newOpcSpace
        //                    annotationSpaceTrafo        = newFlagSpace
        //                }
        //            | false -> model
        //        | _ -> model
        //    | None -> model 
        //| None -> model
         
        
        