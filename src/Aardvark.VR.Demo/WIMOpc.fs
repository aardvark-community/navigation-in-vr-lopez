namespace Demo.Main

open Aardvark.Base
open Aardvark.Base.Incremental

module WIMOpc = 
    open Aardvark.Application
    open Aardvark.VRVis.Opc
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    open Model
    open OpenTK
    open Aardvark.Base.MapExtImplementation
    open Demo.Mutable
    open Demo
    open Aardvark.UI
    open Demo.Menu
    open System.IO.MemoryMappedFiles

    let createNewTrafo con : Trafo3d = 
        let con2Pos = con.pose.deviceToWorld
        let r = con2Pos.GetOrthoNormalOrientation()
        let rot = Rot3d.FromFrame(r.Forward.C0.XYZ, r.Forward.C1.XYZ, r.Forward.C2.XYZ)
        let rotation = rot.GetEulerAngles()
        let rotation1 = V3d(0.0, 0.0, rotation.Z)

        let translation = con2Pos.GetModelOrigin()

        let scale = V3d(0.5, 0.5, 0.5)
                        
        Trafo3d.FromComponents(scale, rotation1, translation)
    
    let showMiniMap model : Model = 
        let userPos = 
            let userHMD = model.controllerInfos |> HMap.tryFind Demo.ControllerKind.HMD
            match userHMD with
            | Some pos -> pos.pose
            | None -> Aardvark.Vr.Pose.none

        let controllerPos = 
            let cp = model.menuModel.controllerMenuSelector
            let newCp = model.controllerInfos |> HMap.tryFind cp.kind
            match newCp with
            | Some id -> id.pose
            | None -> Aardvark.Vr.Pose.none

        let minimapX = (controllerPos.deviceToWorld.GetModelOrigin() - userPos.deviceToWorld.GetModelOrigin()).Normalized
        let minimapY = V3d.Cross(minimapX,V3d.OOI)
        let minimapZ = V3d.Cross(minimapY,minimapX)
        let minimapCoordinateSystem = Trafo3d.FromBasis(-minimapX, -minimapY, minimapZ, controllerPos.deviceToWorld.GetModelOrigin())

        let minimapRt = minimapCoordinateSystem.GetOrthoNormalOrientation()
        let minimapRot = Rot3d.FromFrame(minimapRt.Forward.C0.XYZ, minimapRt.Forward.C1.XYZ, minimapRt.Forward.C2.XYZ)
        let minimapRotation = minimapRot.GetEulerAngles()
        let minimapTrans = minimapCoordinateSystem.GetModelOrigin()
        let minimapScale = V3d(0.0025, 0.0025, 0.0025)

        let minimapPos = 
            Trafo3d.FromComponents(minimapScale, minimapRotation, V3d(minimapTrans.X + 0.75, minimapTrans.Y, minimapTrans.Z - 0.25))
            
        let newWorkSpace = minimapPos//Trafo3d.FromBasis(V3d(0.000866772490015494, 0.000111846836880938, 0.000295737151484485), V3d(-0.000117399006863729, 0.000915138419620922, -2.01914890967389E-06), V3d(-0.00029357732806564, -3.57334077219641E-05, 0.000873956677452764), V3d(-0.424166218656598, -0.256776470921912, 0.798703249673775))
        let newAnnotationSpace = model.initAnnotationSpaceTrafo * newWorkSpace
        let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace
        //Scale now is: 1.048753, 1.048753, 1.048753
        
        {model with  
            WIMopcSpaceTrafo                = newOpcSpace;
            WIMannotationSpaceTrafo         = newAnnotationSpace;
            WIMworkSpaceTrafo               = newWorkSpace;
            landmarkOnController            = PList.empty;
        }

    let editMiniMap kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let newModel = {model with controllerInfos = newControllersPosition}
        
        let controllerPos = newModel.menuModel.controllerMenuSelector

        let userHMD = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
      
        let newWIMuserPos = 
            match userHMD with 
            | Some pos -> 
                newModel.WIMuserPos
                |> PList.map (fun lmkC -> 
                    let newLmkC = pos.pose.deviceToWorld * newModel.workSpaceTrafo.Inverse * newModel.WIMworkSpaceTrafo
                    let rtLmkC = newLmkC.GetOrthoNormalOrientation()
                    let rotLmkC = Rot3d.FromFrame(rtLmkC.Forward.C0.XYZ, rtLmkC.Forward.C1.XYZ, rtLmkC.Forward.C2.XYZ)
                    let rotationLmkC = rotLmkC.GetEulerAngles()
                    let rotationLmkC1 = V3d(0.0, 0.0, rotationLmkC.Z)

                    let translationLmkC = newLmkC.GetModelOrigin()

                    let scaleLmkC = V3d(0.5, 0.5, 0.5)
                    {lmkC with trafo = Trafo3d.FromComponents(scaleLmkC, rotationLmkC1, translationLmkC)}
                )
            | None -> PList.empty

        let updateWIMLandmark = 
            newModel.landmarkOnAnnotationSpace
            |> PList.map (fun landmark -> 
                let newLmkC = landmark.trafo * newModel.WIMworkSpaceTrafo
                let rtLmkC = newLmkC.GetOrthoNormalOrientation()
                let rotLmkC = Rot3d.FromFrame(rtLmkC.Forward.C0.XYZ, rtLmkC.Forward.C1.XYZ, rtLmkC.Forward.C2.XYZ)
                let rotationLmkC = rotLmkC.GetEulerAngles()

                let translationLmkC = newLmkC.GetModelOrigin()

                let scaleLmkC = V3d(0.5, 0.5, 0.5)

                {landmark with trafo = Trafo3d.FromComponents(scaleLmkC, rotationLmkC, translationLmkC)} //landmark.trafo * model.workSpaceTrafo.Inverse * newWorkSpace}
            )

        {newModel with 
            WIMuserPos                      = newWIMuserPos;
            WIMlandmarkOnAnnotationSpace    = updateWIMLandmark
        }
    
    let updateMiniMap kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}

        let controllerPos = 
            let cp = model.menuModel.controllerMenuSelector
            let newCp = model.controllerInfos |> HMap.tryFind cp.kind
            match newCp with
            | Some id -> id.pose
            | None -> Aardvark.Vr.Pose.none
            
        let minimapRt = controllerPos.deviceToWorld.GetOrthoNormalOrientation()
        let minimapRot = Rot3d.FromFrame(minimapRt.Forward.C0.XYZ, minimapRt.Forward.C1.XYZ, minimapRt.Forward.C2.XYZ)
        let minimapRotation = minimapRot.GetEulerAngles()
        let minimapTrans = controllerPos.deviceToWorld.GetModelOrigin()
        let minimapScale = V3d(0.0025, 0.0025, 0.0025)

        let minimapPos = //minimapCoordinateSystem
            Trafo3d.FromComponents(minimapScale, V3d.Zero, minimapTrans)
        
        let newWorkSpace = minimapPos
        let newAnnotationSpace = model.initAnnotationSpaceTrafo * newWorkSpace
        let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace
        

        {model with 
            WIMopcSpaceTrafo                = newOpcSpace;
            WIMannotationSpaceTrafo         = newAnnotationSpace;
            WIMworkSpaceTrafo               = newWorkSpace;
        }

    let checkHoverUserWIM kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        let userOnWIM = 
            model.WIMuserPos
            |> PList.tryFirst
        match newCP, userOnWIM with 
        | Some con, Some userPos -> 
            match con.backButtonPressed with 
            | true -> model 
            | false -> 
                let dist = V3d.Distance(userPos.trafo.GetModelOrigin(), con.pose.deviceToWorld.GetModelOrigin())
                if dist <= 0.1 then 
                    let newMode = {model.menuModel with menu = MenuState.HoverChangeUserWIM}
                    {model with menuModel = newMode}
                else 
                    let newMode1 = {model.menuModel with menu = MenuState.WIMLandmarks}
                    {model with menuModel = newMode1}
        | _, _ -> 
            model

    let changeUserPosWIM kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}
        
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        
        let newUserPos = 
            match newCP with 
            | Some con -> 
                match con.backButtonPressed with 
                | true -> 
                    model.WIMuserPos
                    |> PList.map (fun pos -> 
                        let newTrafo = createNewTrafo con
                        //{pos with trafo = Trafo3d.Translation(con.pose.deviceToWorld.GetModelOrigin())}
                        {pos with trafo = newTrafo}
                    )
                | false -> model.WIMuserPos
            | None -> model.WIMuserPos

        let newUserPosOnAnnotationSpace = 
            match newCP with 
            | Some con -> 
                match con.backButtonPressed with 
                | true -> 
                    model.userPosOnAnnotationSpace
                    |> PList.map (fun uPosAS -> 
                        let newTrafo = createNewTrafo con
                        {uPosAS with 
                            trafo = newTrafo * model.WIMworkSpaceTrafo.Inverse
                            color = C4b.Yellow
                        }
                    )
                | false -> model.userPosOnAnnotationSpace
            | None -> model.userPosOnAnnotationSpace
        
        {model with 
            WIMuserPos              = newUserPos
            userPosOnAnnotationSpace= newUserPosOnAnnotationSpace    
        }
      
    let moveUserToAnnotationSpaceFromWIM model : Model = 
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        match newCP with 
        | Some con -> 
            match con.backButtonPressed with 
            | true -> 
                let upInitialWIM = model.WIMinitialUserPos |> PList.tryFirst
                match upInitialWIM with
                | Some p -> 
                    let newTrafo = createNewTrafo con
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
                        match model.menuModel.menu with
                        | MenuState.HoverChangeUserWIM (*| MenuState.WIMLandmarks | MenuState.Cyllinder *) -> 
                            let updatePosWIMspace =                                 
                                let newTrafo = createNewTrafo con
                                {pos with trafo = newTrafo}
                                |> PList.single
                                //user position in wim

                            let conRotation = con.pose.deviceToWorld.GetOrthoNormalOrientation()

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
                | _, _ -> model 
        | None -> model