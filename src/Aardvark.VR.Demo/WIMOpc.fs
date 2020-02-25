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

        let minimapPos = //minimapCoordinateSystem
            Trafo3d.FromComponents(minimapScale, minimapRotation, V3d(minimapTrans.X + 0.75, minimapTrans.Y, minimapTrans.Z - 0.25))
            
        let newWorkSpace = minimapPos//Trafo3d.FromBasis(V3d(0.000866772490015494, 0.000111846836880938, 0.000295737151484485), V3d(-0.000117399006863729, 0.000915138419620922, -2.01914890967389E-06), V3d(-0.00029357732806564, -3.57334077219641E-05, 0.000873956677452764), V3d(-0.424166218656598, -0.256776470921912, 0.798703249673775))
        // we are not taking into account user's position here
        let newAnnotationSpace = model.initAnnotationSpaceTrafo * newWorkSpace
        let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace
        //Scale now is: 1.048753, 1.048753, 1.048753
        
        let newWIMlandmark = 
            model.landmarkOnAnnotationSpace
            |> PList.map (fun lmk -> 
                let newLmk = lmk.trafo * newWorkSpace
                let rt = newLmk.GetOrthoNormalOrientation()
                let rot = Rot3d.FromFrame(rt.Forward.C0.XYZ, rt.Forward.C1.XYZ, rt.Forward.C2.XYZ)
                let rotation = rot.GetEulerAngles()

                let translation = newLmk.GetModelOrigin()

                let scale = V3d(0.5, 0.5, 0.5)
                {lmk with trafo = Trafo3d.FromComponents(scale, rotation, translation)}
                //{lmk with trafo = lmk.trafo * newWorkSpace}
            ) 

        {model with  
            WIMopcSpaceTrafo                = newOpcSpace;
            WIMannotationSpaceTrafo         = newAnnotationSpace;
            WIMworkSpaceTrafo               = newWorkSpace;
            WIMlandmarkOnAnnotationSpace    = newWIMlandmark;
            landmarkOnController            = PList.empty;
        }

    let editMiniMap kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let newModel = {model with controllerInfos = newControllersPosition}
        
        let controllerPos = newModel.menuModel.controllerMenuSelector

        let userHMD = newModel.controllerInfos |> HMap.tryFind controllerPos.kind
        
        //match userHMD with
        //| Some pos -> 
        //    let newHMDPosList = 
        //        newModel.WIMuserPos
        //        |> PList.map (fun lmk -> 
        //            {lmk with trafo = pos.pose.deviceToWorld * newModel.WIMworkSpaceTrafo.Inverse}
        //        )
        //    {newModel with WIMuserPos = newHMDPosList}
        //| None -> newModel

        let testin = 
            match userHMD with 
            | Some pos -> 
                newModel.WIMuserPos
                |> PList.map (fun lmkC -> 
                    let newLmkC = pos.pose.deviceToWorld * newModel.WIMworkSpaceTrafo
                    let rtLmkC = newLmkC.GetOrthoNormalOrientation()
                    let rotLmkC = Rot3d.FromFrame(rtLmkC.Forward.C0.XYZ, rtLmkC.Forward.C1.XYZ, rtLmkC.Forward.C2.XYZ)
                    let rotationLmkC = rotLmkC.GetEulerAngles()

                    let translationLmkC = newLmkC.GetModelOrigin()

                    let scaleLmkC = V3d(0.5, 0.5, 0.5)
                    {lmkC with trafo = Trafo3d.FromComponents(scaleLmkC, rotationLmkC, translationLmkC)}
                )
            | None -> PList.empty
        
        {newModel with WIMuserPos = testin}


        

    
    
