﻿namespace Demo.Main

open Aardvark.Base
open Aardvark.Base.Incremental

module CyllinderCenter = 
    open Aardvark.Vr
    open Aardvark.Application
    open Aardvark.VRVis.Opc
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    open Model
    open OpenTK
    open Aardvark.Base.MapExtImplementation
    open Demo


    let checkInside kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p

        let model = { model with controllerInfos = newControllersPosition}

        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind

        let getCyllinder = 
            model.cyllinderControl |> PList.tryFirst

        let updateCyllinder = 
            match newCP, getCyllinder with 
            | Some con, Some cyl -> 
                let dist = V3d.Distance(cyl.trafo.GetModelOrigin(), con.pose.deviceToWorld.GetModelOrigin()) 
                if (dist <= 1.5) then 
                    {cyl with isNotInside = false; color = C4b.Cyan}
                else {cyl with isNotInside = true; color = C4b.DarkBlue}
                |> PList.single
            
            | _, _ -> model.cyllinderControl

        {model with cyllinderControl = updateCyllinder}

    let controlCenter kind p model : Model = 
        let controllerPos = 
            let cp = model.menuModel.controllerMenuSelector
            let newCp = model.controllerInfos |> HMap.tryFind cp.kind
            match newCp with
            | Some id -> id.pose
            | None -> Aardvark.Vr.Pose.none
            
        let minimapTrans = V3d(Trafo3d.Identity.GetModelOrigin().X, Trafo3d.Identity.GetModelOrigin().Y, Trafo3d.Identity.GetModelOrigin().Z + 1.0) //controllerPos.deviceToWorld.GetModelOrigin()
        let minimapScale = V3d(0.0025, 0.0025, 0.0025)

        let minimapPos = Trafo3d.FromComponents(minimapScale, V3d.Zero, minimapTrans)
        
        let newWorkSpace = minimapPos
        let newAnnotationSpace = model.initAnnotationSpaceTrafo * newWorkSpace
        let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace

        let model = 
            {model with 
                WIMopcSpaceTrafo                = newOpcSpace;
                WIMannotationSpaceTrafo         = newAnnotationSpace;
                WIMworkSpaceTrafo               = newWorkSpace;
            }

        model 
        |> WIMOpc.editMiniMap kind p
        







