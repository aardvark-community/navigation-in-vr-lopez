namespace Demo.Main

open Aardvark.Base
open Aardvark.Base.Incremental

module Teleport = 
    open Aardvark.Application
    open Aardvark.VRVis.Opc
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    open Model
    open OpenTK
    open Aardvark.Base.MapExtImplementation
    open Aardvark.Prinziple
    open Aardvark.SceneGraph.Opc
    open Demo
    open Aardvark.Base
    open Aardvark.Base.IndexedGeometryPrimitives

    let hitRay kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with
        | Some id -> 
            let controllTrafo = id.pose.deviceToWorld * model.opcSpaceTrafo.Inverse
            let origin = controllTrafo.Forward.TransformPos V3d.Zero
            let controllDir = controllTrafo.Forward.TransformDir V3d.YAxis

            let testRay = Ray3d(origin, controllDir)
                        
            {model with teleportRay = testRay}
        | None -> model

    let teleportUser model : Model = 
        let controllerPos = model.menuModel.controllerMenuSelector
        let newCP = model.controllerInfos |> HMap.tryFind controllerPos.kind
        
        match newCP with 
        | Some id -> 
            match id.backButtonPressed with 
            | true -> 
                let teleportPos = model.teleportBox |> PList.tryFirst
                match teleportPos with 
                | Some tPos -> 
                    let newVector = (tPos.trafo * model.initOpcSpaceTrafo).GetModelOrigin()
                    let newTeleportPos = Trafo3d.Translation(newVector).Inverse 
                    let newWorkSpace = model.initWorkSpaceTrafo * newTeleportPos
                    let newOpcSpace = model.initOpcSpaceTrafo * newWorkSpace
                    let newFlagSpace = model.initAnnotationSpaceTrafo * newWorkSpace

                    { model with 
                        workSpaceTrafo              = newWorkSpace
                        opcSpaceTrafo               = newOpcSpace
                        annotationSpaceTrafo        = newFlagSpace
                    }
                | None -> model
            | false -> model
        | None -> model 

    let rayIntersection kind p model : Model = 
        let newControllersPosition = 
            model 
            |> OpcUtilities.updateControllersInfo kind p
        
        let model = { model with controllerInfos = newControllersPosition}

        let newRay = FastRay3d(model.teleportRay)
        
        let checkIntersection = 
            OpcViewer.Base.Picking.Intersect.intersectWithOpc (Some model.kdTree) newRay
        
        let intersectionVector = 
            match checkIntersection with 
            | Some intersection -> model.teleportRay.GetPointOnRay(intersection)
            | None -> V3d.Zero

        let moveBox = 
            model.teleportBox
            |> PList.map (fun tBox -> 
                let newTrafo = Trafo3d.Translation(intersectionVector) 

                {tBox with trafo = newTrafo; geometry = Box3d.FromSize(V3d(5.0, 1.0, 1.0)); color = C4b.Red}
            )

        let moveCone = 
            let getHmd  = model.controllerInfos |> HMap.tryFind ControllerKind.HMD
            match getHmd with 
            | Some hmd -> 
                model.teleportCone
                |> PList.map (fun cone -> 
                    let newVectorTransform = V3d(intersectionVector.X + 5.0, intersectionVector.Y, intersectionVector.Z)
                    let getRotation = 
                        let newLmkC = hmd.pose.deviceToWorld * model.opcSpaceTrafo.Inverse
                        let rtLmkC = newLmkC.GetOrthoNormalOrientation()
                        let rotLmkC = Rot3d.FromFrame(rtLmkC.Forward.C0.XYZ, rtLmkC.Forward.C1.XYZ, rtLmkC.Forward.C2.XYZ)
                        rotLmkC.GetEulerAngles()
                    let newTrafo = Trafo3d.FromComponents(V3d.One, getRotation, newVectorTransform)

                    {cone with trafo = newTrafo; color = C4b.Red; geometry = Cone.solidCone (cone.trafo.GetModelOrigin()) V3d.Zero 500.0 500.0 20 C4b.Red}
                )
            | None -> model.teleportCone

        let model = {model with teleportBox = moveBox; teleportCone = moveCone}

        model 

