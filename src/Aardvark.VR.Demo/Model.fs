namespace Demo.Main

open Aardvark.Vr
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Aardvark.SceneGraph.Opc
open OpcViewer.Base.Picking
open OpcViewer.Base.Attributes
open Aardvark.Rendering.Vulkan
open Demo.Menu
open Demo
open Aardvark.VRVis.Opc

type State =
    | Pressed
    | Released
    | Hold

type ButtonStates = {
    front : State
    back : State
}

[<DomainType>]
type Polygon = 
    { 
        vertices : V3d[]//plist<V3d>
    }

[<DomainType>]
type Drone = 
    {
        drone           : plist<VisibleBox>
        screen          : plist<VisibleBox>
        droneCamera     : CameraView
        cameraPosition  : Trafo3d
        screenPosition  : Trafo3d
        initControlTrafo : Trafo3d
        initCameraPosition : Trafo3d
    }
module Drone = 
    let initial = 
        {
            drone           = PList.empty
            screen          = PList.empty
            droneCamera     = CameraView.lookAt (V3d.III * 3.0) V3d.Zero V3d.OOI
            cameraPosition  = Trafo3d.Identity
            screenPosition  = Trafo3d.Identity
            initControlTrafo = Trafo3d.Identity
            initCameraPosition = Trafo3d.Identity
        }


[<DomainType>]
type StringInfo = 
    {
        text    : string
        trafo   : Trafo3d
    }
module StringInfo = 
    let initial = 
        {
            trafo = Trafo3d.Translation(V3d.One * 500000.0)
            text = ""
        }

[<DomainType>]
type Model =
    {
        text                : string
        vr                  : bool                
        
        cameraState         : CameraControllerState

        ControllerPosition  : V3d
        offsetToCenter      : V3d
        
        controllerInfos          : hmap<ControllerKind, ControllerInfo> // CURRENT CONTROLLER INFO!
        offsetControllerDistance : float

        //OPC model part
        [<NonIncremental>]
        patchHierarchies            : list<PatchHierarchy> 
        boundingBox                 : Box3d
        rotationAxis                : Trafo3d
        opcInfos                    : hmap<Box3d, OpcData>
        opcAttributes               : AttributeModel
        mainFrustum                 : Frustum
        rotateBox                   : bool
        pickingModel                : PickingModel

        [<NonIncremental>]
        kdTree                      : hmap<Box3d, KdTrees.Level0KdTree>


        initWorkSpaceTrafo          : Trafo3d // describes all accumulated drag and rotation actions by the user (how the opc and everything is moved)
                                              // END -> update initWorkSpaceTrafo by workSpaceTrafo for the next iteration...
        workSpaceTrafo              : Trafo3d // START and MOVE -> initWorkSpaceTrafo * controller-DELTA
        opcSpaceTrafo               : Trafo3d // description of how the opc is moved from 25k to our worldspace-origin (controller space)
        annotationSpaceTrafo        : Trafo3d // (identity) lives at the origin...but later for accuracy reasons...model trafo like opcSpace...
        initOpcSpaceTrafo           : Trafo3d // STATIC opcSpace at Start-CLICK
        initAnnotationSpaceTrafo    : Trafo3d // STATIC annotationSpace at Start-CLICK
        
        initControlTrafo            : Trafo3d // START Controller1 (temporal!)
        init2ControlTrafo           : Trafo3d // START Controller2 (temporal!)

        menuModel                   : MenuModel

        // tranportation additions
        landmarkOnController        : plist<VisibleBox>
        landmarkOnAnnotationSpace   : plist<VisibleBox>
        //landmarkFromStart           : plist<VisibleBox>
        WIMopcSpaceTrafo            : Trafo3d
        WIMworkSpaceTrafo           : Trafo3d
        WIMannotationSpaceTrafo     : Trafo3d
        WIMopcInfos                 : hmap<Box3d, OpcData>
        WIMlandmarkOnController     : plist<VisibleBox>
        WIMlandmarkOnAnnotationSpace: plist<VisibleBox>
        WIMuserPos                  : plist<VisibleBox>
        WIMuserPosCone              : plist<VisibleCone>
        WIMinitialUserPos           : plist<VisibleBox>
        WIMinitialUserPosCone       : plist<VisibleCone>
        userPosOnAnnotationSpace    : plist<VisibleBox>
        cyllinderControl            : plist<VisibleCylinder>

        evaluationLandmarks         : plist<VisibleBox>
        evaluationLandmarksWIM      : plist<VisibleBox>
        evaluationLandmarksWIM2RealWorld : plist<VisibleBox>

        evaluationLandmarksLook     : plist<VisibleBox> 
        evaluationLandmarksWIMLook  : plist<VisibleBox>
        evaluationLandmarksWIM2RealWorldLook : plist<VisibleBox>

        evaluationCounter           : int

        droneControl                : Drone
        droneDistanceToLandmark     : StringInfo
        droneHeight                 : StringInfo
        
        teleportRay                 : Ray3d
        teleportBox                 : plist<VisibleBox> 
        teleportCone                : plist<VisibleCone>


        
    }

module Model =
    open Aardvark.VRVis.Opc

    let initial = 

        let rotateBoxInit = false
        
        let marcPath = @"C:\Users\lopez\Desktop\VictoriaCrater\HiRISE_VictoriaCrater_SuperResolution"
        let publishPath = @"..\data"
        let path = 
            if System.IO.Directory.Exists publishPath then publishPath
            elif System.IO.Directory.Exists marcPath then marcPath
            else failwithf "could not find data dir. current directory i: %s" System.Environment.CurrentDirectory
        
        let patchHierarchiesInit = 
            OpcViewerFunc.patchHierarchiesImport path //"..\bin\data" //"C:\Users\lopez\Desktop\VictoriaCrater\HiRISE_VictoriaCrater_SuperResolution"

        let boundingBoxInit = 
            OpcViewerFunc.boxImport (patchHierarchiesInit)

        let opcInfosInit = 
            OpcViewerFunc.opcInfosImport (patchHierarchiesInit)

        let up =
            OpcViewerFunc.getUpVector boundingBoxInit rotateBoxInit

        let upRotationTrafo = 
            Trafo3d.RotateInto(boundingBoxInit.Center.Normalized, V3d.OOI)

        let cameraStateInit = 
            OpcViewerFunc.restoreCamStateImport boundingBoxInit V3d.OOI

        let startOpcTrafo = Trafo3d.FromBasis(V3d(0.0138907544072255, 0.0370928394273679, 0.410690910035505), V3d(0.11636514267386, 0.393870197365478, -0.0395094556451799), V3d(-0.395603213079913, 0.117157783795495, 0.0027988969790869), V3d(-57141.4217354136, 16979.9987604353, -1399135.09579421))
        
        let loadKdTrees = 
            let kdTreesPerHierarchy =
                [| 
                    for h in patchHierarchiesInit do
                        if true then   
                            yield KdTrees.loadKdTrees h Trafo3d.Identity ViewerModality.XYZ SerializationOpc.binarySerializer                    
                        else 
                            yield HMap.empty
                |]

            let totalKdTrees = kdTreesPerHierarchy.Length
            Log.line "creating %d kdTrees" totalKdTrees

            let kdTrees = 
                kdTreesPerHierarchy                     
                |> Array.Parallel.mapi (fun i e ->
                    Log.start "creating kdtree #%d of %d" i totalKdTrees
                    let r = e
                    Log.stop()
                    r
                )
                |> Array.fold (fun a b -> HMap.union a b) HMap.empty
            kdTrees
        
        {
            text                = "some text"
            vr                  = false

            ControllerPosition      = V3d.OOO
            controllerInfos         = HMap.empty
            offsetToCenter          = V3d.One
            cameraState             = cameraStateInit
            
            patchHierarchies    = patchHierarchiesInit
            boundingBox         = boundingBoxInit
            opcInfos            = opcInfosInit
            opcAttributes       = SurfaceAttributes.initModel path //"..\bin\data" //"C:\Users\lopez\Desktop\VictoriaCrater\HiRISE_VictoriaCrater_SuperResolution"
            mainFrustum         = Frustum.perspective 60.0 0.01 1000.0 1.0
            rotateBox           = rotateBoxInit
            pickingModel        = OpcViewer.Base.Picking.PickingModel.initial

            kdTree              = loadKdTrees

            offsetControllerDistance    = 1.0

            opcSpaceTrafo               = startOpcTrafo//Trafo3d.Translation(-boundingBoxInit.Center) * upRotationTrafo
            workSpaceTrafo              = Trafo3d.Identity
            annotationSpaceTrafo        = Trafo3d.Identity

            initOpcSpaceTrafo           = startOpcTrafo//Trafo3d.Translation(-boundingBoxInit.Center) * upRotationTrafo
            initWorkSpaceTrafo          = Trafo3d.Identity
            initAnnotationSpaceTrafo    = Trafo3d.Identity

            initControlTrafo            = Trafo3d.Identity
            init2ControlTrafo           = Trafo3d.Identity
            rotationAxis                = Trafo3d.Identity

            menuModel                   = Menu.MenuModel.init
            
            // Navigation parameters
            landmarkOnController        = PList.empty
            landmarkOnAnnotationSpace   = PList.empty
            //landmarkFromStart           = PList.empty

            WIMopcSpaceTrafo            = Trafo3d.Translation(V3d(1000000.0, 1000000.0, 1000000.0)) * upRotationTrafo
            WIMworkSpaceTrafo           = Trafo3d.Identity
            WIMannotationSpaceTrafo     = Trafo3d.Identity
            
            WIMopcInfos                 = opcInfosInit
            WIMlandmarkOnController     = PList.empty
            WIMlandmarkOnAnnotationSpace= PList.empty
            WIMuserPos                  = PList.empty
            WIMuserPosCone              = PList.empty
            WIMinitialUserPos           = PList.empty
            WIMinitialUserPosCone       = PList.empty
            userPosOnAnnotationSpace    = PList.empty
            cyllinderControl            = PList.empty


            evaluationLandmarks         = PList.empty
            evaluationLandmarksWIM      = PList.empty
            evaluationLandmarksWIM2RealWorld= PList.empty
            
            evaluationLandmarksLook     = PList.empty 
            evaluationLandmarksWIMLook  = PList.empty
            evaluationLandmarksWIM2RealWorldLook = PList.empty
            
            evaluationCounter           = 0

            droneControl                = Drone.initial
            droneDistanceToLandmark     = StringInfo.initial
            droneHeight                 = StringInfo.initial
            
            teleportRay                 = Ray3d.Invalid
            teleportBox                 = PList.empty
            teleportCone                = PList.empty

        }

    let initMainReset = 
        {
            initial with 
                opcSpaceTrafo                = Trafo3d.Translation -initial.boundingBox.Center * Trafo3d.RotateInto(initial.boundingBox.Center.Normalized, V3d.OOI) 
                annotationSpaceTrafo         = Trafo3d.Identity
                workSpaceTrafo               = Trafo3d.Identity
        }