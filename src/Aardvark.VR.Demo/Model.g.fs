namespace Demo.Main

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Demo.Main

[<AutoOpen>]
module Mutable =

    
    
    type MPolygon(__initial : Demo.Main.Polygon) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Demo.Main.Polygon> = Aardvark.Base.Incremental.EqModRef<Demo.Main.Polygon>(__initial) :> Aardvark.Base.Incremental.IModRef<Demo.Main.Polygon>
        let _vertices = ResetMod.Create(__initial.vertices)
        
        member x.vertices = _vertices :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Demo.Main.Polygon) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_vertices,v.vertices)
                
        
        static member Create(__initial : Demo.Main.Polygon) : MPolygon = MPolygon(__initial)
        static member Update(m : MPolygon, v : Demo.Main.Polygon) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Demo.Main.Polygon> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Polygon =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let vertices =
                { new Lens<Demo.Main.Polygon, Aardvark.Base.V3d[]>() with
                    override x.Get(r) = r.vertices
                    override x.Set(r,v) = { r with vertices = v }
                    override x.Update(r,f) = { r with vertices = f r.vertices }
                }
    
    
    type MDrone(__initial : Demo.Main.Drone) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Demo.Main.Drone> = Aardvark.Base.Incremental.EqModRef<Demo.Main.Drone>(__initial) :> Aardvark.Base.Incremental.IModRef<Demo.Main.Drone>
        let _drone = MList.Create(__initial.drone, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _screen = MList.Create(__initial.screen, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _droneCamera = ResetMod.Create(__initial.droneCamera)
        let _cameraPosition = ResetMod.Create(__initial.cameraPosition)
        let _screenPosition = ResetMod.Create(__initial.screenPosition)
        let _initControlTrafo = ResetMod.Create(__initial.initControlTrafo)
        let _initCameraPosition = ResetMod.Create(__initial.initCameraPosition)
        
        member x.drone = _drone :> alist<_>
        member x.screen = _screen :> alist<_>
        member x.droneCamera = _droneCamera :> IMod<_>
        member x.cameraPosition = _cameraPosition :> IMod<_>
        member x.screenPosition = _screenPosition :> IMod<_>
        member x.initControlTrafo = _initControlTrafo :> IMod<_>
        member x.initCameraPosition = _initCameraPosition :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Demo.Main.Drone) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                MList.Update(_drone, v.drone)
                MList.Update(_screen, v.screen)
                ResetMod.Update(_droneCamera,v.droneCamera)
                ResetMod.Update(_cameraPosition,v.cameraPosition)
                ResetMod.Update(_screenPosition,v.screenPosition)
                ResetMod.Update(_initControlTrafo,v.initControlTrafo)
                ResetMod.Update(_initCameraPosition,v.initCameraPosition)
                
        
        static member Create(__initial : Demo.Main.Drone) : MDrone = MDrone(__initial)
        static member Update(m : MDrone, v : Demo.Main.Drone) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Demo.Main.Drone> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Drone =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let drone =
                { new Lens<Demo.Main.Drone, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.drone
                    override x.Set(r,v) = { r with drone = v }
                    override x.Update(r,f) = { r with drone = f r.drone }
                }
            let screen =
                { new Lens<Demo.Main.Drone, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.screen
                    override x.Set(r,v) = { r with screen = v }
                    override x.Update(r,f) = { r with screen = f r.screen }
                }
            let droneCamera =
                { new Lens<Demo.Main.Drone, Aardvark.Base.CameraView>() with
                    override x.Get(r) = r.droneCamera
                    override x.Set(r,v) = { r with droneCamera = v }
                    override x.Update(r,f) = { r with droneCamera = f r.droneCamera }
                }
            let cameraPosition =
                { new Lens<Demo.Main.Drone, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.cameraPosition
                    override x.Set(r,v) = { r with cameraPosition = v }
                    override x.Update(r,f) = { r with cameraPosition = f r.cameraPosition }
                }
            let screenPosition =
                { new Lens<Demo.Main.Drone, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.screenPosition
                    override x.Set(r,v) = { r with screenPosition = v }
                    override x.Update(r,f) = { r with screenPosition = f r.screenPosition }
                }
            let initControlTrafo =
                { new Lens<Demo.Main.Drone, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.initControlTrafo
                    override x.Set(r,v) = { r with initControlTrafo = v }
                    override x.Update(r,f) = { r with initControlTrafo = f r.initControlTrafo }
                }
            let initCameraPosition =
                { new Lens<Demo.Main.Drone, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.initCameraPosition
                    override x.Set(r,v) = { r with initCameraPosition = v }
                    override x.Update(r,f) = { r with initCameraPosition = f r.initCameraPosition }
                }
    
    
    type MCompass(__initial : Demo.Main.Compass) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Demo.Main.Compass> = Aardvark.Base.Incremental.EqModRef<Demo.Main.Compass>(__initial) :> Aardvark.Base.Incremental.IModRef<Demo.Main.Compass>
        let _trafo = ResetMod.Create(__initial.trafo)
        let _text = ResetMod.Create(__initial.text)
        
        member x.trafo = _trafo :> IMod<_>
        member x.text = _text :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Demo.Main.Compass) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_trafo,v.trafo)
                ResetMod.Update(_text,v.text)
                
        
        static member Create(__initial : Demo.Main.Compass) : MCompass = MCompass(__initial)
        static member Update(m : MCompass, v : Demo.Main.Compass) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Demo.Main.Compass> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Compass =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let trafo =
                { new Lens<Demo.Main.Compass, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.trafo
                    override x.Set(r,v) = { r with trafo = v }
                    override x.Update(r,f) = { r with trafo = f r.trafo }
                }
            let text =
                { new Lens<Demo.Main.Compass, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
    
    
    type MStringInfo(__initial : Demo.Main.StringInfo) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Demo.Main.StringInfo> = Aardvark.Base.Incremental.EqModRef<Demo.Main.StringInfo>(__initial) :> Aardvark.Base.Incremental.IModRef<Demo.Main.StringInfo>
        let _text = ResetMod.Create(__initial.text)
        let _trafo = ResetMod.Create(__initial.trafo)
        
        member x.text = _text :> IMod<_>
        member x.trafo = _trafo :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Demo.Main.StringInfo) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_text,v.text)
                ResetMod.Update(_trafo,v.trafo)
                
        
        static member Create(__initial : Demo.Main.StringInfo) : MStringInfo = MStringInfo(__initial)
        static member Update(m : MStringInfo, v : Demo.Main.StringInfo) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Demo.Main.StringInfo> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module StringInfo =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let text =
                { new Lens<Demo.Main.StringInfo, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let trafo =
                { new Lens<Demo.Main.StringInfo, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.trafo
                    override x.Set(r,v) = { r with trafo = v }
                    override x.Update(r,f) = { r with trafo = f r.trafo }
                }
    
    
    type MModel(__initial : Demo.Main.Model) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<Demo.Main.Model> = Aardvark.Base.Incremental.EqModRef<Demo.Main.Model>(__initial) :> Aardvark.Base.Incremental.IModRef<Demo.Main.Model>
        let _text = ResetMod.Create(__initial.text)
        let _vr = ResetMod.Create(__initial.vr)
        let _cameraState = Aardvark.UI.Primitives.Mutable.MCameraControllerState.Create(__initial.cameraState)
        let _ControllerPosition = ResetMod.Create(__initial.ControllerPosition)
        let _offsetToCenter = ResetMod.Create(__initial.offsetToCenter)
        let _controllerInfos = MMap.Create(__initial.controllerInfos, (fun v -> Demo.Mutable.MControllerInfo.Create(v)), (fun (m,v) -> Demo.Mutable.MControllerInfo.Update(m, v)), (fun v -> v))
        let _offsetControllerDistance = ResetMod.Create(__initial.offsetControllerDistance)
        let _boundingBox = ResetMod.Create(__initial.boundingBox)
        let _rotationAxis = ResetMod.Create(__initial.rotationAxis)
        let _opcInfos = MMap.Create(__initial.opcInfos, (fun v -> OpcViewer.Base.Picking.Mutable.MOpcData.Create(v)), (fun (m,v) -> OpcViewer.Base.Picking.Mutable.MOpcData.Update(m, v)), (fun v -> v))
        let _opcAttributes = OpcViewer.Base.Attributes.Mutable.MAttributeModel.Create(__initial.opcAttributes)
        let _mainFrustum = ResetMod.Create(__initial.mainFrustum)
        let _rotateBox = ResetMod.Create(__initial.rotateBox)
        let _pickingModel = OpcViewer.Base.Picking.Mutable.MPickingModel.Create(__initial.pickingModel)
        let _initWorkSpaceTrafo = ResetMod.Create(__initial.initWorkSpaceTrafo)
        let _workSpaceTrafo = ResetMod.Create(__initial.workSpaceTrafo)
        let _opcSpaceTrafo = ResetMod.Create(__initial.opcSpaceTrafo)
        let _annotationSpaceTrafo = ResetMod.Create(__initial.annotationSpaceTrafo)
        let _initOpcSpaceTrafo = ResetMod.Create(__initial.initOpcSpaceTrafo)
        let _initAnnotationSpaceTrafo = ResetMod.Create(__initial.initAnnotationSpaceTrafo)
        let _initControlTrafo = ResetMod.Create(__initial.initControlTrafo)
        let _init2ControlTrafo = ResetMod.Create(__initial.init2ControlTrafo)
        let _menuModel = Demo.Menu.Mutable.MMenuModel.Create(__initial.menuModel)
        let _landmarkOnController = MList.Create(__initial.landmarkOnController, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _landmarkOnAnnotationSpace = MList.Create(__initial.landmarkOnAnnotationSpace, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _WIMopcSpaceTrafo = ResetMod.Create(__initial.WIMopcSpaceTrafo)
        let _WIMworkSpaceTrafo = ResetMod.Create(__initial.WIMworkSpaceTrafo)
        let _WIMannotationSpaceTrafo = ResetMod.Create(__initial.WIMannotationSpaceTrafo)
        let _WIMopcInfos = MMap.Create(__initial.WIMopcInfos, (fun v -> OpcViewer.Base.Picking.Mutable.MOpcData.Create(v)), (fun (m,v) -> OpcViewer.Base.Picking.Mutable.MOpcData.Update(m, v)), (fun v -> v))
        let _WIMlandmarkOnController = MList.Create(__initial.WIMlandmarkOnController, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _WIMlandmarkOnAnnotationSpace = MList.Create(__initial.WIMlandmarkOnAnnotationSpace, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _WIMuserPos = MList.Create(__initial.WIMuserPos, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _WIMuserPosCone = MList.Create(__initial.WIMuserPosCone, (fun v -> Demo.Mutable.MVisibleCone.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleCone.Update(m, v)), (fun v -> v))
        let _WIMinitialUserPos = MList.Create(__initial.WIMinitialUserPos, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _WIMinitialUserPosCone = MList.Create(__initial.WIMinitialUserPosCone, (fun v -> Demo.Mutable.MVisibleCone.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleCone.Update(m, v)), (fun v -> v))
        let _userPosOnAnnotationSpace = MList.Create(__initial.userPosOnAnnotationSpace, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _teleportRay = ResetMod.Create(__initial.teleportRay)
        let _droneControl = MDrone.Create(__initial.droneControl)
        let _cyllinderControl = MList.Create(__initial.cyllinderControl, (fun v -> Demo.Mutable.MVisibleCylinder.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleCylinder.Update(m, v)), (fun v -> v))
        let _totalCompass = MList.Create(__initial.totalCompass, (fun v -> MCompass.Create(v)), (fun (m,v) -> MCompass.Update(m, v)), (fun v -> v))
        let _evaluationLandmarks = MList.Create(__initial.evaluationLandmarks, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _evaluationLandmarksWIM = MList.Create(__initial.evaluationLandmarksWIM, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _evaluationLandmarksWIM2RealWorld = MList.Create(__initial.evaluationLandmarksWIM2RealWorld, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        let _evaluationCounter = ResetMod.Create(__initial.evaluationCounter)
        let _droneDistanceToLandmark = MStringInfo.Create(__initial.droneDistanceToLandmark)
        let _droneHeight = MStringInfo.Create(__initial.droneHeight)
        let _teleportBox = MList.Create(__initial.teleportBox, (fun v -> Demo.Mutable.MVisibleBox.Create(v)), (fun (m,v) -> Demo.Mutable.MVisibleBox.Update(m, v)), (fun v -> v))
        
        member x.text = _text :> IMod<_>
        member x.vr = _vr :> IMod<_>
        member x.cameraState = _cameraState
        member x.ControllerPosition = _ControllerPosition :> IMod<_>
        member x.offsetToCenter = _offsetToCenter :> IMod<_>
        member x.controllerInfos = _controllerInfos :> amap<_,_>
        member x.offsetControllerDistance = _offsetControllerDistance :> IMod<_>
        member x.patchHierarchies = __current.Value.patchHierarchies
        member x.boundingBox = _boundingBox :> IMod<_>
        member x.rotationAxis = _rotationAxis :> IMod<_>
        member x.opcInfos = _opcInfos :> amap<_,_>
        member x.opcAttributes = _opcAttributes
        member x.mainFrustum = _mainFrustum :> IMod<_>
        member x.rotateBox = _rotateBox :> IMod<_>
        member x.pickingModel = _pickingModel
        member x.initWorkSpaceTrafo = _initWorkSpaceTrafo :> IMod<_>
        member x.workSpaceTrafo = _workSpaceTrafo :> IMod<_>
        member x.opcSpaceTrafo = _opcSpaceTrafo :> IMod<_>
        member x.annotationSpaceTrafo = _annotationSpaceTrafo :> IMod<_>
        member x.initOpcSpaceTrafo = _initOpcSpaceTrafo :> IMod<_>
        member x.initAnnotationSpaceTrafo = _initAnnotationSpaceTrafo :> IMod<_>
        member x.initControlTrafo = _initControlTrafo :> IMod<_>
        member x.init2ControlTrafo = _init2ControlTrafo :> IMod<_>
        member x.menuModel = _menuModel
        member x.landmarkOnController = _landmarkOnController :> alist<_>
        member x.landmarkOnAnnotationSpace = _landmarkOnAnnotationSpace :> alist<_>
        member x.WIMopcSpaceTrafo = _WIMopcSpaceTrafo :> IMod<_>
        member x.WIMworkSpaceTrafo = _WIMworkSpaceTrafo :> IMod<_>
        member x.WIMannotationSpaceTrafo = _WIMannotationSpaceTrafo :> IMod<_>
        member x.WIMopcInfos = _WIMopcInfos :> amap<_,_>
        member x.WIMlandmarkOnController = _WIMlandmarkOnController :> alist<_>
        member x.WIMlandmarkOnAnnotationSpace = _WIMlandmarkOnAnnotationSpace :> alist<_>
        member x.WIMuserPos = _WIMuserPos :> alist<_>
        member x.WIMuserPosCone = _WIMuserPosCone :> alist<_>
        member x.WIMinitialUserPos = _WIMinitialUserPos :> alist<_>
        member x.WIMinitialUserPosCone = _WIMinitialUserPosCone :> alist<_>
        member x.userPosOnAnnotationSpace = _userPosOnAnnotationSpace :> alist<_>
        member x.teleportRay = _teleportRay :> IMod<_>
        member x.droneControl = _droneControl
        member x.cyllinderControl = _cyllinderControl :> alist<_>
        member x.totalCompass = _totalCompass :> alist<_>
        member x.evaluationLandmarks = _evaluationLandmarks :> alist<_>
        member x.evaluationLandmarksWIM = _evaluationLandmarksWIM :> alist<_>
        member x.evaluationLandmarksWIM2RealWorld = _evaluationLandmarksWIM2RealWorld :> alist<_>
        member x.evaluationCounter = _evaluationCounter :> IMod<_>
        member x.droneDistanceToLandmark = _droneDistanceToLandmark
        member x.droneHeight = _droneHeight
        member x.teleportBox = _teleportBox :> alist<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : Demo.Main.Model) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_text,v.text)
                ResetMod.Update(_vr,v.vr)
                Aardvark.UI.Primitives.Mutable.MCameraControllerState.Update(_cameraState, v.cameraState)
                ResetMod.Update(_ControllerPosition,v.ControllerPosition)
                ResetMod.Update(_offsetToCenter,v.offsetToCenter)
                MMap.Update(_controllerInfos, v.controllerInfos)
                ResetMod.Update(_offsetControllerDistance,v.offsetControllerDistance)
                ResetMod.Update(_boundingBox,v.boundingBox)
                ResetMod.Update(_rotationAxis,v.rotationAxis)
                MMap.Update(_opcInfos, v.opcInfos)
                OpcViewer.Base.Attributes.Mutable.MAttributeModel.Update(_opcAttributes, v.opcAttributes)
                ResetMod.Update(_mainFrustum,v.mainFrustum)
                ResetMod.Update(_rotateBox,v.rotateBox)
                OpcViewer.Base.Picking.Mutable.MPickingModel.Update(_pickingModel, v.pickingModel)
                ResetMod.Update(_initWorkSpaceTrafo,v.initWorkSpaceTrafo)
                ResetMod.Update(_workSpaceTrafo,v.workSpaceTrafo)
                ResetMod.Update(_opcSpaceTrafo,v.opcSpaceTrafo)
                ResetMod.Update(_annotationSpaceTrafo,v.annotationSpaceTrafo)
                ResetMod.Update(_initOpcSpaceTrafo,v.initOpcSpaceTrafo)
                ResetMod.Update(_initAnnotationSpaceTrafo,v.initAnnotationSpaceTrafo)
                ResetMod.Update(_initControlTrafo,v.initControlTrafo)
                ResetMod.Update(_init2ControlTrafo,v.init2ControlTrafo)
                Demo.Menu.Mutable.MMenuModel.Update(_menuModel, v.menuModel)
                MList.Update(_landmarkOnController, v.landmarkOnController)
                MList.Update(_landmarkOnAnnotationSpace, v.landmarkOnAnnotationSpace)
                ResetMod.Update(_WIMopcSpaceTrafo,v.WIMopcSpaceTrafo)
                ResetMod.Update(_WIMworkSpaceTrafo,v.WIMworkSpaceTrafo)
                ResetMod.Update(_WIMannotationSpaceTrafo,v.WIMannotationSpaceTrafo)
                MMap.Update(_WIMopcInfos, v.WIMopcInfos)
                MList.Update(_WIMlandmarkOnController, v.WIMlandmarkOnController)
                MList.Update(_WIMlandmarkOnAnnotationSpace, v.WIMlandmarkOnAnnotationSpace)
                MList.Update(_WIMuserPos, v.WIMuserPos)
                MList.Update(_WIMuserPosCone, v.WIMuserPosCone)
                MList.Update(_WIMinitialUserPos, v.WIMinitialUserPos)
                MList.Update(_WIMinitialUserPosCone, v.WIMinitialUserPosCone)
                MList.Update(_userPosOnAnnotationSpace, v.userPosOnAnnotationSpace)
                ResetMod.Update(_teleportRay,v.teleportRay)
                MDrone.Update(_droneControl, v.droneControl)
                MList.Update(_cyllinderControl, v.cyllinderControl)
                MList.Update(_totalCompass, v.totalCompass)
                MList.Update(_evaluationLandmarks, v.evaluationLandmarks)
                MList.Update(_evaluationLandmarksWIM, v.evaluationLandmarksWIM)
                MList.Update(_evaluationLandmarksWIM2RealWorld, v.evaluationLandmarksWIM2RealWorld)
                ResetMod.Update(_evaluationCounter,v.evaluationCounter)
                MStringInfo.Update(_droneDistanceToLandmark, v.droneDistanceToLandmark)
                MStringInfo.Update(_droneHeight, v.droneHeight)
                MList.Update(_teleportBox, v.teleportBox)
                
        
        static member Create(__initial : Demo.Main.Model) : MModel = MModel(__initial)
        static member Update(m : MModel, v : Demo.Main.Model) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<Demo.Main.Model> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Model =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let text =
                { new Lens<Demo.Main.Model, System.String>() with
                    override x.Get(r) = r.text
                    override x.Set(r,v) = { r with text = v }
                    override x.Update(r,f) = { r with text = f r.text }
                }
            let vr =
                { new Lens<Demo.Main.Model, System.Boolean>() with
                    override x.Get(r) = r.vr
                    override x.Set(r,v) = { r with vr = v }
                    override x.Update(r,f) = { r with vr = f r.vr }
                }
            let cameraState =
                { new Lens<Demo.Main.Model, Aardvark.UI.Primitives.CameraControllerState>() with
                    override x.Get(r) = r.cameraState
                    override x.Set(r,v) = { r with cameraState = v }
                    override x.Update(r,f) = { r with cameraState = f r.cameraState }
                }
            let ControllerPosition =
                { new Lens<Demo.Main.Model, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.ControllerPosition
                    override x.Set(r,v) = { r with ControllerPosition = v }
                    override x.Update(r,f) = { r with ControllerPosition = f r.ControllerPosition }
                }
            let offsetToCenter =
                { new Lens<Demo.Main.Model, Aardvark.Base.V3d>() with
                    override x.Get(r) = r.offsetToCenter
                    override x.Set(r,v) = { r with offsetToCenter = v }
                    override x.Update(r,f) = { r with offsetToCenter = f r.offsetToCenter }
                }
            let controllerInfos =
                { new Lens<Demo.Main.Model, Aardvark.Base.hmap<Demo.ControllerKind,Demo.ControllerInfo>>() with
                    override x.Get(r) = r.controllerInfos
                    override x.Set(r,v) = { r with controllerInfos = v }
                    override x.Update(r,f) = { r with controllerInfos = f r.controllerInfos }
                }
            let offsetControllerDistance =
                { new Lens<Demo.Main.Model, System.Double>() with
                    override x.Get(r) = r.offsetControllerDistance
                    override x.Set(r,v) = { r with offsetControllerDistance = v }
                    override x.Update(r,f) = { r with offsetControllerDistance = f r.offsetControllerDistance }
                }
            let patchHierarchies =
                { new Lens<Demo.Main.Model, Microsoft.FSharp.Collections.List<Aardvark.SceneGraph.Opc.PatchHierarchy>>() with
                    override x.Get(r) = r.patchHierarchies
                    override x.Set(r,v) = { r with patchHierarchies = v }
                    override x.Update(r,f) = { r with patchHierarchies = f r.patchHierarchies }
                }
            let boundingBox =
                { new Lens<Demo.Main.Model, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.boundingBox
                    override x.Set(r,v) = { r with boundingBox = v }
                    override x.Update(r,f) = { r with boundingBox = f r.boundingBox }
                }
            let rotationAxis =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.rotationAxis
                    override x.Set(r,v) = { r with rotationAxis = v }
                    override x.Update(r,f) = { r with rotationAxis = f r.rotationAxis }
                }
            let opcInfos =
                { new Lens<Demo.Main.Model, Aardvark.Base.hmap<Aardvark.Base.Box3d,OpcViewer.Base.Picking.OpcData>>() with
                    override x.Get(r) = r.opcInfos
                    override x.Set(r,v) = { r with opcInfos = v }
                    override x.Update(r,f) = { r with opcInfos = f r.opcInfos }
                }
            let opcAttributes =
                { new Lens<Demo.Main.Model, OpcViewer.Base.Attributes.AttributeModel>() with
                    override x.Get(r) = r.opcAttributes
                    override x.Set(r,v) = { r with opcAttributes = v }
                    override x.Update(r,f) = { r with opcAttributes = f r.opcAttributes }
                }
            let mainFrustum =
                { new Lens<Demo.Main.Model, Aardvark.Base.Frustum>() with
                    override x.Get(r) = r.mainFrustum
                    override x.Set(r,v) = { r with mainFrustum = v }
                    override x.Update(r,f) = { r with mainFrustum = f r.mainFrustum }
                }
            let rotateBox =
                { new Lens<Demo.Main.Model, System.Boolean>() with
                    override x.Get(r) = r.rotateBox
                    override x.Set(r,v) = { r with rotateBox = v }
                    override x.Update(r,f) = { r with rotateBox = f r.rotateBox }
                }
            let pickingModel =
                { new Lens<Demo.Main.Model, OpcViewer.Base.Picking.PickingModel>() with
                    override x.Get(r) = r.pickingModel
                    override x.Set(r,v) = { r with pickingModel = v }
                    override x.Update(r,f) = { r with pickingModel = f r.pickingModel }
                }
            let initWorkSpaceTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.initWorkSpaceTrafo
                    override x.Set(r,v) = { r with initWorkSpaceTrafo = v }
                    override x.Update(r,f) = { r with initWorkSpaceTrafo = f r.initWorkSpaceTrafo }
                }
            let workSpaceTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.workSpaceTrafo
                    override x.Set(r,v) = { r with workSpaceTrafo = v }
                    override x.Update(r,f) = { r with workSpaceTrafo = f r.workSpaceTrafo }
                }
            let opcSpaceTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.opcSpaceTrafo
                    override x.Set(r,v) = { r with opcSpaceTrafo = v }
                    override x.Update(r,f) = { r with opcSpaceTrafo = f r.opcSpaceTrafo }
                }
            let annotationSpaceTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.annotationSpaceTrafo
                    override x.Set(r,v) = { r with annotationSpaceTrafo = v }
                    override x.Update(r,f) = { r with annotationSpaceTrafo = f r.annotationSpaceTrafo }
                }
            let initOpcSpaceTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.initOpcSpaceTrafo
                    override x.Set(r,v) = { r with initOpcSpaceTrafo = v }
                    override x.Update(r,f) = { r with initOpcSpaceTrafo = f r.initOpcSpaceTrafo }
                }
            let initAnnotationSpaceTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.initAnnotationSpaceTrafo
                    override x.Set(r,v) = { r with initAnnotationSpaceTrafo = v }
                    override x.Update(r,f) = { r with initAnnotationSpaceTrafo = f r.initAnnotationSpaceTrafo }
                }
            let initControlTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.initControlTrafo
                    override x.Set(r,v) = { r with initControlTrafo = v }
                    override x.Update(r,f) = { r with initControlTrafo = f r.initControlTrafo }
                }
            let init2ControlTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.init2ControlTrafo
                    override x.Set(r,v) = { r with init2ControlTrafo = v }
                    override x.Update(r,f) = { r with init2ControlTrafo = f r.init2ControlTrafo }
                }
            let menuModel =
                { new Lens<Demo.Main.Model, Demo.Menu.MenuModel>() with
                    override x.Get(r) = r.menuModel
                    override x.Set(r,v) = { r with menuModel = v }
                    override x.Update(r,f) = { r with menuModel = f r.menuModel }
                }
            let landmarkOnController =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.landmarkOnController
                    override x.Set(r,v) = { r with landmarkOnController = v }
                    override x.Update(r,f) = { r with landmarkOnController = f r.landmarkOnController }
                }
            let landmarkOnAnnotationSpace =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.landmarkOnAnnotationSpace
                    override x.Set(r,v) = { r with landmarkOnAnnotationSpace = v }
                    override x.Update(r,f) = { r with landmarkOnAnnotationSpace = f r.landmarkOnAnnotationSpace }
                }
            let WIMopcSpaceTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.WIMopcSpaceTrafo
                    override x.Set(r,v) = { r with WIMopcSpaceTrafo = v }
                    override x.Update(r,f) = { r with WIMopcSpaceTrafo = f r.WIMopcSpaceTrafo }
                }
            let WIMworkSpaceTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.WIMworkSpaceTrafo
                    override x.Set(r,v) = { r with WIMworkSpaceTrafo = v }
                    override x.Update(r,f) = { r with WIMworkSpaceTrafo = f r.WIMworkSpaceTrafo }
                }
            let WIMannotationSpaceTrafo =
                { new Lens<Demo.Main.Model, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.WIMannotationSpaceTrafo
                    override x.Set(r,v) = { r with WIMannotationSpaceTrafo = v }
                    override x.Update(r,f) = { r with WIMannotationSpaceTrafo = f r.WIMannotationSpaceTrafo }
                }
            let WIMopcInfos =
                { new Lens<Demo.Main.Model, Aardvark.Base.hmap<Aardvark.Base.Box3d,OpcViewer.Base.Picking.OpcData>>() with
                    override x.Get(r) = r.WIMopcInfos
                    override x.Set(r,v) = { r with WIMopcInfos = v }
                    override x.Update(r,f) = { r with WIMopcInfos = f r.WIMopcInfos }
                }
            let WIMlandmarkOnController =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.WIMlandmarkOnController
                    override x.Set(r,v) = { r with WIMlandmarkOnController = v }
                    override x.Update(r,f) = { r with WIMlandmarkOnController = f r.WIMlandmarkOnController }
                }
            let WIMlandmarkOnAnnotationSpace =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.WIMlandmarkOnAnnotationSpace
                    override x.Set(r,v) = { r with WIMlandmarkOnAnnotationSpace = v }
                    override x.Update(r,f) = { r with WIMlandmarkOnAnnotationSpace = f r.WIMlandmarkOnAnnotationSpace }
                }
            let WIMuserPos =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.WIMuserPos
                    override x.Set(r,v) = { r with WIMuserPos = v }
                    override x.Update(r,f) = { r with WIMuserPos = f r.WIMuserPos }
                }
            let WIMuserPosCone =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleCone>>() with
                    override x.Get(r) = r.WIMuserPosCone
                    override x.Set(r,v) = { r with WIMuserPosCone = v }
                    override x.Update(r,f) = { r with WIMuserPosCone = f r.WIMuserPosCone }
                }
            let WIMinitialUserPos =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.WIMinitialUserPos
                    override x.Set(r,v) = { r with WIMinitialUserPos = v }
                    override x.Update(r,f) = { r with WIMinitialUserPos = f r.WIMinitialUserPos }
                }
            let WIMinitialUserPosCone =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleCone>>() with
                    override x.Get(r) = r.WIMinitialUserPosCone
                    override x.Set(r,v) = { r with WIMinitialUserPosCone = v }
                    override x.Update(r,f) = { r with WIMinitialUserPosCone = f r.WIMinitialUserPosCone }
                }
            let userPosOnAnnotationSpace =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.userPosOnAnnotationSpace
                    override x.Set(r,v) = { r with userPosOnAnnotationSpace = v }
                    override x.Update(r,f) = { r with userPosOnAnnotationSpace = f r.userPosOnAnnotationSpace }
                }
            let teleportRay =
                { new Lens<Demo.Main.Model, Aardvark.Base.Ray3d>() with
                    override x.Get(r) = r.teleportRay
                    override x.Set(r,v) = { r with teleportRay = v }
                    override x.Update(r,f) = { r with teleportRay = f r.teleportRay }
                }
            let droneControl =
                { new Lens<Demo.Main.Model, Demo.Main.Drone>() with
                    override x.Get(r) = r.droneControl
                    override x.Set(r,v) = { r with droneControl = v }
                    override x.Update(r,f) = { r with droneControl = f r.droneControl }
                }
            let cyllinderControl =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleCylinder>>() with
                    override x.Get(r) = r.cyllinderControl
                    override x.Set(r,v) = { r with cyllinderControl = v }
                    override x.Update(r,f) = { r with cyllinderControl = f r.cyllinderControl }
                }
            let totalCompass =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.Main.Compass>>() with
                    override x.Get(r) = r.totalCompass
                    override x.Set(r,v) = { r with totalCompass = v }
                    override x.Update(r,f) = { r with totalCompass = f r.totalCompass }
                }
            let evaluationLandmarks =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.evaluationLandmarks
                    override x.Set(r,v) = { r with evaluationLandmarks = v }
                    override x.Update(r,f) = { r with evaluationLandmarks = f r.evaluationLandmarks }
                }
            let evaluationLandmarksWIM =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.evaluationLandmarksWIM
                    override x.Set(r,v) = { r with evaluationLandmarksWIM = v }
                    override x.Update(r,f) = { r with evaluationLandmarksWIM = f r.evaluationLandmarksWIM }
                }
            let evaluationLandmarksWIM2RealWorld =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.evaluationLandmarksWIM2RealWorld
                    override x.Set(r,v) = { r with evaluationLandmarksWIM2RealWorld = v }
                    override x.Update(r,f) = { r with evaluationLandmarksWIM2RealWorld = f r.evaluationLandmarksWIM2RealWorld }
                }
            let evaluationCounter =
                { new Lens<Demo.Main.Model, System.Int32>() with
                    override x.Get(r) = r.evaluationCounter
                    override x.Set(r,v) = { r with evaluationCounter = v }
                    override x.Update(r,f) = { r with evaluationCounter = f r.evaluationCounter }
                }
            let droneDistanceToLandmark =
                { new Lens<Demo.Main.Model, Demo.Main.StringInfo>() with
                    override x.Get(r) = r.droneDistanceToLandmark
                    override x.Set(r,v) = { r with droneDistanceToLandmark = v }
                    override x.Update(r,f) = { r with droneDistanceToLandmark = f r.droneDistanceToLandmark }
                }
            let droneHeight =
                { new Lens<Demo.Main.Model, Demo.Main.StringInfo>() with
                    override x.Get(r) = r.droneHeight
                    override x.Set(r,v) = { r with droneHeight = v }
                    override x.Update(r,f) = { r with droneHeight = f r.droneHeight }
                }
            let teleportBox =
                { new Lens<Demo.Main.Model, Aardvark.Base.plist<Demo.VisibleBox>>() with
                    override x.Get(r) = r.teleportBox
                    override x.Set(r,v) = { r with teleportBox = v }
                    override x.Update(r,f) = { r with teleportBox = f r.teleportBox }
                }
