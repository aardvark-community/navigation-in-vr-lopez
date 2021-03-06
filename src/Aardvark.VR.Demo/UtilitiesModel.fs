namespace Demo

open Aardvark.Base.Incremental
open Aardvark.Vr
open Aardvark.Base
open Aardvark.Base.IndexedGeometryPrimitives


type ControllerKind =
| HMD = 0
| LightHouseA = 3
| LightHouseB = 4
| ControllerA = 1
| ControllerB = 2

module ControllerKind =
    let fromInt i =
        i |> enum<ControllerKind>
    let toInt (vrC : ControllerKind) =
        vrC |> int

type ControllerButtons = 
| Joystick  = 0
| Back      = 1
| Side      = 2
| Home      = 3

module ControllerButtons = 
    let fromInt i = 
        i |> enum<ControllerButtons>
    
    let toInt (button : ControllerButtons) = 
        button |> int
    


[<DomainType>]
type ControllerInfo = {
    kind              : ControllerKind
    buttonKind        : ControllerButtons
    pose              : Pose
    backButtonPressed : bool
    frontButtonPressed: bool
    joystickPressed   : bool
    sideButtonPressed : bool
    homeButtonPressed : bool
    joystickHold      : bool
}

module ControllerInfo = 
    let initial = 
        {
            kind                = ControllerKind.ControllerA
            buttonKind          = ControllerButtons.Joystick
            pose                = Pose.none
            backButtonPressed   = false
            frontButtonPressed  = false
            joystickPressed     = false
            sideButtonPressed   = false
            homeButtonPressed   = false
            joystickHold        = false
        }


[<DomainType>]
type VisibleBox = {
    geometry : Box3d
    color : C4b
    trafo : Trafo3d
    isHovered : bool
    [<TreatAsValue>]
    id : string
}


module VisibleBox =
    
    let initial = 
        {
            geometry  = Box3d.FromSize(V3d(0.10, 0.5, 0.05))//Box3d.FromCenterAndSize(V3d.Zero, V3d.One)
            color = C4b.Red
            trafo = Trafo3d.Identity
            isHovered = false
            //size = V3d.One
            id = ""
        }

    let createVisibleBox (color : C4b) (position : V3d) (rotation : Pose) = 
        let x = (position - rotation.deviceToWorld.GetModelOrigin()).Normalized
        let y = V3d.Cross(x,V3d.OOI)
        let z = V3d.Cross(y,x)
        let newCoordinateSystem = Trafo3d.FromBasis(-x,y,z, position)
        {   initial 
                with
                color = color     
                trafo =  newCoordinateSystem
                id = System.Guid.NewGuid().ToString()
        }

    let createDrawingPoint (color : C4b) (position : V3d)= 
        {
            initial 
                with 
                geometry = Box3d.FromCenterAndSize(V3d.Zero, V3d(0.01,0.01,0.01))
                color = color 
                trafo = Trafo3d.Translation(position) 
                id = System.Guid.NewGuid().ToString()
        }

    let createFlag (color : C4b) (position : V3d) = 
        {
            initial 
                with
                color = color
                trafo = Trafo3d.Translation(position)
                id = System.Guid.NewGuid().ToString()
                geometry = Box3d.FromSize(V3d(0.01, 0.01, 0.15))
        }
    let createDrone (color : C4b) (position : V3d) = 
        {
            initial 
                with
                color = color
                trafo = Trafo3d.Translation(position)
                id = System.Guid.NewGuid().ToString()
                geometry = Box3d.FromSize(V3d(0.45, 0.45, 0.15))
        }

    let createDroneScreen (color : C4b) (position : V3d) = 
        {
            initial 
                with
                color = color
                trafo = Trafo3d.Translation(position)
                id = System.Guid.NewGuid().ToString()
                geometry = Box3d.FromSize(V3d(0.05, 3.5, 3.5))
        }
    
    let createDroneScreenOnController (color : C4b) (position : V3d) = 
        {
            initial 
                with
                color = color
                trafo = Trafo3d.Translation(position)
                id = System.Guid.NewGuid().ToString()
                geometry = Box3d.FromSize(V3d(0.05, 1.2, 1.2))
        }

    

[<DomainType>]
type VisibleCone = {
    geometry : IndexedGeometry
    color    : C4b
    trafo    : Trafo3d
}

module VisibleCone = 

    let initial = 
        {
            color    = C4b.Red
            geometry = Cone.solidCone V3d.Zero V3d.Zero 1.0 1.0 50 C4b.Red
            trafo    = Trafo3d.Identity
            
        }
    let createVisibleCone (color : C4b) (position : V3d) = 
        {
            initial 
                with 
                color = color 
                trafo = Trafo3d.Translation(position)
                geometry = Cone.solidCone position V3d.Zero 5.0 0.5 20 color
                
        }

[<DomainType>]
type VisibleSphere = {
    geometry : Sphere3d
    color : C4b
    trafo : Trafo3d
    radius : float
    distance : string
    [<TreatAsValue>]
    id : string
}

module VisibleSphere = 
    let initial = 
        {
            geometry = Sphere3d.FromRadius(1.0)
            color = C4b.White
            trafo = Trafo3d.Identity
            id = ""
            radius = 1.0
            distance = ""
        }

    let createSphere (color : C4b) (position : V3d) (radius : float) = 
        {
            initial 
                with
                color = color 
                trafo = Trafo3d.Translation(V3d(position.X, position.Y, position.Z))
                id = System.Guid.NewGuid().ToString()
                geometry = Sphere3d.FromRadius(radius)
                radius = radius
        }

[<DomainType>]
type VisibleCylinder = 
    {
        geometry    : Cylinder3d
        color       : C4b
        trafo       : Trafo3d
        radius      : float
        angle       : string
        isNotInside    : bool
        [<TreatAsValue>]
        id          : string
    }

module VisibleCylinder = 
    let initial = 
        {
            geometry = Cylinder3d()
            color    = C4b.White
            trafo    = Trafo3d.Identity
            radius   = 1.0
            isNotInside = true 
            angle    = ""
            id       = ""
        }

    let createCylinder (color : C4b) (position : V3d) (radius : float) = 
        {
            initial with 
                color = color
                trafo = Trafo3d.Translation(V3d(position.X, position.Y, position.Z))
                id = System.Guid.NewGuid().ToString()
                geometry = Cylinder3d(V3d(position.X, position.Y - 2.5, position.Z), V3d(position.X, position.Y + 2.5, position.Z), radius)
                radius = radius
        }
