namespace Demo.Menu

open Aardvark.Base.Incremental
open Aardvark.Vr
open Aardvark.Base
open Demo
open DevILSharp
open DevILSharp


type MenuState = 
| PlaceLandmarks
| WIMLandmarks
| Scale
| WIM
| NodeBased
| Reset

[<DomainType>]
type MenuModel = 
    {
        mainMenuBoxes               : plist<VisibleBox>
        boxHovered                  : option<string>
        menu                        : MenuState
        initialMenuState            : MenuState
        menuButtonPressed           : bool
        initialMenuPosition         : Pose
        initialMenuPositionBool     : bool
        controllerMenuSelector      : ControllerInfo

    }

module MenuModel = 
    let init = 
        {
            mainMenuBoxes               = PList.empty
            boxHovered                  = None
            menu                        = MenuState.Scale
            initialMenuState            = MenuState.Scale
            menuButtonPressed           = false
            initialMenuPosition         = Pose.none
            initialMenuPositionBool     = false
            controllerMenuSelector      = ControllerInfo.initial
        }