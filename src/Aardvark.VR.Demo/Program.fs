open Aardvark.Base
open Aardvark.UI
open Aardvark.Vr
open Aardium
open Demo.Main
open Suave
open Demo
open Aardvark.VRVis.Opc


[<EntryPoint>]
let main argv =
    Ag.initialize()
    SerializationOpc.registry.RegisterFactory (fun _ -> KdTrees.level0KdTreePickler)
    Aardvark.Init()
    Aardium.init()

    let app = VRApplication.create (VRDisplay.OpenVR 1.0) 8 false
    let mapp = ComposedApp.start app (Demo.app app.Runtime)
    
    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart app.Runtime mapp
    ] |> ignore
    
    Aardium.run {
        url "http://localhost:4321"
    }

    0
