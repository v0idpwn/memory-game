open! Core_kernel
open! Bonsai_web

let component = Bonsai.const (Vdom.Node.text "hello world")

let _ : (unit, unit, never_returns, never_returns) Start.Handle.t =
  Start.start_standalone ~initial_input:() ~bind_to_element_with_id:"app" component
