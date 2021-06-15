open! Core_kernel
open! Bonsai_web

let board = Bonsai.of_module (module Board_component) ~default_model:Board_component.Model.default

let _ : (unit, unit, never_returns, never_returns) Start.Handle.t =
  Start.start_standalone ~initial_input:() ~bind_to_element_with_id:"app" board
