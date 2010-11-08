open OcamlUtilities
open OtterQueue
open OtterQueue.Queue

let queues = OtterQueue.Queue.queues

let get targets_ref = function
    (* TODO: return different queues *)
    | `ClosestToTargets -> new ClosestToTargetsQueue.t
    | `Generational `ClosestToTargets -> new GenerationalQueue.t (new ClosestToTargetsQueue.t)
    | qt -> OtterQueue.Queue.get qt

let get_default targets_ref = get targets_ref !OtterQueue.Queue.default_queue

