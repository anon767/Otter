open OcamlUtilities

let queues = OtterQueue.Queue.queues

let get targets_ref = function
    | `ClosestToTargets -> new ClosestToTargetsQueue.t targets_ref
    | `Generational `ClosestToTargets -> new OtterQueue.GenerationalQueue.t (new ClosestToTargetsQueue.t targets_ref)
    | qt -> OtterQueue.Queue.get qt

let get_default targets_ref = get targets_ref !OtterQueue.Queue.default_queue

