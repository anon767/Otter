open OtterCore
open OtterReporter

module Reporter = ErrorReporter.Make (BackOtterErrors)

class ['self] t = ['self] Reporter.t

