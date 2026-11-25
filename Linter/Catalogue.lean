import Linter.Catalogue.Style
import Linter.Catalogue.Safety
import Linter.Catalogue.Errors
import Linter.Catalogue.Types
import Linter.Catalogue.Security
import Linter.Catalogue.Checks
import Linter.Catalogue.Metrics
import Linter.Catalogue.Boundary
import Linter.Intake

namespace Linter.Catalogue

def rules : List Linter.Rule :=
  Style.rules ++ Safety.rules ++ Errors.rules ++ Types.rules ++ Security.rules ++ Checks.rules ++ Metrics.rules ++ Boundary.rules

def discovery : Linter.Intake.Policy where
  vendored := [".git", ".lake", ".spago", "node_modules", "output", "dist-newstyle"]
  foreign := { script := "js", host := "purs", language := .javascript }
  generated := [.header 5 [["Generated", "generated", "GENERATED"], ["Do not edit", "DO NOT EDIT", "do not edit"]], .minified 3 1000]
  limits := { entries := 20000, total := 200 * 1000 * 1000, entry := 20 * 1000 * 1000 }

end Linter.Catalogue
