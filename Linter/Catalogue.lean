import Linter.Catalogue.Style
import Linter.Catalogue.Safety
import Linter.Catalogue.Errors
import Linter.Intake

namespace Linter.Catalogue

def rules : List Linter.Rule := Style.rules ++ Safety.rules ++ Errors.rules

def discovery : Linter.Intake.Policy where
  vendored := [".git", ".lake", ".spago", "node_modules", "output", "dist-newstyle"]

end Linter.Catalogue
