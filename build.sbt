scalacOptions += "-feature"

scalacOptions += "-deprecation"

scalacOptions += "-Xlog-free-terms"

scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")

