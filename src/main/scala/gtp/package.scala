package hayago

// http://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html

package object gtp {

  type GtpVertex = Either[game.Signal, game.Intersection]

  lazy val knownCommands = {
    import scala.reflect.runtime._
    val instanceMirror = CMD |> currentMirror.reflect
    instanceMirror.symbol.asClass.typeSignature.members
      .filter (s => s.isTerm && s.asTerm.isAccessor)
      .map (instanceMirror reflectMethod _.asMethod)
      .map (_.apply ())
      .map (_.toString)
      .toList
  }

  object CMD {
    // Adminstrative
    val protocol_version    = "protocol_version"
    val name                = "name"
    val version             = "version"
    val known_command       = "known_command"
    val list_commands       = "list_commands"
    val quit                = "quit"
    // Setup
    val boardsize           = "boardsize"
    val clear_board         = "clear_board"
    val komi                = "komi"
    // Play
    val play                = "play"
    val genmove             = "genmove"
    val undo                = "undo"
    // Debug
    val showboard           = "showboard"
  }
}