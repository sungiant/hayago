package hayago

// http://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html
object GTP {
  import java.util.Properties
  import cats.std.all._
  import cats.syntax.eq._
  import cats.state._
  import cats._
  import scala.util._
  import cats.Traverse.ops._
  import cats.Monad.ops._
  import Engine._
  import scala.concurrent.Future

  // Character values 0-31 and 127 are control characters in ASCII.
  // The following control characters have a specific meaning in the protocol:
  // HT (dec 9)	Horizontal Tab
  // LF (dec 10)	Line Feed
  // CR (dec 13)	Carriage Return
  def preprocess(line: String): Option[String] = line
    // Remove all occurences of CR and other control characters except for HT and LF.
    .filter(c => (c > 31 && c < 127) || c === 9 || c === 10)
    // For each line with a hash sign (#), remove all text following and including this character.
    .split('#').head
    // Convert all occurences of HT to SPACE.
    .map { case 9 => ' '; case s => s }
    // Discard any empty or white-space only lines.
    .toString match {
    case s if s.isEmpty => None
    case s => Some(s)
  }

  def gtpLoop(line: String): Boolean = {
    println(s"received line: $line")
    val response = preprocess(line).map(GtpCommand.unapply).map {
      case Some(cmd) =>
        adminstrativeCommandHandler
          .orElse(setupCommandHandler)
          .orElse(playCommandHandler)
          .orElse(debugCommandHandler)
          .applyOrElse (cmd, (_: GtpCommand) => GtpResponse.failure (s"No command handler defined for: $cmd"))
      case None => GtpResponse.failure ("?")
    }

    response match {
      case Some(r) => println(r); true
      case None => false
    }
  }


  case class BoardConfig()
  case class GameConfig (boardSize: Int, boardConfig: BoardConfig)

  case class GtpCommand (id: Option[Int], cmd: String, args: List[String])
  object GtpCommand {
    private object ID { def unapply(str: String): Option[Int] = Try(str.toInt).toOption }
    def unapply(str: String): Option[GtpCommand] = str.split("\\s+").toList match {
      case ID(id) :: cmd :: args => Some(GtpCommand(Some(id), cmd, args))
      case cmd :: args => Some(GtpCommand(None, cmd, args))
      case _ => None
    }
  }

  sealed trait GtpResponseType
  object Success extends GtpResponseType
  object Failure extends GtpResponseType

  case class GtpResponse(responseType: GtpResponseType, id: Option[Int], entities: List[String]) {
    override def toString = (responseType match {
      case Success => "="
      case Failure => "?"
    }) + (id match {
      case Some(i) => s" $i "
      case None => " "
    }) + entities.mkString(" ")
  }

  object GtpResponse {
    def success(id: Option[Int], entities: List[String]) = GtpResponse(Success, id, entities)
    def failure(msg: String) = GtpResponse(Failure, None, msg :: Nil)
  }

  /*


sealed case class GtpStatus (code: Int)
object GTP_QUIT extends GtpStatus (-1)
object GTP_OK extends GtpStatus (0)
object GTP_FATAL extends GtpStatus (1)
*/
  object GtpString { def unapply(str: String): Option[String] = Some (str) // todo: i think only ASCII is allowed
  object GtpColour { def unapply(str: String): Option[Colour] = ???
  object GtpVertex { def unapply(str: String): Option[Location] = ???
  object GtpInt { def unapply(str: String): Option[Int] = Try(str.toInt).toOption }
  object GtpFloat { def unapply(str: String): Option[Float] = Try(str.toFloat).toOption }

  object CommandIdentifier {
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

  import scala.reflect.runtime.universe._
  val rm = scala.reflect.runtime.currentMirror
  val instanceMirror = rm.reflect (CommandIdentifier)
  val knownCommands2: List[String] = rm.classSymbol (CommandIdentifier.getClass).toType.members.collect {
    case m: MethodSymbol if m.isVal && m.isPublic && m.typeSignature == String =>
      instanceMirror.reflectMethod (m).apply().asInstanceOf[String]
  }.toList

  // Todo: generate this from the above
  val knownCommands: List[String] =
    CommandIdentifier.protocol_version ::
    CommandIdentifier.name ::
    CommandIdentifier.version ::
    CommandIdentifier.known_command ::
    CommandIdentifier.list_commands ::
    CommandIdentifier.quit ::
    CommandIdentifier.boardsize ::
    CommandIdentifier.clear_board ::
    CommandIdentifier.komi ::
    CommandIdentifier.play ::
    CommandIdentifier.genmove ::
    CommandIdentifier.undo ::
    CommandIdentifier.showboard :: Nil

  type $[%, &] = StateT[Future, %, &]
  val ms = MonadState[$, GameState]

  val adminstrativeCommandHandler: PartialFunction [GtpCommand, StateT[Future, GameState, GtpResponse]] = {

    // arguments  : none
    // effects    : none
    // output     : version_number ~ int version_number - Version of the GTP Protocol
    // fails      : never
    // comments   : For this specification 2.
    case GtpCommand(id, CommandIdentifier.protocol_version, Nil) => StateT.pure[Future, GameState, GtpResponse] {
      GtpResponse.success(id, "2" :: Nil)
    }

    // arguments  : none
    // effects    : none
    // output     : name ~ string* name - Name of the engine
    // fails      : never
    // comments   : E.g. ``GNU Go'', ``GoLois'', ``Many Faces of Go''. The name does not include any version
    //              information, which is provided by the version command.
    case GtpCommand(id, CommandIdentifier.name, Nil) => StateT.pure[Future, GameState, GtpResponse] {
      GtpResponse.success(id, "Hayago Engine" :: Nil)
    }

    // arguments  : none
    // effects    : none
    // output     : version ~ string* version - Version of the engine
    // fails      : never
    // comments   : E.g. ``3.1.33'', ``10.5''. Engines without a sense of version number should return the empty string.
    case GtpCommand(id, CommandIdentifier.version, Nil) => StateT.pure[Future, GameState, GtpResponse] {
      GtpResponse.success(id, "0.0.1" :: Nil)
    }

    // arguments  : command_name ~ string command_name - Name of a command
    // effects    : none
    // output     : known ~ boolean known - ``true'' if the command is known by the engine, ``false'' otherwise
    // fails      : never
    // comments   : The protocol makes no distinction between unknown commands and known but unimplemented ones. Do not
    //              declare a command as known if it is known not to work.
    case GtpCommand(id, CommandIdentifier.known_command, GtpString (command_name) :: Nil) => StateT.pure[Future, GameState, GtpResponse] {
      GtpResponse.success(id, knownCommands.contains(command_name).toString :: Nil)
    }

    // arguments  : none
    // effects    : none
    // output     : commands ~ string& commands - List of commands, one per row
    // fails      : never
    // comments   : Include all known commands, including required ones and private extensions.
    case GtpCommand(id, CommandIdentifier.list_commands, Nil) => StateT.pure[Future, GameState, GtpResponse] {
      GtpResponse.success(id, knownCommands)
    }

    // arguments  : none
    // effects    : The session is terminated and the connection is closed.
    // output     : none
    // fails      : never
    // comments   : The full response of this command must be sent before the engine closes the connection. The
    //              controller must receive the response before the connection is closed on its side.
    case GtpCommand(id, CommandIdentifier.quit, Nil) => StateT.pure[Future, GameState, GtpResponse] {
      GtpResponse.success(id, Nil)
    }
  }

  val setupCommandHandler: PartialFunction [GtpCommand, StateT[Future, GameState, GtpResponse]] = {

    // arguments  : size ~ int size - New size of the board.
    // effects    : The board size is changed. The board configuration, number of captured stones, and move history
    //              become arbitrary.
    // output     : none
    // fails      : Syntax error. If the engine cannot handle the new size, fails with the error message
    //              "unacceptable size".
    // comments   : In GTP version 1 this command also did the work of clear_board. This may or may not be true for
    //              implementations of GTP version 2. Thus the controller must call clear_board explicitly. Even if the
    //              new board size is the same as the old one, the board configuration becomes arbitrary.
    case GtpCommand(id, CommandIdentifier.boardsize, GtpInt (size) :: Nil) => for {
      gs <- ms.get
      r <- size match {
        case x if x >= gs.setup.boardSize => for {
          _ <- ms.set (gs.copy (setup = gs.setup.copy (boardSize = size)))
        } yield GtpResponse.success(id, Nil)
        case _ => StateT.pure[Future, GameState, GtpResponse] (GtpResponse.success(id, "unacceptable size" :: Nil))
     }
    } yield r

    // arguments  : none
    // effects    : The board is cleared, the number of captured stones is reset to zero for both colors and the move
    //              history is reset to empty.
    // output     : none
    // fails      : never
    // comments   :
    case GtpCommand(id, CommandIdentifier.clear_board, Nil) => StateT.pure[Future, GameState, GtpResponse] {
      ???
    }


    // arguments  : new_komi ~ float new_komi - New value of komi.
    // effects    : Komi is changed.
    // output     : none
    // fails      : syntax error
    // comments   : The engine must accept the komi even if it should be ridiculous.
    case GtpCommand(id, CommandIdentifier.komi, GtpFloat (komi) :: Nil) => StateT.pure[Future, GameState, GtpResponse] {
      ???
    }
  }

  val playCommandHandler: PartialFunction [GtpCommand, StateT[Future, GameState, GtpResponse]] = {

    // arguments  : move ~ move move - Color and vertex of the move
    // effects    : A stone of the requested color is played at the requested vertex. The number of captured stones is
    //              updated if needed and the move is added to the move history.
    // output     : none
    // fails      : syntax error, illegal move. In the latter case, fails with the error message ``illegal move''.
    // comments   : Consecutive moves of the same color are not considered illegal from the protocol point of view.
    case GtpCommand(id, CommandIdentifier.play, GtpColour (colour) :: GtpVertex (location) :: Nil) => StateT.pure[Future, GameState, GtpResponse] {
      ???
    }

    // arguments  : color ~ color color - Color for which to generate a move.
    // effects    : A stone of the requested color is played where the engine chooses. The number of captured stones
    //              is updated if needed and the move is added to the move history.
    // output     : vertex ~ vertex$\vert$string vertex - Vertex where the move was played or the string ``resign''.
    // comments   : Notice that ``pass'' is a valid vertex and should be returned if the engine wants to pass.
    //              Use ``resign'' if you want to give up the game. The controller is allowed to use this command for
    //              either color, regardless who played the last move.
    case GtpCommand(id, CommandIdentifier.genmove, GtpColour (colour) :: Nil) => StateT.pure[Future, GameState, GtpResponse] {
      ???
    }

    // arguments  : none
    // effects    : The board configuration and the number of captured stones are reset to the state before the last
    //              move. The last move is removed from the move history.
    // output     : none
    // fails      : If the engine is unable to take back the last move, fails with the error message "cannot undo".
    // comments   : If you want to take back multiple moves, use this command multiple times. The engine may fail to
    //              undo if the move history is empty or if the engine only maintains a partial move history, which has
    //              been exhausted by previous undos. It is never possible to undo handicap placements. Use clear_board
    //              if you want to start over. An engine which never is able to undo should not include this command
    //              among its known commands.
    case GtpCommand(id, CommandIdentifier.undo, Nil) => StateT.pure[Future, GameState, GtpResponse] {
      ???
    }
  }

  val debugCommandHandler: PartialFunction [GtpCommand, StateT[Future, GameState, GtpResponse]] = {
    // arguments  : none
    // effects    : none
    // output     : board ~ string*& board - A diagram of the board position.
    // fails      : never
    // comments   : The engine may draw the board as it likes. It is, however, required to place the coordinates as
    //              described in section 2.11. This command is only intended to help humans with debugging and the
    //              output should never need to be parsed by another program.
    case GtpCommand(id, CommandIdentifier.boardsize, Nil) => for {
      gs <- ms.get
    } yield GtpResponse.success (id, PrettyPrinter.stringify (gs.boardState2DA) :: Nil)
  }
}
