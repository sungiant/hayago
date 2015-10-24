package hayago

// http://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html
object GTP {
  import cats.std.all._
  import cats.syntax.all._
  import cats.state._
  import cats._
  import scala.util._
  import cats.Monad.ops._
  import Engine._
  import scala.concurrent.Future

  private object CommandIdentifier {
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

  private case class GtpCommand (id: Option[Int], cmd: String, args: List[String])
  private object GtpCommand {
    private object ID { def unapply(str: String): Option[Int] = Try(str.toInt).toOption }
    def unapply(str: String): Option[GtpCommand] = str.split("\\s+").toList match {
      case ID(id) :: cmd :: args => Some(GtpCommand(Some(id), cmd, args))
      case cmd :: args => Some(GtpCommand(None, cmd, args))
      case _ => None
    }
  }

  private sealed trait GtpResponseType
  private object Success extends GtpResponseType
  private object Failure extends GtpResponseType

  private case class GtpResponse (responseType: GtpResponseType, id: Option[Int], entities: List[String]) {
    override def toString = (responseType match {
      case Success => "="
      case Failure => "?"
    }) + (id match {
      case Some(i) => s" $i "
      case None => " "
    }) + entities.mkString(" ")
  }

  private object GtpResponse {
    def success(id: Option[Int], entities: List[String]) = GtpResponse(Success, id, entities)
    def failure(msg: String) = GtpResponse(Failure, None, msg :: Nil)
  }

  // An int is an unsigned integer in the interval  $0 <= x <= 2^{31} - 1$.
  private object GtpInt { def unapply (str: String): Option[Int] = Try(str.toInt).toOption }
  // A float is a floating point number representable by a 32 bit IEEE 754 float.
  private object GtpFloat { def unapply (str: String): Option[Float] = Try(str.toFloat).toOption }
  // A string is a sequence of printable, non-whitespace characters. Strings are case sensitive.
  private object GtpString { def unapply (str: String): Option[String] = if (str.isEmpty) None else Some (str) }
  // A vertex is a board coordinate consisting of one letter and one number, as defined in section 2.11,
  // or the string ``pass''. Vertices are not case sensitive. Examples: ``B13'', ``j11''.
  private type GtpVertex = Either[Game.Signal, Game.Board.Intersection]
  private object GtpVertex { def unapply (str: String): Option[GtpVertex] = str match {
    case "pass" => Some (Left (Game.Pass))
    case "resign" => Some (Left (Game.Resign))
    case Game.Board.Intersection (i) => Some (Right (i))
    case _ => None
  }}
  // A color is one of the strings ``white'' or ``w'' to denote white, or ``black'' or ``b'' to denote black.
  // Colors are not case sensitive.
  private object GtpColour { def unapply (str: String): Option[Game.Colour] = str.toLowerCase match {
    case "white" | "w" => Some (Game.White)
    case "black" | "b" => Some (Game.Black)
  }}
  // A move is the combination of one color and one vertex, separated by space. Moves are not case sensitive.
  // Examples: ``white h10'', ``B F5'', ``w pass''.
  private type GtpMove = (Game.Colour, GtpVertex)
  private object GtpMove { def unapply (str: String): Option[GtpMove] = ??? }
  // A boolean is one of the strings ``false'' and ``true''.
  private object GtpBoolean { def unapply (str: String): Option[Boolean] = str.toLowerCase match {
    case "false" => Some (false)
    case "true" => Some (true)
  }}

  private lazy val knownCommands = {
    import scala.reflect.runtime._
    val instanceMirror = currentMirror.reflect (CommandIdentifier)
    instanceMirror.symbol.asClass.typeSignature.members
      .filter (s => s.isTerm && s.asTerm.isAccessor)
      .map (instanceMirror reflectMethod _.asMethod)
      .map (_.apply ())
      .map (_.toString)
      .toList
  }

  sealed trait GtpStatus
  object Exit extends GtpStatus
  object Fatal extends GtpStatus
  object OK extends GtpStatus

  // Character values 0-31 and 127 are control characters in ASCII.
  // The following control characters have a specific meaning in the protocol:
  // HT (dec 9)	Horizontal Tab
  // LF (dec 10)	Line Feed
  // CR (dec 13)	Carriage Return
  def gtpLoop (line: String) (implicit MF: Monad[Future]): StateT[Future, Game.State, GtpStatus] = line
    // Remove all occurrences of CR and other control characters except for HT and LF.
    .filter(c => (c > 31 && c < 127) || c === 9 || c === 10)
    // For each line with a hash sign (#), remove all text following and including this character.
    .split('#').head
    // Convert all occurrences of HT to SPACE.
    .map { case 9 => ' '; case s => s }
    // Discard any empty or white-space only lines.
    .toString match {
      case str if str.isEmpty => StateT.pure[Future, Game.State, GtpStatus] (OK)
      case str =>
        GtpCommand.unapply (str) match {
          case None => StateT.pure[Future, Game.State, GtpStatus] (OK)
          case Some (cmd) => for {
            gameState <- ms.get
            handlerNotFound = (_: GtpCommand) => StateT.pure[Future, Game.State, GtpResponse] (GtpResponse.failure ("handler not found"))
            result <- commandHandler.applyOrElse (cmd, handlerNotFound).map { gtpResponse =>
              if (gtpResponse.responseType == Failure) Fatal
              else OK
            }
          } yield result
        }
    }

  private def commandHandler (implicit MF: Monad[Future]): PartialFunction [GtpCommand, StateT[Future, Game.State, GtpResponse]] = {

    // protocol_version
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : none
    // output     : version_number ~ int version_number - Version of the GTP Protocol
    // fails      : never
    // comments   : For this specification 2.
    case GtpCommand (id, CommandIdentifier.protocol_version, Nil) => StateT.pure[Future, Game.State, GtpResponse] {
      GtpResponse.success (id, "2" :: Nil)
    }

    // name
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : none
    // output     : name ~ string* name - Name of the engine
    // fails      : never
    // comments   : E.g. ``GNU Go'', ``GoLois'', ``Many Faces of Go''. The name does not include any version
    //              information, which is provided by the version command.
    case GtpCommand (id, CommandIdentifier.name, Nil) => StateT.pure[Future, Game.State, GtpResponse] {
      GtpResponse.success (id, "Hayago Engine" :: Nil)
    }

    // version
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : none
    // output     : version ~ string* version - Version of the engine
    // fails      : never
    // comments   : E.g. ``3.1.33'', ``10.5''. Engines without a sense of version number should return the empty string.
    case GtpCommand (id, CommandIdentifier.version, Nil) => StateT.pure[Future, Game.State, GtpResponse] {
      GtpResponse.success (id, "0.0.1" :: Nil)
    }

    // known_command
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : command_name ~ string command_name - Name of a command
    // effects    : none
    // output     : known ~ boolean known - ``true'' if the command is known by the engine, ``false'' otherwise
    // fails      : never
    // comments   : The protocol makes no distinction between unknown commands and known but unimplemented ones. Do not
    //              declare a command as known if it is known not to work.
    case GtpCommand (id, CommandIdentifier.known_command, GtpString (command_name) :: Nil) => StateT.pure[Future, Game.State, GtpResponse] {
      GtpResponse.success (id, knownCommands.contains (command_name).toString :: Nil)
    }

    // list_commands
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : none
    // output     : commands ~ string& commands - List of commands, one per row
    // fails      : never
    // comments   : Include all known commands, including required ones and private extensions.
    case GtpCommand (id, CommandIdentifier.list_commands, Nil) => StateT.pure[Future, Game.State, GtpResponse] {
      GtpResponse.success (id, knownCommands)
    }

    // quit
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : The session is terminated and the connection is closed.
    // output     : none
    // fails      : never
    // comments   : The full response of this command must be sent before the engine closes the connection. The
    //              controller must receive the response before the connection is closed on its side.
    case GtpCommand (id, CommandIdentifier.quit, Nil) => StateT.pure[Future, Game.State, GtpResponse] {
      GtpResponse.success (id, Nil)
    }

    // boardsize
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : size ~ int size - New size of the board.
    // effects    : The board size is changed. The board configuration, number of captured stones, and move history
    //              become arbitrary.
    // output     : none
    // fails      : Syntax error. If the engine cannot handle the new size, fails with the error message
    //              "unacceptable size".
    // comments   : In GTP version 1 this command also did the work of clear_board. This may or may not be true for
    //              implementations of GTP version 2. Thus the controller must call clear_board explicitly. Even if the
    //              new board size is the same as the old one, the board configuration becomes arbitrary.
    case GtpCommand (id, CommandIdentifier.boardsize, GtpInt (size) :: Nil) => for {
      gs <- ms.get
      unacceptableSize = StateT.pure[Future, Game.State, GtpResponse] (GtpResponse.failure ("unacceptable size"))
      response <- size match {
        case x if x >= gs.setup.boardSize => for {
          _ <- ms.set (gs.copy (setup = gs.setup.copy (boardSize = size)))
        } yield GtpResponse.success(id, Nil)
        case _ => unacceptableSize
     }
    } yield response

    // clear_board
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : The board is cleared, the number of captured stones is reset to zero for both colors and the move
    //              history is reset to empty.
    // output     : none
    // fails      : never
    // comments   :
    case GtpCommand (id, CommandIdentifier.clear_board, Nil) => for {
      gs <- ms.get
      _ <- ms.set (gs.copy (history = Nil))
    } yield GtpResponse.success (id, Nil)


    // komi
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : new_komi ~ float new_komi - New value of komi.
    // effects    : Komi is changed.
    // output     : none
    // fails      : syntax error
    // comments   : The engine must accept the komi even if it should be ridiculous.
    case GtpCommand (id, CommandIdentifier.komi, GtpFloat (komi) :: Nil) => for {
      gs <- ms.get
      _ <- ms.set (gs.copy (setup = gs.setup.copy (komi = komi)))
    } yield GtpResponse.success (id, Nil)

    // play
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : move ~ move move - Color and vertex of the move
    // effects    : A stone of the requested color is played at the requested vertex. The number of captured stones is
    //              updated if needed and the move is added to the move history.
    // output     : none
    // fails      : syntax error, illegal move. In the latter case, fails with the error message ``illegal move''.
    // comments   : Consecutive moves of the same color are not considered illegal from the protocol point of view.
    case GtpCommand (id, CommandIdentifier.play, GtpColour (colour) :: GtpVertex (vertex) :: Nil) => for {
      gameState <- ms.get
      illegalMove = StateT.pure[Future, Game.State, GtpResponse] (GtpResponse.failure ("illegal move"))
      response <- (gameState, Game.Turn (vertex)) match {
        case (gs, pt) if !gs.isTurnLegal (pt) => illegalMove
        case (_, Game.Turn.pass) => illegalMove
        case (gs, pt) if gs.colourToPlayNext != colour => for {
          _ <- ms.set (gs.copy (history = gs.history :+ Game.Turn.pass :+ pt))
        } yield GtpResponse.success (id, Nil)
        case (gs, pt) => for {
          _ <- ms.set (gs.copy (history = gs.history :+ pt))
        } yield GtpResponse.success (id, Nil)
      }
    } yield response

    // genmove
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : color ~ color color - Color for which to generate a move.
    // effects    : A stone of the requested color is played where the engine chooses. The number of captured stones
    //              is updated if needed and the move is added to the move history.
    // output     : vertex ~ vertex$\vert$string vertex - Vertex where the move was played or the string ``resign''.
    // fails      : never.
    // comments   : Notice that ``pass'' is a valid vertex and should be returned if the engine wants to pass.
    //              Use ``resign'' if you want to give up the game. The controller is allowed to use this command for
    //              either color, regardless who played the last move.
    case GtpCommand (id, CommandIdentifier.genmove, GtpColour (colour) :: Nil) => for {
      _ <- takeTurn
      gameState <- ms.get
    } yield gameState.history.lastOption.map (_.action) match {
      case Some (Left (Game.Pass)) => GtpResponse.success (id, "pass" :: Nil)
      case Some (Left (Game.Resign)) => GtpResponse.success (id, "resign" :: Nil)
      case Some (Right (i)) => GtpResponse.success (id, i.toString :: Nil)
      case _ => GtpResponse.failure ("unexpected error")
    }

    // undo
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
    case GtpCommand (id, CommandIdentifier.undo, Nil) => for {
      gameState <- ms.get
      cannotUndo = StateT.pure[Future, Game.State, GtpResponse] (GtpResponse.failure ("cannot undo"))
      response <- gameState.history.reverse match {
        case head :: tail => for {
          _ <- ms.set (gameState.copy (history = tail.reverse))
        } yield GtpResponse.success (id, Nil)
        case _ => cannotUndo
      }
    } yield response

    // showboard
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : none
    // output     : board ~ string*& board - A diagram of the board position.
    // fails      : never
    // comments   : The engine may draw the board as it likes. It is, however, required to place the coordinates as
    //              described in section 2.11. This command is only intended to help humans with debugging and the
    //              output should never need to be parsed by another program.
    case GtpCommand (id, CommandIdentifier.showboard, Nil) => for {
      gs <- ms.get
    } yield GtpResponse.success (id, PrettyPrinter.stringify (gs.board) :: Nil)
  }
}
