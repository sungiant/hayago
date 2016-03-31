package hayago.gtp

import hayago._
import scala.util._
import cats.std.all._
import cats.syntax.all._
import cats.state._
import cats._
import scala.concurrent.Future

sealed trait ProtocolStatus
object ProtocolStatus {
  object Exit extends ProtocolStatus
  object Fatal extends ProtocolStatus
  object OK extends ProtocolStatus
}

object Protocol {
  // Character values 0-31 and 127 are control characters in ASCII.
  // The following control characters have a specific meaning in the protocol:
  // HT (dec 9) Horizontal Tab
  // LF (dec 10)  Line Feed
  // CR (dec 13)  Carriage Return
  def process (line: String) (implicit MF: Monad[Future]): StateT[Future, game.State, ProtocolStatus] = line
    // Remove all occurrences of CR and other control characters except for HT and LF.
    .filter(c => (c > 31 && c < 127) || c === 9 || c === 10)
    // For each line with a hash sign (#), remove all text following and including this character.
    .split('#').head
    // Convert all occurrences of HT to SPACE.
    .map { case 9 => ' '; case s => s }
    // Discard any empty or white-space only lines.
    .toString match {
      case str if str.isEmpty => StateT.pure[Future, game.State, ProtocolStatus] (ProtocolStatus.OK)
      case str =>
        Command.unapply (str) match {
          case None => StateT.pure[Future, game.State, ProtocolStatus] (ProtocolStatus.OK)
          case Some (gtpCommand) =>
            print (gtpCommand)
            for {
            gameState <- ms.get
            handlerNotFound = (_: Command) =>
              StateT.pure[Future, game.State, Response] (Response.failure ("handler not found"))
            gtpResponse <- commandHandler.applyOrElse (gtpCommand, handlerNotFound)
            newGameState <- ms.get
          } yield {
              print (gtpResponse)
              if (gtpResponse.responseType == ResponseType.Failure) ProtocolStatus.OK
              else if (gtpCommand.cmd == CMD.quit) ProtocolStatus.Exit
              else if (newGameState.isComplete) ProtocolStatus.Exit
              else ProtocolStatus.OK
            }
        }
    }

  private def commandHandler (implicit MF: Monad[Future]): PartialFunction [Command, StateT[Future, game.State, Response]] = {

    // protocol_version
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : none
    // output     : version_number ~ int version_number - Version of the GTP Protocol
    // fails      : never
    // comments   : For this specification 2.
    case Command (id, CMD.protocol_version, Nil) => StateT.pure[Future, game.State, Response] {
      Response.success (id, "2" :: Nil)
    }

    // name
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : none
    // output     : name ~ string* name - Name of the engine
    // fails      : never
    // comments   : E.g. ``GNU Go'', ``GoLois'', ``Many Faces of Go''. The name does not include any version
    //              information, which is provided by the version command.
    case Command (id, CMD.name, Nil) => StateT.pure[Future, game.State, Response] {
      Response.success (id, "Hayago Engine" :: Nil)
    }

    // version
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : none
    // output     : version ~ string* version - Version of the engine
    // fails      : never
    // comments   : E.g. ``3.1.33'', ``10.5''. Engines without a sense of version number should return the empty string.
    case Command (id, CMD.version, Nil) => StateT.pure[Future, game.State, Response] {
      Response.success (id, "0.0.1" :: Nil)
    }

    // known_command
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : command_name ~ string command_name - Name of a command
    // effects    : none
    // output     : known ~ boolean known - ``true'' if the command is known by the engine, ``false'' otherwise
    // fails      : never
    // comments   : The protocol makes no distinction between unknown commands and known but unimplemented ones. Do not
    //              declare a command as known if it is known not to work.
    case Command (id, CMD.known_command, GtpString (command_name) :: Nil) =>
      StateT.pure[Future, game.State, Response] {
        Response.success (id, knownCommands.contains (command_name).toString :: Nil)
      }

    // list_commands
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : none
    // output     : commands ~ string& commands - List of commands, one per row
    // fails      : never
    // comments   : Include all known commands, including required ones and private extensions.
    case Command (id, CMD.list_commands, Nil) => StateT.pure[Future, game.State, Response] {
      Response.success (id, knownCommands)
    }

    // quit
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : none
    // effects    : The session is terminated and the connection is closed.
    // output     : none
    // fails      : never
    // comments   : The full response of this command must be sent before the engine closes the connection. The
    //              controller must receive the response before the connection is closed on its side.
    case Command (id, CMD.quit, Nil) => StateT.pure[Future, game.State, Response] {
      Response.success (id, Nil)
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
    case Command (id, CMD.boardsize, GtpInt (size) :: Nil) => for {
      gs <- ms.get
      unacceptableSize = StateT.pure[Future, game.State, Response] (Response.failure ("unacceptable size"))
      response <- size match {
        case x if x >= gs.setup.boardSize || gs.board.stones.isEmpty => for {
          _ <- ms.set (gs.copy (setup = gs.setup.copy (boardSize = size)))
        } yield Response.success(id, Nil)
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
    case Command (id, CMD.clear_board, Nil) => for {
      gs <- ms.get
      _ <- ms.set (gs.copy (history = Nil))
    } yield Response.success (id, Nil)


    // komi
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : new_komi ~ float new_komi - New value of komi.
    // effects    : Komi is changed.
    // output     : none
    // fails      : syntax error
    // comments   : The engine must accept the komi even if it should be ridiculous.
    case Command (id, CMD.komi, GtpFloat (komi) :: Nil) => for {
      gs <- ms.get
      _ <- ms.set (gs.copy (setup = gs.setup.copy (komi = komi)))
    } yield Response.success (id, Nil)

    // play
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // arguments  : move ~ move move - Color and vertex of the move
    // effects    : A stone of the requested color is played at the requested vertex. The number of captured stones is
    //              updated if needed and the move is added to the move history.
    // output     : none
    // fails      : syntax error, illegal move. In the latter case, fails with the error message ``illegal move''.
    // comments   : Consecutive moves of the same color are not considered illegal from the protocol point of view.
    case Command (id, CMD.play, GtpColour (colour) :: GtpVertex (vertex) :: Nil) => for {
      gameState <- ms.get
      _ <- gameState match {
        case gs if gs.colourToPlayNext == colour => StateT.pure[Future, game.State, Unit] (())
        case gs => ms.set (gs.copy (history = gs.history :+ game.Turn.create (game.Signal.Pass)))
      }
      gameStateEx <- ms.get
      illegalMove = StateT.pure[Future, game.State, Response] (Response.failure ("illegal move"))
      t = vertex.toString |> game.Turn.create
      response <- (gameStateEx, t) match {
        case (gs, pt) if !gs.isTurnLegal (pt) => illegalMove
        case (gs, pt) => for {
          _ <- ms.set (gs.copy (history = gs.history :+ pt))
        } yield Response.success (id, Nil)
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
    case Command (id, CMD.genmove, GtpColour (colour) :: Nil) => for {
      gameState <- ms.get
      _ <- gameState match {
        case gs if gs.colourToPlayNext == colour => StateT.pure[Future, game.State, Unit] (())
        case gs => ms.set (gs.copy (history = gs.history :+ game.Turn.create (game.Signal.Pass)))
      }
      _ <- engine.takeRandomTurn
      gameStateEx <- ms.get
    } yield gameStateEx.history.lastOption.map (_.action) match {
      case Some (Left (game.Signal.Pass)) => Response.success (id, "pass" :: Nil)
      case Some (Left (game.Signal.Resign)) => Response.success (id, "resign" :: Nil)
      case Some (Right (i)) => Response.success (id, i.toString :: Nil)
      case _ => Response.failure ("unexpected error")
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
    case Command (id, CMD.undo, Nil) => for {
      gameState <- ms.get
      cannotUndo = StateT.pure[Future, game.State, Response] (Response.failure ("cannot undo"))
      response <- gameState.history.reverse match {
        case head :: tail => for {
          _ <- ms.set (gameState.copy (history = tail.reverse))
        } yield Response.success (id, Nil)
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
    case Command (id, CMD.showboard, Nil) => for {
      gs <- ms.get
    } yield Response.success (id, newLine + gs.board.stringify :: Nil)
  }
}



