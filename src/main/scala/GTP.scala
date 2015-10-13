package hayago

import cats.std.all._
import java.util.Properties
import cats.syntax.eq._
import scala.util._

object GTP {

  /*
protocol_version
name
version
known_command
list_commands
quit
boardsize
clear_board
komi
play
genmove


object gtp_all_legal extends Command ("all_legal")
object gtp_attack extends Command ("attack")
object gtp_playblack extends Command ("black")
object gtp_set_boardsize extends Command ("boardsize")
object gtp_what_color extends Command ("color")
object gtp_combination_attack extends Command ("combination_attack")
object gtp_countlib extends Command ("countlib")
object gtp_debug_influence extends Command ("debug_influence")
object gtp_debug_move_influence extends Command ("debug_move_influence")
object gtp_decrease_depths extends Command ("decrease_depths")
object gtp_defend extends Command ("defend")
object gtp_dragon_data extends Command ("dragon_data")
object gtp_dragon_status extends Command ("dragon_status")
object gtp_dump_stack extends Command ("dump_stack")
object gtp_echo extends Command ("echo")
object gtp_estimate_score extends Command ("estimate_score")
object gtp_eval_eye extends Command ("eval_eye")
object gtp_final_score extends Command ("final_score")
object gtp_final_status extends Command ("final_status")
object gtp_final_status_list extends Command ("final_status_list")
object gtp_findlib extends Command ("findlib")
object gtp_fixed_handicap extends Command ("fixed_handicap")
object gtp_genmove_black extends Command ("genmove_black")
object gtp_genmove_white extends Command ("genmove_white")
object gtp_get_life_node_counter extends Command ("get_life_node_counter")
object gtp_get_owl_node_counter extends Command ("get_owl_node_counter")
object gtp_get_reading_node_counter extends Command ("get_reading_node_counter")
object gtp_get_trymove_counter extends Command ("get_trymove_counter")
object gtp_genmove extends Command ("gg_genmove")
object gtp_help extends Command ("help")
object gtp_increase_depths extends Command ("increase_depths")
object gtp_influence extends Command ("influence")
object gtp_is_legal extends Command ("is_legal")
object gtp_set_komi extends Command ("komi")
object gtp_set_level extends Command ("level")
object gtp_loadsgf extends Command ("loadsgf")
object gtp_move_influence extends Command ("move_influence")
object gtp_name extends Command ("name")
object gtp_new_score extends Command ("new_score")
object gtp_owl_attack extends Command ("owl_attack")
object gtp_owl_defend extends Command ("owl_defend")
object gtp_popgo extends Command ("popgo")
object gtp_captures extends Command ("captures")
object gtp_protocol_version extends Command ("protocol_version")
object gtp_quit extends Command ("quit")
object gtp_report_uncertainty extends Command ("report_uncertainty")
object gtp_reset_life_node_counter extends Command ("reset_life_node_counter")
object gtp_reset_owl_node_counter extends Command ("reset_owl_node_counter")
object gtp_reset_reading_node_counter extends Command ("reset_reading_node_counter")
object gtp_reset_trymove_counter extends Command ("reset_trymove_counter")
object gtp_same_dragon extends Command ("same_dragon")
object gtp_showboard extends Command ("showboard")
object gtp_top_moves_black extends Command ("top_moves_black")
object gtp_top_moves_white extends Command ("top_moves_white")
object gtp_trymove extends Command ("trymove")
object gtp_tune_move_ordering extends Command ("tune_move_ordering")
object gtp_undo extends Command ("undo")
object gtp_version extends Command ("version")
object gtp_playwhite extends Command ("white")
object gtp_worm_cutstone extends Command ("worm_cutstone")
object gtp_worm_data extends Command ("worm_data")

sealed case class GtpStatus (code: Int)
object GTP_QUIT extends GtpStatus (-1)
object GTP_OK extends GtpStatus (0)
object GTP_FATAL extends GtpStatus (1)
*/


  /*
An engine is expected to keep track of the following state information:

board size
board configuration
number of captured stones of either color
move history
komi
time settings
*/


  type GtpCommandHandler = PartialFunction[GtpCommand, GtpResponse]


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
        GtpCommandHandlers.all.collectFirst { case x if x.isDefinedAt(cmd) => x } match {
          case Some(handler) => handler.apply(cmd)
          case None => GtpResponse.failure("?")
        }

      case None => GtpResponse.failure("?")
    }

    response match {
      case Some(r) => println(r); true
      case None => false
    }
  }


  case class BoardConfig()

  case class GameState(boardSize: Int, boardConfig: BoardConfig)


  //sealed case class GtpCommand (id: Option[Int], cmd: String, args: List[String])

  object GtpCommandHandlers {
    val protocolVersionHandler: PartialFunction[GtpCommand, GtpResponse] = {
      case GtpCommand(id, "protocol_version", Nil) =>
        GtpResponse.success(id, "2" :: Nil)
    }

    val nameHandler: PartialFunction[GtpCommand, GtpResponse] = {
      case GtpCommand(id, "name", Nil) =>
        GtpResponse.success(id, "Hayago Engine v0.0.1" :: Nil)
    }

    val all =
      GtpCommandHandlers.protocolVersionHandler ::
        GtpCommandHandlers.nameHandler ::
        Nil
  }

  //sealed trait GtpCommand { def id: Option[Int]; def run: GtpResponse }

  case class GtpCommand(id: Option[Int], cmd: String, args: List[String])

  object GtpCommand {

    private object ID {
      def unapply(str: String): Option[Int] = Try(str.toInt).toOption
    }

    def unapply(str: String): Option[GtpCommand] = str.split("\\s+").toList match {
      case ID(id) :: cmd :: args => Some(GtpCommand(Some(id), cmd, args))
      case cmd :: args => Some(GtpCommand(None, cmd, args))
      case _ => None
    }
  }

  /*{

  def unapply (id: Option[Int], cmd: String, args: List[String]): Option[GtpCommand] = (cmd, args) match {
    case ("protocol_version", Nil) => Some (GtpCommand_ProtocolVersion (id))
    case _ => None
  }

  def unapply (str: String): Option[GtpCommand] =
}*/

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

}
