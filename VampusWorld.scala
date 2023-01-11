import akka.actor._

case object PingMessage
case object PongMessage
case object StartMessage
case object StopMessage

class Speleologist(navigator: ActorRef) extends Actor {}
class Navigator extends Actor {}

object VampusWorld extends App {
  val system = ActorSystem("VampusWorld")
  val navigator = system.actorOf(Props(new Navigator), name = "navigator")
  val speleologist = system.actorOf(Props(new Speleologist(navigator)), name = "speleologist")
}