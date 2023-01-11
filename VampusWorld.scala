import akka.actor._

case object WhereAmIMessage
case object WhatIShouldDoMessage
case object Action
case object LocationInfo


class Speleologist(navigator: ActorRef, environment: ActorRef) extends Actor {
  def receive = {}
}
class Navigator extends Actor {
  def receive = {}
}
class Environment extends Actor {
  def receive = {}
}

object VampusWorld extends App {
  val system = ActorSystem("VampusWorld")
  val navigator = system.actorOf(Props(new Navigator), name = "navigator")
  val environment = system.actorOf(Props(new Environment), name = "environment")
  val speleologist = system.actorOf(Props(new Speleologist(navigator, environment)), name = "speleologist")
}