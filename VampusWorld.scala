import akka.actor._
import scala.util.Random

case object StartMessage
case object WhereAmIMessage
case class WhatIShouldDoMessage(locationDescription: String)
case class ActionMessage(action: String)
case class LocationInfoMessage(location: String)
case class StopMessage(result: Int)


class Speleologist(navigator: ActorRef, environment: ActorRef) extends Actor {
  val pArray = Array("It looks like a pit", "Here is a pit", "I see a pit");
  val bArray = Array("I feel breeze here", "There is a breeze", "It`s a cool breeze here");
  val sArray = Array("I feel snatch here", "There is a snatch", "It`s a snatch here");
  val wArray = Array("I see a wampus", "A wampus is here", "Oh, it`s wampus");
  val gArray = Array("I found gold", "A lot of gold", "It looks like gold");
  val nArray = Array("I found nothing", "Its nothing here", "Pretty quiet, nothing");
  def receive = {
    case StartMessage =>
        println("Speleologist get StartMessage message")
        environment ! WhereAmIMessage
    case StopMessage(result) =>
        println("Speleologist get StopMessage message")
        result match {
          case 0 => println("Gold was not found...")
          case 1 => println("Gold was found!!!")
        }
        context.stop(self)
    case LocationInfoMessage(location) =>
        var locationDescription: String = "";
        println("Location: " + location)
        location match {
          case "P" => locationDescription = Random.shuffle(pArray.toList).head
          case "B" => locationDescription = Random.shuffle(bArray.toList).head
          case "S" => locationDescription = Random.shuffle(sArray.toList).head
          case "W" => locationDescription = Random.shuffle(wArray.toList).head
          case "G" => locationDescription = Random.shuffle(gArray.toList).head
          case whoa => locationDescription = Random.shuffle(nArray.toList).head
        }

        println("Speleologist get LocationInfo message" + " location: " + location)
        navigator ! WhatIShouldDoMessage(locationDescription)
    case ActionMessage(actionDescription) =>
        var action = ""
        println("Speleologist get Action message" + " action: " + action)
        if actionDescription contains "left" then
          action = "L"
        if actionDescription contains "right" then
          action = "R"
        if actionDescription contains "shoot" then
          action = "S"
        if actionDescription contains "forward" then
          action = "F"
        if actionDescription contains "climb" then
          action = "C"
        if actionDescription contains "grab" then
          action = "G"
       
        environment ! ActionMessage(action)
  }
}
class Navigator extends Actor {
  var isGoldFound: Boolean = false
  def receive = {
    case WhatIShouldDoMessage(locationDescription) => 
        var actionDescription: String = ""
        println("Navigator get WhatIShouldDoMessage message" + " location description: " + locationDescription)
        if locationDescription.contains("pit") then
          actionDescription = "Turn right"
        if locationDescription.contains("breeze") then
          actionDescription = "Turn left"
        if locationDescription.contains("snatch") then
          actionDescription = "Turn left"
        if locationDescription.contains("wampus") then
          actionDescription = "Go shoot him!"
        if locationDescription.contains("gold") then
          actionDescription = "Try to grab it"
          isGoldFound = true
        if locationDescription.contains("nothing") == true then
          actionDescription = "Go forward"
          if isGoldFound == true then
            actionDescription = "Lets climb out of here"
            sender() ! ActionMessage(actionDescription)
            context.stop(self)
          else 
            actionDescription = "Go forward"

        sender() ! ActionMessage(actionDescription)
  }
}
class Environment extends Actor {
  var isGoldCollected: Boolean = false;
  var speleologistCoords = Array(0,0)
  var cave = Array.ofDim[String](4,4)
  cave(0)(0) = "";
  cave(0)(1) = "B";
  cave(0)(2) = "P";
  cave(0)(3) = "B";
  cave(1)(0) = "S";
  cave(1)(1) = "";
  cave(1)(2) = "B";
  cave(1)(3) = "";
  cave(2)(0) = "W";
  cave(2)(1) = "B,S,G";
  cave(2)(2) = "P";
  cave(2)(3) = "B";
  cave(3)(0) = "S";
  cave(3)(1) = "";
  cave(3)(2) = "B";
  cave(3)(3) = "P";
  def receive = {
    case WhereAmIMessage => 
        println("Environment get WhereAmIMessage message")
        sender() ! LocationInfoMessage(cave(speleologistCoords(0))(speleologistCoords(1)))
    case ActionMessage(action) =>
        println("Environment get ActionMessage message" + " action: " + action)
        action match {
          case "L" => 
            if speleologistCoords(1) > 0 then
              speleologistCoords(1) = speleologistCoords(1) - 1
          case "R" => 
            if speleologistCoords(1) < 3 then
              speleologistCoords(1) = speleologistCoords(1) + 1
          case "S" => 
            if cave(speleologistCoords(0))(speleologistCoords(1)) == "W" then
              println("Wampus is dead")
          case "F" => 
            if speleologistCoords(0) < 3 then
              speleologistCoords(0) = speleologistCoords(0) + 1
          case "C" => 
            if isGoldCollected == true then
              sender() ! StopMessage(1)
            else
              sender() ! StopMessage(0)
            context.stop(self)
          case "G" => 
            if cave(speleologistCoords(0))(speleologistCoords(1)) == "G" then 
              println("Gold is collected")
              isGoldCollected = true
        }
        sender() ! LocationInfoMessage(cave(speleologistCoords(0))(speleologistCoords(1)))
  }
}

object VampusWorld extends App {
  val system = ActorSystem("VampusWorld")
  val navigator = system.actorOf(Props(new Navigator), name = "navigator")
  val environment = system.actorOf(Props(new Environment), name = "environment")
  val speleologist = system.actorOf(Props(new Speleologist(navigator, environment)), name = "speleologist")

  speleologist ! StartMessage
}