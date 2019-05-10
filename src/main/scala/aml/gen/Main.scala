package aml.gen

import amf.core.client.ExitCodes
import aml.gen.commands.{CommandParser, GenerateCommand}

import scala.concurrent.Await
import scala.concurrent.duration._

object Main extends App {
  try {
    CommandParser.parse(args) match {
      case Some(cfg) => Await.result(GenerateCommand(cfg).execute(), 5 minutes)
      case _         => System.exit(ExitCodes.WrongInvocation)
    }
    System.exit(ExitCodes.Success)
  } catch {
    case e: Exception =>
      System.err.println(e.getMessage)
      System.exit(ExitCodes.Success)
  }
}
