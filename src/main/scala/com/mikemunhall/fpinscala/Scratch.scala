package com.mikemunhall.fpinscala

import ch4._

object Scratch extends App {

  case class Employee(name: String, department: String)

  def lookupByName(name: String): Option[Employee] = Some(Employee(name, "engineering"))

  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)

  println(joeDepartment)
}
