package com.techmonad.typeclasses.solution3



object Exercise {

  implicit val jsonConverter = new JsonConverter[Person]{
    override def convert(value: Person):JValue = ???
  }

  println("Json : " + JsonWriter.write(Person("Satendra",29, "India")))
}

case class Person(name:String, age:Int, country:String)