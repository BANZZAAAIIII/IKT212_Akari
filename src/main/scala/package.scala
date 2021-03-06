package com.akari

//https://stackoverflow.com/questions/3400734/package-objects
package object types {

    // Define globals here
    lazy val Wall  = 'X'
    lazy val Empty = '_'
    lazy val Light = '*'
    lazy val Zero  = '0'
    lazy val One   = '1'
    lazy val Two   = '2'
    lazy val Three = '3'
    lazy val Four  = '4'
    // Define type aliases here
    type Row = List[Char]
    type Matrix = List[Row]

    case class Position (row: Int, col:Int)
}