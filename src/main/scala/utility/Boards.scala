package utility

import com.akari.types._
import utility.Util.filter_space

object Boards {
  val simple_board: Matrix = List(
    "_ 1 _ _".toList.filter(filter_space),
    "_ X X _".toList.filter(filter_space),
    "_ 2 _ _".toList.filter(filter_space),
    "_ _ 3 X".toList.filter(filter_space),
    "_ _ _ _".toList.filter(filter_space),
    "_ _ 2 _".toList.filter(filter_space)
  )

  val board7x7: Matrix = List(
    "_ _ _ _ _ _ _".toList.filter(filter_space),
    "2 _ _ _ _ _ _".toList.filter(filter_space),
    "_ 1 _ 2 0 _ _".toList.filter(filter_space),
    "X _ _ _ _ _ _".toList.filter(filter_space),
    "X _ _ _ _ _ _".toList.filter(filter_space),
    "_ _ _ _ _ 0 _".toList.filter(filter_space),
    "2 _ X _ _ _ _".toList.filter(filter_space)
  )

  val id_7x7_a2Bj0fBc3cBf1j10a: Matrix = List(
    "_ 2 X _ _ _ _".toList.filter(filter_space),
    "_ _ _ _ _ _ 0".toList.filter(filter_space),
    "_ _ _ _ _ _ X".toList.filter(filter_space),
    "_ _ _ 3 _ _ _".toList.filter(filter_space),
    "X _ _ _ _ _ _".toList.filter(filter_space),
    "1 _ _ _ _ _ _".toList.filter(filter_space),
    "_ _ _ _ 1 0 _".toList.filter(filter_space)
  )


  val id_7x7_c3mBc2a2a2a0c2m0c: Matrix = List(
    "_ _ _ 3 _ _ _".toList.filter(filter_space),
    "_ _ _ _ _ _ _".toList.filter(filter_space),
    "_ _ _ 2 _ _ _".toList.filter(filter_space),
    "2 _ 2 _ 2 _ 0".toList.filter(filter_space),
    "_ _ _ 2 _ _ _".toList.filter(filter_space),
    "_ _ _ _ _ _ _".toList.filter(filter_space),
    "_ _ _ 0 _ _ _".toList.filter(filter_space)
  )

  val id_4x8_3AaBa3d1a3c0nBb: Matrix = List(
    "_ X _ 3 _ _ _ _ ".toList.filter(filter_space),
    "1 _ 3 _ _ _ 0 _ ".toList.filter(filter_space),
    "_ _ _ _ _ _ _ _ ".toList.filter(filter_space),
    "_ _ _ _ _ X _ _ ".toList.filter(filter_space)
  )

  val deterministic_numbers_test: Matrix = List(
    "_ X _ 3 _ _ 0 _ ".toList.filter(filter_space),
    "2 _ 3 _ _ _ _ _ ".toList.filter(filter_space),
    "_ _ _ _ _ _ _ _ ".toList.filter(filter_space),
    "_ _ _ _ _ _ 4 _ ".toList.filter(filter_space),
    "_ _ _ _ _ _ _ _ ".toList.filter(filter_space)
  )


}
