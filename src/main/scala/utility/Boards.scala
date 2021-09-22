package utility

import com.akari.types._
import utility.Util.filter_space

object Boards {
  val simple_board: Matrix = List(
    "_ _ _ *".toList.filter(filter_space),
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

  val big_boy: Matrix = List(
    "_____1_____________X".toList,
    "_X____X______X__X___".toList,
    "___2_____20___1___X0".toList,
    "___X__3___1____0____".toList,
    "_XX_X3_X_X_X1__2__21".toList,
    "2_____XX2__X_____X__".toList,
    "__X_X____1__0_XX____".toList,
    "X_02___1____X__X2_X_".toList,
    "_____0_10_1_1_X_____".toList,
    "_XX__X__X__________X".toList,
    "___X____X_2__1___1__".toList,
    "__4___XX_4_____1X_XX".toList,
    "_4__X___X_X___1_2__X".toList,
    "__3XX______0_____X__".toList,
    "_____X_3_XX0____1_1_".toList
  )
}
