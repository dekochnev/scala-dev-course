package ru.otus.module1

import scala.util.Random


final case class BallBox(balls: List[Int]) {
  /**
   * Вытащить один шарик без возврата.
   * Возвращает кортеж (новая коробка, Some(true) если чёрный (0), Some(false) если белый (1)).
   * Если коробка пуста — (this, None).
   */
  def pullOne(using r: Random): (BallBox, Option[Boolean]) =
    if balls.isEmpty then (this, None)
    else
      val idx = r.nextInt(balls.size)
      val (before, afterWithHead) = balls.splitAt(idx)
      val ball :: after = afterWithHead // safe: isEmpty уже проверен
      val isBlack = ball == 0   // 0 = чёрный, 1 = белый
      (BallBox(before ++ after), Some(isBlack))

  def pullFirstIsBlack(using r: Random): Boolean =
    pullOne(using r) match
      case (_, Some(isBlack)) => isBlack
      case (_, None) => false

  /**
   * Вытащить два шарика без возврата: 1-й чёрный и 2-й белый (это событие AB)
   * Возвращает true, если условие задачи (1 чёрный, 2 белый) выполнено, иначе false.
   */
  def pullTwoBlackThenWhite(using r: Random): Boolean =
    val (boxAfter1, firstOpt) = this.pullOne(using r)
    firstOpt match
      case None => false
      case Some(firstIsBlack) if !firstIsBlack => false // первый не чёрный
      case Some(_) =>
        val (_, secondOpt) = boxAfter1.pullOne(using r)
        secondOpt match
          case None => false
          case Some(secondIsBlack) => !secondIsBlack // второй должен быть белым
}

object BallBox {
  def fixedBlackWhiteBox: BallBox = BallBox(List(0, 0, 0, 1, 1, 1))
}

/*
class BallsExperiment {
  private val shuffleBin = Random.shuffle(List[Boolean](true, true, true, false, false, false))

  def isFirstBlackSecondWhite: Boolean = shuffleBin(0) == false && shuffleBin(1) == true

  def isFirstBlack: Boolean = shuffleBin(0) == 0

  def get_bin(): List[Int] = shuffleBin
}
*/

/*
class Experiment(num_of_exp: Int = 0) {
  private val num_of_experiments: Int = num_of_exp
  private var prob_A: Float = 0       // P(A), событие A - появление чёрного шара при первом испытании
                                      // событие B - появление белого шара при втором испытании
  private var prob_AB: Float = 0      // P(AB), событие AB - в первом испытании появится черный шар, а во втором — белый
  private var probability: Float = 0  // условная вероятность P(A|B) = P(AB) / P(A)

  def calculate(): Float = {
    // проводим все эксперименты
    val experiments = (0 until num_of_experiments).toList.map(_ => new BallsExperiment)
    // кол-во событий A (1-й шар чёрный) / кол-во экспериментов
    prob_A = experiments.map(e => e.isFirstBlack).count(_ == true).toFloat / num_of_experiments
    // кол-во событий одновременного возникновения A и B = AB (1-й шар черный, а 2-й шар белый) / кол-во экспериментов
    prob_AB = experiments.map(e => e.isFirstBlackSecondWhite).count(_ == true).toFloat / num_of_experiments
    // итоговая вероятность по формуле условной вероятности
    probability = prob_AB / prob_A
    probability
  }

  def show_results(): Unit = {
    val bin = (new BallsExperiment).get_bin()
    println(s"")
    println(s"Задача:\n  Дана урна с шарами: ${bin}.\n  Какова условная вероятность вытащить из урны 1-й шар чёрный и 2-й шар белый?\n")
    if (prob_A != 0) {
      println(s"Проведено экспериментов: ${num_of_experiments}")
      println(s"Вероятность A: ${prob_A}")
      println(s"Вероятность AB: ${prob_AB}")
      println(s"Условная вероятность P(A|B) = P(AB) / P(A) = ${probability}\n")
    }
  }
}
*/