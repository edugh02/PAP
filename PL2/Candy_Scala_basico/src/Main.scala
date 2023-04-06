import scala.util.Random
import java.util.Date
import scala.util.Random

object Main {
  def createRandomList(n: Int, m: Int, lim_inf: Int, lim_sup: Int): List[Int] = {
    val r = new Random()
    val range = lim_inf to lim_sup
    List.fill(n*m)(r.nextInt(range.length)).map(i => range(i))
  }

  def esVacia(l: List[Int]): Boolean = l match {
    case Nil => true
    case _ => false
  }


  def generarMatriz(n: Int, m: Int, lim_inf: Int, lim_sup: Int): List[List[Int]] = {
    val random = new Random()
    val rango = lim_sup - lim_inf + 1
    List.tabulate(n, m)((_, _) => random.nextInt(rango) + lim_inf)
  }

  def imprimirMatriz(matriz: List[List[Int]], n: Int, m: Int): Unit = {
    def imprimirFila(fila: List[Int]): Unit = {
      if (esVacia(fila)) {
        println("|")
      } else {
        print(s"|${fila.head}")
        imprimirFila(fila.tail)
      }
    }

    matriz match {
      case Nil =>
        println("---------")
      case fila :: resto =>
        imprimirFila(fila.take(m))
        imprimirMatriz(resto, n, m)
    }
  }

  def cuantas_vidas(vidasAntes: Int,quitarVida: Boolean): Int = {
    if(quitarVida) vidasAntes-1
    else vidasAntes
  }


  def main(args: Array[String]): Unit = {
    val rnd = new Random(new Date().getTime())
    val tecla: Char = ' '

    imprimirMatriz(generarMatriz(6,7,1,6),6,7)
    //Obtencion de valores de modo de juego
    println("Bienvenido a Cundio Crack")
    println("Introduce el modo de juego con el que quieres jugar: ")
    println(" 1. Automatico")
    println(" 2. Manual")
    val modo: Int  = scala.io.StdIn.readInt()  //automático o manual
    println("Introduce la dificultad con la que quieres jugar: ")
    println(" 1. Facil")
    println(" 2. Normal")
    val dificultad: Int  = scala.io.StdIn.readInt()  //dificultad del juego
    println("Introduce el numero de filas que quieres que tenga el tablero:")
    val n: Int  = scala.io.StdIn.readInt() // número de filas
    println("Introduce el numero de columnas que quieres que tenga el tablero:")
    val m: Int  = scala.io.StdIn.readInt() //número de columnas

    var lim_inf: Int = 1 // valor mínimo
    var lim_sup: Int = 6 // valor máximo
    if (dificultad == 1) {
      lim_sup = 4 // valor máximo
    }



  }
}