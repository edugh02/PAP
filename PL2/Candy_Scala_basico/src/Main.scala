import scala.util.Random
import java.util.Date

object Main {

  def esVacia(l: List[Int]): Boolean = l match {
    case Nil => true
    case _ => false
  }

  def contiene(lst: List[Int], n: Int): Boolean = {
    if (lst.isEmpty) {
      false
    } else if (lst.head == n) {
      true
    } else {
      contiene(lst.tail, n)
    }
  }

  def agregarElemento[A](elemento: A, lista: List[A]): List[A] = lista match {
    case Nil => elemento :: Nil
    case cabeza :: cola => elemento :: cabeza :: cola
  }

  def getElem( matriz: List[Int],index: Int): Int = { //empezando por el índice 0
    if (index == 0) matriz.head
    else getElem(matriz.tail,(index - 1))
  }

  def deja_n(l: List[Int],n: Int): List[Int] = { //devuelve una nueva lista con los n primeros
    if (n == 0) Nil
    else agregarElemento(l.head, deja_n(l.tail,(n - 1)))
  }


  def quita_n(l: List[Int],n: Int): List[Int] = { // quita los n primeros
    if (n == 1) l.tail
    else quita_n( l.tail,(n - 1))
  }
  def imprimirMatriz(matriz: List[Int], n: Int, m: Int): Unit = {
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
      case _ =>
        imprimirFila(deja_n(matriz,m))
        imprimirMatriz(quita_n(matriz,m), n, m)
    }
  }


  def cuantas_vidas(vidasAntes: Int,quitarVida: Boolean): Int = {
    if(quitarVida) vidasAntes-1
    else vidasAntes
  }

  def eliminarElementos(matriz: List[Int], vector: List[Int], posicion: Int, fila:Int, columna:Int): List[Int] = {
    if(posicion==fila*columna) Nil
    else if (contiene(vector,posicion)) agregarElemento(-1,eliminarElementos(matriz, vector, posicion+1,fila,columna))
    else agregarElemento(getElem(matriz,posicion),eliminarElementos(matriz, vector, posicion+1,fila,columna))
  }

  def crearMatrizAleatoria(posicion: Int, fila:Int, columna:Int, lim_inf: Int, lim_sup: Int): List[Int] = {
    val random = new Random()
    val aleatorio = random.nextInt(lim_sup - lim_inf + 1) + lim_inf
    if (posicion == fila*columna) Nil
    else agregarElemento(aleatorio, crearMatrizAleatoria(posicion + 1, fila,columna,lim_inf, lim_sup))
  }

  def main(args: Array[String]): Unit = {
    val rnd = new Random(new Date().getTime())
    val tecla: Char = ' '
    println(contiene(List(1,2,3,4,5),7))
    imprimirMatriz(crearMatrizAleatoria(0,6,7,1,6),6,7)
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