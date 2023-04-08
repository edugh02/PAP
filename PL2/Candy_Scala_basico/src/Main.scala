import scala.util.Random
import java.util.Date

object Main {

  // funcion auxiliar para saber si una lista esta vacia
  def esVacia(l: List[Int]): Boolean = l match {
    case Nil => true
    case _ => false
  }
// funcion auxiliar para saber si un elemento esta en una lista
  def contiene(lst: List[Int], n: Int): Boolean = {
    if (lst.isEmpty) {
      false
    } else if (lst.head == n) {
      true
    } else {
      contiene(lst.tail, n)
    }
  }
// funcion auxiliar para calcular la longitud de una lista
  def longitud[A](lista: List[A]): Int = {
    def auxiliar(lista: List[A], contador: Int): Int = lista match {
      case Nil => contador
      case _ :: resto => auxiliar(resto, contador + 1)
    }

    auxiliar(lista, 0)
  }

// es una funcion auxiliar para agregar elementos a una lista (es lo mismo que ::)
  def agregarElemento[A](elemento: A, lista: List[A]): List[A] = lista match {
    case Nil => elemento :: Nil
    case cabeza :: cola => elemento :: cabeza :: cola
  }
  //función auxiliar para coger el elemento de la posicion index de una lista
  def getElem( matriz: List[Int],index: Int): Int = { //empezando por el índice 0
    if (index == 0) matriz.head
    else getElem(matriz.tail,(index - 1))
  }
//función auxiliar identica a take
  def deja_n(l: List[Int],n: Int): List[Int] = { //devuelve una nueva lista con los n primeros
    if (n == 0) Nil
    else agregarElemento(l.head, deja_n(l.tail,(n - 1)))
  }
  //función auxiliar identica a drop
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
//función para eliminar todos los elementos de la matriz cuyas posiciones estén en el vector
  def eliminarElementos(matriz: List[Int], vector: List[Int], posicion: Int, filas:Int, columnas:Int): List[Int] = {
    if(posicion==filas*columnas) Nil
    else if (contiene(vector,posicion)) agregarElemento(-1,eliminarElementos(matriz, vector, posicion+1,filas,columnas))
    else agregarElemento(getElem(matriz,posicion),eliminarElementos(matriz, vector, posicion+1,filas,columnas))
  }
//función para crear una matriz de tamaño n*m con valores aleatorios entre lim_inf y lim_sup
  def crearMatrizAleatoria(posicion: Int, filas:Int, columnas:Int, lim_inf: Int, lim_sup: Int): List[Int] = {
    val random = new Random()
    val aleatorio = random.nextInt(lim_sup - lim_inf + 1) + lim_inf
    if (posicion == filas*columnas) Nil
    else agregarElemento(aleatorio, crearMatrizAleatoria(posicion + 1, filas,columnas,lim_inf, lim_sup))
  }

  //función que, aleatoriamente, pone a -1 la columna o la fila entera de la posicion en la que se encuentra la bomba
  def explotarBomba(matriz: List[Int], filas: Int, columnas: Int, filaObjetivo: Int, columnaObjetivo: Int): List[Int] = {
    val random = new Random()
    val fila_o_columna: Int = random.nextInt(2)
    if (fila_o_columna == 1) {
      explotarBombaColumna(matriz, columnaObjetivo,filas,columnas,0,0)
    } else {
      explotarBombaFila(matriz,columnas, filaObjetivo,0)
    }
  }

  // función que pone todos los elementos de la columna en la que se encuentra la bomba a -1
  def explotarBombaColumna(matriz: List[Int], columnaObjetivo: Int, filas: Int, columnas: Int, n: Int, m: Int): List[Int] = {
    if (n>filas-1 || m>columnas-1) Nil
    else if (m == columnaObjetivo && m == columnas - 1) agregarElemento(-1, explotarBombaColumna(matriz, columnaObjetivo, filas, columnas, n + 1, 0))
    else if (m == columnaObjetivo) agregarElemento(-1, explotarBombaColumna(matriz, columnaObjetivo, filas, columnas, n, m + 1))
    else if (m == columnas - 1) agregarElemento(getElem(matriz, n*columnas + m), explotarBombaColumna(matriz, columnaObjetivo, filas, columnas, n + 1, 0))
    else agregarElemento(getElem(matriz, n * columnas + m), explotarBombaColumna(matriz, columnaObjetivo, filas, columnas, n, m + 1))
  }


  // función que pone todos los elementos de la fila en la que se encuentra la bomba a -1
  def explotarBombaFila(matriz: List[Int],columnas: Int, filaObjetivo: Int, posicion: Int): List[Int] = {
    if (posicion==longitud(matriz)) Nil
    else if (posicion/columnas==filaObjetivo) agregarElemento(-1,explotarBombaFila(matriz,columnas,filaObjetivo,posicion+1))
    else agregarElemento(getElem(matriz,posicion),explotarBombaFila(matriz,columnas,filaObjetivo,posicion+1))
  }

//función que pone a -1 los elementos de la matriz que estén en la misma fila o columna que el elemento en la posición indicada
  def explotarRompecabezas(matriz: List[Int], filas: Int, columnas: Int, elem: Int ,posicionActual: Int): List[Int] = {
    println("entrando a explotar")
    if (posicionActual==filas*columnas) Nil
    else if (elem == getElem(matriz, posicionActual)) agregarElemento(-1,explotarRompecabezas(matriz,filas,columnas,elem,posicionActual+1))
    else agregarElemento(getElem(matriz,posicionActual),explotarRompecabezas(matriz,filas,columnas,elem,posicionActual+1))
  }

  def explotarTNT(matriz: List[Int], filas: Int, columnas: Int, filaObjetivo: Int, columnaObjetivo: Int, x: Int, y: Int): List[Int] = {
    if (x > filas - 1 || y > columnas - 1) List()
    else if (x <= filaObjetivo + 4 && x >= filaObjetivo - 4 && y <= columnaObjetivo + 4 && y >= columnaObjetivo - 4) {
      if (y == columnas - 1) agregarElemento(-1, explotarTNT(matriz, filas, columnas, filaObjetivo, columnaObjetivo, x + 1, 0))
      else agregarElemento(-1, explotarTNT(matriz, filas, columnas, filaObjetivo, columnaObjetivo, x, y + 1))
    } else if (y == columnas - 1) agregarElemento(getElem(matriz, x*columnas + y), explotarTNT(matriz, filas, columnas, filaObjetivo, columnaObjetivo, x + 1, 0))
    else agregarElemento(getElem(matriz, x*columnas + y), explotarTNT(matriz, filas, columnas, filaObjetivo, columnaObjetivo, x, y + 1))
  }


  def primerVacio(vector: List[Int], n: Int, m: Int, pos: Int = 0): Int = {
    if (getElem(vector,pos) == -1) pos
    else if (pos == n * m - 1) -99 // no hay más elementos vacíos
    else primerVacio(vector, n, m, pos + 1)
  }

  def estaEnVector(vector: List[Int], filas: Int, columnas: Int, posicionCandy: Int, posicionActual: Int=0): Boolean = {
    if (posicionActual == longitud(vector)) false
    else if (getElem(vector,posicionActual) == posicionCandy) true
    else estaEnVector(vector, filas, columnas,posicionCandy, posicionActual + 1)
  }

  def reemplazarEnPosicion[A](lista: List[A], pos: Int, elem: A): List[A] = {
    if (lista.isEmpty) Nil // Si la lista está vacía, no hay nada que reemplazar
    else if (pos == 0) elem :: lista.tail // Si estamos en la posición 0, reemplazamos el primer elemento
    else lista.head :: reemplazarEnPosicion(lista.tail, pos - 1, elem) // En otro caso, seguimos avanzando por la lista
  }


  //crear un vector de longitud filas*columnas y poner todos los elementos a -1
  def crearVector(filas: Int, columnas: Int, posicion:Int=0): List[Int] = {
    if (filas*columnas==posicion) Nil
    else agregarElemento(-1,crearVector(filas,columnas,posicion+1))
  }
  def ver_candy(matriz: List[Int], filas: Int, columnas: Int, filaObjetivo: Int, columnaObjetivo: Int, vector: List[Int], elemento: Int): List[Int] = {
    val caramelo = filaObjetivo * columnas + columnaObjetivo // Posición en la matriz de las coordenadas

    // Comprobamos que la posición no ha sido ya insertada
    if (!estaEnVector(vector, filas, columnas, caramelo) && getElem(matriz,caramelo) == elemento) {
      // Insertamos en la primera posición que se encuentre vacía del vector, la posición del caramelo
      val pos = primerVacio(vector, filas, columnas)
      val vectorCambiado = reemplazarEnPosicion(vector, pos, caramelo)
      if (filaObjetivo != 0) { // Adyacente de arriba
        ver_candy(matriz, filas, columnas,  filaObjetivo - 1, columnaObjetivo, vectorCambiado, elemento)
      }
      if (filaObjetivo != filas - 1) { // Adyacente de abajo
        ver_candy(matriz, filas, columnas, filaObjetivo + 1, columnaObjetivo, vectorCambiado, elemento)
      }
      if (columnaObjetivo != 0) { // Adyacente de la izquierda
        ver_candy(matriz, filas, columnas, filaObjetivo, columnaObjetivo - 1, vectorCambiado, elemento)
      }
      if (columnaObjetivo != columnas - 1) { // Adyacente de la derecha
        ver_candy(matriz, filas, columnas, filaObjetivo, columnaObjetivo + 1, vectorCambiado, elemento)
      }
      vectorCambiado
    }
    else vector
  }


  //función principal que determina la posición a investigar ejecuta las acciones correspondientes
  def jugar(vidas: Int, modo: Int, dificultad: Int, filas: Int, columnas: Int, lim_inf: Int, lim_sup: Int): Unit = {
    val matriz: List[Int] = crearMatrizAleatoria(0, filas, columnas, lim_inf, lim_sup)
    imprimirMatriz(matriz, filas, columnas)
    if (vidas == 0) {
      println("Has perdido, te has quedado sin vidas")
    } else {
      if (modo == 2) { //obtencion de filas y columnas por parte del usuario
        println("Introduce la fila de la casilla que quieres revisar: ")
        val filaObjetivo: Int = scala.io.StdIn.readInt()
        println("Introduce la columna de la casilla que quieres revisar: ")
        val columnaObjetivo: Int = scala.io.StdIn.readInt()
        val posicion: Int = (filaObjetivo) * columnas + (columnaObjetivo )
        //comprobamos si las coordenadas están dentro del rango de la matriz
        if (columnaObjetivo > columnas - 1 || filaObjetivo > filas - 1 || columnaObjetivo < 0 || filaObjetivo < 0) {
          println("\nCOORDENADAS NO VALIDAS, introduce unas coordenadas dentro del rango\n\n")
          jugar(vidas, modo, dificultad, filas, columnas, lim_inf, lim_sup)
        }
        val vector=crearVector(filas,columnas)
        val elemento = getElem(matriz,posicion); //caramelo en las coordenadas indicadas
        imprimirMatriz(ver_candy(matriz,filas,columnas,filaObjetivo,columnaObjetivo,vector,elemento), filas, columnas)
        //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      }
      else {
        val random = new Random()
        //numeros aleatorios menores que filas y columnas
        val filaObjetivo: Int = random.nextInt(filas) + 1
        println("Fila escogida: " + filaObjetivo)
        val columnaObjetivo: Int = random.nextInt(columnas) + 1
        println("Columna escogida: " + columnaObjetivo)
        val posicion: Int = (filaObjetivo - 1) * columnas + (columnaObjetivo - 1)
        println("Pulsa enter para continuar")
        scala.io.StdIn.readLine()
        //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        jugar(vidas, modo, dificultad, filas, columnas, lim_inf, lim_sup)
      }

    }
  }


  def main(args: Array[String]): Unit = {
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
    val filas: Int  = scala.io.StdIn.readInt() // número de filas
    println("Introduce el numero de columnas que quieres que tenga el tablero:")
    val columnas: Int  = scala.io.StdIn.readInt() //número de columnas

    val lim_inf: Int = 1 // valor mínimo
    if (dificultad == 1) {
      val lim_sup = 4 // valor máximo
      jugar(5,modo,dificultad,filas,columnas,lim_inf,lim_sup)
    }else{
      val lim_sup: Int = 6 // valor máximo
      jugar(5,modo,dificultad,filas,columnas,lim_inf,lim_sup)
    }
  }
}