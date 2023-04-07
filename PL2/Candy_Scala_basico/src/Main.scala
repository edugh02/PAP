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

  def longitud[A](lista: List[A]): Int = {
    def auxiliar(lista: List[A], contador: Int): Int = lista match {
      case Nil => contador
      case _ :: resto => auxiliar(resto, contador + 1)
    }

    auxiliar(lista, 0)
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

  //aleatoriamente, pone a -1 la columna o la fila entera de la posicion en la que se encuentra la bomba
  def explotarBomba(matriz: List[Int], filas: Int, columnas: Int, filaObjetivo: Int, columnaObjetivo: Int): List[Int] = {
    val random = new Random()
    val fila_o_columna: Int = random.nextInt(1) + 1
    if (fila_o_columna == 1) {
      if(columnaObjetivo==0) explotarBombaPrimeraColumna(matriz, columnas,0)
      else explotarBombaColumna(matriz, columnaObjetivo,0)
    } else {
      explotarBombaFila(matriz,columnas, filaObjetivo,0)
    }
  }

  //pone a -1 la columna  entera en la que se encuentra la bomba
  def explotarBombaColumna(matriz: List[Int],columnaObjetivo: Int, posicion: Int): List[Int] = {
    if (posicion==longitud(matriz)-1) Nil
    else if (posicion%columnaObjetivo==0) agregarElemento(-1,explotarBombaColumna(matriz,columnaObjetivo,posicion+1))
    else agregarElemento(getElem(matriz,posicion),explotarBombaColumna(matriz,columnaObjetivo,posicion+1))
  }


  // caso especial, que pone a -1 la primera columna de la matriz                                              -->// CREO QUE NO ESTÁ BIEN, HAY Q PROBARLO
  def explotarBombaPrimeraColumna(matriz: List[Int], filaObjetivo: Int, columnaObjetivo: Int): List[Int] = {
    def explotarBombaPrimeraColumnaAux(matriz: List[Int], fila: Int, columna: Int): List[Int] = {
      if (matriz.isEmpty) Nil
      else if (fila == filaObjetivo && columna == columnaObjetivo) agregarElemento(-1,explotarBombaPrimeraColumnaAux(matriz.tail, fila + 1, columna))
      else if (columna == 0) agregarElemento(-1,explotarBombaPrimeraColumnaAux(matriz.tail, fila + 1, columna))
      else agregarElemento(matriz.head, explotarBombaPrimeraColumnaAux(matriz.tail, fila + 1, columna + 1))
    }

    explotarBombaPrimeraColumnaAux(matriz, 0, 0)
  }


  def explotarBombaFila(matriz: List[Int],columnas: Int, filaObjetivo: Int, posicion: Int): List[Int] = {
    if (posicion==longitud(matriz)-1) Nil
    else if (posicion/columnas==filaObjetivo) agregarElemento(-1,explotarBombaFila(matriz,columnas,filaObjetivo,posicion+1))
    else agregarElemento(getElem(matriz,posicion),explotarBombaFila(matriz,columnas,filaObjetivo,posicion+1))
  }

  def jugar(vidas: Int, modo: Int, dificultad: Int, filas: Int, columnas: Int, lim_inf: Int, lim_sup: Int): Unit = {
    val matriz: List[Int] = crearMatrizAleatoria(0,filas,columnas,lim_inf,lim_sup)
    imprimirMatriz(matriz,filas,columnas)
    if (vidas == 0) {
      println("Has perdido, te has quedado sin vidas")
    } else {
      if(modo==2){ //obtencion de filas y columnas por parte del usuario
        println("Introduce la fila de la casilla que quieres revisar: ")
        val filaObjetivo: Int = scala.io.StdIn.readInt()
        println("Introduce la columna de la casilla que quieres revisar: ")
        val columnaObjetivo: Int = scala.io.StdIn.readInt()
        val posicion: Int = (filaObjetivo - 1) * columnas + (columnaObjetivo - 1)
        //comprobamos si las coordenadas están dentro del rango de la matriz
        if (columnaObjetivo > columnas - 1 || filaObjetivo > filas - 1 || columnaObjetivo < 0 || filaObjetivo < 0) {
          println("\nCOORDENADAS NO VALIDAS, introduce unas coordenadas dentro del rango\n\n")
          jugar(vidas, modo, dificultad, filas, columnas, lim_inf, lim_sup)
        }
      }
      else{
        val random = new Random()
        //numeros aleatorios menores que filas y columnas
        val filaObjetivo: Int = random.nextInt(filas) + 1
        println("Fila escogida: " + filaObjetivo)
        val columnaObjetivo: Int = random.nextInt(columnas) + 1
        println("Columna escogida: " + columnaObjetivo)
        val posicion: Int = (filaObjetivo - 1) * columnas + (columnaObjetivo - 1)
        println("Pulsa enter para continuar")
        scala.io.StdIn.readLine()
      }

    }
  }

  def main(args: Array[String]): Unit = {
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