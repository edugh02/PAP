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
        println("") //[38;5;165;5;214m
      } else {
        if (fila.head == 1) print(s"\u001b[46m\u001b[30m ${fila.head} \u001b[0m")
        else if (fila.head == 2) print(s"\u001b[41m\u001b[30m ${fila.head} \u001b[0m")
        else if (fila.head == 3) print(s"\u001b[45m\u001b[30m ${fila.head} \u001b[0m")
        else if (fila.head == 4) print(s"\u001b[42m\u001b[30m ${fila.head} \u001b[0m")
        else if (fila.head == 5) print(s"\u001b[44m\u001b[30m ${fila.head} \u001b[0m")
        else if (fila.head == 6) print(s"\u001b[43m\u001b[30m ${fila.head} \u001b[0m")
        else if (fila.head == 7) print(s"\u001b[47m\u001b[30m B \u001b[0m") //bomba
        else if (fila.head == 8) print(s"\u001b[47m\u001b[30mTNT\u001b[0m") //tnt
        else if (fila.head == 9) print(s"\u001b[40m\u001b[36mR1 \u001b[0m") //r1
        else if (fila.head == 10) print(s"\u001b[40m\u001b[31mR2 \u001b[0m") //r2
        else if (fila.head == 11) print(s"\u001b[40m\u001b[35mR3 \u001b[0m") //r3
        else if (fila.head == 12) print(s"\u001b[40m\u001b[32mR4 \u001b[0m") //r4
        else if (fila.head == 13) print(s"\u001b[40m\u001b[34mR5 \u001b[0m") //r5
        else if (fila.head == 14) print(s"\u001b[40m\u001b[33mR6 \u001b[0m") //r6
        else if (fila.head == -1) print(s"\u001b[47m\u001b[37m   \u001b[0m") //vacío
        else print(s"${fila.head}")

        imprimirFila(fila.tail)
      }
    }

    matriz match {
      case Nil =>
        println("---------")
      case _ =>
        imprimirFila(deja_n(matriz, m))
        imprimirMatriz(quita_n(matriz, m), n, m)
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

  def reemplazarEnPosicion[A](lista: List[A], pos: Int, elem: A): List[A] = {
    if (lista.isEmpty) Nil // Si la lista está vacía, no hay nada que reemplazar
    else if (pos == 0) elem :: lista.tail // Si estamos en la posición 0, reemplazamos el primer elemento
    else lista.head :: reemplazarEnPosicion(lista.tail, pos - 1, elem) // En otro caso, seguimos avanzando por la lista
  }

  def ver_candy(matriz: List[Int], filas: Int, columnas: Int, filaObjetivo: Int, columnaObjetivo: Int, elemento: Int): List[Int] = {
    val caramelo = filaObjetivo * columnas + columnaObjetivo
    val matrizActualizada =
      if (getElem(matriz, caramelo) != -1 && getElem(matriz, caramelo) == elemento) {
        val nuevaMatriz = reemplazarEnPosicion(matriz, caramelo, -1)
        val arriba = if (filaObjetivo > 0 && getElem(nuevaMatriz, (filaObjetivo - 1) * columnas + columnaObjetivo) != -1)
          ver_candy(nuevaMatriz, filas, columnas, filaObjetivo - 1, columnaObjetivo, elemento)
        else nuevaMatriz
        val abajo = if (filaObjetivo < filas - 1 && getElem(arriba, (filaObjetivo + 1) * columnas + columnaObjetivo) != -1)
          ver_candy(arriba, filas, columnas, filaObjetivo + 1, columnaObjetivo, elemento)
        else arriba
        val izq = if (columnaObjetivo > 0 && getElem(abajo, filaObjetivo * columnas + columnaObjetivo - 1) != -1)
          ver_candy(abajo, filas, columnas, filaObjetivo, columnaObjetivo - 1, elemento)
        else abajo
        if (columnaObjetivo < columnas - 1 && getElem(izq, filaObjetivo * columnas + columnaObjetivo + 1) != -1)
          ver_candy(izq, filas, columnas, filaObjetivo, columnaObjetivo + 1, elemento)
        else izq
      } else matriz
    matrizActualizada
  }

  def contador_borrados(matriz: List[Int],filas: Int, columnas: Int,posicion:Int): Int = {
    if (posicion== filas*columnas) 0
    else if (getElem(matriz,posicion) == -1) {
      1+contador_borrados(matriz,filas,columnas,posicion+1)
    }
    else contador_borrados(matriz,filas,columnas,posicion+1)
  }

  //metodo para rellenar los elementos borrados de la matriz con nuevos elementos aleatorios
  def rellenar_huecos(matriz: List[Int], filas: Int, columnas: Int, posicion: Int = 0, lim_inf: Int, lim_sup: Int): List[Int] = {
    if (posicion == filas * columnas) matriz
    else if (getElem(matriz, posicion) == -1) {
      val random = new Random()
      val matrizCambiada = reemplazarEnPosicion(matriz, posicion, random.nextInt(lim_sup - lim_inf + 1) + lim_inf)
      rellenar_huecos(matrizCambiada, filas, columnas, posicion + 1, lim_inf, lim_sup)
    }
    else rellenar_huecos(matriz, filas, columnas, posicion + 1, lim_inf, lim_sup)
  }

  //función principal que determina la posición a investigar ejecuta las acciones correspondientes
  def jugar(matriz:List[Int], vidas: Int, modo: Int, dificultad: Int, filas: Int, columnas: Int, lim_inf: Int, lim_sup: Int): Unit = {
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
          jugar(matriz,vidas, modo, dificultad, filas, columnas, lim_inf, lim_sup)
        }
        val elemento = getElem(matriz,posicion)
        if (elemento<7){
          val matrizElementosBorrados = ver_candy(matriz,filas,columnas,filaObjetivo,columnaObjetivo,elemento)
          imprimirMatriz(matrizElementosBorrados, filas, columnas)

          val contador = contador_borrados(matrizElementosBorrados,filas,columnas,0)
          if (contador == 1){
            val nuevaMatriz_menosvida = rellenar_huecos(matrizElementosBorrados,filas,columnas,0,lim_inf,lim_sup)
            jugar(nuevaMatriz_menosvida,vidas-1,modo,dificultad,filas,columnas,lim_inf,lim_sup)
          }
          else if (contador==5){
            //Se forma Bomba
            val matrizB = reemplazarEnPosicion(matrizElementosBorrados,filaObjetivo*columnas+columnaObjetivo,7)
            val nuevaMatrizB = rellenar_huecos(matrizB,filas,columnas,0,lim_inf,lim_sup)
            jugar(nuevaMatrizB,vidas,modo,dificultad,filas,columnas,lim_inf,lim_sup)
          }
          else if (contador==6) {
            val matrizTNT = reemplazarEnPosicion(matrizElementosBorrados,filaObjetivo*columnas+columnaObjetivo,8)
            val nuevaMatrizTNT = rellenar_huecos(matrizTNT,filas,columnas,0,lim_inf,lim_sup)
            jugar(nuevaMatrizTNT,vidas,modo,dificultad,filas,columnas,lim_inf,lim_sup)
          }
          else if (contador>=7){
            //Se forma rompecabezas
            val random = new Random()
            val rx: Int = random.nextInt(lim_sup) + 9
            val matrizRx = reemplazarEnPosicion(matrizElementosBorrados,filaObjetivo*columnas+columnaObjetivo,rx)
            val nuevaMatrizRx = rellenar_huecos(matrizRx,filas,columnas,0,lim_inf,lim_sup)
            jugar(nuevaMatrizRx,vidas,modo,dificultad,filas,columnas,lim_inf,lim_sup)
          }
          else {
            //de 2 a 4
            val nuevaMatriz = rellenar_huecos(matrizElementosBorrados,filas,columnas,0,lim_inf,lim_sup)
            jugar(nuevaMatriz,vidas,modo,dificultad,filas,columnas,lim_inf,lim_sup)
          }
          //caramelo en las coordenadas indicadas
        }else if (elemento ==7 ){
          val matrizBombaExplotada = explotarBomba(matriz,filas,columnas,filaObjetivo,columnaObjetivo)
          imprimirMatriz(matrizBombaExplotada,filas,columnas)
          val matrizBombaExplotada_rellenada = rellenar_huecos(matrizBombaExplotada,filas,columnas,0,lim_inf,lim_sup)
          jugar(matrizBombaExplotada_rellenada,vidas,modo,dificultad,filas,columnas,lim_inf,lim_sup)
        }
        else if (elemento ==8 ) {
          val matrizTNTExplotada = explotarTNT(matriz,filas,columnas,filaObjetivo,columnaObjetivo,0,0)
          imprimirMatriz(matrizTNTExplotada,filas,columnas)
          val matrizTNTExplotada_rellenada = rellenar_huecos(matrizTNTExplotada,filas,columnas,0,lim_inf,lim_sup)
          jugar(matrizTNTExplotada_rellenada,vidas,modo,dificultad,filas,columnas,lim_inf,lim_sup)
        }
        else if (elemento >=9 ) {
          val matrizRompecabezasExplotada = explotarRompecabezas(matriz,filas, columnas,elemento-8,0)
          val matriz_sin_rx = reemplazarEnPosicion(matrizRompecabezasExplotada,filaObjetivo*columnas+columnaObjetivo,-1)
          imprimirMatriz(matriz_sin_rx,filas,columnas)
          val matrizRompecabezasExplotada_rellenada = rellenar_huecos(matriz_sin_rx,filas,columnas,0,lim_inf,lim_sup)
          jugar(matrizRompecabezasExplotada_rellenada,vidas,modo,dificultad,filas,columnas,lim_inf,lim_sup)
        }

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
        jugar(matriz,vidas, modo, dificultad, filas, columnas, lim_inf, lim_sup)
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
      val lim_sup = 3 // valor máximo
      val matrizFacil: List[Int] = crearMatrizAleatoria(0, filas, columnas, lim_inf, lim_sup)
      jugar(matrizFacil,5,modo,dificultad,filas,columnas,lim_inf,lim_sup)
    }else{
      val lim_sup: Int = 6 // valor máximo
      val matrizNormal: List[Int] = crearMatrizAleatoria(0, filas, columnas, lim_inf, lim_sup)
      jugar(matrizNormal,5,modo,dificultad,filas,columnas,lim_inf,lim_sup)
    }
  }
}