object Main {
  def main(args: Array[String]): Unit = {


    def seleccionarElems[A](lCond:List[Boolean],lValores:List[A]):List[A]={
      if (lCond.length != lValores.length) throw new Exception("Las listas no tienen la misma longitud")
      else{
          if (lCond.isEmpty) Nil
          else{
            if (lCond.head) lValores.head :: seleccionarElems(lCond.tail,lValores.tail)
            else seleccionarElems(lCond.tail,lValores.tail)
          }
      }
    }

    val l= seleccionarElems(List(true,false,true),List(1,2,3)) = List(1,3)

    println(l)
  }
}