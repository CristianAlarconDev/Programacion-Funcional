object EjerciciosIntermedios {
  //Ejercicio 1
  def buscarElemento(lista: List[Int], elementoBuscado: Int): Boolean = {
    def auxiliar(lista: List[Int], elementoBuscado: Int): Boolean = lista match {
      case Nil => false
      case head :: tail_sin_importancia if (elementoBuscado == head) => true
      case head_sin_importancia :: tail => auxiliar(tail, elementoBuscado)
    }
    auxiliar(lista, elementoBuscado)
  }

  //Ejercicio 2
  def filtrar(listas: List[List[Int]], elemento: Int): List[Int] = {
    def filtrarListas(listas: List[List[Int]], elemento: Int): List[Int] = listas match {
      case Nil => Nil
      case head :: tail =>
        head.filter(nro => nro > elemento) ::: filtrarListas(tail, elemento)
    }
    filtrarListas(listas, elemento)
  }

  def filtrarConFlatMap(listas: List[List[Int]], elemento: Int): List[Int] = {
    listas.flatMap(identity).filter(nro => nro > elemento)
  }
  //Ejercicio 3 (rehacerlo con recursion y/o collect con pattern matching)
  def filtrarConDiferencia(lista: List[Int], nro: Int): List[Int] = {
    lista.map(elemento => elemento - nro).filter(diferencia => diferencia > 10)
  }
}
  //Ejercicio 6
  def esCapicua(palabra:String):Boolean={
    val reversa= palabra.reverse
    palabra==reversa
  }
  def esCapicuav2(palabra: String): Boolean = {
    def auxiliar(listaChar: List[Char]): Boolean = listaChar match {
      case Nil => false
      case _ :: Nil => true
      case head :: tail =>
        if (head == tail.last) auxiliar(tail.init)
        else false
    }
    auxiliar(palabra.toList)
  }
  //Ejercicio 8
  def maximo(lista:List[Int]): Int ={
    def auxiliar(lista:List[Int], maximoActual:Int):Int = lista match {
      case Nil=>maximoActual
      case head::tail if (head>maximoActual)=>auxiliar(tail, head)
      case headNoMaximo::tail => auxiliar(tail, maximoActual)
    }
  auxiliar(lista, lista.head)
}