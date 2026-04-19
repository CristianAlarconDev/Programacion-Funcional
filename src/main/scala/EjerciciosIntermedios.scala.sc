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

}