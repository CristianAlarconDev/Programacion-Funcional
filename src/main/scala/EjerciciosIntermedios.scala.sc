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


}