
@main
def main(): Unit = {
  println("EJERCICIOS INTERMEDIOS ===")

  // --- Ejercicio 1: Buscar Elemento ---
  println("\n> Ej 1: buscarElemento")
  val listaBusqueda = List(1, 2, 3, 4, 5)
  println(s"¿Está el 3? ${EjerciciosIntermedios.buscarElemento(listaBusqueda, 3)}")
  assert(EjerciciosIntermedios.buscarElemento(listaBusqueda, 3))
  assert(!EjerciciosIntermedios.buscarElemento(listaBusqueda, 99))


}

