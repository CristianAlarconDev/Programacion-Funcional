
@main
def main(): Unit = {
  println("EJERCICIOS INTERMEDIOS ===")

  // --- Ejercicio 1: Buscar Elemento ---
  println("\n> Ej 1: buscarElemento")
  val listaBusqueda = List(1, 2, 3, 4, 5)
  println(s"¿Está el 3? ${EjerciciosIntermedios.buscarElemento(listaBusqueda, 3)}")
  assert(EjerciciosIntermedios.buscarElemento(listaBusqueda, 3))
  assert(!EjerciciosIntermedios.buscarElemento(listaBusqueda, 99))

  // --- Ejercicio 2: Filtrar lista de listas ---
  println("\n> Ej 2: filtrar")
  val listasDeListas = List(List(1, 15, 3), List(20, 2, 8), List(5, 12))
  val resultadoFiltrado = EjerciciosIntermedios.filtrar(listasDeListas, 10)
  val resultadoFiltrado2 = EjerciciosIntermedios.filtrarConFlatMap(listasDeListas, 10)
  println(s"Mayores a 10: $resultadoFiltrado")
  assert(resultadoFiltrado == List(15, 20, 12))
  println(s"Mayores a 10 usando FlatMap: $resultadoFiltrado2")
  assert(resultadoFiltrado2 == List(15, 20, 12))

  // --- Ejercicio 6: Capicúa ---
  println("\n> Ej 6: esCapicua")
  println(s"¿'neuquen' es capicua? ${EjerciciosIntermedios.esCapicuav2("neuquen")}")
  assert(EjerciciosIntermedios.esCapicuav2("radar"))
  assert(!EjerciciosIntermedios.esCapicuav2("scala"))
}