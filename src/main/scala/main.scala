package com.calarcon.ejercicios
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

  // --- Ejercicio 3: Filtrar desde lista los elementos que
  // tienen una diferencia mayor a 10 desde n ---
  val listaPrueba = List(16, 17, 18, 19, 20)
  val n = 8
  val resultado = EjerciciosIntermedios.filtrarConDiferencia(listaPrueba, n)
  assert(resultado == List(11, 12))

  // --- Ejercicio 6: Capicúa ---
  println("\n> Ej 6: esCapicua")
  println(s"¿'neuquen' es capicua? ${EjerciciosIntermedios.esCapicuav2("neuquen")}")
  assert(EjerciciosIntermedios.esCapicuav2("radar"))
  assert(!EjerciciosIntermedios.esCapicuav2("scala"))

  // --- Ejercicio 8: Capicúa ---
  println("\n> Ej 8: maximo")
  println(s"Maximo de la lista: ${EjerciciosIntermedios.maximo(List(1, 2, 3, 4, 5))}")
  assert(EjerciciosIntermedios.maximo(List(1, 2, 3, 4, 5)) == 5)
  println(s"Maximo de la lista sin match: ${EjerciciosIntermedios.maximoSinMatch(List(1, 2, 3, 4, 5))}")
  assert(EjerciciosIntermedios.maximoSinMatch(List(1, 2, 3, 4, 5)) == 5)

  // --- Ejercicio 9: Top K ---
  println("\n> Ej 9: topNro")
  val numeros = List(70, 100, 80, 90)
  val top2Max = EjerciciosIntermedios.topNro(numeros, 2, EjerciciosIntermedios.maximoDePar)
  println(s"Top 2 Máximos: $top2Max")
  assert(top2Max == List(100, 90))
  println("\n ¡Si llegamos aca todo esta bien!")

  // Input: "x" -> Lexer -> List(VAR("x"))
  val tokensA = List(VAR("x"))
  println(s"INPUT A: "+ tokensA)
  println(s"TEST A: ${Parser.parse(tokensA)}")


  // Solo guarda la variable y deja el resto para analizar luego
  val tokensB = List(VAR("abc"), SPACE, DOT)
  val (astB, restoB) = (Parser.parseExpression(tokensB))
  println(s"INPUT B: " + tokensB)
  println(s"Test B - AST: $astB") // Salida: Variable("abc")
  println(s"Test B - Resto: $restoB") // Salida: List(SPACE, DOT)

  //Estos son para el caso de abstraccion, luego voy a mover todos a un archivo test
  val tokensC = List(LAMBDA, VAR("x"), DOT, VAR("x"))
  println(s"INPUT C: " + tokensC)
  println(s"Test C (λx.x): ${Parser.parse(tokensC)}")

  val tokensD = List(LAMBDA, VAR("x"), DOT, LAMBDA, VAR("y"), DOT, VAR("x"))
  println(s"INPUT C: " + tokensD)
  println(s"Test D (λx.λy.x): ${Parser.parse(tokensD)}")


}