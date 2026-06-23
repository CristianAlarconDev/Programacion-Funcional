import scala.annotation.tailrec
//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit = {
/*
  def contarOcurrencias(lista:List[String]): Map[String, Int] = {
    def auxiliar(restantes:List[String], acumulador:Map[String, Int]):Map[String, Int]= restantes match {
      case Nil=>acumulador;
      case palabra::restantes=>
        val cuentaActual =acumulador.getOrElse(palabra,0)
        val nuevoAcumulador=acumulador.updated(palabra, cuentaActual+1)
        auxiliar(restantes, nuevoAcumulador);
    }
    auxiliar(lista, Map())
  }*/
  /*prueba*/
  val palabrasDePrueba = List(
    "scala", "funcional", "java", "scala",
    "recursividad", "funcional", "scala",
    "map", "pattern", "matching", "map", "scala"
  )
  println("Resultado del conteo:")
  //println(contarOcurrencias(palabrasDePrueba))
  def eliminarPalabra(lista:List[String], palabraBuscada:String):List[String]={
    def auxiliar (restantes:List[String], acumulador:List[String]):List[String]= restantes match{
      case Nil=>acumulador.reverse;
      case palabra::restantes if palabra!=palabraBuscada =>auxiliar(restantes, palabra::acumulador);
      case _::restantes=> auxiliar(restantes, acumulador);
    }
    auxiliar(lista, List() )
  }
  println("Resultado al borrar la palabra:")
  println(eliminarPalabra(palabrasDePrueba, "scala"));

  def costoTotal(gastos: List[(String, Int)], cotizaciones: Map[String, Int]): Int={
    @tailrec
    def auxiliar(restantes:List[(String, Int)],  gastosCalculados:Int):Int= restantes match {
      case Nil => gastosCalculados;
      case (moneda, gastoEnMoneda)::restantes =>
        val cotizacion=cotizaciones.getOrElse(moneda,1);
        val gastoParcial= gastoEnMoneda*cotizacion;
        auxiliar(restantes, gastoParcial+gastosCalculados)
    }
    auxiliar(gastos,0);
  }

  val gastosHechos = List(("USD", 10), ("EUR", 5), ("ARS", 1500))
  val misCotizaciones = Map("USD" -> 1000, "EUR" -> 1100)
  val resultado = costoTotal(gastosHechos, misCotizaciones)

  println(s"El costo total es: $resultado")
//PAAAAAAAAAAAAAAAAAAAAAAARCIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAL
  def adaptar(palabras: List[String]): List[String]={
    @tailrec
    def auxiliar(restantes:List[String], acumulador:List[String]):List[String]=restantes match{
      case Nil => acumulador.reverse;
      case palabra::restantes if palabra =="Pelota" =>{auxiliar(restantes, "Balón"::acumulador)}
      case palabra::restantes if palabra =="Che" =>{auxiliar(restantes, "Hey"::acumulador)}
      case palabra::restantes if palabra == "Apostar" => { auxiliar(restantes, acumulador)}
      case otraPalabra::restantes => auxiliar(restantes, otraPalabra::acumulador)
    }
    auxiliar(palabras, List())
  }
  println("Se adapto la lista:")
  println(adaptar(List("Nacho", "Che", "Pelota", "Apostar", "Hey", "Apostar", "Che")))

  def campeon(partidos:List[(String, String, String)]):String={
    def auxiliar (restantes:List[(String, String, String)], acumulador:Map[String, Int]): Map[String, Int] =restantes match{
      case Nil => acumulador
      case (equipo, otroEquipo, resultado)::tail =>
        {
          val ganador = if (resultado == "Equipo1") equipo else otroEquipo
          val puntosParciales = acumulador.getOrElse(ganador, 0)
          val acumuladorActual = acumulador.updated(ganador, puntosParciales + 1)
          auxiliar(tail, acumuladorActual)
        }
    }
    def maxLista(lista:List[(String, Int)]): (String, Int) = {

      def auxiliar (restantes:List[(String, Int)], maximoFinal:(String, Int)): (String, Int) =restantes match {
        case Nil=>maximoFinal
        case (equipoActual, puntosActuales)::tail =>
          val (equipo, puntos)=maximoFinal
          val nuevoMaximo = if (puntos<puntosActuales) (equipoActual, puntosActuales) else maximoFinal
          auxiliar (tail, nuevoMaximo)
      }
      auxiliar(lista.tail, lista.head)
    }
    val resultadosMap = auxiliar(partidos, Map())
    val resultadosList=resultadosMap.toList
    val (equipoGanador, puntaje)=maxLista(resultadosList)
    equipoGanador
  }



  def mejoresVendedores(ventas: List[(String, Int)]): List[String]={

    def auxiliar(restantes:List[(String, Int)], acumulador:Map[String, Int]):Map[String, Int]= restantes match{
      case Nil =>acumulador
      case (vendedor, ventas)::tail =>
        val ventasParciales= acumulador.getOrElse(vendedor,0)
        val nuevoAcumulador= acumulador.updated(vendedor, ventasParciales+ventas)
        auxiliar(tail, nuevoAcumulador)
    }
    val ventasTotales= auxiliar(ventas, Map())
    val listaVentasTotales =ventasTotales.toList
    val mejores= listaVentasTotales.filter{
      case (vendedor, ventas)=> ventas>=2000
    }
    //mayor a menor, por eso el ´-´
    val mejoresOrdenadas = mejores.sortBy{
      case (vendedor, ventas)=> -ventas
    }
    mejoresOrdenadas.map{
      case (vendedor, ventas)=>vendedor
    }
  }
  val listaVentas = List(
    ("Lucía", 1000),
    ("Pedro", 500),
    ("Lucía", 2000),
    ("Sofía", 500),
    ("Pedro", 1500),
    ("Lucía", 500),
    ("Mario", 2100)
  )

  val res = mejoresVendedores(listaVentas)
  println(s"Los mejores vendedores son: $res")

  def esSolvente(consumos: List[List[(String, String, Int)]], tiposCambio: Map[String, Int], saldo: Int): Boolean ={
    val consumosFlat= consumos.flatten
    val consumosValidos= consumosFlat.filter{
      case (nombre, moneda, costo) => costo>=0
    }
    val gastosConvertidos= consumosValidos.map{
      case(nombre, moneda, costo)=>
        val valorCambio= tiposCambio.getOrElse(moneda, 1)
        costo*valorCambio
    }
    val sumaGastos=gastosConvertidos.sum
    sumaGastos<=saldo
  }
  def diferenciasMayores(n: Int, lista: List[Int]): List[Int]={
    val listaDiferenciada = lista.map(numero=>Math.abs(numero-n))
    listaDiferenciada.filter(valor=>valor>=10)
  }



  //PARCIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAL
  def conNumeros(inicio: Int, fin: Int): List[Int] = {
    List(inicio, fin)
  }

  // Genera el rango usando el azúcar sintáctico de Scala
  def entreNumerosDeclarativo(inicio: Int, fin: Int): List[Int] = {
    (inicio to fin).toList
  }

  // Genera el rango mediante recursión pura
  def entreNumeros(inicio: Int, fin: Int): List[Int] = {
    if (inicio > fin) Nil
    else inicio :: entreNumeros(inicio + 1, fin)
  }

  // Intersección de listas: se queda con lo que está en AMBAS
  def repetidos(lista1: List[Int], lista2: List[Int]): List[Int] = {
    lista1.filter(elemento => lista2.contains(elemento))
  }

  // Eliminación de duplicados en una sola lista
  def eliminarRepetidos(lista: List[Int]): List[Int] = {
    lista.distinct
  }
//EJERCICIO 4

  def aplicar(lista:List[Int], function: Int=>Int):List[Int]={
    lista.map(function)
  }
  val miLista= List(7,8,9)
  //FUNCION FLECHA O LAMBDA
  val duplicar = (nro :Int) => nro*2
  //DUPLICADA
  //println(aplicar(miLista, duplicar))
  //SIN DUPLICAR
  //println(miLista)

//EJERCICIO 5
  def sumar(lista: List[Int]): Int={
    lista.sum
  }
  //tener en cuenta el stack si es una lsita grande
  def sumarRecursivamente(lista: List[Int]): Int={
    if (lista.isEmpty){
      0
    }
    else{
      lista.head+sumarRecursivamente(lista.tail)
    }
  }
  val otraLista= List(10,20,30)
  //suma usando metodo de scala
  //println(sumar(otraLista))
  //suma usando recursividad
  //println(sumarRecursivamente(otraLista))

  //recursividad de cola o tail recursion
  def sumarLista(lista: List[Int]): Int = {
    @tailrec
    def auxiliar(subLista: List[Int], acumulador: Int): Int = {
      subLista match {
        case Nil =>
          acumulador
        case _ =>
          auxiliar(subLista.tail, acumulador + subLista.head)
      }
    }
    auxiliar(lista, 0)
  }
  val unaLista=List(40,60)
  //println(sumarLista(unaLista))


}

