package com.calarcon.ejercicios
object EjerciciosCaseClasses {

  //Ejercicio 1
  case class Producto(nombre: String, precio: Double)

  def filtrarPorPrecio(productos: List[Producto], limite: Double): List[Producto] = {
    productos.filter(producto => producto.precio < limite)
  }
  //Ejercicio 2
  case class PuntoCoordenada(coordenadaX: Double, coordenadaY:Double)
  def distanciaEntre(unPunto:PuntoCoordenada, otroPunto:PuntoCoordenada):Double={
    import scala.math.sqrt
    val difX = otroPunto.coordenadaX - unPunto.coordenadaX
    val difY = otroPunto.coordenadaY - unPunto.coordenadaY
    sqrt((difX * difX) + (difY * difY))

  }
  //Ejercicio 3
  //tipos de datos abstractos,
  // luego usando pattern matching deberia poder considerar ambos casos
  sealed trait Archivo
  case class ArchivoSimple(nombre: String, tamano: Int) extends Archivo
  case class Carpeta(nombre: String, archivos: List[Archivo]) extends Archivo

  //No es recursion de cola porque queda pendiente el sum luego de la llamada recursiva
  def tamanioTotal(archivo: Archivo): Int = archivo match {
    case ArchivoSimple(_, tamanio) => tamanio
    case Carpeta(_, archivos) =>
      archivos.map(arc => tamanioTotal(arc)).sum
  }

}