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
}