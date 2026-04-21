package com.calarcon.ejercicios
object EjerciciosCaseClasses {

  //Ejercicio 1
  case class Producto(nombre: String, precio: Double)

  def filtrarPorPrecio(productos: List[Producto], limite: Double): List[Producto] = {
    productos.filter(producto => producto.precio < limite)
  }

}