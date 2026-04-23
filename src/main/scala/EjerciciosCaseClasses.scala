package com.calarcon.ejercicios

import scala.annotation.tailrec

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

  //Alternativa, misma idea pero con acumulador dentro del fold; sigue sin ser tail recursion
  def tamanioTotalConFold(archivo: Archivo): Int = archivo match {
    case ArchivoSimple(_, tamanio) => tamanio
    case Carpeta(_, archivos) =>
      archivos.foldLeft(0)((acumulador, arch) => acumulador + tamanioTotalConFold(arch))
  }
  //Otra alternativa ahora si con tail recursion
  def tamanioTotalConTailRec(archivoInicial: Archivo): Int = {
    @tailrec
    def auxiliar(pendientes: List[Archivo], acumulador: Int): Int = pendientes match {
      case Nil => acumulador
      case ArchivoSimple(_, tamanio) :: resto =>
        auxiliar(resto, acumulador + tamanio)
      case Carpeta(_, hijos) :: resto =>
        auxiliar(hijos ::: resto, acumulador)
    }
    auxiliar(List(archivoInicial), 0)
  }
  //Ejercicios de parcial

  //Ejercicio 2
  def contarLetras(palabra:String):Int={
    val soloLetras=palabra.filter(_.isLetter)
    soloLetras.length
  }

  def doblar(lista: List[String]): List[String] = {
    @tailrec
    def auxiliar(pendientes: List[String], acumulador: List[String]): List[String] = pendientes match {
      case Nil => acumulador
      case "perro" :: tail =>
        auxiliar(tail, acumulador :+ "sabueso")
      case head :: tail if contarLetras(head) > 10 =>
        auxiliar(tail, acumulador)
      case head :: tail if contarLetras(head) == 4 =>
        auxiliar(tail, acumulador :+ head :+ head)
      case head :: tail =>
        auxiliar(tail, acumulador :+ head)
    }
    auxiliar(lista, Nil)
  }
  //Ejercicio 3
  import scala.annotation.tailrec

  def limpiar(texto: String): List[String] = {
    texto.toLowerCase
      .split("\\W+")
      .filter(p => p.nonEmpty && p != "y")
      .toList
  }


  def palabraMasFrecuenteMix(texto: String): String = {
    val palabras = limpiar(texto)

    @tailrec
    def contar(lista: List[String], acc: Map[String, Int]): Map[String, Int] = lista match {
      case Nil => acc
      case h :: t => contar(t, acc + (h -> (acc.getOrElse(h, 0) + 1)))
    }

    val frecuencias = contar(palabras, Map.empty)
    val (palabra, cuenta) = frecuencias.maxBy(par => par._2)
    palabra
    }
  }


