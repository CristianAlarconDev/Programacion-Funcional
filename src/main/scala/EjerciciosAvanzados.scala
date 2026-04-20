package com.calarcon.ejercicios

import scala.annotation.tailrec

object EjerciciosAvanzados {
  //Ejercicio 1
  def contar(palabras:List[String]):Map[String,Int]={
    @tailrec
    def auxiliar(palabras:List[String], map: Map[String, Int]):Map[String, Int]= palabras match {
      case Nil => map
      case head::tail if map.contains(head)  =>
        val cantidadApariciones= map(head)
        val nuevoMapa =map+(head->(cantidadApariciones + 1))
        auxiliar(tail, nuevoMapa)
      case head::tail =>
        val mapNuevo= map + (head->1)
        auxiliar(tail, mapNuevo)
    }
    val mapaVacio = Map[String, Int]()
    auxiliar(palabras, mapaVacio)
  }
  def contarV2(palabras:List[String]):Map[String,Int]={
    @tailrec
    def auxiliar (palabras:List[String], palabrasMap:Map[String, Int]):Map[String, Int] = palabras match{
      case Nil => palabrasMap
      case head::tail =>
        val cantApariciones= palabrasMap.getOrElse(head,0)
        val nuevoMapa= palabrasMap+(head->(cantApariciones+1))
        auxiliar(tail, nuevoMapa)
    }
    auxiliar(palabras, Map[String, Int]())

  }
  //Ejercicio 2
  def contarPalabras(texto:String):Map[String,Int]={
    val listaPalabras= texto.toLowerCase.split("\\W+").toList.filter(_.nonEmpty)
    contarV2(listaPalabras)
  }

  //Ejercicio 3

  def agruparPares(numeros:List[Int]):List[Int]={
    @tailrec
    def auxiliar (restantes:List[Int], acumulados:List[Int]):List[Int] = restantes match{
      case Nil => acumulados
      case head::tail =>
        val (iguales, distintos) = tail.partition(n => n == head)
        val bloque = head :: iguales
        auxiliar(distintos, acumulados ::: bloque)
    }
    auxiliar(numeros, List())

  }
}