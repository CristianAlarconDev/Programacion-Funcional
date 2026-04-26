package com.calarcon.ejercicios

import scala.annotation.tailrec
sealed trait AST

// Para las variables o argumentos, como x,y,z
case class Variable(nombre: String) extends AST

// La abstraccion o funcion anonima, lambda x.y por ejemplo con x variable e y cuerpo
case class Abstraccion(parametro: Variable, cuerpo: AST) extends AST

//Cuando se evalua uan funcion con argumento, ej ((x y) z) con (x y) funcion y z argumento
case class Aplicacion(funcion: AST, argumento: AST) extends AST

//RECORDAR TANTO CUERPO COMO ARGUMENTO O FUNCIÓN PUEDEN SER NODOS U HOJAS, AST


