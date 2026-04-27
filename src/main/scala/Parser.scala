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

object Parser {
def parse(tokens: List[Token]): AST = {
  val (astResultante, sobrantes) = parseExpression(tokens)
  astResultante
}

// devuelve tupla por ahora, arbol y lista
def parseExpression(tokens: List[Token]): (AST, List[Token]) = tokens match {

  // Si el primer token es una Variable
  case VAR(nombre) :: resto =>
    (Variable(nombre), resto)
  //Verifico si tiene la estructura de una abstracción
  case LAMBDA :: VAR(param) :: DOT :: resto =>
    val (cuerpoAST, sobrantes) = parseExpression(resto)
    (Abstraccion(Variable(param), cuerpoAST), sobrantes)
  //Para el caso de abstracción
  case LPAR :: resto =>
    val (primeraExpresion, trasPrimera) = parseExpression(resto)
    analizarCierreParentesis(primeraExpresion, trasPrimera)
}
  private def analizarCierreParentesis(exp: AST, tokens: List[Token]): (AST, List[Token]) = tokens match {
    // Caso aplicacion <LPAR> <lambda expresion> <SPACE> <lambda expresion> <RPAR>
    case SPACE :: resto =>
      val (argumento, trasArgumento) = parseExpression(resto)
      trasArgumento match {
        case RPAR :: finalTokens => (Aplicacion(exp, argumento), finalTokens)
        case _ => throw new Exception("Error: Se esperaba ')' tras el argumento de la aplicación")
      }
    // Caso agrupacion: (lambda x.x)  devuelve la expresión interna
    case RPAR :: resto => (exp, resto)
  }
}