package com.calarcon.ejercicios

import scala.annotation.tailrec

sealed trait Token
case object LAMBDA extends Token
case object SPACE  extends Token
case object DOT    extends Token
case object LPAR   extends Token
case object RPAR   extends Token
case class VAR(nombre: String) extends Token

def esEspecial(c: Char): Boolean =
  c == 'λ' || c == '.' || c == ' ' || c == '(' || c == ')'

def lexer(input: String): List[Token] = {
  @tailrec
  def auxiliar(caracteres: List[Char], acumulador: List[Token]): List[Token] = caracteres match {
    case Nil => acumulador

    case 'λ' :: tail => auxiliar(tail, acumulador :+ LAMBDA)
    case '.' :: tail => auxiliar(tail, acumulador :+ DOT)
    case ' ' :: tail => auxiliar(tail, acumulador :+ SPACE)
    case '(' :: tail => auxiliar(tail, acumulador :+ LPAR)
    case ')' :: tail => auxiliar(tail, acumulador :+ RPAR)

    // caso variable
    case head :: tail =>
      val (nombreVar, resto) = caracteres.span(c => !esEspecial(c))

      val tokenVar = VAR(nombreVar.mkString)
      auxiliar(resto, acumulador :+ tokenVar)
  }
  auxiliar(input.toList, List())
}