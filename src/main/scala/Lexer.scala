package com.calarcon.ejercicios
sealed trait Token
case object LAMBDA extends Token
case object SPACE  extends Token
case object DOT    extends Token
case object LPAR   extends Token
case object RPAR   extends Token
case class VAR(nombre: String) extends Token