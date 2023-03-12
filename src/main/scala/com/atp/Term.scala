package com.atp

enum Term {
  case Var(i: Int)
  case Abs(body: Term)
  case App(left: Term, right: Term)

  def shift(i: Int, c: Int): Term = {
    this match
      case Var(n) => Var(n + (if n < c then 0 else i))
      case Abs(body) => Abs(body.shift(i, c + 1))
      case App(left, right) => App(left.shift(i, c), right.shift(i, c))
  }

  def sub(m: Int, e: Term): Term = {
    this match
      case Var(n) => if n == m then e else Var(n)
      case Abs(body) => Abs(body.sub(m + 1, e.shift(1, 0)))
      case App(left, right) => App(left.sub(m, e), right.sub(m, e))
  }

  def reduce1(): Term = {
    this match
      case App(Abs(body), right) => body.sub(0, right.shift(1, 0)).shift(-1, 0)
      case anything => anything
  }

  def reduce(): Term = {
    def innerReduce(t: Term): Term = {
      if t == t.reduce1() then
        t
      else
        innerReduce(t.reduce1())
    }
    innerReduce(this)
  }
}
