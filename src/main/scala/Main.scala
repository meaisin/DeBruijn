import com.atp.Term
import com.atp.Term.*

@main def main(): Unit = {
  val term: Term = App(Abs(Abs(App(Var(1), Var(2)))), Var(1))
  println(term.reduce())
}
