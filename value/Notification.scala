package value

class Notification(val message: String) extends Value {
  override def toString = message
}

object Notification {
  def apply(msg: String) = new Notification(msg)

  val OK = Notification("OK")
  val DONE = Notification("DONE")
  val UNSPECIFIED = Notification("UNSPECIFIED")
}