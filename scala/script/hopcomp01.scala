import test.hopcomp._
val wnd=new CompareWindow
wnd.show()
val examples=Array.fill(2)(wnd.randomExample)
wnd.teach(examples,5)
