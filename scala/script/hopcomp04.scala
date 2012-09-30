import test.hopcomp._
val wnd = new CompareWindow
wnd.show()
val examples = Array.fill(2)(wnd.randomExample)
wnd.bio.setMu(0.005)
wnd.bio.defaultIn = -3
wnd.restInhibit = -3
wnd.teach(examples, 5)