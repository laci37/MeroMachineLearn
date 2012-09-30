import test.hopcomp._
val wnd = new CompareWindow2
wnd.show()
val examples = Array.fill(2)(wnd.randomExample)
wnd.bio.setMu(0.005)
wnd.bio.defaultIn = -3
wnd.restInhibit = -3
wnd.teach(examples, 5)
wnd.inhibit=false
wnd.present(examples(0))()
wnd.present(examples(1))()