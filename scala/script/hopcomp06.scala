import test.hopcomp._
val wnd = new CompareWindow
wnd.show()
val examples = Array.fill(2)(wnd.randomExample)
wnd.bio.setMu(0.01)
wnd.bio.setMaxG(0.1)
wnd.disc.mu=0.02
wnd.teach(examples, 5)
wnd.inhibit=false
wnd.present(examples(0))()
wnd.present(examples(1))()