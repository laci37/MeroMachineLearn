import test.biorecog._
val pan = new Panel
val wnd = new javax.swing.JFrame
wnd.add(pan)
wnd.setSize(400, 200)
wnd.show
pan.samples = Array(
  Array(true, true, true,
    false, false, false,
    false, false, false),
  Array(false, false, false,
    true, true, true,
    false, false, false),
  Array(false, false, false,
    false, false, false,
    true, true, true))
pan.teach(5)