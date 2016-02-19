actor GPU
  var oam: (Array[U8] iso | None) = None
  var vram: (Array[U8] iso | None) = None

  let cpu: CPU tag

  var line: USize = 0

  new create(c: CPU tag) =>
    cpu = c

  be lock_oam(o: (Array[U8] iso | None)) => oam = consume o
  be lock_vram(v: (Array[U8] iso | None)) =>
    vram = consume v
    paint_line()

  fun paint_line() => None
