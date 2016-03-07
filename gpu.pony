use "collections"
use "lib:SDL2"
use "debug"

// SDL calls
use @SDL_Init[ISize](flags: U32)
use @SDL_CreateWindow[Pointer[_SDLWindow] tag](title: Pointer[U8] tag,
    x: ISize, y: ISize, w: ISize, h: ISize, flags: U32)
use @SDL_CreateRenderer[Pointer[_SDLRenderer] tag](
    window: Pointer[_SDLWindow] tag, index: ISize, flags: U32)
use @SDL_CreateTexture[Pointer[_SDLTexture] tag](
    renderer: Pointer[_SDLRenderer] tag, format: U32, access: ISize,
    w: ISize, h: ISize)

use @SDL_UpdateTexture[ISize](texture: Pointer[_SDLTexture] tag,
    rect: Pointer[_SDLRect] tag, pixels: Pointer[U8] tag /* void* */,
    pitch: ISize)

use @SDL_RenderClear[ISize](renderer: Pointer[_SDLRenderer] tag)
use @SDL_RenderCopy[ISize](renderer: Pointer[_SDLRenderer] tag,
    texture: Pointer[_SDLTexture] tag,
    srcrect: Pointer[_SDLRect] tag, dstrect: Pointer[_SDLRect] tag)
use @SDL_RenderPresent[None](renderer: Pointer[_SDLRenderer] tag)

use @SDL_DestroyWindow[None](window: Pointer[_SDLWindow] tag)
use @SDL_DestroyRenderer[None](renderer: Pointer[_SDLRenderer] tag)
use @SDL_DestroyTexture[None](texture: Pointer[_SDLTexture] tag)

use @SDL_GetError[Pointer[U8] box]()


primitive _SDLWindow
primitive _SDLRenderer
primitive _SDLTexture
primitive _SDLRect


actor GPU
  var oam: (Array[U8] iso | None) = None
  var vram: (Array[U8] iso | None) = None

  let cpu: CPU tag

  let width: ISize = 160
  let height: ISize = 144
  let scale: ISize = 1 // Attempts to render the texture scaled up by this much.

  let _window: Pointer[_SDLWindow] tag
  let _renderer: Pointer[_SDLRenderer] tag
  let _texture: Pointer[_SDLTexture] tag

  // An array of (offset, relative-y, absolute-x)
  var spritesByCoordinate: Array[((USize, U8) | None)] ref =
      Array[((USize, U8) | None)].init(None, 168)
  var spriteAt: Array[((U8, U8, U8, U8) | None)] ref =
      Array[((U8, U8, U8, U8) | None)].init(None, 160)

  var pixels: Array[U32] ref = Array[U32].init(0xffffffff, (width * height).usize())

  let colors: Array[U32] = [
    0xffffffff, // White
    0xffcccccc, // Light grey
    0xff666666, // Dark grey
    0xff000000  // Black
  ]


  new create(c: CPU tag) =>
    cpu = c

    let e1 = @SDL_Init[ISize](U32(0x0020) /* INIT_VIDEO */)
    if e1 != 0 then
      Debug("SDL_Init failed: " + sdl_error())
    end

    _window = @SDL_CreateWindow[Pointer[_SDLWindow] tag]("Ponyboy".cstring(),
        ISize(100), ISize(100), (width * scale).isize(),
        (height * scale).isize(), U32(0004))
        // SDL_WINDOW_SHOWN

    _renderer = @SDL_CreateRenderer[Pointer[_SDLRenderer] tag](_window,
        ISize(-1), U32(6) /* ACCELERATED | PRESENTVSYNC */)

    _texture = @SDL_CreateTexture[Pointer[_SDLTexture] tag](_renderer,
        U32(0x16362004) /* PIXELFORMAT_ARGB8888 */,
        ISize(1), /* TEXTUREACCESS_STREAMING */
        width, height)


  be lock_oam(o: (Array[U8] iso | None)) => oam = consume o
  be lock_vram(v: (Array[U8] iso | None)) => vram = consume v

  // Send back the memory areas, if I have them.
  be disabled() =>
    cpu.gpu_done(oam = None, vram = None)

  be paint_line(ly: U8, lcdc: U8, scy: U8, scx: U8, wy: U8, wx: U8,
      bgp: U8, obp0: U8, obp1: U8) =>
    """
    Plan for painting each line. There are a few possible stacking orders, and
    most of have to worry about transparency.

    First, some preprocessing:
    - Run through the sprites in REVERSE address order (in the OAM) populating
      a table of sprites by X-coordinate. That gives the lowest addresses
      priority, as specified. ($FF is used for no-sprite, since there are only
      40 OAM entries.
    - Scan forward through that table, counting the sprites. Only the leftmost
      10 are actually displayed. Populate a list of which sprite entry is at
      each pixel. (Again, $FF is no sprite.)

    Then we process the pixels from right to left.
    - If the sprite is on top of the background, figure out the colour.
      - If nonzero, check the palette (OBP0 or 1) and determine the real colour.
        Write it into the screen buffer.
      - If 0, that's transparent, so ignore the sprite and draw the background.
    - Draw the window, if present and the sprite wasn't on top or is
      transparent. Even window colour 0 is on top.
    - If no window, then draw the background. Unless the colour is 0, in which
      case the under-sprites can be seen.
    - Sprites that appear under the background are here, but colour 0 is still
      transparent.
    - BG colour 0 is under everything, as a last resort.

    Things to remember:
    - The background is 256x256 and scrollable, and it wraps around when it's
      too wide.
    - The window position is at WX-7, WY. It can't scroll.
    - Double-height sprites are 16px tall, and the odd-numbered sprite is below
      the even-numbered one.
    - Sprites can be flipped in both dimensions.
    """
    let sprite_height: ISize = if (lcdc and 0x04) == 0 then 8 else 16 end
    try for i in Range[USize](0, 40) do spritesByCoordinate(i) = None end end
    try
      for i in Reverse[USize](39, 0) do
        let offset = 4 * i
        let y = (oam as Array[U8] iso)(offset).isize() - 16
        if (y <= ly.isize()) and (y.isize() < (ly.isize() + sprite_height)) then
          // This should be lowered by 8, but it's easier to index this way.
          let x = (oam as Array[U8] iso)(offset + 1).isize()
          spritesByCoordinate(x.usize()) = (i, (ly.isize() - y).u8())
        end
      end
    end

    // Now loop forward through spritesByCoordinate, counting up to 10 and then
    // setting them all to None after that.
    var sprite_count: USize = 0
    for i in Range[USize](0, 168) do
      if sprite_count >= 10 then
        try spritesByCoordinate(i) = None end
      else
        match try spritesByCoordinate(i) end
        | (_, _) =>
          sprite_count = sprite_count + 1
        end
      end
    end

    try for i in Range[USize](0, 160) do spriteAt(i) = None end end

    // Now looping backwards through the spritesByCoordinate, populating
    // spriteAt.
    // Remember the X-coordinate offset!
    try
      for i in Reverse[USize](167, 0) do
        match spritesByCoordinate(i)
        | (let s: USize, let y: U8) =>
          // First, fetch the flags for this sprite and figure out if it needs
          // to be flipped.
          var y': U8 = y
          var tile: U8 = (oam as Array[U8] iso)(s + 2)
          if sprite_height == 16 then
            if y >= 8 then
              tile = (tile and 0xfe) or 1
              y' = y - 8
            else
              tile = tile and 0xfe
            end
          end

          // Now compute the vertical flip.
          let flags: U8 = (oam as Array[U8] iso)(s + 3)
          if (flags and 0x40) != 0 then // Vertical flip
            y' = 7 - y'
          end

          // Now y' and tile are nice and subjective to the sprite. We need to
          // write tuples with the right subjective X-coordinate into spriteAt.
          let flipX = (flags and 0x20) != 0

          // Handle flipping for the attrs where needed.
          let lo: USize = (i.isize() - 8).max(0).usize()
          let hi: USize = i.min(160)
          for j in Range[USize](lo, hi) do
            let x = (j.isize() - 8) - lo.isize()
            spriteAt(j) = (y, if flipX then 7 - x.u8() else x.u8() end, tile, flags)
          end
        end
      end
    else
      Debug("Error while building spriteAt")
    end

    // Now spriteAt contains the (y, x, tile, flags) tuples I'm actually working
    // with. Those coordinates are subjective: they're the offsets into this
    // sprite, accounting for inversions in both dimensions.

    // Now, the final phase: work across the pixels left to right (either way
    // will do) and compute which layer and colour is actually on top.

    try
      for lx in Range[USize](0, 160) do
        // Topmost layer: Sprites when above the background, and enabled.
        if ((lcdc and 0x02) != 0) and
            renderSprites(true /* above BG */, ly, lx, obp0, obp1) then
          continue
        end

        // Second layer: Window, when enabled and scrolled up sufficiently.
        if ((lcdc and 0x20) != 0) and (wy <= ly) and (wx <= (lx.u8() + 7)) then
          renderWindow(ly, lx, wy, wx, lcdc, bgp)
          // The window, even its color 0, renders on top. If the window
          // overlays this location, then it always wins and we unconditionally
          // continue.
          continue
        end

        // Third layer: Background, colors 1-3 when enabled.
        if (lcdc and 0x01) != 0 then
          if renderBackground(ly, lx, scy, scx, lcdc, bgp) then
            continue
          end
        end

        // Fourth layer: Sprites, when enabled and under the background.
        if ((lcdc and 0x02) != 0) and
            renderSprites(false /* below BG */, ly, lx, obp0, obp1) then
          continue
        end

        // Fifth layer: Background color at palette index 0, when BG is enabled.
        if (lcdc and 0x01) != 0 then
          setColor(lx, ly.usize(), bgColor(0, bgp))
          continue
        end

        // Final fallback: White.
        setColor(lx, ly.usize(), 0)
      end
    else
      Debug("Error while rendering the row.")
    end

    // When we're done painting the line, we unlock the memory and return it to
    // the CPU.
    cpu.gpu_done(vram = None, oam = None)

  be vblank() =>
    """
    Expected to be called at the start of VBlank - that is, when all visible
    lines have been rendered and it's time to paint to the display.

    This behavior flips the color array in pixels into the SDL texture, and onto
    the screen.
    """
    // Copy the pixel data from a Array[U32] to Array[U8].
    // TODO: It should probably always be this, and use SDL_LockTexture instead.
    var px = Array[U8].undefined((width * height * 4).usize())
    // TODO: I think this is little-endian specific! Not ideal, but meh.
    try
      for i in Range[USize](0, (width * height).usize()) do
        px((4 * i) + 0) = ( pixels(i)        and 255).u8() // blue, low byte
        px((4 * i) + 1) = ((pixels(i) >>  8) and 255).u8() // green, second byte
        px((4 * i) + 2) = ((pixels(i) >> 16) and 255).u8() // red, third byte
        px((4 * i) + 3) = ((pixels(i) >> 24) and 255).u8() // alpha, high byte
      end
    else
      Debug("Error copying pixels to px. Can't happen?")
      return
    end

    var e = @SDL_UpdateTexture[ISize](_texture, Pointer[_SDLRect],
        px.cstring(), 4 * width)
    Debug("===> Top of vblank")

    if e < 0 then Debug("Error in SDL_UpdateTexture: " + sdl_error()); return end

    Debug("===> After UpdateTexture")
    e = @SDL_RenderClear[ISize](_renderer)
    if e < 0 then Debug("Error in SDL_RenderClear: " + sdl_error()); return end

    Debug("===> After RenderClear")
    e = @SDL_RenderCopy[ISize](_renderer, _texture,
        Pointer[_SDLRect], Pointer[_SDLRect])
    if e < 0 then Debug("Error in SDL_RenderCopy: " + sdl_error()); return end

    Debug("===> After RenderCopy")
    @SDL_RenderPresent[None](_renderer)
    Debug("===> After RenderPresent -- end of vblank")


  fun ref renderSprites(aboveBG: Bool, ly: U8, lx: USize, obp0: U8, obp1: U8):
      Bool ? =>
    """
    Renders sprites from spriteAt that appear above or below the background
    depending on aboveBG.

    Returns true if there is such a sprite at this pixel, and its color was
    not transparent.
    """
    match spriteAt(lx)
    | (let y: U8, let x: U8, let tile: U8, let flags: U8) =>
      // OBJ above BG when bit 7 = 0, hence the awkward comparison.
      if ((flags and 0x80) == 0) == aboveBG then
        // Determine the colour based on the tile number.
        let pi = spritePaletteIndex(y, x, tile)
        if pi != 0 then
          setColor(lx, ly.usize(), spriteColor(pi, flags, obp0, obp1))
          return true
        end
      end
    end
    false

  fun ref renderWindow(ly: U8, lx: USize, wy: U8, wx: U8, lcdc: U8, bgp: U8) ? =>
    // The window is always on top of the BG. Its corner is movable, but
    // it's not scrollable, so check which tile map it's using and grab
    // that tile.
    let base: USize = if (lcdc and 0x40) != 0 then 0x1c00 else 0x1800 end
    // TODO: Double-check this math.
    let relX = (lx.isize() - (wx.isize() - 7)).usize()
    let relY = ly.usize() - wy.usize()
    // Each tile is 8x8, so we need to shift both right by 3.
    let tileIndex = ((relY >> 3) * 32) + (relX >> 3)
    let tile = (vram as Array[U8] iso)(base + tileIndex)

    let pi = backgroundPaletteIndex((relY and 7).u8(), (relX and 7).u8(),
        tile, lcdc)
    setColor(lx, ly.usize(), bgColor(pi, bgp))

  fun ref renderBackground(ly: U8, lx: USize, scy: U8, scx: U8, lcdc: U8,
      bgp: U8): Bool ? =>
    // The background is 256x256 pixels and scrolls, so the actual
    // location we're looking for is (x + sx, y + sy) modulo 256.

    // Grab the right map data for the background.
    let base: USize = if (lcdc and 0x08) != 0 then 0x1c00 else 0x1800 end

    // Calculate the actual location we're targeting.
    let relX = (lx + scx.usize()) and 255
    let relY = (ly.usize() + scy.usize()) and 255

    // Tiles are 8x8, so we're actually looking for these shifted by 3.
    let tileX = relX >> 3
    let tileY = relY >> 3
    let tileIndex = (tileY * 32) + tileX
    let tile = (vram as Array[U8] iso)(base + tileIndex)

    let pi = backgroundPaletteIndex((relY and 7).u8(), (relX and 7).u8(),
        tile, lcdc)
    // Color 0 is always behind under-sprites.
    if pi > 0 then
      setColor(lx, ly.usize(), bgColor(pi, bgp))
      return true
    end
    false

  fun spritePaletteIndex(y: U8, x: U8, tile: U8): U8 =>
    """Returns the index into the palette for the given tile and sprite."""
    let addr = tile.usize() * 16
    tilePaletteIndex(y, x, addr)

  fun backgroundPaletteIndex(y: U8, x: U8, tile: U8, lcdc: U8): U8 =>
    """
    For background or window, checks which tile data region is in use, and
    returns the correct palette number at that location.
    """
    let addr = if (lcdc and 0x10) != 0 then // 1 = unsigned from 0x8000
      tile.usize() * 16
    else // 0 = signed from 0x9000
      (0x1000 + (tile.i8().isize() * 16)).usize()
    end

    tilePaletteIndex(y, x, addr)

  fun tilePaletteIndex(y: U8, x: U8, addr: USize): U8 =>
    """
    Given the address of the start of a tile in VRAM, and the
    sprite-relative x and y coordinates, returns the index into the palette.
    """
    // Each two bytes represents a line, so advance the address by y * 2
    let addr' = addr + (y.usize() * 2)
    // Then we're looking at the 7-x'th bit in that byte and the next one.
    let shift = 7 - x
    try
      let lo = (vram as Array[U8] iso)(addr)
      let hi = (vram as Array[U8] iso)(addr + 1)
      ((lo >> shift) and 1) or (((hi >> shift) and 1) << 1)
    else
      Debug("Error looking up the palette index for a tile.")
      0
    end

  fun spriteColor(paletteIndex: U8, flags: U8, obp0: U8, obp1: U8): U8 =>
    indexToColorNumber(paletteIndex,
        if (flags and 0x10) != 0 then obp1 else obp0 end)

  // Nothing special for handling backgrounds in monochrome mode.
  fun bgColor(paletteIndex: U8, palette: U8): U8 =>
    indexToColorNumber(paletteIndex, palette)

  fun indexToColorNumber(index: U8, palette: U8): U8 =>
    // Shift it left by paletteIndex * 2 and take the two lowest bits.
    (palette >> (index * 2)) and 3


  fun ref setColor(x: USize, y: USize, color: U8) =>
    """
    Takes a real color number (0-3) and writes that colour into the texture.
    0 = white, 1 = light gray, 2 = dark grey, 3 = black.
    For now I'm using simple-minded pure greyscale, not classic GB greenish.
    0 = #fff, 1 = #ddd, 2 = #555, 3 = #000
    """
    try
      pixels((y * width.usize()) + x) = colors(color.usize())
    else
      Debug("Error writing to pixel (" + x.string() + ", " + y.string() + ")")
    end


  fun tag sdl_error(): String val =>
    recover val String.copy_cstring(@SDL_GetError[Pointer[U8] box]()) end

  be dispose() =>
    @SDL_DestroyTexture[None](_texture)
    @SDL_DestroyRenderer[None](_renderer)
    @SDL_DestroyWindow[None](_window)
    @SDL_Quit[None]()


