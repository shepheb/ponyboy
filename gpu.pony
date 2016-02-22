use "collections"
use "lib:SDL2"
use "debug"

primitive _SDLWindow
primitive _SDLRenderer
primitive _SDLTexture

actor GPU
  var oam: (Array[U8] iso | None) = None
  var vram: (Array[U8] iso | None) = None

  let cpu: CPU tag

  let width: USize = 160
  let height: USize = 160
  let scale: USize = 1 // Attempts to render the texture scaled up by this much.

  let _window: Pointer[_SDLWindow] tag
  let _renderer: Pointer[_SDLRenderer] tag
  let _texture: Pointer[_SDLTexture] tag

  // An array of (offset, relative-y, absolute-x)
  var spritesByCoordinate: Array[((USize, U8) | None)] ref =
      Array[((USize, U8) | None)](168)
  var spriteAt: Array[((U8, U8, U8, U8) | None)] ref =
      Array[((U8, U8, U8, U8) | None)](160)

  var pixels: Array[U32] ref = Array[U32](width * height)

  let colors: Array[U32] = [
    0xffffffff, // White
    0xffcccccc, // Light grey
    0xff666666, // Dark grey
    0xff000000  // Black
  ]


  new create(c: CPU tag) =>
    cpu = c

    let e1 = @SDL_Init[USize](U32(0x0020) /* INIT_VIDEO */)
    if e1 != 0 then
      Debug("SDL_Init failed: " +
          String.copy_cstring(@SDL_GetError[Pointer[U8]]()))
    end

    _window = @SDL_CreateWindow[Pointer[_SDLWindow]]("Ponyboy".cstring(),
        USize(100), USize(100), width * scale, height * scale, U32(0004))
        // SDL_WINDOW_SHOWN

    _renderer = @SDL_CreateRenderer[Pointer[_SDLRenderer]](_window, ISize(-1),
        U32(6) /* ACCELERATED | PRESENTVSYNC */)

    _texture = @SDL_CreateTexture[Pointer[_SDLTexture]](_renderer,
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
        if (lcdc and 0x02) != 0 then // Sprites enabled
          match spriteAt(lx)
          | (let y: U8, let x: U8, let tile: U8, let flags: U8) =>
            if (flags and 0x80) == 0 then // OBJ above BG when bit 7 = 0
              // Determine the colour based on the tile number.
              let pi = spritePaletteIndex(y, x, tile)
              if pi != 0 then
                setColor(lx, ly.usize(), spriteColor(pi, flags, obp0, obp1))
                continue
              end
            end
          end
        end

        // Second layer: Window, when enabled and scrolled up sufficiently.
        if ((lcdc and 0x20) != 0) and (wy <= ly) and (wx <= (lx.u8() + 7)) then
          // The window, even its color 0, renders on top. The window is not
          // scrollable, so check which tile map it's using and grab that tile.
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
          continue
        end

        // START HERE with the third layer.
        // Third layer: Background, colors 1-3 when enabled.

        // Fourth layer: Sprites, when enabled and under the background.

        // Fifth layer: Background color at palette index 0, when BG is enabled.

        // Final fallback: White.

      end
    else
      Debug("Error while rendering the row.")
    end
    /*
    Bit 7 - LCD Display Enable             (0=Off, 1=On)
    Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
    Bit 5 - Window Display Enable          (0=Off, 1=On)
    Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
    Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
    Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
    Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
    Bit 0 - BG Display (for CGB see below) (0=Off, 1=On)
    */


    /*
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
    */


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
        pixels((y * width) + x) = colors(color.usize())
      else
        Debug("Error writing to pixel (" + x.string() + ", " + y.string() + ")")
      end



  be dispose() =>
    @SDL_DestroyTexture[None](_texture)
    @SDL_DestroyRenderer[None](_renderer)
    @SDL_DestroyWindow[None](_window)
    @SDL_Quit[None]()


