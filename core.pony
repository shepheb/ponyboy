use "collections"
use "time"
use "debug"

type GBFormat is
    FormatSettingsHolder[FormatHexSmallBare, PrefixDefault, FormatSettingsDefault]

primitive Ports
  fun lcdc(): U16 => 0xff40
  fun stat(): U16 => 0xff41
  fun scy(): U16 => 0xff42
  fun scx(): U16 => 0xff43
  fun ly(): U16 => 0xff44
  fun lyc(): U16 => 0xff45
  fun wx(): U16 => 0xff4a
  fun wy(): U16 => 0xff4b
  fun bgp(): U16 => 0xff47
  fun obp0(): U16 => 0xff48
  fun obp1(): U16 => 0xff49
  fun dma(): U16 => 0xff46
  // TODO: Sound!
  fun joyp(): U16 => 0xff00
  // TODO: Link cable!
  fun tdiv(): U16 => 0xff04
  fun tima(): U16 => 0xff05
  fun tma(): U16 => 0xff06
  fun tac(): U16 => 0xff07

  fun iflag(): U16 => 0xff0f
  fun ienable(): U16 => 0xffff

primitive Interrupts
  fun vblank_mask(): U8 => 0x01
  fun stat_mask():   U8 => 0x02
  fun timer_mask():  U8 => 0x04
  fun serial_mask(): U8 => 0x08
  fun joypad_mask(): U8 => 0x10

  fun vector_vblank(): U16 => 0x40
  fun vector_stat():   U16 => 0x48
  fun vector_timer():  U16 => 0x50
  fun vector_serial(): U16 => 0x58
  fun vector_joypad(): U16 => 0x60

actor CPU
  """
  Plan and design for the CPU:
  - Gets passed the loaded ROM as an Array[U8] val at startup.
  - Sets up the CPU
  - Creates a GPU and sound controller.
  - Repeatedly calls run_single_opcode, counting elapsed cycles.
    - Cycles through each display line on a fixed schedule (see below).
    - At point where the memory becomes inaccessible to the CPU, tell the GPU to
      paint that line.
    - At the start of the VBlank, flip the painted lines onto the display.
    - The clock frequency of the Gameboy is 4,194,304 clocks per second.
      - Which is 1,048,576 machine cycles per second.
  - OAM and VRAM are isos that are flipped periodically between CPU and GPU.
    - Those two are independent! See below for the timing and how that works.
  - DMAs can be kicked off anytime according to some docs. Others say that the
    OAM needs to be accessible to start a DMA. HBlanks are too short for a
    complete DMA, so this doesn't work.
    - Since only HRAM is accessible by the CPU during the DMA, we can simply do
      the copy instantly and continue blocking all access to the memory.

  Here's the complete state diagram for the CPU and GPU, with timings:
  - Mode 1: VBlank, CPU owns the memory. We start here, effectively, because the
    display starts out disabled.
  ======
  - Mode 2: OAM locked. The CPU sends the OAM iso to the GPU.
    STAT, LY, etc. are updated at this point.
    Timing: "77-83 clks" is 80 clks = 20 cycles
  - Mode 3: Both locked. The CPU sends the VRAM iso to the GPU.
    The line is actually rendered at this point.
    Timing: "169-175 clks" 172 clks = 43 cycles
  - Mode 0: HBlank. Both memory isos are returned to the CPU.
    Timing: "201-207 clks" is 204 clks = 51 cycles
  ======
  The above cycles once for each line from 0 to 143, then VBlank begins. The
  LY is still updated every 456 ticks, but there's 10(?) lines worth of dead
  time. VBlank lasts 4560 clks = 1140 cycles. A complete frame lasts 70224 clks
  or 17556 cycles, for a framerate of 1,048,576 / 17556 = 59.7275 fps

  Since VBlank follows HBlank, no iso juggling happens there.


  Timers and such
  ===============
  - $FF04 DIV is incremented at a rate of 16,384Hz, which is every 64 cycles.
  - $FF05 TIMA is incremented at one of four rates. It can be written, in which
    case it continues ticking from the new value.
  - $FF06 TMA is the "modulus", this value is put into TIMA after it exceeds $FF
  - $FF07 TAC is the timer control. It enables and disables the timer, and
    chooses the frequency:
    - 00:   4096 Hz = every 256 cycles
    - 01: 262144 Hz = every   4 cycles
    - 02:  65535 Hz = every  16 cycles
    - 03:  16384 Hz = every  64 cycles


  TODO: See if I can pin down exactly when a DMA can be launched.
  """
  let gpu: GPU tag

  var regs: Array[U8] ref // A F B C D E H L
  var flags: U8 // that's ZNHC----
  var pc: U16
  var sp: U16 // Full-descending. The programmer is responsible for aiming this
              // at startup.

  // There are many different memory areas. All memory access should be done
  // through the memory accessing words rb and wb, since they are correctly
  // mapped to all the various locations.
  var rom: Array[U8] // Read-only, maybe banked, found on the cart.
  var ram: Array[U8] // Read-write, maybe banked, found on the cart.

  // Read-write, but restricted to VBlank. Lobbed back and forth with the GPU,
  // hence the None.
  var vram: (Array[U8] iso | None)
  var oam:  (Array[U8] iso | None)

  var wram: Array[U8] // Read-write, no bank (except in CGB mode). Built in.
  var hram: Array[U8] // Small array of high memory, built in. $FF80-FFFE.

  // Some, though not all, of the I/O ports in 0xff00-0xff7f are readable.
  var ports: Array[U8]

  // Offset for the switchable ROM, 0x4000-7ffff.
  var rom_offset: USize = 0
  // Offset for the current bank of external RAM. None when there is no such
  // memory.
  var ext_ram_offset: USize = 0

  var interrupts_enabled: Bool = false

  // Since interrupts are enabled and disabled after the instruction after the
  // EI/DI is executed, this counter starts at 2 or -2 and ticks towards 0. When
  // it goes from +/- 1 to 0, the interrupt flag actually changes.
  var interrupt_counter: I8 = 0

  var halted: Bool = false
  var stopped: Bool = false

  var dma_counter: USize = 0

  var div_counter: USize = 0
  var timer_counter: USize = 0
  // Indexed by the mode number in TAC.
  let timer_maximums: Array[USize] = [256, 4, 16, 64]

  var frame_counter: USize = 0
  let lcd_mode_lengths: Array[USize] = [
    51,   // 0: HBlank
    1140, // 1: VBlank
    20,   // 2: OAM locked
    43    // 3: All locked
  ]
  var waiting_on_gpu: Bool = false

  // Buttons are indexed thus:
  // right, left, up, down; A, B, select, start
  // This corresponds to the order they appear in the JOYP register.
  var buttons_down: Array[Bool] = Array[Bool](8)
  var buttons_dpad_selected: Bool = false


  new reset(cart: Array[U8] val) =>
    // TODO: Figure out the memory controller, ROM and RAM sizes and use the
    // right values. Currently assumes 32KB ROM, no RAM, and no banking.
    gpu = GPU.create(this)
    regs = [0x01, 0xb0, 0x00, 0x13, 0x00, 0xd8, 0x01, 0x4d]
    flags = 0
    pc = 0x100
    sp = 0xfffe
    rom = Array[U8](0x8000)
    cart.copy_to(rom, 0, 0, 0x8000)

    ram = Array[U8](0)

    vram  = recover Array[U8](0x2000) end
    oam   = recover Array[U8](0xa0) end
    wram  = Array[U8](0x2000)
    hram  = Array[U8](0x80)
    ports = Array[U8](0x80)

    // Setting up the initial values of all the ports.
    try
      ports(0x05) = 0 // TIMA
      ports(0x06) = 0 // TMA
      ports(0x07) = 0 // TAC

      ports(0x10) = 0x80 // NR10
      ports(0x11) = 0xbf // NR11
      ports(0x12) = 0xf3 // NR12
      ports(0x14) = 0xbf // NR14
      ports(0x16) = 0xf3 // NR21
      ports(0x17) = 0x00 // NR22
      ports(0x19) = 0xbf // NR24
      ports(0x1a) = 0x7f // NR30
      ports(0x1b) = 0xff // NR31
      ports(0x1c) = 0x9f // NR32
      ports(0x1e) = 0xbf // NR33
      ports(0x20) = 0xff // NR41
      ports(0x21) = 0x00 // NR42
      ports(0x22) = 0x00 // NR43
      ports(0x23) = 0xbf // NR30
      ports(0x24) = 0x77 // NR50
      ports(0x25) = 0xf3 // NR51
      ports(0x26) = 0xf1 // NR52

      ports(0x40) = 0x91 // LCDC
      ports(0x42) = 0x00 // SCY
      ports(0x43) = 0x00 // SCX
      ports(0x45) = 0x00 // LYC
      ports(0x47) = 0xfc // BGP
      ports(0x48) = 0xff // OBP0
      ports(0x49) = 0xff // OBP1
      ports(0x4a) = 0x00 // WY
      ports(0x4b) = 0x00 // WX

      hram(0x7f) = 0x00 // IE
    end



  // Memory map:
  // 0000-3fff: 16KB ROM, bank 0 (fixed)
  // 4000-7fff: 16KB ROM, bank 1..nn (switchable)
  // 8000-9fff: 8KB VRAM (switchable only in CGB)
  // a000-bfff: 8KB external cart RAM (switchable, maybe none)
  // c000-dfff: 8KB WRAM (GB's own internal RAM)
  // e000-fdff: mirror of c000-ddff
  // fe00-fe9f: OAM, the Sprite attributes
  // fea0-feff: Unused
  // ff00-ff7f: I/O registers
  // ff80-fffe: High RAM (HRAM)
  // ffff:      Interrupt enable register

  fun rb(addr: U16): U8 =>
    """Responsible for the complicated memory map above."""

    // While DMA is in progress, only the HRAM section is readable.
    if dma_counter > 0 then
      if (addr >= 0xff80) and (addr < 0xffff) then
        return try hram((addr - 0xff80).usize()) else 0xff end
      else
        return 0xff
      end
    end

    try
      if addr < 0x4000 then
        rom(addr.usize())
      elseif addr < 0x8000 then
        rom((addr - 0x4000).usize() + rom_offset)
      elseif addr < 0xa000 then
        try
          (vram as Array[U8] iso)((addr - 0x8000).usize())
        else
          0xff
        end
      elseif addr < 0xc000 then
        ram((addr - 0xa000).usize())
      elseif addr < 0xe000 then
        wram((addr - 0xc000).usize())
      elseif addr < 0xfe00 then
        wram((addr - 0xe000).usize())
      elseif addr < 0xfea0 then
        try
          (vram as Array[U8] iso)((addr - 0xfe00).usize())
        else
          0xff
        end
      elseif addr < 0xff00 then
        0 // Unused portion
      elseif addr < 0xff80 then
        rport(addr - 0xff00)
      elseif addr < 0xffff then
        hram((addr - 0xff80).usize())
      else
        error
      end
    else
      0
    end

  fun rport(port: U16): U8 =>
    let raw: U8 = try ports(port.usize()) else 0 end
    match port
    | 0xff46 => 0 // DMA is write-only.
    | 0xff00 => // JOYP
      // 0 is pressed! So we build a negative flag and then invert it.
      let base: USize = if buttons_dpad_selected then 0 else 4 end
      let negative: U8 = 0xf0 or
          (if buttons_down(base + 0) then 1 else 0 end) or
          (if buttons_down(base + 1) then 2 else 0 end) or
          (if buttons_down(base + 2) then 4 else 0 end) or
          (if buttons_down(base + 3) then 8 else 0 end)
      not negative

    // Sound stuff
    | 0xff11 => raw and 0xc0 // NR11
    | 0xff13 => 0 // NR13 is write-only.
    | 0xff14 => raw and 0x40 // Only one readable bit in NR14.
    | 0xff16 => raw and 0xc0 // NR21
    | 0xff18 => 0 // NR23 is also write-only.
    | 0xff19 => raw and 0x40 // Only one readable bit in NR24.
    | 0xff1d => 0 // NR33 is write-only.
    | 0xff1e => raw and 0x40 // Only one readable bit in NR34.
    | 0xff23 => raw and 0x40 // Only one readable bit in NR44.
    else
      raw
    end

  // Little-endian 16-bit reads and writes.
  fun rw(addr: U16): U16 => rb(addr).u16() or (rb(addr+1).u16() << 8)



  fun ref wb(addr: U16, value: U8) =>
    """Responsible for the complicated memory map above."""

    // While DMA is in progress, only the HRAM is accessible.
    if dma_counter > 0 then
      if (addr >= 0xff80) and (addr < 0xffff) then
        try hram((addr - 0xff80).usize()) = value end
      end
      return
    end


    try
      if addr < 0x8000 then
        return // ROM is not writable...
      elseif addr < 0xa000 then
        try
          (vram as Array[U8] iso)((addr - 0x8000).usize()) =  value
        end
      elseif addr < 0xc000 then
        ram((addr - 0xa000).usize()) = value
      elseif addr < 0xe000 then
        wram((addr - 0xc000).usize()) = value
      elseif addr < 0xfe00 then
        wram((addr - 0xe000).usize()) = value
      elseif addr < 0xfea0 then
        try
          (vram as Array[U8] iso)((addr - 0xfe00).usize()) = value
        end
      elseif addr < 0xff00 then
        return // Unused portion
      elseif addr < 0xff80 then
        wport(addr - 0xff00, value)
      elseif addr < 0xffff then
        hram((addr - 0xff80).usize()) = value
      else
        error
      end
    end

  // Little-endian 16-bit reads and writes.
  fun ref ww(addr: U16, v: U16) =>
    wb(addr, (v and 0xff).u8())
    wb(addr, (v >> 8).u8())

  fun ref wport(port: U16, value: U8) =>
    let old = try ports(port.usize()) else return end
    var nu = value
    match port
    | 0xff40 => // LCDC
      // Most of these settings matter to the GPU, but if the LCD gets disabled
      // we need to get the memory back from the GPU ASAP.
      if ((old and 0x80) > 0) and ((value and 0x80) == 0) then
        // Freshly disabled. Make it so.
        gpu.disabled()
        // It should immediately send back the memory.
        // Set the STAT mode to 1 (VBlank)
        let stat = rb(Ports.stat())
        wb(Ports.stat(), (stat and (not 3)) or 1)
      end
    | 0xff41 => nu = value and 0x74
    | 0xff44 => return
    | 0xff46 => // DMA
      // DMA can start anytime, and "instantly" fills the OAM from the target
      // area, which always value << 8.
      // TODO: This is actually kinda busted, since the OAM and VRAM might be
      // passed to the GPU.
      match oam | None => return end
      for i in Range[U16](0, 0xa0) do
        try
          (oam as Array[U8] iso)(i.usize()) = rb((value.u16() << 8) or i)
        end
      end
      dma_counter = 160

      return
    | 0xff00 => // JOYP
      // Writing to this, choose which to select.
      // TODO: What happens if both or neither are selected?
      if (value and 0x20) == 0 then
        buttons_dpad_selected = false
      elseif (value and 0x10) == 0 then
        buttons_dpad_selected = true
      end
      return

    // Timers
    | 0xff04 => nu = 0 // DIV - Writing anything here resets it to 0.
    end
    try ports(port.usize()) = nu end


  fun r_a(): U8 => try regs(0) else 0 end
  fun r_f(): U8 => try regs(1) else 0 end
  fun r_b(): U8 => try regs(2) else 0 end
  fun r_c(): U8 => try regs(3) else 0 end
  fun r_d(): U8 => try regs(4) else 0 end
  fun r_e(): U8 => try regs(5) else 0 end
  fun r_h(): U8 => try regs(6) else 0 end
  fun r_l(): U8 => try regs(7) else 0 end

  fun r_af(): U16 => try (regs(0).u16() << 8) or regs(1).u16() else 0 end
  fun r_bc(): U16 => try (regs(2).u16() << 8) or regs(3).u16() else 0 end
  fun r_de(): U16 => try (regs(4).u16() << 8) or regs(5).u16() else 0 end
  fun r_hl(): U16 => try (regs(6).u16() << 8) or regs(7).u16() else 0 end

  fun ref w_a(v: U8) => try regs(0) = v end
  fun ref w_f(v: U8) => try regs(1) = v end
  fun ref w_b(v: U8) => try regs(2) = v end
  fun ref w_c(v: U8) => try regs(3) = v end
  fun ref w_d(v: U8) => try regs(4) = v end
  fun ref w_e(v: U8) => try regs(5) = v end
  fun ref w_h(v: U8) => try regs(6) = v end
  fun ref w_l(v: U8) => try regs(7) = v end

  fun ref w_af(v: U16) =>
    try
      regs(0) = (v >> 8).u8()
      regs(1) = (v and 0xff).u8()
    end
  fun ref w_bc(v: U16) =>
    try
      regs(2) = (v >> 8).u8()
      regs(3) = (v and 0xff).u8()
    end
  fun ref w_de(v: U16) =>
    try
      regs(4) = (v >> 8).u8()
      regs(5) = (v and 0xff).u8()
    end
  fun ref w_hl(v: U16) =>
    try
      regs(6) = (v >> 8).u8()
      regs(7) = (v and 0xff).u8()
    end

  // Stack
  fun ref pushb(v: U8) =>
    sp = sp - 1
    wb(sp, v)
  fun ref pushw(v: U16) =>
    sp = sp - 2
    ww(sp, v)

  fun ref popb(): U8 =>
    let r = rb(sp)
    sp = sp + 1
    r
  fun ref popw(): U16 =>
    let r = rw(sp)
    sp = sp + 2
    r

  // Flags
  fun r_flag(mask: U8): Bool => (flags and mask) != 0
  fun r_fZ(): Bool => r_flag(0x80)
  fun r_fN(): Bool => r_flag(0x40)
  fun r_fH(): Bool => r_flag(0x20)
  fun r_fC(): Bool => r_flag(0x10)

  fun ref s_flag(mask: U8) => flags = flags or mask
  fun ref s_fZ() => s_flag(0x80)
  fun ref s_fN() => s_flag(0x40)
  fun ref s_fH() => s_flag(0x20)
  fun ref s_fC() => s_flag(0x10)

  fun ref c_flag(mask: U8) => flags = flags and (not mask)
  fun ref c_fZ() => c_flag(0x80)
  fun ref c_fN() => c_flag(0x40)
  fun ref c_fH() => c_flag(0x20)
  fun ref c_fC() => c_flag(0x10)

  fun ref set_flag(mask: U8, set: Bool) =>
    if set then s_flag(mask) else c_flag(mask) end
  fun ref set_fZ(set: Bool) => set_flag(0x80, set)
  fun ref set_fN(set: Bool) => set_flag(0x40, set)
  fun ref set_fH(set: Bool) => set_flag(0x20, set)
  fun ref set_fC(set: Bool) => set_flag(0x10, set)


  // PC accessors
  fun pc_rb(): U8 => rb(pc)
  fun ref pc_rb_plus(): U8 =>
    let b = rb(pc)
    pc = pc + 1
    b
  fun ref pc_rw_plus(): U16 =>
    pc_rb_plus().u16() or (pc_rb_plus().u16() << 8)


  // Interrupts
  fun ref request_int(mask: U8) =>
    let f = rb(Ports.iflag())
    wb(Ports.iflag(), f or mask)

  fun ref clear_int(mask: U8) =>
    let f = rb(Ports.iflag())
    wb(Ports.iflag(), f and (not mask))

  fun ref handle_int(mask: U8) =>
    halted = false
    stopped = false
    interrupts_enabled = false
    interrupt_counter = 0
    // Work up the mask. The least significant bits are highest priority.
    if (mask and Interrupts.vblank_mask()) > 0 then
      pc = Interrupts.vector_vblank()
    elseif (mask and Interrupts.timer_mask()) > 0 then
      pc = Interrupts.vector_timer()
    elseif (mask and Interrupts.serial_mask()) > 0 then
      pc = Interrupts.vector_serial()
    elseif (mask and Interrupts.joypad_mask()) > 0 then
      pc = Interrupts.vector_joypad()
    else
      Debug("Bad interrupt mask - no such interrupt: " + mask.string())
    end



  // Behaviors that send signals from other parts of the system.
  be gpu_done(v: (Array[U8] iso | None), o: (Array[U8] iso | None)) =>
    match vram | None => vram = consume v end
    match oam  | None => oam  = consume o end

    if waiting_on_gpu then
      waiting_on_gpu = false
      run()
    end

  be button_down(b: USize) => handle_button(b, true)
  be button_up(b: USize)   => handle_button(b, false)

  fun ref handle_button(b: USize, down: Bool) =>
    try buttons_down(b) = down end

    // Trigger a joypad interrupt on downward strokes only.
    if down then
      request_int(Interrupts.joypad_mask())

      // The CPU might have been stopped. If it was, re-enable it.
      if stopped then
        stopped = false
        run()
      end
    end


  // Main interpreter function.
  be run() =>
    """
    Main interpreter.
    - Update interrupt_counter and interrupts_enabled if needed.
    - Check for and execute interrupts.
    - Run an instruction.
    - Bump counters and timers. (Might trigger timer interrupt.)
    - Transition LCD modes if necessary. (Might trigger STAT interrupt.)
    - Call run() again (usually)
    """

    // When we're stopped, bail until a joypad signal awakens us.
    if stopped then return end

    // Adjust the interrupt_counter.
    match interrupt_counter
    | 0 => None
    | 1 => interrupts_enabled = true; interrupt_counter = 0
    | 2 => interrupt_counter = 1
    | -1 => interrupts_enabled = false; interrupt_counter = 0
    | -2 => interrupt_counter = -1
    end

    // Now check if an interrupt needs to be handled.
    if interrupts_enabled then
      let masked = rb(Ports.iflag()) and rb(Ports.ienable()) and 0x1f
      if masked > 0 then
        handle_int(masked)
      end
    end

    // If handle_interrupt fired, it just disables interrupts, pushes PC, and
    // sets PC to the interrupt vector. Either way, I just keep executing.

    // If we're stopped or halted, don't run any instruction.
    // When halted, however, we let the display keep ticking around.

    let cycles = if stopped or halted then
      1
    else
      run_single_opcode(pc_rb_plus())
    end

    if dma_counter > 0 then
      dma_counter = if cycles > dma_counter then 0 else dma_counter - cycles end
    end

    // There are several timers and things to bump:
    // DIV is blindly bumped every 64 cycles.
    // TIMA is bumped according to the TAC frequency (and if enabled)
    // The latter triggers interrupts.
    div_counter = div_counter + cycles
    if div_counter >= 64 then
      div_counter = div_counter - 64
      let a = Ports.tdiv()
      wb(a, rb(a) + 1)
    end

    let tac = rb(Ports.tac())
    if (tac and 0x04) > 0 then
      timer_counter = timer_counter + cycles
      let max = try timer_maximums((tac and 3).usize()) else 0xff end
      if timer_counter >= max then
        timer_counter = timer_counter - max
        var t = rb(Ports.tima())
        if t == 0xff then
          t = rb(Ports.tma())
          request_int(Interrupts.timer_mask())
        else
          t = t + 1
        end
        wb(Ports.tima(), t)
      end
    end

    var keep_running = true

    // The last and most important counter is the one for LCD modes.
    // We bump our internal count of that one too.
    // Do nothing here if the LCD is disabled.
    let ctrl = rb(Ports.lcdc())
    if (ctrl and 0x80) > 0 then
      var stat = rb(Ports.stat())
      var mode = stat and 3
      var ly_changed = false

      // Bump the counter.
      frame_counter = frame_counter + cycles
      let max = try lcd_mode_lengths(mode.usize()) else 0 end
      if frame_counter >= max then
        match mode
        | 1 => // Ending VBlank. Move to mode 2, OAM locked.
          gpu.lock_oam(oam = None)
          mode = 2
          if (stat and 0x20) > 0 then request_int(Interrupts.stat_mask()) end
          wb(Ports.ly(), 0) // Move to line 0.
          ly_changed = true

        | 2 => // Moving to mode 3, full lockup.
          gpu.lock_vram(vram = None)
          gpu.paint_line(rb(Ports.ly()), rb(Ports.lcdc()), rb(Ports.scy()),
              rb(Ports.scx()), rb(Ports.wy()), rb(Ports.wx()), rb(Ports.bgp()),
              rb(Ports.obp0()), rb(Ports.obp1()))
          mode = 3
          // No interrupt for this mode.

        | 3 => // Moving to HBlank.
          mode = 0
          if (stat and 0x08) > 0 then request_int(Interrupts.stat_mask()) end

          // If we don't have the memory back yet, we need to wait for the GPU.
          match oam  | None => keep_running = false end
          match vram | None => keep_running = false end

        | 0 => // Leaving HBlank for either VBlank or mode 2.
          // First, bump the line number.
          ly_changed = true
          var ly = rb(Ports.ly())
          ly = ly + 1
          if ly > 143 then // Move to VBlank
            mode = 1
            if (stat and 0x10) > 0 then request_int(Interrupts.stat_mask()) end
            request_int(Interrupts.vblank_mask())
            gpu.vblank()
          else // Move to mode 2 (and lock the OAM)
            mode = 2
            gpu.lock_oam(oam = None)
            if (stat and 0x20) > 0 then request_int(Interrupts.stat_mask()) end
          end

          wb(Ports.ly(), ly)
        end

        // Set stat to have the new mode.
        stat = (stat and 0xfc) or mode
      end

      // Separately, during VBlank, we still want to keep the LY moving.
      // A full line is 456 clks, or 114 cycles long.
      if mode == 1 then
        if ((frame_counter - cycles) / 114) < (frame_counter / 114) then
          wb(Ports.ly(), rb(Ports.ly()) + 1)
          ly_changed = true
        end
      end

      // And finally, we check the coincidence interrupt if LY changed.
      if ly_changed then
        let ly = rb(Ports.ly())
        Debug("New LY: " + ly.string())
        let lyc = rb(Ports.lyc())
        let coincidence = ly == lyc

        // Trigger the interrupt only on this first transition.
        if coincidence and ((stat and 0x40) > 0) then
          request_int(Interrupts.stat_mask())
        end

        stat = (stat and (not 0x04)) or (if coincidence then 0x04 else 0 end)
      end

      wb(Ports.stat(), stat)
    end

    if keep_running then run() end


  // Returns the number of (real machine, not clock) cycles required.
  fun ref run_single_opcode(op: U8): USize =>
    var cycles: USize = 1
    match op
    // 8-bit Loads - immediate
    | 0x06 => cycles = 2; op_ld_imm(2)
    | 0x0e => cycles = 2; op_ld_imm(3)
    | 0x16 => cycles = 2; op_ld_imm(4)
    | 0x1e => cycles = 2; op_ld_imm(5)
    | 0x26 => cycles = 2; op_ld_imm(6)
    | 0x2e => cycles = 2; op_ld_imm(7)
    // 8-bit loads - register copy
    | 0x78 => op_ld_reg(0, 2)
    | 0x79 => op_ld_reg(0, 3)
    | 0x7a => op_ld_reg(0, 4)
    | 0x7b => op_ld_reg(0, 5)
    | 0x7c => op_ld_reg(0, 6)
    | 0x7d => op_ld_reg(0, 7)
    | 0x7e => cycles = 2; op_ld_hl(0)
    | 0x7f => op_ld_reg(0, 0)
    | 0x40 => op_ld_reg(2, 2)
    | 0x41 => op_ld_reg(2, 3)
    | 0x42 => op_ld_reg(2, 4)
    | 0x43 => op_ld_reg(2, 5)
    | 0x44 => op_ld_reg(2, 6)
    | 0x45 => op_ld_reg(2, 7)
    | 0x46 => cycles = 2; op_ld_hl(2)
    | 0x47 => op_ld_reg(2, 0)
    | 0x48 => op_ld_reg(3, 2)
    | 0x49 => op_ld_reg(3, 3)
    | 0x4a => op_ld_reg(3, 4)
    | 0x4b => op_ld_reg(3, 5)
    | 0x4c => op_ld_reg(3, 6)
    | 0x4d => op_ld_reg(3, 7)
    | 0x4e => cycles = 2; op_ld_hl(3)
    | 0x4f => op_ld_reg(3, 0)
    | 0x50 => op_ld_reg(4, 2)
    | 0x51 => op_ld_reg(4, 3)
    | 0x52 => op_ld_reg(4, 4)
    | 0x53 => op_ld_reg(4, 5)
    | 0x54 => op_ld_reg(4, 6)
    | 0x55 => op_ld_reg(4, 7)
    | 0x56 => cycles = 2; op_ld_hl(4)
    | 0x57 => op_ld_reg(4, 0)
    | 0x58 => op_ld_reg(5, 2)
    | 0x59 => op_ld_reg(5, 3)
    | 0x5a => op_ld_reg(5, 4)
    | 0x5b => op_ld_reg(5, 5)
    | 0x5c => op_ld_reg(5, 6)
    | 0x5d => op_ld_reg(5, 7)
    | 0x5e => cycles = 2; op_ld_hl(5)
    | 0x5f => op_ld_reg(5, 0)
    | 0x60 => op_ld_reg(6, 2)
    | 0x61 => op_ld_reg(6, 3)
    | 0x62 => op_ld_reg(6, 4)
    | 0x63 => op_ld_reg(6, 5)
    | 0x64 => op_ld_reg(6, 6)
    | 0x65 => op_ld_reg(6, 7)
    | 0x66 => cycles = 2; op_ld_hl(6)
    | 0x67 => op_ld_reg(6, 0)
    | 0x68 => op_ld_reg(7, 2)
    | 0x69 => op_ld_reg(7, 3)
    | 0x6a => op_ld_reg(7, 4)
    | 0x6b => op_ld_reg(7, 5)
    | 0x6c => op_ld_reg(7, 6)
    | 0x6d => op_ld_reg(7, 7)
    | 0x6e => cycles = 2; op_ld_hl(7)
    | 0x6f => op_ld_reg(7, 0)
    | 0x70 => cycles = 2; op_ld_to_hl(2)
    | 0x71 => cycles = 2; op_ld_to_hl(3)
    | 0x72 => cycles = 2; op_ld_to_hl(4)
    | 0x73 => cycles = 2; op_ld_to_hl(5)
    | 0x74 => cycles = 2; op_ld_to_hl(6)
    | 0x75 => cycles = 2; op_ld_to_hl(7)
    | 0x36 => cycles = 3; op_ld_to_hl(2)

    // 8-bit Loads - Extras with A
    | 0x0a => cycles = 2; op_ld_a_indir(r_bc())
    | 0x1a => cycles = 2; op_ld_a_indir(r_de())
    | 0xfa => cycles = 4; op_ld_a_indir(pc_rw_plus())
    | 0x3e => cycles = 2; op_ld_imm(0)
    | 0x02 => cycles = 2; op_ld_indir_a(r_bc())
    | 0x12 => cycles = 2; op_ld_indir_a(r_de())
    | 0x77 => cycles = 2; op_ld_indir_a(r_hl())
    | 0xea => cycles = 4; op_ld_indir_a(pc_rw_plus())

    // A and HL Loads with inc and dec
    | 0x3a => cycles = 2; op_ldp_ahl(-1)
    | 0x32 => cycles = 2; op_ldp_hla(-1)
    | 0x2a => cycles = 2; op_ldp_ahl(1)
    | 0x22 => cycles = 2; op_ldp_hla(1)

    // HRAM loads: LD A, (C) and LD (C), A
    | 0xf2 => cycles = 2; try op_ld_a_indir(regs(3).u16() + 0xff00) end
    | 0xe2 => cycles = 2; try op_ld_indir_a(regs(3).u16() + 0xff00) end
    // HRAM loads: with immediate
    | 0xe0 => cycles = 3; op_ld_a_indir(pc_rb_plus().u16() + 0xff00)
    | 0xf0 => cycles = 3; op_ld_indir_a(pc_rb_plus().u16() + 0xff00)

    // 16-bit loads
    | 0x01 => cycles = 3; w_bc(pc_rw_plus())
    | 0x11 => cycles = 3; w_de(pc_rw_plus())
    | 0x21 => cycles = 3; w_hl(pc_rw_plus())
    | 0x31 => cycles = 3; sp = pc_rw_plus()
    // SP-related loads
    | 0xf9 => cycles = 2; sp = r_hl()
    | 0xf8 => cycles = 3; op_ld_hl_sp()
    | 0x08 => cycles = 5; ww(pc_rw_plus(), sp)

    // Stack loads
    | 0xf5 => cycles = 4; pushw(r_af())
    | 0xc5 => cycles = 4; pushw(r_bc())
    | 0xd5 => cycles = 4; pushw(r_de())
    | 0xe5 => cycles = 4; pushw(r_hl())
    | 0xf1 => cycles = 3; w_af(popw())
    | 0xc1 => cycles = 3; w_bc(popw())
    | 0xd1 => cycles = 3; w_de(popw())
    | 0xe1 => cycles = 3; w_hl(popw())


    // 8-bit ALU ops
    // ADD
    | 0x87 => op_add_reg(0)
    | 0x80 => op_add_reg(2)
    | 0x81 => op_add_reg(3)
    | 0x82 => op_add_reg(4)
    | 0x83 => op_add_reg(5)
    | 0x84 => op_add_reg(6)
    | 0x85 => op_add_reg(7)
    | 0x86 => cycles = 2; op_add_lit(rb(r_hl()))
    | 0xc6 => cycles = 2; op_add_lit(pc_rb_plus())
    // ADC
    | 0x8f => op_adc_reg(0)
    | 0x88 => op_adc_reg(2)
    | 0x89 => op_adc_reg(3)
    | 0x8a => op_adc_reg(4)
    | 0x8b => op_adc_reg(5)
    | 0x8c => op_adc_reg(6)
    | 0x8d => op_adc_reg(7)
    | 0x8e => cycles = 2; op_add_lit(rb(r_hl()), true)
    | 0xce => cycles = 2; op_add_lit(pc_rb_plus(), true)
    // SUB
    | 0x97 => op_sub_reg(0)
    | 0x90 => op_sub_reg(2)
    | 0x91 => op_sub_reg(3)
    | 0x92 => op_sub_reg(4)
    | 0x93 => op_sub_reg(5)
    | 0x94 => op_sub_reg(6)
    | 0x95 => op_sub_reg(7)
    | 0x96 => cycles = 2; op_sub_lit(rb(r_hl()))
    | 0xd6 => cycles = 2; op_sub_lit(pc_rb_plus())
    // SBC
    | 0x9f => op_sbc_reg(0)
    | 0x98 => op_sbc_reg(2)
    | 0x99 => op_sbc_reg(3)
    | 0x9a => op_sbc_reg(4)
    | 0x9b => op_sbc_reg(5)
    | 0x9c => op_sbc_reg(6)
    | 0x9d => op_sbc_reg(7)
    | 0x9e => cycles = 2; op_sub_lit(rb(r_hl()), true)
    // Nonexistent | 0xde => cycles = 2; op_sub_lit(pc_rb_plus(), true)

    // AND
    | 0xa7 => op_and_reg(0)
    | 0xa0 => op_and_reg(2)
    | 0xa1 => op_and_reg(3)
    | 0xa2 => op_and_reg(4)
    | 0xa3 => op_and_reg(5)
    | 0xa4 => op_and_reg(6)
    | 0xa5 => op_and_reg(7)
    | 0xa6 => cycles = 2; op_and_lit(rb(r_hl()))
    | 0xe6 => cycles = 2; op_and_lit(pc_rb_plus())
    // OR
    | 0xb7 => op_or_reg(0)
    | 0xb0 => op_or_reg(2)
    | 0xb1 => op_or_reg(3)
    | 0xb2 => op_or_reg(4)
    | 0xb3 => op_or_reg(5)
    | 0xb4 => op_or_reg(6)
    | 0xb5 => op_or_reg(7)
    | 0xb6 => cycles = 2; op_or_lit(rb(r_hl()))
    | 0xf6 => cycles = 2; op_or_lit(pc_rb_plus())
    // XOR
    | 0xaf => op_xor_reg(0)
    | 0xa8 => op_xor_reg(2)
    | 0xa9 => op_xor_reg(3)
    | 0xaa => op_xor_reg(4)
    | 0xab => op_xor_reg(5)
    | 0xac => op_xor_reg(6)
    | 0xad => op_xor_reg(7)
    | 0xae => cycles = 2; op_xor_lit(rb(r_hl()))
    | 0xee => cycles = 2; op_xor_lit(pc_rb_plus())

    // CP
    | 0xbf => op_cp_reg(0)
    | 0xb8 => op_cp_reg(2)
    | 0xb9 => op_cp_reg(3)
    | 0xba => op_cp_reg(4)
    | 0xbb => op_cp_reg(5)
    | 0xbc => op_cp_reg(6)
    | 0xbd => op_cp_reg(7)
    | 0xbe => cycles = 2; op_cp_lit(rb(r_hl()))
    | 0xfe => cycles = 2; op_cp_lit(pc_rb_plus())

    // INC
    | 0x3c => op_inc(0)
    | 0x04 => op_inc(2)
    | 0x0c => op_inc(3)
    | 0x14 => op_inc(4)
    | 0x1c => op_inc(5)
    | 0x24 => op_inc(6)
    | 0x2c => op_inc(7)
    | 0x34 => cycles = 3; op_inc_hl()
    // DEC
    | 0x3d => op_dec(0)
    | 0x05 => op_dec(2)
    | 0x0d => op_dec(3)
    | 0x15 => op_dec(4)
    | 0x1d => op_dec(5)
    | 0x25 => op_dec(6)
    | 0x2d => op_dec(7)
    | 0x35 => cycles = 3; op_dec_hl()

    // 16-bit ALU
    // ADD HL, n
    | 0x09 => cycles = 2; op_add_wide(r_bc())
    | 0x19 => cycles = 2; op_add_wide(r_de())
    | 0x29 => cycles = 2; op_add_wide(r_hl())
    | 0x39 => cycles = 2; op_add_wide(sp)
    // ADD SP, n
    | 0xe8 => cycles = 4; op_add_sp(pc_rb_plus())

    // INC
    | 0x03 => cycles = 2; w_bc(r_bc() + 1)
    | 0x13 => cycles = 2; w_de(r_de() + 1)
    | 0x23 => cycles = 2; w_hl(r_hl() + 1)
    | 0x33 => cycles = 2; sp = sp + 1
    // INC
    | 0x0b => cycles = 2; w_bc(r_bc() - 1)
    | 0x1b => cycles = 2; w_de(r_de() - 1)
    | 0x2b => cycles = 2; w_hl(r_hl() - 1)
    | 0x3b => cycles = 2; sp = sp - 1


    // Miscellany
    // DAA - The dreaded retarded Binary Coded Decimal fixer thingy.
    | 0x27 => op_daa()
    // CPL - Flips all bits of A
    | 0x2f => op_cpl()
    // CCF - Complement carry flag
    | 0x3f => op_ccf()
    // SCF - Set carry flag
    | 0x37 => op_scf()
    // NOP
    | 0x00 => None
    // HALT
    | 0x76 => halted = true
    // STOP
    | 0x10 => pc_rb_plus(); stopped = true
    // DI
    | 0xf3 => interrupt_counter = -2
    // EI
    | 0xfb => interrupt_counter = 2

    // Rotates and shifts
    // RLCA
    | 0x07 => rotate_reg(0, false, false)
    // RLA
    | 0x17 => rotate_reg(0, false, true)
    // RRCA
    | 0x0f => rotate_reg(0, true, false)
    // RRA
    | 0x1f => rotate_reg(0, true, true)

    // Jumps
    // JP nn
    | 0xc3 => cycles = 3; pc = pc_rw_plus()
    // JP cc,nn
    | 0xc2 => cycles = 3; op_jump_immed(not r_fZ())
    | 0xca => cycles = 3; op_jump_immed(    r_fZ())
    | 0xd2 => cycles = 3; op_jump_immed(not r_fC())
    | 0xda => cycles = 3; op_jump_immed(    r_fC())
    // JP (HL)
    // TODO: This uses the value actually in HL, not where (HL) points.
    // The spec is a little unclear here. Also, just 1 cycle, really?
    | 0xe9 => pc = r_hl()
    // JR n - relative jump by signed immediate byte
    | 0x18 => cycles = 2; op_jump_rel_immed(true)
    // JR n - relative jump by signed immediate byte
    | 0x20 => cycles = 2; op_jump_rel_immed(not r_fZ())
    | 0x28 => cycles = 2; op_jump_rel_immed(    r_fZ())
    | 0x30 => cycles = 2; op_jump_rel_immed(not r_fC())
    | 0x38 => cycles = 2; op_jump_rel_immed(    r_fC())
    // CALL nn
    | 0xcd => cycles = 3; op_call(true)
    // CALL cc, nn
    | 0xc4 => cycles = 3; op_call(not r_fZ())
    | 0xcc => cycles = 3; op_call(    r_fZ())
    | 0xd4 => cycles = 3; op_call(not r_fC())
    | 0xdc => cycles = 3; op_call(    r_fC())

    // Restarts
    // RST n
    | 0xc7 => cycles = 8; op_rst(0x00)
    | 0xcf => cycles = 8; op_rst(0x08)
    | 0xd7 => cycles = 8; op_rst(0x10)
    | 0xdf => cycles = 8; op_rst(0x18)
    | 0xe7 => cycles = 8; op_rst(0x20)
    | 0xef => cycles = 8; op_rst(0x28)
    | 0xf7 => cycles = 8; op_rst(0x30)
    | 0xff => cycles = 8; op_rst(0x38)

    // Returns
    // RET
    | 0xc9 => cycles = 2; op_ret(true)
    // RET cc
    | 0xc0 => cycles = 2; op_ret(not r_fZ())
    | 0xc8 => cycles = 2; op_ret(    r_fZ())
    | 0xd0 => cycles = 2; op_ret(not r_fC())
    | 0xd8 => cycles = 2; op_ret(    r_fC())
    // RETI
    // TODO: Does this have the same next-two-cycles behavior as EI? Coded thus.
    | 0xd9 => cycles = 2; interrupt_counter = 2; op_ret(true)

    // CB extended opcodes
    | 0xcb => run_single_extended_opcode(pc_rb_plus())

    else
      Debug("Unknown standard opcode: " + op.string())
    end
    cycles

  // Returns the number of (real machine, not clock) cycles required.
  fun ref run_single_extended_opcode(op: U8): USize =>
    // Defaults to 2 here because of the extra opcode read.
    var cycles: USize = 2
    match op
    // RLC
    | 0x07 => rotate_reg(0, false, false)
    | 0x00 => rotate_reg(2, false, false)
    | 0x01 => rotate_reg(3, false, false)
    | 0x02 => rotate_reg(4, false, false)
    | 0x03 => rotate_reg(5, false, false)
    | 0x04 => rotate_reg(6, false, false)
    | 0x05 => rotate_reg(7, false, false)
    | 0x06 => cycles = 4; rotate_hl(false, false)
    // RL
    | 0x17 => rotate_reg(0, false, true)
    | 0x10 => rotate_reg(2, false, true)
    | 0x11 => rotate_reg(3, false, true)
    | 0x12 => rotate_reg(4, false, true)
    | 0x13 => rotate_reg(5, false, true)
    | 0x14 => rotate_reg(6, false, true)
    | 0x15 => rotate_reg(7, false, true)
    | 0x16 => cycles = 4; rotate_hl(false, true)
    // RRC
    | 0x0f => rotate_reg(0, true, false)
    | 0x08 => rotate_reg(2, true, false)
    | 0x09 => rotate_reg(3, true, false)
    | 0x0a => rotate_reg(4, true, false)
    | 0x0b => rotate_reg(5, true, false)
    | 0x0c => rotate_reg(6, true, false)
    | 0x0d => rotate_reg(7, true, false)
    | 0x0e => cycles = 4; rotate_hl(true, false)
    // RR
    | 0x1f => rotate_reg(0, true, true)
    | 0x18 => rotate_reg(2, true, true)
    | 0x19 => rotate_reg(3, true, true)
    | 0x1a => rotate_reg(4, true, true)
    | 0x1b => rotate_reg(5, true, true)
    | 0x1c => rotate_reg(6, true, true)
    | 0x1d => rotate_reg(7, true, true)
    | 0x1e => cycles = 4; rotate_hl(true, true)

    // SLA
    | 0x27 => shift_reg(0, false, false)
    | 0x20 => shift_reg(2, false, false)
    | 0x21 => shift_reg(3, false, false)
    | 0x22 => shift_reg(4, false, false)
    | 0x23 => shift_reg(5, false, false)
    | 0x24 => shift_reg(6, false, false)
    | 0x25 => shift_reg(7, false, false)
    | 0x26 => cycles = 4; shift_hl(false, false)
    // SRA
    | 0x2f => shift_reg(0, true, true)
    | 0x28 => shift_reg(2, true, true)
    | 0x29 => shift_reg(3, true, true)
    | 0x2a => shift_reg(4, true, true)
    | 0x2b => shift_reg(5, true, true)
    | 0x2c => shift_reg(6, true, true)
    | 0x2d => shift_reg(7, true, true)
    | 0x2e => cycles = 4; shift_hl(true, true)
    // SRL
    | 0x3f => shift_reg(0, true, false)
    | 0x38 => shift_reg(2, true, false)
    | 0x39 => shift_reg(3, true, false)
    | 0x3a => shift_reg(4, true, false)
    | 0x3b => shift_reg(5, true, false)
    | 0x3c => shift_reg(6, true, false)
    | 0x3d => shift_reg(7, true, false)
    | 0x3e => cycles = 4; shift_hl(true, false)

    // BIT
    | 0x47 => bit_test_reg(0)
    | 0x40 => bit_test_reg(2)
    | 0x41 => bit_test_reg(3)
    | 0x42 => bit_test_reg(4)
    | 0x43 => bit_test_reg(5)
    | 0x44 => bit_test_reg(6)
    | 0x45 => bit_test_reg(7)
    | 0x46 => cycles = 4; bit_test_hl()
    // SET
    | 0xc7 => bit_set_reg(0)
    | 0xc0 => bit_set_reg(2)
    | 0xc1 => bit_set_reg(3)
    | 0xc2 => bit_set_reg(4)
    | 0xc3 => bit_set_reg(5)
    | 0xc4 => bit_set_reg(6)
    | 0xc5 => bit_set_reg(7)
    | 0xc6 => cycles = 4; bit_set_hl()
    // RES
    | 0x87 => bit_reset_reg(0)
    | 0x80 => bit_reset_reg(2)
    | 0x81 => bit_reset_reg(3)
    | 0x82 => bit_reset_reg(4)
    | 0x83 => bit_reset_reg(5)
    | 0x84 => bit_reset_reg(6)
    | 0x85 => bit_reset_reg(7)
    | 0x86 => cycles = 4; bit_reset_hl()

    else
      Debug("Unknown extended opcode " + op.string())
    end
    cycles


  // 8-bit Loads
  fun ref op_ld_imm(d: USize) => try regs(d) = pc_rb_plus() end
  fun ref op_ld_reg(d: USize, s: USize) => try regs(d) = regs(s) end
  fun ref op_ld_hl(d: USize) => try regs(d) = rb(r_hl()) end
  fun ref op_ld_to_hl(s: USize) => try wb(r_hl(), regs(s)) end
  fun ref op_ld_a_indir(addr: U16) => try regs(0) = rb(addr) end
  fun ref op_ld_indir_a(addr: U16) => try wb(addr, regs(0)) end
  fun ref op_ldp_ahl(delta: I16) =>
    try
      let hl = r_hl()  // TODO: Double-check the math here - sign safe?
      regs(0) = rb(hl)
      w_hl((hl.i16() + delta).u16())
    end
  fun ref op_ldp_hla(delta: I16) =>
    try
      let hl = r_hl()
      wb(hl, regs(0))
      w_hl((hl.i16() + delta).u16())
    end

  // 16-bit screwy SP load - loads SP+n into HL, with addition-ish flags.
  fun ref op_ld_hl_sp() =>
    c_fZ()
    c_fN()
    let b = pc_rb_plus()
    set_fH(((sp and 0xf)  + (b.u16() and 0xf)) > 0xff)
    set_fC(((sp and 0xff) +  b.u16())          > 0xff)
    w_hl(sp + b.i8().i16().u16())

  // Addition
  fun ref op_add_reg(s: USize) => try op_add_lit(regs(s)) end
  fun ref op_adc_reg(s: USize) => try op_add_lit(regs(s), true) end

  // Sets the flags for addition and returns the result.
  fun ref op_add_inner(a: U8, b: U8, carry: Bool = false): U8 =>
    let c: U8 = if carry and r_fC() then 1 else 0 end
    set_fH(((a and 0xf) + (b and 0xf) + c) > 0xf)
    set_fC((a.u16() + b.u16() + c.u16()) > 0xff)
    let r = a + b + c
    set_fZ(r == 0)
    c_fN()
    r

  fun ref op_add_lit(v: U8, carry: Bool = false) =>
    try regs(0) = op_add_inner(regs(0), v, carry) end

  // Subtraction
  fun ref op_sub_reg(s: USize) => try op_sub_lit(regs(s)) end
  fun ref op_sbc_reg(s: USize) => try op_sub_lit(regs(s), true) end

  fun ref op_sub_lit(v: U8, carry: Bool = false) =>
    try regs(0) = op_sub_inner(regs(0), v, carry) end

  // Called by SUB, SBC, CP and DEC.
  fun ref op_sub_inner(a: U8, b: U8, carry: Bool = false): U8 =>
    let b' = if carry and r_fC() then b + 1 else b end
    set_fH( ( ((a and 0xf) or 0x10) - (b' and 0xf) ) > 0xf)
    set_fC( ((a.u16() or 0x100) - b'.u16()) > 0xff )
    let r = a + b'
    set_fZ(r == 0)
    s_fN()
    r


  // Bitwise ops
  fun ref op_and_reg(s: USize) => try op_and_lit(regs(s)) end
  fun ref op_and_lit(v: U8) =>
    try
      let r = regs(0) and v
      regs(0) = r
      set_fZ(r == 0)
      c_fN()
      s_fH() // TODO: This is strange. Check if it's a typo.
      c_fC()
    end

  fun ref op_or_reg(s: USize) => try op_or_lit(regs(s)) end
  fun ref op_or_lit(v: U8) =>
    try
      let r = regs(0) or v
      regs(0) = r
      set_fZ(r == 0)
      c_fN()
      c_fH()
      c_fC()
    end

  fun ref op_xor_reg(s: USize) => try op_xor_lit(regs(s)) end
  fun ref op_xor_lit(v: U8) =>
    try
      let r = regs(0) xor v
      regs(0) = r
      set_fZ(r == 0)
      c_fN()
      c_fH()
      c_fC()
    end


  fun ref op_cp_reg(s: USize) => try op_cp_lit(regs(s)) end
  // CP is SUB but discard the result.
  fun ref op_cp_lit(v: U8) => try op_sub_inner(regs(0), v) end

  // INC and DEC
  // Catch with these: They don't change C for some reason. Save and set it.
  fun ref op_inc(r: USize) =>
    try
      let oldC = r_fC()
      regs(r) = op_add_inner(regs(r), 1)
      set_fC(oldC)
    end
  fun ref op_inc_hl() =>
    let a = r_hl()
    let oldC = r_fC()
    wb(a, op_add_inner(rb(a), 1))
    set_fC(oldC)

  fun ref op_dec(r: USize) =>
    try
      let oldC = r_fC()
      regs(r) = op_sub_inner(regs(r), 1)
      set_fC(oldC)
    end
  fun ref op_dec_hl() =>
    let a = r_hl()
    let oldC = r_fC()
    wb(a, op_sub_inner(rb(a), 1))
    set_fC(oldC)

  fun ref op_add_wide(v: U16) =>
    c_fN()
    let a = r_hl()

    set_fH( ((a and 0xfff) + (v and 0xfff)) > 0xfff )
    set_fC( (a.u32() + v.u32()) > 0xffff )

    w_hl(a + v)

  fun ref op_add_sp(v: U8) =>
    c_fZ()
    c_fN()
    set_fH( ((sp and 0xf).u8() + (v and 0xf)) > 0xf )
    set_fC( ((sp and 0xff) + v.u16()) > 0xff)
    sp = sp + v.i8().i16().u16()


  fun ref op_daa() =>
    try
      var a = regs(0).u16()
      if r_fN() then
        if r_fH() then
          a = (a - 6) and 0xff
        end
        if r_fC() then
          a = a - 0x60
        end
      else
        if r_fH() or ((a and 0xf) > 9) then
          a = a + 0x06
        end
        if r_fC() or (a > 0x9f) then
          a = a + 0x60
        end
      end

      c_fH()
      set_fC((a and 0x100) == 0x100)
      let a' = a.u8()
      set_fZ(a' == 0)
      regs(0) = a'
    end

  fun ref op_cpl() =>
    try
      regs(0) = not regs(0)
      s_fN()
      s_fH()
    end

  fun ref op_ccf() =>
    c_fN()
    c_fH()
    set_fC(not r_fC())
  fun ref op_scf() =>
    c_fN()
    c_fH()
    s_fC()


  // For rotate operations, we always have:
  // - Clear N and H
  // - C is the old bit 7/0
  // - Z is whether the result is 0.
  // thruCarry means that the OLD C is rotated in, rather than the other end.
  fun ref rotate_inner(v: U8, rightward: Bool = true, thruCarry: Bool = false): U8 =>
    let res = match (rightward, thruCarry)
      | (true, true) => rotate_right_carry(v)
      | (true, false) => rotate_right(v)
      | (false, true) => rotate_left_carry(v)
      | (false, false) => rotate_left(v)
      else 0
      end
    c_fN()
    c_fH()
    set_fZ(res == 0)
    res

  fun ref rotate_reg(r: USize, rightward: Bool = true, thruCarry: Bool = false) =>
    try regs(r) = rotate_inner(regs(r), rightward, thruCarry) end

  fun ref rotate_hl(rightward: Bool = true, thruCarry: Bool = false) =>
    let a = r_hl()
    wb(a, rotate_inner(rb(a), rightward, thruCarry))

  fun ref rotate_right_carry(v: U8): U8 =>
    let c = (v and 1) == 1
    let r = (v >> 1) or (if r_fC() then 0x80 else 0 end)
    set_fC(c)
    r

  fun ref rotate_right(v: U8): U8 =>
    let b = (v and 1)
    let r = (v >> 1) or (b << 7)
    set_fC(b == 1)
    r

  fun ref rotate_left_carry(v: U8): U8 =>
    let c = (v and 0x80) != 0
    let r = (v << 1) or (if r_fC() then 1 else 0 end)
    set_fC(c)
    r

  fun ref rotate_left(v: U8): U8 =>
    let b = (v and 0x80)
    let r = (v << 1) or (b >> 7)
    set_fC(b != 0)
    r

  fun ref shift_right(v: U8, arithmetic: Bool): U8 =>
    set_fC((v and 1) != 0)
    c_fN()
    c_fH()
    let r = (v >> 1) or (if arithmetic then v and 0x80 else 0 end)
    set_fZ(r == 0)
    r

  fun ref shift_left(v: U8): U8 =>
    set_fC((v and 0x80) != 0)
    c_fN()
    c_fH()
    let r = v << 1
    set_fZ(r == 0)
    r

  fun ref shift_reg(r: USize, rightward: Bool, arithmetic: Bool) =>
    try
      let v = regs(r)
      regs(r) = if rightward then
          shift_right(v, arithmetic) else shift_left(v) end
    end

  fun ref shift_hl(rightward: Bool, arithmetic: Bool) =>
    let a = r_hl()
    wb(a, if rightward then
        shift_right(rb(a), arithmetic) else shift_left(rb(a)) end)

  // TODO: Double-check how these are actually encoded. I'm assuming the bit is
  // encoded as a literal following byte, but there might be some other magic.
  fun ref bit_test_inner(v: U8) =>
    c_fN()
    s_fH()
    let bit = pc_rb_plus()
    set_fZ((v and (1 << bit)) == 0)

  fun ref bit_test_reg(r: USize) => try bit_test_inner(regs(r)) end
  fun ref bit_test_hl() => bit_test_inner(rb(r_hl()))

  fun ref bit_set_inner(v: U8): U8 =>
    let bit = pc_rb_plus()
    v or (1 << bit)

  fun ref bit_set_reg(r: USize) => try regs(r) = bit_set_inner(regs(r)) end
  fun ref bit_set_hl() => let a = r_hl(); wb(a, bit_set_inner(rb(a)))

  fun ref bit_reset_inner(v: U8): U8 =>
    let bit = pc_rb_plus()
    v and (not (1 << bit))

  fun ref bit_reset_reg(r: USize) => try regs(r) = bit_reset_inner(regs(r)) end
  fun ref bit_reset_hl() => let a = r_hl(); wb(a, bit_reset_inner(rb(a)))

  fun ref op_jump_immed(cond: Bool) =>
    let target = pc_rw_plus()
    if cond then pc = target end

  fun ref op_jump_rel_immed(cond: Bool) =>
    let offset = pc_rb_plus().i8().i16().u16()
    if cond then pc = pc + offset end

  fun ref op_call(cond: Bool) =>
    let target = pc_rw_plus()
    if cond then
      pushw(pc)
      pc = target
    end

  fun ref op_rst(target: U16) =>
    pushw(pc)
    pc = target

  fun ref op_ret(cond: Bool) =>
    if cond then
      pc = popw()
    end


