# Gamepony

Gameboy emulator written in Pony, to run in SDL.

## Gameboy Basics

The Gameboy is a microprocessor similar to a Z80 with a custom GPU. The original
was 4-color LCD greyscale.

The original clock speed was 4,194,304 Hz. (Note that some documents refer to
"machine cycles" while others refer to "clock cycles". There are 4 raw clock
cycles to a machine cycle (basic instruction, like a NOP).

### Gameboy Breakdown

The Gameboy is logically split into several parts:

- The CPU, which reads instructions and executes them.
- The memory unit, which supplies data to the CPU.
- The GPU, which renders frames based on various memory regions.
- The cartridge, which may have a complex mapping scheme based on particular
  registers.
- The input handlers, which accept user input on the physical buttons and ping
  them in the CPU.
- The sound system, which runs in parallel on its own coprocessor, driven by
  fairly straightforward envelopes and other synthesizer stuff.

## Pony Design

- Sound is trivial, it just gets occasional messages when its control registers
  are written.
    - One interesting catch with the sound is that its registers can be twiddled
      any time, including while sound is playing. Therefore to emulate sound
      correctly, the CPU timing rules must be followed decently.
- VRAM and OAM are tossed back and forth between the CPU and GPU. However, the
  GPU never writes it, only reads.
    - If we really want the GPU running in parallel (not necessary, but would
      help for speed on a multicore machine) then it's necessary to really
      exchange these.
- Interrupts, the STOP opcode, and the HALT opcode both suggest that a
  fundamentally async scheme is probably the best option.
- That makes a fully async memory controller that both the other two have tags
  for a possibility - but probably more work than it's worth. Just toss the iso
  back and forth and let each instruction be fully synchronous.
- The GPU needs a surface to draw on, 160x144, 4 colour greyscale.

### Timings

In order to simulate the timings with reasonable fidelity (for sound, see above)
it becomes necessary every instruction, or every few instructions, to keep the
emulator from racing ahead. Assuming competent implementation and compilation,
we have roughly a half-million CPU instructions per GB instruction, which is
plenty of time so long as things are cached decently and so on.

Some driver should wait, spinning if necessary, and then trigger each GB
instruction to be processed, etc.

