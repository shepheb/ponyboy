use "files"

actor Main
  new create(env: Env) =>
    // Expects the first argument to be the name of the ROM file.
    let name = try
      env.args(1)
    else
      env.out.print("Expected the ROM name to be provided.")
      return
    end

    env.out.print("ROM file name: " + name)

    let caps = recover val FileCaps.set(FileRead).set(FileStat) end

    try
      with file = OpenFile(FilePath(env.root, name, caps)) as File do
        let bytes : Array[U8] val = file.read(file.size())
        let cpu = CPU.reset(bytes)
        cpu.run()
      end
    end

