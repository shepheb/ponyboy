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

    try
      with file = OpenFile(FilePath(env.root as AmbientAuth, name, FileCaps + FileRead + FileStat)) as File do
        let bytes : Array[U8] val = file.read(file.size())
        let cpu = CPU.reset(env, bytes)
        cpu.run()
      end
    else
      env.out.print("Failed to load ROM.")
    end

