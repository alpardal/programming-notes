<!-- source: https://medium.com/zendesk-engineering/running-a-child-process-in-ruby-properly-febd0a2b6ec8 -->

# Cases:


* 1: You want to run something, but don’t need its output

```ruby
  system('rm', '-r', directory) or raise "Failed to remove #{directory}"
```

Protip: if you want to run a command without arguments, you should actually use:

```ruby
  system(["ls", "ls"])`
```

 …because otherwise `system` will take your single string to be a shell string.


* 2: You want to capture stdout as a string (and inherit stderr):

```ruby
  stdout, status = Open3.capture2('unzip', '-l', zipfile)
  raise <error> unless status.success?
```

 (you can also pass a `stdin_data: <string>` option if you need to provide some input)


* 3: You want to capture stdout as a stream:

 … because it might be huge, or you want to process each line as it arrives.
 This allows you to write to `stdin` as a stream, too.

```ruby
  Open3.popen2('unzip', '-l', zipfile) do |stdin, stdout, status_thread|
    stdout.each_line do |line|
      puts "LINE: #{line}"
    end
    raise "Unzip failed"  unless status_thread.value.success?
  end
```

* 4: You need to inherit stdin

  This is a tricky edge case to figure out from the `open3` docs. Each of the functions in that module support the same options as `Open3.popen3`. Which says that its options are passed through to `Process.spawn`. Which has lots of options for controlling redirections and file descriptors. Unfortunately, the docs don’t mention one crucial point — whatever redirections you pass will be ignored, because `popen3` always overrides the redirection options with its own pipes.

  So if you do need to inherit `stdin` and `Kernel#system` won’t do, `IO.popen` may be your only choice. e.g. to inherit `stdin` and read `stdout` as a string:

```ruby
  # I don’t know why you're piping a zip file into `stdin`,
  # but I’m not the judging type...

  output = IO.popen(['unzip', '-l', '-'], in: :in) do |io|
    io.read
  end
  raise 'unzip failed' unless $?.success?
  puts output
```

* 5: Bonus round: avoiding deadlocks

  There’s one more gotcha when it comes to dealing with subprocesses: deadlocks. This can be an issue when you want to process both `stdout` and `stderr` of a child. If one of these pipes fill up their OS buffer with unconsumed output, the OS will block the process until somebody reads that buffered data. But if your parent process is busy waiting for the _other_ stream, you’ll get a deadlock.

  If you do decide to handle both streams yourself, you’ll need to use threads or `select` to read from whichever stream has data. But generally the best advice is to just to:

  - inherit `stderr` or redirect it to a file
  - combine `stderr` and `stdout` via `Open3.popen2e` or something similar

* 6: Gem suggestion:

  Stripe’s `subprocess` library is promising.
  It's a direct port of python’s `subprocess` module, which is one of the best modules I know of for doing this sort of thing. And stripe gets bonus points for explicitly disallowing `sh` syntax — you must provide an array of arguments.
