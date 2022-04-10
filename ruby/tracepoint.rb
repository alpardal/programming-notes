TracePoint.trace(:call, :return, :c_call, :c_return) do |tp|
  event = tp.event.to_s.sub(/(.+(call|return))/, '\2').rjust(6, " ")
  message = "#{event} of #{tp.defined_class}##{tp.callee_id} on #{tp.self.inspect}"
  # if you call `return` on any non-return events, it'll raise error
  message += " => #{tp.return_value.inspect}" if tp.event == :return || tp.event == :c_return
  puts(message)
end

class Adder
  def plus_1(n)
    n + 2
  end
end

adder = Adder.new
input = ARGV[0].to_i
puts(adder.plus_1(input))
