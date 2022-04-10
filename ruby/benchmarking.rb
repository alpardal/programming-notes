
# stdlib:
require 'benchmark'

Benchmark.bm do |x|
  x.report('some test') do
    3.times { puts 'yay!' }
  end
end

# gem benchmark-ips:
require 'benchmark/ips'
require 'set'

list = ('a'..'zzzz').to_a
set = Set.new(list)

Benchmark.ips do |x|
  x.report('set access') { set.include?('foo') }
  x.report('ary access') { list.include?('foo') }
end

# profiler: stackprof (github.com/tmm1/stackprof)
require 'stackprof'

StackProf.run(mode: :cpu, out: 'results_file.dump') do
  5000.times { puts 'should test something...' }
end

# view profiling result:
puts `stackprof results_file.dump --text`
`rm results_file.dump`

# counting created objects:
# GC.stat(:total_allocated_object)

# with gem: allocation_tracer(github.com/ko1/allocation_tracer)
require 'allocation_tracer'

ObjectSpace::AllocationTracer.trace do
  1000.times {
    ['foo', {}]
  }
end
puts ObjectSpace::AllocationTracer.allocated_count_table
