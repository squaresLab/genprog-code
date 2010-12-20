input = File.new(ARGV[0],'r').to_a
input.map!{|x| x.to_s.split(':')[2].chomp.to_f}
vals = Hash.new(0)
input.each do |val|
	vals[val] = vals[val] + 1
end
vals.keys.sort.each do |k|
	puts "#{k}: #{vals[k]}"
end
puts "Begin keys:"
puts vals.keys.sort
puts "Begin vals"
puts vals.keys.sort.map{|x| vals[x]}
