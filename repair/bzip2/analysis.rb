counts = Dir.glob("*.count")
nums = counts.map{|x| File.new(x.to_s.chomp,"r").to_a[0].split(" ")[0].to_i}
hist = Hash.new(0)
nums.each do |num|
	hist[num] = hist[num] + 1
end
hist.keys.sort.each do |key|
	puts "#{key}"
end
puts "--"
hist.keys.sort.each do |key|
	puts "#{hist[key]}"
end
