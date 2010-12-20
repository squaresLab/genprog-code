methods = File.new('methods','r').to_a
methods.map{|x| x.to_s.chomp}.each do |method|
	puts "Running #{method}"
	`rm *.cache`
	puts "Removed cache"
	`../repair run-robust --mutate-func #{method} > #{method}`
end
