methods = File.new('method_list','r').to_a.map{|x| x.split(':')[0].chomp}
#puts methods
methods.each do |m|
	`cp #{m} functions/`
end
