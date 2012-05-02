(0..100).each do |x|
	`../repair run-normal --seed #{x} > normal.#{x}`
end
