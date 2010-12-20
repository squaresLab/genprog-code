rbfiles = Dir.glob("robus*.c")
rbfiles.map{|x| x.split(".")[0]}.each do |x|
	`gcc -O2 -S #{x}.c`
	`diff repair.s #{x}.s > #{x}.diff`
	`wc -l #{x}.diff > #{x}.count`
end
