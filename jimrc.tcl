puts "Loaded jimrc.tcl"
catch {
	set incname includes.zip
	foreach dir {./ tcc/ ../} { 
		set incpath [file join $dir $incname]
		if {[file exists $incpath]} {
			set tcc4tcl::dir $incpath
			puts "set include to $incpath"
			break
		}
	}
}
