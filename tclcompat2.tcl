# https://wiki.tcl-lang.org/page/subcommands
#puts "loaded "
proc _subcommands {cmd content} {
    #array set a $content
    if [dict exists $content $cmd] {
        return [uplevel 1 [dict get $content $cmd]]
    } else {
        return [uplevel 1 [dict get $content *]]
    }
}

# setup info sharedlibext
proc sharedlibext {} {
    set outfileext .so
    switch -glob -- $::tcl_platform(os)-$::tcl_platform(pointerSize) {
        "Linux-*" - "linux-*" {
            set outfileext .so
        }
        "Windows*" - "windows*" - "mingw*" {
            set outfileext .dll
        }
    }
    return $outfileext
}
catch {rename ::info ::_info}
proc ::info {cmd args} {
     _subcommands $cmd {
         test {return "test_ok $args"}
         sharedlibextension {
             return [sharedlibext]
         }
         * {
             return [uplevel 1 ::_info $cmd $args]
         }
     }
}

# setup package require more compatible with tcl
proc package_require {name {version 1.0}} {
    proc package_exists {path name} {
        foreach fname [list pkgIndex.tcl $name.tcl $name[info sharedlibextension] ${name}.so] {
            #puts "searchin $path $fname"
            if {[file exists [file join $path $fname]]} {
                return $fname
            }
        }
        return ""
    }
    # first make sure package isn't loaded already
    if {$name in [_package names]} {
        return 1.0
    }
    #puts "search package $name $version"
    set apath [string map {\\ /} [pwd]]
    set ppath [file join [pwd] $name]
    set pathlist $apath
    lappend pathlist $ppath
    lappend pathlist {*}$::auto_path
    foreach path $pathlist {
        set path [string map {\\ /} $path]
        if {[set fname [package_exists $path $name]] !=""} {
            #puts "found package $path $fname"
            set ext [string tolower [file extension $fname]]
            if {$fname == "pkgIndex.tcl"} {
                # handle pkgIndex files wich usually have ifneeded in their source
                set sourcecmd [namespace eval :: "
                set dir $path
                source [file join $path $fname]
                "]
                if {[lindex $sourcecmd 0] == $name} {
                    set sourcecmd [lindex $sourcecmd end]
                    namespace eval :: "
                    set dir $path
                    $sourcecmd
                    "
                }
                return 1.0
            }
            set fname [file join $path $fname]
            if {$ext == ".tcl"} {
                # handlöe normal tcl files as package loaders
                namespace eval :: "
                set dir $path
                source $fname
                "
                return 1.0
            }            
            if {$ext in [list .so .dll]} {
                # handle binary extensions
                namespace eval :: "load $fname"
                return 1.0
            }
        } 
    }
    return [_package require $name]
}

catch {rename ::package ::_package}
proc ::package {cmd args} {
     _subcommands $cmd {
         req {
             return [package_require {*}$args]
         }
         require {
             return [package_require {*}$args]
         }
         ifneeded {
             return $args
         }
         * {
             return [uplevel 1 ::_package $cmd {*}$args]
         }
     }
}
