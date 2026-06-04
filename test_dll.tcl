# sample code for tcc4tcl
# build a dll
# dll implements 
# a callback-setter and
# a callback-caller
#
# next build intern/memory code
# to link against dll
# set callback to a tcl-proc
# call caller from c
# return value to tcl
#
catch {source tcc4tcl.tcl}
# init
set ::tcc4tcl::dir zip=includes.zip;# or the basedir of your includes

# dll
set handle [tcc4tcl::new libcb libcb dll]
$handle ccode {
    typedef int (callback) (int , int);
    callback *cbf;
    DLLEXPORT int setcallback (callback *funcadr) {
        cbf=funcadr;
        return 0;
    }
    DLLEXPORT int call_callback (int a, int b) {
        int r=(cbf)(a,b);
        return r;
    }
}
puts [$handle code];# show the generated code
if {[catch {$handle go} e]} {puts "Error: $e... continue"}
puts "libcb created"

# call dll
set handle [tcc4tcl::new]
$handle add_library_path .
$handle add_library cb

$handle ccode {
    typedef int (callback) (int , int);
    extern int setcallback (callback *funcadr);
    extern int call_callback (int a, int b);
}

$handle proc tcladd {int a int b} int {
    return [expr $a+$b]
}

#test some errorhandling from c-level
$handle proc raiseError {char* errmsg} ok {
    error $errmsg
}
$handle cproc testerr {} void {
    raiseError("user error for test");
}


proc tcladd_dyn {a b} {
    return [expr $a+$b]
}

$handle tclwrap tcladd_dyn {int a int b} int tcladd_dyn

$handle cproc initcb_static {} int {
    setcallback(&tcladd);
    return 0;
}
$handle cproc initcb_dynamic {} int {
    setcallback(&tcladd_dyn);
    return 0;
}


$handle cproc callcb {int a int b} int {
    int r= call_callback(a,b);
    return r;
}

puts [$handle code];# show the generated code
$handle go;# compile

initcb_static
puts "static tcl proc as callback 1+2= [callcb 1 2] == 3"
initcb_dynamic
puts "dynamic tcl proc as callback 1+2= [callcb 1 2] == 3"

puts "testing error handler"
testerr
puts "this should not be reached"

