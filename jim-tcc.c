/*
 *  JimTCC - Jim binding to Tiny C Compiler
 * 
 *  Copyright (c) 2007 Mark Janssen
 *  Copyright (c) 2014 Roy Keene
 *  Modified 2022 by Michael Richter
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */


# ifdef _WIN32
#  define TCC_TARGET_PE 1
# endif
#include <jim.h>

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include "libtcc.h"

//#define TCC4JIM_DODELETE
//#define TCC_RELOCATE_AUTO

// some code taken from tcl.h
typedef void *ClientData;


// don't include unistd.h since this conflicts with Jim/compat/unistd.h
#define	_UNISTD_H	1


/* In tcc 0.9.28 mob b671fc0 from Feb, 9th 2024 an API/ABI break appeared
in tcc_relocate, dropping the second argument
old version tcc_relocate(s, TCC_RELOCATE_AUTO)
new version tcc_relocate(s)
*/

#ifdef TCC_RELOCATE_AUTO
#define _TCC_RELOCATE_ tcc_relocate(s, TCC_RELOCATE_AUTO)
#else
#define _TCC_RELOCATE_ tcc_relocate(s)
#endif

struct JimTCCState {
	TCCState *s;
	int relocated;
	int initok;
    /* output type, see TCC_OUTPUT_XXX */
    int output_type;
};
typedef struct JimTCCState JimTCCState;
struct JimTCCObj {
    Jim_Interp* interp;
    Jim_Obj* obj;
};
typedef struct JimTCCObj JimTCCObj;

/*void tcc_delete_run(TCCState *s1);*/

static int Tcc4JimSetupCompiler(JimTCCState *ts) {
    //
    TCCState *s;
    s=ts->s;
    tcc_set_output_type(s,ts->output_type);
    #define TCC_USE_PTR_SIZE
    #ifndef TCC_USE_PTR_SIZE
        tcc_define_symbol(s, "__SIZE_TYPE__", "unsigned long");
        tcc_define_symbol(s, "__PTRDIFF_TYPE__", "long int");
        tcc_define_symbol(s, "__SIZE_TYPE__", "unsigned long int");
        tcc_define_symbol(s, "__PTRDIFF_TYPE__", "long int");
    #endif
    return 0;
}

static void Tcc4JimAppendSymbol (ClientData cdata, const char *name, const void *val) {
    struct JimTCCObj* ts;
    Jim_Obj *mystring;

    ts = (struct JimTCCObj *) cdata;
    Jim_Interp* interp= ts->interp;
    Jim_Obj* listObj=ts->obj;
    mystring=Jim_NewStringObj(interp,name, -1);
    Jim_ListAppendElement(interp, listObj, mystring);
}

static int Tcc4JimListSymbols (Jim_Interp * interp, TCCState *s) {
    
    static struct JimTCCObj listObj;
    
    Jim_Obj *my_list = Jim_NewStringObj(interp,"SYMBOLTABLE", -1);
    listObj.interp=interp;
    listObj.obj=my_list;
    tcc_list_symbols (s, &listObj, Tcc4JimAppendSymbol);
    Jim_SetResult(interp, my_list);
    return JIM_OK;    

}

static void Tcc4JimErrorFunc(Jim_Interp * interp, char * msg) {
	Jim_AppendStrings(interp, Jim_GetResult(interp), msg, "\n", NULL);
}

static void Tcc4JimCCommandDeleteProc(Jim_Interp * interp, ClientData cdata) {
	struct JimTCCState *ts;
    ts = (struct JimTCCState *) cdata;

	/* carefull with this */
	/* regular tcc_delete will also kill our compiled code */
	/* so we need to use a modified version that kills all, but runtime_memory */
	/* therefor libtcc.c has to be tuned accordingly */
	/* this should be done by modd_tcc.Jim automatically */
	#ifdef TCC4JIM_DODELETE
	TCCState *s ;
        s = ts->s;
        tcc_delete(s);
        ts->s = NULL;
	#endif
	

	Jim_Free((void *) ts);
}

static void Tcc4JimDeleteClientData(Jim_Interp *interp, ClientData cdata) {
	/*
	 * ClientData is a Jim_Obj*, that was passed in 
	 * at command creation
	 */
	Jim_Obj *cdata_o = (Jim_Obj *)cdata;

	if (cdata_o != NULL) {
		Jim_DecrRefCount(interp,cdata_o);
	}
}

static int Tcc4JimHandleCmd (Jim_Interp *interp, int objc, Jim_Obj * const objv[]){
    jim_wide val;
	Jim_Obj *val_o;
	void *val_p;
	void *val_p2;
	int index;
	int res;
	
	struct JimTCCState *ts;
	TCCState *s;
	Jim_Obj *sym_addr;
	ClientData cdata;
	cdata = Jim_CmdPrivData(interp);	
	Jim_SetResultString(interp,"",0);
	
	static const char *options[] = {
		"add_include_path", "add_file",  "add_library", 
		"add_library_path", "add_symbol", "command", "nrcommand", "compile",
		"define", "get_symbol", "output_file", "undefine", "set_options", "list_symbols",
		"delete", 
		(char *) NULL
	};
	enum options {
		TCC4JIM_ADD_INCLUDE, TCC4JIM_ADD_FILE, TCC4JIM_ADD_LIBRARY, 
		TCC4JIM_ADD_LIBRARY_PATH, TCC4JIM_ADD_SYMBOL, TCC4JIM_COMMAND,
		TCC4JIM_NRCOMMAND, TCC4JIM_COMPILE, TCC4JIM_DEFINE, TCC4JIM_GET_SYMBOL,
		TCC4JIM_OUTPUT_FILE, TCC4JIM_UNDEFINE, TCC4JIM_SET_OPTIONS, TCC4JIM_LIST_SYMBOLS,
		TCC4JIM_DELETE
	};
	int rv;
	
	ts = (struct JimTCCState *) cdata;
	s = ts->s;
	if (s==NULL) {
        Jim_AppendStrings(interp, Jim_GetResult(interp), "tcc: no valid instance found", NULL);
        return JIM_ERR;
    }	    
    if (objc < 2) {
        Jim_WrongNumArgs(interp, 1, objv, "subcommand arg ?arg ...?");
        return JIM_ERR;
    }

    if (Jim_GetEnum(interp, objv[1], options, &index, "option", JIM_ERRMSG | JIM_ENUM_ABBREV) != JIM_OK) {
        return JIM_ERR;
    }

    switch (index) {
        case TCC4JIM_DELETE:
            tcc_delete(s);
            ts->s=NULL;
            return JIM_OK;            
        case TCC4JIM_SET_OPTIONS:
            if (objc != 3) {
                Jim_WrongNumArgs(interp, 2, objv, "options");
                return JIM_ERR;
            } else {
                tcc_set_options(s, Jim_String(objv[2]));
                return JIM_OK;
            }
        case TCC4JIM_ADD_INCLUDE:   
            if (objc != 3) {
                Jim_WrongNumArgs(interp, 2, objv, "path");
                return JIM_ERR;
            } else {
                tcc_add_include_path(s, Jim_String(objv[2]));
                return JIM_OK;
            }
        case TCC4JIM_ADD_FILE:   
            if (objc != 3) {
                Jim_WrongNumArgs(interp, 2, objv, "filename");
                return JIM_ERR;
            } else {
                if (!ts->initok) {
                    Tcc4JimSetupCompiler(ts);
                    ts->initok=1;
                }
                if(tcc_add_file(s, Jim_String(objv[2]))!=0) {
                    return JIM_ERR;
                } else {
                    return JIM_OK;
                }
            }
        case TCC4JIM_ADD_LIBRARY:
            if (objc != 3) {
                Jim_WrongNumArgs(interp, 2, objv, "lib");
                return JIM_ERR;
            } else {
                if (!ts->initok) {
                    Tcc4JimSetupCompiler(ts);
                    ts->initok=1;
                }
                tcc_add_library(s, Jim_String(objv[2]));
                return JIM_OK;
            }
            
        case TCC4JIM_ADD_LIBRARY_PATH:
            if (objc != 3) {
                Jim_WrongNumArgs(interp, 2, objv, "path");
                return JIM_ERR;
            } else {
                tcc_add_library_path(s, Jim_String(objv[2]));
                return JIM_OK;
            }
        case TCC4JIM_ADD_SYMBOL:
            if (objc != 4) {
                Jim_WrongNumArgs(interp, 2, objv, "symbol value");
                return JIM_ERR;
            }

            rv = Jim_GetWideExpr(interp, objv[3], &val);
            if (rv != JIM_OK) {
                return JIM_ERR;
            }

            val_p = (void *) (intptr_t) val;

            if (!ts->initok) {
                Tcc4JimSetupCompiler(ts);
                ts->initok=1;
            }

            tcc_add_symbol(s,Jim_String(objv[2]), val_p); 
            return JIM_OK; 
        case TCC4JIM_COMMAND:
            if (objc != 4 && objc != 5) {
                Jim_WrongNumArgs(interp, 2, objv, "Jimname cname ?clientData?");
                return JIM_ERR;
            }

            if (!ts->relocated) {     
                if(_TCC_RELOCATE_!=0) {
                    Jim_AppendStrings(interp, Jim_GetResult(interp), "relocating failed", NULL);
                    return JIM_ERR;
                } else {
                    ts->relocated=1;
                }
            }

            val_p = tcc_get_symbol(s, Jim_String(objv[3]));
            if (val_p == NULL) {
                Jim_AppendStrings(interp, Jim_GetResult(interp), "symbol '", Jim_String(objv[3]),"' not found", NULL);
                return JIM_ERR;
            }
    
            /* the ClientData */
            if (objc == 5) {
                val_o = objv[4];
                Jim_IncrRefCount(val_o);
            } else {
                val_o = NULL;
            }

            Jim_CreateCommand(interp, Jim_String(objv[2]), val_p, val_o, Tcc4JimDeleteClientData);
            return JIM_OK;
        case TCC4JIM_NRCOMMAND:
	        if (objc != 5 && objc != 6) {
		    Jim_WrongNumArgs(interp, 3, objv, "jimname cname nrcname ?clientData?");
		    return JIM_ERR;
	        }
            
	        if (!ts->relocated) {
		    if(_TCC_RELOCATE_!=0) {
		        Jim_AppendStrings(interp, Jim_GetResult(interp), "relocating failed", NULL);
		        return JIM_ERR;
		    } else {
		        ts->relocated=1;
		    }
	        }
            
	        val_p = tcc_get_symbol(s, Jim_String(objv[3]));
	        if (val_p == NULL) {
                Jim_AppendStrings(interp, Jim_GetResult(interp), "symbol '", Jim_String(objv[3]),"' not found", NULL);
                return JIM_ERR;
	        }
            
	        val_p2 = tcc_get_symbol(s, Jim_String(objv[4]));
	        if (val_p2 == NULL) {
                Jim_AppendStrings(interp, Jim_GetResult(interp), "symbol '", Jim_String(objv[4]),"' not found", NULL);
                return JIM_ERR;
            }
    
            /* the ClientData */
            if (objc == 6) {
                val_o = objv[5];
                Jim_IncrRefCount(val_o);
            } else {
                val_o = NULL;
            }

	        /*printf("symbol: %x\n",val); */
	        //Jim_NRCreateCommand(interp, Jim_String(objv[2]), val_p, val_p2, val_o, Tcc4JimDeleteClientData);
            return JIM_OK;
        case TCC4JIM_COMPILE:
            if(ts->relocated == 1) {
                Jim_AppendStrings(interp, Jim_GetResult(interp), "code already relocated, cannot compile more",NULL);
                return JIM_ERR;
            }
            if (objc < 3) {
                Jim_WrongNumArgs(interp, 2, objv, "ccode");
                return JIM_ERR;
            } else {
                if (!ts->initok) {
                    Tcc4JimSetupCompiler(ts);
                    ts->initok=1;
                }
                int i;
                i = tcc_compile_string(s,Jim_String(objv[2]));
                if (i!=0) {
                    Jim_AppendStrings(interp, Jim_GetResult(interp),"Compilation failed\n",NULL);
                    return JIM_ERR;
                } else {
                    return JIM_OK;
                }
            }
        case TCC4JIM_DEFINE:
            if (objc != 4) {
                Jim_WrongNumArgs(interp, 2, objv, "symbol value");
                return JIM_ERR;
            }
            tcc_define_symbol(s,Jim_String(objv[2]),Jim_String(objv[3]));
            return JIM_OK;
            
        case TCC4JIM_LIST_SYMBOLS: 
            return Tcc4JimListSymbols(interp, s);
        case TCC4JIM_GET_SYMBOL:
            if (objc != 3) {
                Jim_WrongNumArgs(interp, 2, objv, "symbol");
                return JIM_ERR;
            }
            if (!ts->relocated) {     
                if(_TCC_RELOCATE_!=0) {
                    Jim_AppendStrings(interp, Jim_GetResult(interp), "relocating failed", NULL);
                    return JIM_ERR;
                } else {
                    ts->relocated=1;
                }
            }
            val_p = tcc_get_symbol(s,Jim_String(objv[2]));
            if(val_p == NULL) {
                Jim_AppendStrings(interp, Jim_GetResult(interp), "symbol '", Jim_String(objv[2]),"' not found", NULL);
                return JIM_ERR;
            }
            sym_addr = Jim_NewWideObj(interp,(intptr_t) val_p);
            Jim_SetResult(interp, sym_addr);
            return JIM_OK; 
        case TCC4JIM_OUTPUT_FILE:
            if (objc != 3) {
                Jim_WrongNumArgs(interp, 2, objv, "filename");
                return JIM_ERR;
            }
            if (ts->relocated) {     
                Jim_AppendStrings(interp, Jim_GetResult(interp), "code already relocated, cannot output to file", NULL);
                return JIM_ERR;
            }
            if (ts->output_type == TCC_OUTPUT_MEMORY) {     
                Jim_AppendStrings(interp, Jim_GetResult(interp), "output_type memory not valid for output to file", NULL);
                return JIM_ERR;
            }
            res = tcc_output_file(s,Jim_String(objv[2]));
            ts->relocated=1;
            if (res!=0) {
                Jim_AppendStrings(interp, Jim_GetResult(interp), "output to file failed", NULL);
                return JIM_ERR;
            } else {
                return JIM_OK;
            }
        case TCC4JIM_UNDEFINE:
            if (objc != 3) {
                Jim_WrongNumArgs(interp, 2, objv, "symbol");
                return JIM_ERR;
            }
            tcc_undefine_symbol(s,Jim_String(objv[2]));
            return JIM_OK;
        
        default:
            exit(-1);
            //Jim_Panic("internal error during option lookup");
    }
    return JIM_OK;
} 

static int Tcc4JimCreateCmd(Jim_Interp *interp, int objc, Jim_Obj * const objv[]){
	struct JimTCCState *ts;
	TCCState *s;
    	int index;

	Jim_SetResultString(interp,"",0);
    	
	static const char *types[] = {
		//"memory", "exe", "dll", "obj", "preprocess",    (char *) NULL
		// 0.9.26 to 0.9.27 the enum changed
		"","memory", "exe", "dll", "obj", "preprocess",    (char *) NULL
	};
	// since tcc devel keeps changing the order of TCC_OUTPUT_ options we have to map the defined macros back to tcc4Jim textual tpyes, sigh
	static const int enumtypes[] = {
		0,
		TCC_OUTPUT_MEMORY,
		TCC_OUTPUT_EXE,
		TCC_OUTPUT_DLL,
		TCC_OUTPUT_OBJ,
		TCC_OUTPUT_PREPROCESS,
		0
	};
	
	if (objc < 3 || objc > 4) {
		Jim_WrongNumArgs(interp, 1, objv, "tcc_libary_path ?output_type? handle");
		return JIM_ERR;
	}

	if (objc == 3) {
		index = TCC_OUTPUT_MEMORY;
	} else {
		if (Jim_GetEnum(interp, objv[2], types, &index, "type", JIM_ERRMSG | JIM_ENUM_ABBREV) != JIM_OK) {
			return JIM_ERR;
		}
		index = enumtypes[index];
	}
	s = tcc_new();
	
	if (s == NULL) {
		return(JIM_ERR);
	}
	ts = (void *) Jim_Alloc(sizeof(*ts));
	ts->output_type = index;

	tcc_set_lib_path(s, Jim_String(objv[1]));
	
	ts->s = s;
    ts->relocated = 0;
    ts->initok = 0;


	tcc_set_error_func(s, interp, (void *)&Tcc4JimErrorFunc);

	Jim_CreateCommand(interp,Jim_String(objv[objc-1]),Tcc4JimHandleCmd,ts,Tcc4JimCCommandDeleteProc);

	Jim_SetResult(interp, objv[objc-1]);

	return JIM_OK;
}

#if (defined(_WIN32) && (defined(_MSC_VER)|| defined(__TINYC__)  || (defined(__BORLANDC__) && (__BORLANDC__ >= 0x0550)) || defined(__LCC__) || defined(__WATCOMC__) || (defined(__GNUC__) && defined(__declspec))))
#undef DLLIMPORT
#undef DLLEXPORT
#   define DLLIMPORT __declspec(dllimport)
#   define DLLEXPORT __declspec(dllexport)
#else
#undef DLLIMPORT
#undef DLLEXPORT
#   define DLLIMPORT __attribute__(dllimport)
#   if defined(__GNUC__) && __GNUC__ > 3
#       define DLLEXPORT __attribute__ ((visibility("default")))
#   else
#       define DLLEXPORT
#   endif
#endif


//DLLEXPORT
int Jim_tccInit(Jim_Interp *interp) {
    //printf("Jim_tccInit Interp: %p\n",interp);
    Jim_PackageProvideCheck(interp, "tcc");
	Jim_CreateCommand(interp, "tcc4jim", Tcc4JimCreateCmd, NULL, NULL);
	Jim_CreateCommand(interp, "tcc4tcl", Tcc4JimCreateCmd, NULL, NULL);
	Jim_SetVariableStrWithStr(interp,  "::TCC_VERSION", "tcc for jim 250222; compiled on " __DATE__ "-" __TIME__);
	return JIM_OK;
}
#undef DLLIMPORT
#undef DLLEXPORT

