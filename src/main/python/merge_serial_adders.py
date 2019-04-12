#! /usr/bin/python3

import sys

def merge_two_mods( modA, modB, newMod ):
    a_txt = modA.split('\n')
    b_txt = modB.split('\n')
    module_num = b_txt[0].split( "_" )[-1].split( "(" )[0]
    new_mod_txt = newMod + " " + newMod.split()[0] + "_" + module_num + "(\n"
    new_mod_txt += '\n'.join( a_txt[1:-1] ) + ",\n"
    needed_lines = [ ( ".io_a", ".io_a_2" ), ( ".io_b", ".io_b_2" ), ( ".io_out", ".io_out_2" ) ]
    for exp_in, replacement in needed_lines:
        for x in b_txt:
            if exp_in in x:
                break
        new_mod_txt += x.replace( exp_in, replacement ) + "\n"
    new_mod_txt += ");\n"
    return new_mod_txt    

def merge_serial_adders( txt, module_to_merge, replacement_mod ):
    modules = txt.split( ");\n" )
    check_txt = module_to_merge + " SerialAdder"
    valid_modules = [ x for x in modules if check_txt in x ]
    invalid_modules = [ x for x in modules if check_txt not in x ][:-1]
    # join up the valid modules and put in a carry4 block
    num_merges = int( len( valid_modules ) / 2 )
    invalid_modules += valid_modules[2*num_merges:]
    compute_txt = ""
    for i in range( num_merges ):
        compute_txt +=  merge_two_mods( valid_modules[2*i], valid_modules[2*i + 1], replacement_mod )
    # put the invalid modules back the way they were
    compute_txt += ");\n".join( invalid_modules ) + ");\n"
    return compute_txt

if __name__ == "__main__":
    fname = sys.argv[1]
    module_to_merge = sys.argv[2]
    f = open( fname )
    txt = f.read()
    new_txt = merge_serial_adders( txt, module_to_merge, sys.argv[3] )
    f = open( sys.argv[4], "w" )
    f.write( new_txt )
    f.close()
