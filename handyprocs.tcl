proc llappend {listname ind args} {upvar $listname givenlist; set temp [lindex $givenlist $ind]; foreach arg $args {lappend temp $arg};lset givenlist $ind $temp; return 0}
## Above this line is proc under test

## Below this line is part of reload_procfiles
set list_of_procnames [list check_another_design write_logs_to_file get_unique_list flatten_list get_nonempty_list formatlist write_to_file get_max_of_list read_from_file is_even is_number get_folders_from_path insert_intolist_beforemarker mkdir_if_nonexist grep read_hash_from_txt standard_dc_write echo_list proof_read_script sort_list get_driving_pin create_mut echo_array]
if {[info exists env(DBOX)]} { set dropboxpath $::env(DBOX) } else { set dropboxpath ~/Dropbox }
source $dropboxpath/tcl/procs_tool_independent.tcl


## Below this line are procs
proc get_indexed_array {givenlist} {array set retarray {};foreach i [zeroto [llength $givenlist]] {set retarray($i) [lindex $givenlist $i]};return [array get retarray]}
proc get_nth_sublist {givenlist {n 2} {offset {}}} {set ret {}; if {![llength $offset]} {set offset [expr $n-1]}
	foreach i [vadd [lincrement 0 [expr [llength $givenlist]-1] $n] $offset] {lappend ret [lindex $givenlist $i]}; return $ret}
proc vadd {givenlist {const 1}} {set ret {};foreach i [indsof $givenlist] {lappend ret [expr [lindex $givenlist $i]+$const]}; return $ret }
proc are_genuine_filenames {names} {set ret [lrepeat [llength $names] 1];
	foreach i [zeroto [llength $names]] {set name [lindex $names $i]; upvar $name $name
		if {![info exists $name]} { lset ret $i 0
		} elseif {![llength [subst "\$$name"]]} {lset ret $i 0
		} elseif {![file exists [subst "\$$name"]]} { lset ret $i 0
		}
	}; return $ret
}
proc get_next_color {} {upvar #0 colorind colorind;set rainbow [list blue yellow light_red purple light_orange light_blue light_purple light_green orange red green]
	set colorind_good 1; if {![info exists colorind]} {set colorind_good 0} elseif {![is_integer $colorind]} { set colorind_good 0 }; set rainbowlength [llength $rainbow]
	if {!$colorind_good} {set colorind 0} else { set colorind [expr ($colorind+1) % $rainbowlength]}; return [lindex $rainbow $colorind]
}
proc stamp_freshness {varnames} {;## append current time in minutes since epoch of said varname to stalenessof($varname)
	upvar #0 stalenessof stalenessof; array set stalenessof {};set stamp [expr [clock seconds]/60]; foreach varname $varnames {set stalenessof($varname) $stamp};return 0
}
proc lappend_to_collection {listname index collection {unique {}}} {;## like append_to_collection, but append to an element in a list
	upvar $listname $listname; set temp [eval "lindex \$$listname $index"]; if {[string match *unique $unique]} {set ret [append_to_collection temp $collection -unique]
	} else {set ret [append_to_collection temp $collection]}; lset $listname $index $temp; return $ret
}
proc also_satisfy {varname value} {;## usage: also_satisfy varname [some_condition]
	upvar #0 $varname $varname; set $varname [expr [subst "\$$varname"]*$value]
	return [subst "\$$varname"]
}
proc are_fresh {names} {set myname are_fresh;
	upvar #0 stalenessof stalenessof; upvar #0 msg msg; upvar #0 freshness freshness;
	array set stalenessof {}; set reset_freshness 0; if {![info exists freshness]} {set reset_freshness 1} elseif {![is_integer $freshness]} {set reset_freshness 1}
	set freshness [expr ($reset_freshness)?0:$freshness]
	set allset [require_variables [list {stalenessof {llength [get_values_of_array %s]}} {freshness {is_integer $%s}}]]
	foreach name $names { set fresh 1; if {![info exists stalenessof($name)]} { set fresh 0; set stalenessof($name) 0
					} elseif {![is_integer $stalenessof($name)]} { set fresh 0; set stalenessof($name) 0;
					} elseif {$stalenessof($name) < $freshness} { set fresh 0; }
			if {!$fresh} {set msg_entry "$myname: \$stalenessof($name)=$stalenessof($name) is not fresh."; lappend msg $msg_entry; return 0 }
	}; return 1
}
proc require_variables {entries} {upvar #0 msg msg;upvar #0 myname myname; set allgood 1; foreach entrie $entries { set name [lindex $entrie 0]; upvar #0 $name $name;
		set data_okay [info exists $name]; if {$data_okay && [llength $entrie]>1} {
			set form [string map [list %s $name] [lindex $entrie 1]];set data_okay [expr $data_okay*[eval [format $form $name]]]
			if {!$data_okay} {set msg_entry [format "$myname: \$$name exists, but fails requirement %s." [lindex $entrie 1]]; lappend msg $msg_entry;return 0}
		} elseif {!$data_okay} { set msg_entry "$myname: \$$name is required but not defined."; lappend msg $msg_entry;return 0 }
	}; return $allgood
}
proc zeroto {number} {return [lincrement 0 [expr $number-1]]}
proc default_listvalue_to_be {varname value {value_type {is_integer}} } {upvar #0 $varname $varname;
        if {![info exists $varname]} {set $varname $value}
	if {![eval "$value_type \$$varname"]} {set $varname $value}
}

proc arraysearch {arraylist value {options -exact}} {set scriptname [lindex [info level 0] 0];upvar #0 anslist anslist;
        if {[llength $arraylist] == 1} {upvar $arraylist original_array 
        } elseif {[is_array $arraylist]} {array set original_array $arraylist
        } elseif {![llength $arraylist]} {array set original_array {}
        } else { 
                return -code error "not an array. usage: $scriptname \[array get some_array|arrayname\] {elementformat e.g. %value} {method e.g. eval}\n"
        }
    set arraycontents [array get original_array];set foundvalues [eval "lsearch -all $options \$arraycontents \$value"]
    set returnkeys {};foreach foundvalue $foundvalues {lappend returnkeys [lindex $arraycontents [expr $foundvalue-1]]}
	set anslist $returnkeys;return $returnkeys
}
proc ainsideb {a b {sortoption {-integer -increasing}}} { set sortedb [eval "lsort $sortoption {$b}"];
	return [expr ($a>=[lindex $sortedb 0])&&($a<=[lindex $sortedb 1])]
}
proc ind_array {arraylist key} {
        if {[llength $arraylist] == 1} {upvar $arraylist original_array 
        } elseif {[is_array $arraylist]} {array set original_array $arraylist
        } elseif {![llength $arraylist]} {array set original_array {}
        } else { 
                return -code error "not an array. usage: ind_array \[array get some_array|arrayname\] {elementformat e.g. %value} {method e.g. eval}\n"
        }
	return $original_array($key)
}
proc keysof {arraylist} {
        if {[llength $arraylist] == 1} {upvar $arraylist original_array 
        } elseif {[is_array $arraylist]} {array set original_array $arraylist
        } elseif {![llength $arraylist]} {array set original_array {}
        } else { 
                return -code error "not an array. usage: format_array \[array get some_array|arrayname\] {elementformat e.g. %value} {method e.g. eval}\n"
        }
	return [array names original_array]
}
proc indsof {srclist} {return [lincrement 0 [expr [llength $srclist]-1]]}
proc lolsearch {{srclist {}} {pattern {}} {switches {}} } {;
	## error when set srclist {{OR2X1 OR2X1} {OR2X1 AND2X1}};set pattern {OR2X1 AND2X1};set switches -all
    ## error when srclist items and patterns contain []
	set returnlist $srclist;if {[llength [lsearch -inline $switches "*-inline*"]]} {set inline 1} else {set inline 0};
	if {[llength [lsearch -inline $switches "*-all*"]]} {set all 1} else {set all 0};
	if {[llength [lsearch -inline $switches "*-not*"]]} {set not 1} else {set not 0};set srcind [lincrement 0 [expr [llength $srclist]-1]];
	set switches [lsearch -all -not -inline [split $switches { }] {-inline}];set switches [lsearch -all -not -inline [split $switches { }] {-not}];
	set switches [lsearch -all -not -inline [split $switches { }] {-all}];
	set evalforms [list "set returnind \[lsearch $switches -all \[formatlist \$returnlist {lindex {%s} %s} eval\] \$subpat\]"\
			"set returnlist \[sublist_byindices \$returnlist \$returnind\]" "set srcind \[sublist_byindices \$srcind \$returnind\]"]
	for {set ind 0} {$ind<[llength $pattern]} {incr ind} {set subpat [lindex $pattern $ind];foreach cmd $evalforms {eval [format $cmd %s $ind]}}
	if {$not} {set srcind [lsearch -all -not -regexp [lincrement 0 [expr [llength $srclist]-1]] [format "^(%s)$" [join $srcind |]]]};
	if {!$all} {set srcind [lindex $srcind 0]};set returnlist [sublist_byindices $srclist $srcind];
	if {$inline} {return $returnlist} else {return $srcind}
}
proc get_lsort_index {srclist {lsortoption -real}} {
	set x {}; for {set i 0} {$i<[llength $srclist]} {incr i} { lappend x [list $i [lindex $srclist $i]] }
	return [get_ith_element_of_list [flatten_list [eval "lsort $lsortoption -index 1 \[list $x\]"]] 2 -1]
}
proc array_sort {arrayname {lsortoption -integer}} {
        if {[llength $arrayname] == 1} {
                upvar $arrayname thearray
        } elseif {[is_array $arrayname]} {
                array set thearray $arrayname
	} elseif {[string match help $arrayname]} {
		puts "Usage: array_sort {arrayname|\[array get arrayname\]} \[lsortoptions, e.g.-decreasing, default=-integer\]\n"
		return 1
	}
    reset_array thearray [sub_array_v2 thearray {llength {%value}} eval]
    if {[llength [array names thearray]]} {set x [list];foreach {k v} [array get thearray] {lappend x [list $k $v]}
		eval [format "set returnlist \[flatten_list \[lsort $lsortoption -index 1 \$x\] \]"]
		return $returnlist
    } else {        
        puts "not an array. usage: array_sort \[array get some_array\]\n"
        return 0        
    }                               
}
proc sortkeys_byvalues {arrayname {lsortoption -integer} {keylist *}} {
        if {[llength $arrayname] == 1} {
                upvar $arrayname thearray
        } elseif {[is_array $arrayname]} {
                array set thearray $arrayname
	} elseif {[string match help $arrayname]} {
		puts "Usage: sortkeys_byvalues {arrayname|\[array get arrayname\]} \[lsortoptions, e.g.-decreasing, default=-integer\]\n"
		return 1
	}
    reset_array thearray [sub_array_v2 thearray {llength {%value}} eval];set subarray {}
    if {[string match * $keylist]} {set subarray [array names thearray]} else { 
        foreach key $keylist {lappend subarray $key;lappend subarray $thearray($key)}
    }
    if {[llength $subarray]} {
	set x [list];foreach {k v} [array get thearray] { lappend x [list $k $v] }
	eval [format "set returnlist \[formatlist \[lsort $lsortoption -index 1 \$x\] {lindex {%s} 0} eval\]" %s]
	return $returnlist
    } else { #puts "not an array. usage: sortkeys_byvalues \[array get some_array\]\n"
        return {}
    }                               
}
proc convert_csv_to_tex { srcfilename {texheadfilename /mnt/c/Users/shiqh/Dropbox/latex/ieee_article_head.tex} {returnfilename {}}} {
	if {[file exists $srcfilename]} {
		set csvparsed [parse_csv $srcfilename]
		set titles [lindex $csvparsed 0]
		set responses [lrange $csvparsed 1 end]
		if {![llength $returnfilename]} { set returnfilename [join [list [lindex [split $srcfilename .] 0] ".tex"] {}]}
		set returnlines {}
		for {set j 0} {$j < [llength $responses]} {incr j} {
			set response [concat [lindex $responses $j] [lrepeat [llength $titles] "No answer provided."]]
			lappend returnlines [format "\\section{Response %s}" $j]
			for {set i 0} {$i < [llength $titles]} {incr i} {
				lappend returnlines [format "\\subsection{%s}" [lindex $titles $i]]
				if {[llength $response] >= $i} {
					lappend returnlines [format "%s" [lindex $response $i]]
				}
			}
		}
		if {[file exists $texheadfilename]} {
			puts "Success: \$texheadfilename found at $texheadfilename, using it.\n"
			set publishlines [list [format "%s{$texheadfilename}\n %s{%s}\n%s\n%s{document}" {\input} {\title} [lindex [split [lindex [split $srcfilename .] 0] /] end] {\maketitle} {\begin}] [join $returnlines "\n"] [format "%s{document}" \\end]]
			#exec cat $texheadfilename $tempfilename "\\end{document}" > $returnfilename
		} else {
			puts "Minor issue: \$texheadfilename = $texheadfilename not found, you'll need to manually finish tex syntax.\n"
			set publishlines $returnlines
			#exec mv $tempfilename $returnfilename
		}
		set returnfilename [linewritevar publishlines {} $returnfilename]
		return $returnfilename
	} else { puts "Specified file $srcfilename does not seem to exist.\n"; return {}}
}
proc linewritevar {varname {putsoption -nonewline} {destfilename {}} } {
	if {![llength $destfilename]} { set destfilename ${varname}.txt }
	set fileid [open $destfilename w]
	upvar $varname var
	foreach cell $var { eval "puts $putsoption $fileid \$cell" }
	close $fileid
	if {[file exists $destfilename]} {return $destfilename} else {return {}}
}
proc parse_csv { filename } {
	package require csv
	if {[file exists $filename]} {
		set filelines [read_from_file $filename]
		set returnlist {}
		foreach fileline $filelines {
			lappend returnlist [::csv::split $fileline]
		}
		return $returnlist
	} else { puts "Specified file $filename does not seem to exist.\n"; return {}}
}
proc lreverse { thelist } {
	set len [llength $thelist]
	set halflen [expr int(double($len)/2)]
	for { set i 0} {$i<$halflen} {incr i} {
		set temp [lindex $thelist $i]
		lset thelist $i [lindex $thelist [expr $len-1-$i]]
		lset thelist [expr $len-1-$i] $temp
	}
	return $thelist
}
proc lpop {listname} {
	upvar #0 $listname thelist;set ret [lindex $thelist end];set thelist [lrange $thelist 0 end-1];return $ret
}
proc filter_list {thelist {evalexpression {}}} {
	set returnlist {}; set evalexpression [string map {%s $cell} $evalexpression]
	foreach cell $thelist {
		if {[eval $evalexpression]} { lappend returnlist $cell }
	}
	return $returnlist
} 
proc get_indexed_array_of { sourcelist {start_index_at 0} } {
	set returnlist {}
	for {set i 0} {$i < [llength $sourcelist]} {incr i} {
		lappend returnlist [expr $i+$start_index_at]; lappend returnlist [lindex $sourcelist $i]
	}
	return $returnlist
}
proc get_arrayofindex_bykey { sourcelist {start_index_at 0} } {
	set returnlist {}
	for {set i 0} {$i < [llength $sourcelist]} {incr i} {
		 lappend returnlist [lindex $sourcelist $i];lappend returnlist [expr $i+$start_index_at]
	}
	return $returnlist
}

proc get_first_match_wrt_mark { source_list {target_pattern {^[\d\.]+$}} {mark slack} {lfind_cmd {expr %s > $mark_position}} } {
	if {[string match help $source_list]} {
		puts "Usage:\n\tget_first_match_wrt_mark source_list {target_pattern {^\[\d\.\]+$}} {mark slack} {lfind_cmd {expr %s > \$mark_position}}\n"
	} else {
		set mark_position [lsearch -exact $source_list $mark]
		return [lindex $source_list [lindex [lfind [lsearch -all -regexp $source_list $target_pattern] [subst [format $lfind_cmd {%s}]] ] 0 ]]
	}
}
proc get_path_from_filename {source_filename} {
	if {[file exists $source_filename]} {
		return [join [lrange [split $source_filename /] 0 end-1] /]
	} else {
		return {}
	}
}
proc vmult {la mult} { set ret {};for {set ind 0} {$ind < [llength $la]} {incr ind} { lappend ret [expr [lindex $la $ind]*$mult] }
	return $ret
}
proc ladd {la lb} { set lengths [list [llength $la] [llength $lb]]; set longerind [lsearch $lengths [lmax $lengths]];set shorterind [expr 1-$longerind]
	set list [list $la $lb];set ret {};
	for {set ind 0} {$ind < [lindex $lengths $shorterind]} {incr ind} { lappend ret [expr [lindex $la $ind]+[lindex $lb $ind]] }
	for {set ind [lindex $lengths $shorterind]} {$ind < [lindex $lengths $longerind]} {incr ind} {lappend ret [lindex $list $longerind $ind]}
	return $ret
}
proc arrayadd {arraya arrayb} { set targetarrayname {foundarraya foundarrayb};
	foreach arraylist [list $arraya $arrayb] { 
		if {[llength $arraylist] == 1} { upvar $arraylist [lindex $targetarrayname 0];
		} elseif {[is_array $arraylist]} { array set [lindex $targetarrayname 0] $arraylist;
		} else { return -code error "not an array. usage: arrayadd \[array get some_array|arrayname\] \[array get some_array|arrayname\]"
		}; set targetarrayname [lindex $targetarrayname end];
	}
	array set retarray {}; set key_superset [get_unique_list [concat [array names foundarraya] [array names foundarrayb]]]
	foreach key $key_superset {lappend foundarraya($key) {};lappend foundarrayb($key) {}
		set retarray($key) [lsum [get_flattened_list [get_nonempty_list [concat $foundarraya($key) $foundarrayb($key)]]]]
	};return [array get retarray]
}
proc lsum {l} {
    set total 0;set flat [get_flattened_list $l]
    foreach nxt $flat {
        set total [expr $total+$nxt]
    }
    return $total
}
proc lfind { givenlist expression } {
	set list_to_return {}
	foreach item $givenlist {
		if {[eval [format $expression $item]]} { lappend list_to_return $item }
	}
	return $list_to_return
}
proc ternary_set {varname condition truereturn falsereturn} {
	upvar $varname $varname
	if {[eval $condition]} {
		set returnvar $truereturn
	} else {
		set returnvar $falsereturn
	}
	set $varname $returnvar
	return $returnvar
}
proc if_undefined_set { varnames defaultvalues } {
	if {![llength $defaultvalues]} { set defaultvalues [list {}] }
	set i 0; set eval_length [get_extremes_of_list [list [llength $varnames] [llength $defaultvalues] ] <] 
	while {$i < $eval_length} {
		set varname [lindex $varnames $i]; set defaultvalue [lindex $defaultvalues $i]
		upvar $varname $varname
		if {![info exists $varname]} { set $varname $defaultvalue }
		incr i
	}
}
proc get_random_pins { {total_number 1} {source_collection {}} } {
	if {![sizeof_collection [get_pins $source_collection]]} { set all_pins [get_pins -of_objects [get_cells -filter {@is_hierarchical==false}]] } else { set all_pins $source_collection }
	set return_collection {}; set total_number_of_pins [sizeof_collection $all_pins]
	for { set i 0 } { $i < $total_number } { incr i } {
		append_to_collection return_collection [index_collection $all_pins [expr int(rand()*$total_number_of_pins)] ]
	}
	return $return_collection
}
proc evenodd_split_list {givenlist} {
        set odd_list {}; set even_list {};
## this relies on total number of $data_port_regs being even
	for {set i 0} {$i < [llength $givenlist]} {incr i} {
		if {[is_even $i]} { lappend even_list [lindex $givenlist $i]} else {lappend odd_list [lindex $givenlist $i] }
	}
	return [list 0 $even_list 1 $odd_list]
}
proc create_sandbox_design {{tagname bk} {original_top {}} } {
        if {![llength $original_top]} { set original_top [current_design_name] }
        set sandbox_top [format "%s_%s" $original_top $tagname]
        if {[sizeof_collection [get_designs -quiet $sandbox_top]]} { remove_design $sandbox_top }
        copy_design $original_top $sandbox_top; current_design $sandbox_top
}

proc list_set { namelist valuelist } {
	set set_count 0; set total_set [get_extremes_of_list [concat [llength $namelist] [llength $valuelist]] <]
	while {$set_count < $total_set} {
		upvar [lindex $namelist $set_count] [lindex $namelist $set_count]
		set [lindex $namelist $set_count] [lindex $valuelist $set_count]
		incr set_count
	}
	return 1
}
proc get_feedthrough_nets {} {
	set nets [remove_from_collection -intersect [get_nets -of_objects [get_ports -filter {@port_direction==in}]] [get_nets -of_objects [get_ports -filter {@port_direction==out}]] ]
	return [remove_from_collection $nets [get_nets -of_objects [get_pins -of_objects $nets]] ]
}
proc get_empty_nets {} {
	set nets_without_ports [remove_from_collection [get_nets] [get_nets -of_objects [get_ports]] ]
	return [remove_from_collection $nets_without_ports [get_nets -of_objects [get_pins -of_objects $nets_without_ports]] ]
}
proc copy_part_of_design {specified_cells {arguments {}} } {
        set default_arguments [list [list target_design_naming_form "%s_extracted"] ]
        lappend arguments {}
        initvars $default_arguments; initvars $arguments
        set original_top [current_design_name]
	set names_of_specified_cells [get_object_name [get_cells $specified_cells]]
        set target_design_name [format $target_design_naming_form $original_top]
        if {[sizeof_collection [get_designs -quiet $target_design_name]]} { remove_design $target_design_name   }
	set msg {};
	copy_design $original_top $target_design_name; current_design $target_design_name
        set foundcells [get_cells $names_of_specified_cells]
	remove_cell [remove_from_collection [get_cells] $foundcells]
	## To allow bus-ed nets and ports be removed ##
	remove_bus *
	set nets_without_driving_cell [remove_from_collection [get_nets] [get_nets -of_objects [get_pins -filter {@pin_direction==out}] ]] 
	set undriven_nets [remove_from_collection $nets_without_driving_cell [get_nets -of_objects [get_ports -filter {@port_direction==in}] ]]
	#set undriven_nets_without_driven_cell [remove_from_collection $nets_without_driving_cell [get_nets -of_objects [get_pins -filter {@pin_direction==out}] ]]
	## undriven_nets that drives ports or cell inputs are removed too ##
	remove_net $undriven_nets
	set unconnected_ports [remove_from_collection [get_ports] [get_ports -of_objects [get_nets]] ]
	remove_port $unconnected_ports
	make_buses_for_design
	return $msg
}
proc remove_register { registers_to_remove {arguments {}} } {
	set default_arguments [list [list target_design_naming_form "%s_extracted"] [list inverter_model saed32rvt_tt1p05v25c/INVX1_RVT] [list inverter_pin_purpose [list A A Z Y]] [list inverter_naming_form "%s_qn_inverter"] ]
        initvars $default_arguments; initvars $arguments
	reset_array inverter_pin_map $inverter_pin_purpose
	set names_of_registers_to_remove [get_object_name [get_cells $registers_to_remove] ]; set msg {}
        set original_top [current_design_name]
        set target_design_name [format $target_design_naming_form $original_top]
        if {[sizeof_collection [get_designs -quiet $target_design_name]]} { remove_design $target_design_name   }
        set msg {};set specified_cell_names [get_object_name [eval [format "get_cells %s" $registers_to_remove] ] ]
        copy_design $original_top $target_design_name; current_design $target_design_name
        set found_cells [get_cells $specified_cell_names]; set sofar_successful 1
	foreach cell_name [get_object_name $found_cells] {
		set cell [get_cells $cell_name]
		set dpin_net [get_nets -of_objects [filter [get_pins -of_objects $cell] -regexp {@full_name =~ ".*D$"}] ]; set dpin_net_name [get_object_name $dpin_net]
		set qpin_net [get_nets -of_objects [filter [get_pins -of_objects $cell] -regexp {@full_name =~ ".*Q$"}] ]; set qpin_net_name [get_object_name $qpin_net]
		set qnpin_net [get_nets -of_objects [filter [get_pins -of_objects $cell] -regexp {@full_name =~ ".*QN$"}] ]; set qnpin_net_name [get_object_name $qnpin_net]
		set qpin_pin_names [get_object_name [get_pins -of_objects $qpin_net -filter {@pin_direction==in}]]
		set qpin_port_names [get_object_name [get_ports -of_objects $qpin_net -filter {@port_direction==out}]]
		if {[sizeof_collection $qnpin_net]} {
			set inverter_name [string map {/ _} [format $inverter_naming_form $cell_name]]
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "create_cell %s $inverter_model" $inverter_name ] ] } else { return 0 }
                        if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\] " $dpin_net_name $inverter_name/$inverter_pin_map(A) ] ] } else { return 0 }
                        if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\] " $qnpin_net_name $inverter_name/$inverter_pin_map(Z) ] ] } else { return 0 }
		}
                if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "remove_cell %s" $cell_name ] ] } else { return 0 }
                if {[llength $qpin_pin_names]} { if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "disconnect_net \[get_nets %s\] \[get_pins \[list %s\]\] " $qpin_net_name $qpin_pin_names ] ]  } else { return 0 }
		}
                if {[llength $qpin_pin_names]} { if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins \[list %s\]\] " $dpin_net_name $qpin_pin_names ] ]  } else { return 0 }
		}
                if {[llength $qpin_port_names]} { if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "disconnect_net \[get_nets %s\] \[get_ports \[list %s\]\] " $qpin_net_name $qpin_port_names ] ]  } else { return 0 }
		}
                if {[llength $qpin_port_names]} { if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_ports \[list %s\]\] " $dpin_net_name $qpin_port_names ] ]  } else { return 0 }
		}
	}
	return $msg
}
proc latch_into_mux { cells_to_replace {arguments {}}} {
        set default_arguments [list [list target_design_naming_form "%s_extracted"] [list mux_naming_form "%s_replacement_mux"] [list latch_pin_purpose [list LATCHX1_RVT [list D D Q Q clk CLK]]] [list mux_model saed32rvt_tt1p05v25c/MUX21X1_RVT] [list mux_pin_purpose [list D A2 feedback A1 clk S0 Q Y]] ]
        lappend arguments {}
        initvars $default_arguments; initvars $arguments
        reset_array mux_pin_map $mux_pin_purpose; reset_array latch_pin_map $latch_pin_purpose; foreach latch_model [array names latch_pin_map] { reset_array ${latch_model}_map $latch_pin_map($latch_model) }
        set original_top [current_design_name]
        set target_design_name [format $target_design_naming_form $original_top]
        if {[sizeof_collection [get_designs -quiet $target_design_name]]} { remove_design $target_design_name   }
        set msg {};set specified_cell_names [get_object_name [eval [format "get_cells %s" $cells_to_replace] ] ]
        copy_design $original_top $target_design_name; current_design $target_design_name
        set found_cells [get_cells $specified_cell_names]; set sofar_successful 1
	foreach cell_name [get_object_name $found_cells] {
		set cell [get_cells $cell_name]; set cell_ref [get_attribute $cell ref_name]
		if {![info exists latch_pin_map($cell_ref)]} {
			puts "Error: cell $cell_name is of model $cell_ref , whose pin info is not provided. Skipped.\n"
		} else {
			set mux_name [string map {/ _} [format $mux_naming_form $cell_name]]
			set d_pin_name [subst [format "$cell_name/\$%s" [subst ${cell_ref}_map(D)]]]; set d_net_name [get_object_name [get_nets -of_objects $d_pin_name]]
			set q_pin_name [subst [format "$cell_name/\$%s" [subst ${cell_ref}_map(Q)]]]; set q_net_name [get_object_name [get_nets -of_objects $q_pin_name]]
			set clk_pin_name [subst [format "$cell_name/\$%s" [subst ${cell_ref}_map(clk)]]]; set clk_net_name [get_object_name [get_nets -of_objects $clk_pin_name]]
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "create_cell %s $mux_model" $mux_name ] ] } else { return 0 }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "remove_cell %s" $cell_name ] ] } else { return 0 }
			#if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "disconnect_net \[get_nets %s\] \[get_pins %s\] " $d_net_name $d_pin_name ] ]  } else { return 0 }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\] " $d_net_name $mux_name/$mux_pin_map(D) ] ]  } else { return 0 }
			#if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "disconnect_net \[get_nets %s\] \[get_pins %s\] " $q_net_name $q_pin_name ] ]  } else { return 0 }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\] " $q_net_name $mux_name/$mux_pin_map(Q) ] ]  } else { return 0 }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\] " $q_net_name $mux_name/$mux_pin_map(feedback) ] ]  } else { return 0 }
			#if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "disconnect_net \[get_nets %s\] \[get_pins %s\] " $clk_net_name $clk_pin_name ] ]  } else { return 0 }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\] " $clk_net_name $mux_name/$mux_pin_map(clk) ] ]  } else { return 0 }
		}
	}
}
proc register_io_ports {{arguments {}}} {
	set default_arguments [list target_design_naming_form "%s_boundaryregistered" clock_name cmp_gclk[0] substitute_net_naming_form "%s_old" register_naming_form "%s_net_reg" register_model saed32rvt_tt1p05v25c/DFFX1_RVT register_pin_purpose [list D D Q Q clk CLK] ports_to_register {} ]
        reset_array argument_array $default_arguments; 
	if {[is_array $arguments]} { array set argument_array $arguments } elseif {[expr ![llength $arguments] || [string match *help $arguments]]} { puts "Usage:\n\tregister_io_ports <arguments>\nDefault arguments:\n"; echo_array argument_array; return 1 }
	puts "Parameters of this run:\n\n"; echo_array argument_array
	foreach key [array names argument_array] { set $key $argument_array($key); puts "$key =\t$argument_array($key)\n" }
	reset_array register_pin_map $register_pin_purpose
	set original_top [current_design_name]
	set target_design_name [format $target_design_naming_form $original_top]
        if {[sizeof_collection [get_designs -quiet $target_design_name]]} { remove_design $target_design_name   }
        set specified_port_names [get_object_name [eval [format "get_ports %s" $ports_to_register] ] ]
        copy_design $original_top $target_design_name; current_design $target_design_name
	set found_clk [get_ports $clock_name]; set found_clk_name [get_object_name $found_clk]
	set msg "Message:"; lappend msg [format "Ports to process:\n\t%s" [join $specified_port_names ,]];set sofar_successful 1
	if {![sizeof_collection $found_clk]} { 
		quiet_eval_unless_error [format "create_port %s -direction in" $clock_name] 
		set found_clk [get_ports $clock_name]; set found_clk_name [get_object_name $found_clk]
	}
	set clk_net_name [get_object_name [get_nets -of_objects $found_clk]]
	if {![llength $clk_net_name]} {
		quiet_eval_unless_error [format "create_net_if_havent_already $found_clk_name"]
		quiet_eval_unless_error [format "connect_net_if_havent_already \[get_nets $found_clk_name\] \[get_ports $found_clk_name\]"]
		set clk_net_name [get_object_name [get_nets -of_objects $found_clk]]
	}
	if {[sizeof_collection $found_clk]} {
		set in_ports [remove_from_collection [get_ports $specified_port_names -filter {@port_direction==in}] $found_clk]
		set in_ports [get_ports -of_objects [get_nets -of_objects $in_ports] -filter {@port_direction==in}]
		#set port_count 0; set nrof_ports [sizeof_collection $in_ports]
		set in_port_nets [get_nets -of_objects $in_ports]
		set in_port_net_count 0; set nrof_port_nets [sizeof_collection $in_port_nets]
		while {$in_port_net_count < $nrof_port_nets} {
			set in_port_net [index_collection $in_port_nets $in_port_net_count]
			set ports_to_disconnect [remove_from_collection -intersect [get_ports -of_objects $in_port_net] $in_ports]
			set original_net_name [get_object_name $in_port_net]; 
			foreach_in_collection port $ports_to_disconnect {
				set substitute_net_name [string map {/ _} [format $substitute_net_naming_form [get_object_name $port]]]
				set register_name [string map {/ _} [format $register_naming_form $port]]
				echo [format "Now processing port = %s, net = $original_net_name, substitute_net_name = $substitute_net_name" [get_object_name $port]]
				if {$sofar_successful} { 
					quiet_eval_unless_error [format "create_net %s" $substitute_net_name ]
					quiet_eval_unless_error [format "disconnect_net \[get_nets $original_net_name\] \[get_ports %s\] " [get_object_name $port]]
					quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_ports %s\] " [get_object_name $port]]
					quiet_eval_unless_error [format "create_cell %s $register_model" $register_name ]
					quiet_eval_unless_error [format "connect_net \[get_nets $original_net_name\] \[get_pins %s\] " [format "$register_name/%s" $register_pin_map(Q)]]
					quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_pins %s\] " [format "$register_name/%s" $register_pin_map(D)]]
					set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $clk_net_name\] \[get_pins %s\] " $register_name/$register_pin_map(clk)]]
				} else { 
					echo_list $msg > register_io_ports.msg; return 0 
				}
			}
			incr in_port_net_count
		}
		set out_ports [get_ports $specified_port_names -filter {@port_direction==out}]
		set out_ports [get_ports -of_objects [get_nets -of_objects $out_ports] -filter {@port_direction==out}]
		set out_port_nets [get_nets -of_objects $out_ports]
		##set port_count 0; set nrof_ports [sizeof_collection $out_ports]
		set out_port_net_count 0; set nrof_port_nets [sizeof_collection $out_port_nets]
		while {$out_port_net_count < $nrof_port_nets} {
			set out_port_net [index_collection $out_port_nets $out_port_net_count]
			set ports_to_disconnect [remove_from_collection -intersect [get_ports -of_objects $out_port_net] $out_ports]
			set original_net_name [get_object_name $out_port_net]; 
			foreach_in_collection port $ports_to_disconnect {
				set substitute_net_name [string map {/ _} [format $substitute_net_naming_form [get_object_name $port]]]
				set register_name [string map {/ _} [format $register_naming_form $port]]
				echo [format "Now processing port = %s, net = $original_net_name, substitute_net_name = $substitute_net_name" [get_object_name $port]]
				if {$sofar_successful} { 
					quiet_eval_unless_error [format "create_net %s" $substitute_net_name ]
					quiet_eval_unless_error [format "disconnect_net \[get_nets $original_net_name\] \[get_ports %s\] " [get_object_name $port]]
					quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_ports %s\] " [get_object_name $port]]
					quiet_eval_unless_error [format "create_cell %s $register_model" $register_name ]
					quiet_eval_unless_error [format "connect_net \[get_nets $original_net_name\] \[get_pins %s\] " [format "$register_name/%s" $register_pin_map(D)]]
					quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_pins %s\] " [format "$register_name/%s" $register_pin_map(Q)]]
					set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $clk_net_name\] \[get_pins %s\] " $register_name/$register_pin_map(clk)]]
				} else { 
					echo_list $msg > register_io_ports.msg; return 0 
				}
			}
			incr out_port_net_count
		}
#		while {$port_count < $nrof_ports} {
#			set in_port [index_collection $in_ports $port_count]
#			set original_net [get_nets -of_objects $in_port]
#			set original_net_name [get_object_name $original_net]; set substitute_net_name [string map {/ _} [format $substitute_net_naming_form [get_object_name $original_net]]]
#			set pins_on_original_net [get_pins -of_objects $original_net -filter {@pin_direction==in}]
#			set out_ports_on_original_net [get_ports -of_objects $original_net -filter {@port_direction==out}]
#			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "create_net %s" $substitute_net_name ] ] } else { return 0 }
#			foreach_in_collection pin $pins_on_original_net {
#				echo [format "Now processing pin = %s, port = %s, net = %s" [get_object_name $pin] [get_object_name $in_port] [get_object_name $original_net]]
#				if {$sofar_successful} { 
#					set sofar_succsessful [quiet_eval_unless_error [format "disconnect_net \[get_nets $original_net_name\] \[get_pins %s\] " [get_object_name $pin]] ]
#					set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_pins %s\] " [get_object_name $pin]] ] 
#				} else { echo_list $msg > register_io_ports.msg; return 0 }
#			}
#			foreach_in_collection port $out_ports_on_original_net {
#				echo [format "Now processing port = %s, inport = %s, net = %s" [get_object_name $port] [get_object_name $in_port] [get_object_name $original_net]]
#				if {$sofar_successful} { 
#					set sofar_succsessful [quiet_eval_unless_error [format "disconnect_net \[get_nets $original_net_name\] \[get_ports %s\] " [get_object_name $port]] ] 
#					set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_ports %s\] " [get_object_name $port]] ]  
#				} else { echo_list $msg > register_io_ports.msg; return 0 }
#			}
#			set register_name [string map {/ _} [format $register_naming_form $original_net_name]]
#			if {$sofar_successful} { 
#				set sofar_succsessful [quiet_eval_unless_error [format "create_cell %s $register_model" $register_name ] ]
#				set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $original_net_name\] \[get_pins %s\] " $register_name/$register_pin_map(D)] ]
#				set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_pins %s\] " $register_name/$register_pin_map(Q)] ] 
#				set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $clk_net_name\] \[get_pins %s\] " $register_name/$register_pin_map(clk)] ] 
#			} else { return 0 }
#			incr port_count
#		}
#		while {$port_count < $nrof_ports} {
#			set out_port [index_collection $out_ports $port_count]
#			set original_net [get_nets -of_objects $out_port]
#			set original_net_name [get_object_name $original_net]; set substitute_net_name [string map {/ _} [format $substitute_net_naming_form [get_object_name $original_net]]]
#			set pins_on_original_net [get_pins -of_objects $original_net]
#			set out_ports_on_original_net [get_ports -of_objects $original_net -filter {@port_direction==out}]
#			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "create_net %s" $substitute_net_name ] ] } else { return 0 }
#			foreach_in_collection pin $pins_on_original_net {
#				echo [format "Now processing pin = %s, port = %s, net = %s" [get_object_name $pin] [get_object_name $out_port] [get_object_name $original_net]]
#				if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "disconnect_net \[get_nets $original_net_name\] \[get_pins %s\] " [get_object_name $pin]] ] } else { return 0 }
#				if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_pins %s\] " [get_object_name $pin]] ] } else { return 0 }
#			}
#			foreach_in_collection port $out_ports_on_original_net {
#				echo [format "Now processing port = %s, inport = %s, net = %s" [get_object_name $port] [get_object_name $out_port] [get_object_name $original_net]]
#				if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "disconnect_net \[get_nets $original_net_name\] \[get_ports %s\] " [get_object_name $port]] ] } else { return 0 }
#				if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_ports %s\] " [get_object_name $port]] ] } else { return 0 }
#			}
#			set register_name [string map {/ _} [format $register_naming_form $original_net_name]]
#			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "create_cell %s $register_model" $register_name ] ] } else { return 0 }
#			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $original_net_name\] \[get_pins %s\] " [format "$register_name/%s" $register_pin_map(Q)]] ] } else { return 0 }
#			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $substitute_net_name\] \[get_pins %s\] " [format "$register_name/%s" $register_pin_map(D)]] ] } else { return 0 }
#			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net \[get_nets $clk_net_name\] \[get_pins %s\] " [format "$register_name/%s" $register_pin_map(clk)]] ] } else { return 0 }
#			incr port_count
#		}
	}
	echo_list $msg > msg.inf
}
proc copy_hier_cells {specified_cells {arguments {}} } {
	set default_arguments [list [list target_design_naming_form "%s_extracted"] ]
	lappend arguments {}
	initvars $default_arguments; initvars $arguments
	set original_top [current_design_name]
	set foundcells [get_cells $specified_cells]
	reset_array return_array [list cell_names [get_object_name $foundcells]]; set all_pins {}; set all_nets {}; set all_ports {}
	reset_array port_array {}; reset_array cell_array {};reset_array net_array {};reset_array pin_array {}
	foreach_in_collection cell $foundcells {
		set pins [get_pins -of_objects $cell]; append_to_collection -unique all_pins $pins
		set cell_name [get_object_name $cell]
		foreach_in_collection pin $pins { 
			set pin_name [get_object_name $pin]; set net [get_nets -of_objects $pin]; set net_name [get_object_name $net]; set ports [get_ports -of_objects $net]; set port_names [get_object_name $ports]
			if {[llength $net_name]} {
				set pin_array($pin_name) $net_name
				append_to_collection -unique all_nets $net; append_to_collection -unique all_ports $ports
				set net_array($net_name) $port_names
				foreach_in_collection port $ports { set port_name [get_object_name $port]; set port_array($port_name) [get_attribute $port port_direction] }
			} else {
				set all_pins [remove_from_collection $all_pins $pin]
			}
		}
		if {[sizeof_collection [filter $cell {@is_hierarchical==true}]]} {
			set cell_array($cell_name) [get_attribute $cell ref_name]
		} else {
			set cell_array($cell_name) [get_object_name [get_lib_cells -of_objects $cell]]
		}
		set cell_array(Logic0) [list -logic 0]
		set cell_array(Logic1) [list -logic 1]
	}
	set return_array(pin_names) [get_object_name $all_pins];set return_array(net_names) [get_object_name $all_nets];set return_array(port_names) [get_object_name $all_ports]
	set target_design_name [format $target_design_naming_form $original_top]
	if {[sizeof_collection [get_designs -quiet $target_design_name]]} { remove_design $target_design_name	}
	create_design $target_design_name; current_design $target_design_name
	foreach cell_name $return_array(cell_names) {
		redirect -variable msg "create_cell $cell_name $cell_array($cell_name)"
		if {[llength [lsearch -inline -regexp $msg {[Ee][Rr][Rr][Oo][Rr]}]]} { echo $msg }
	}
	foreach pin_name $return_array(pin_names) {
		set net_name $pin_array($pin_name)
		redirect -variable msg "create_net_if_havent_already $net_name"; if {[llength [lsearch -inline -regexp $msg {[Ee][Rr][Rr][Oo][Rr]}]]} { echo $msg }
		redirect -variable msg "connect_net_if_havent_already \[get_nets $net_name\] \[get_pins $pin_name\]"; if {[llength [lsearch -inline -regexp $msg {[Ee][Rr][Rr][Oo][Rr]}]]} { echo $msg }
		foreach port_name $net_array($net_name) {
			redirect -variable msg [format "create_port_if_havent_already $port_name -direction %s" $port_array($port_name)]; if {[llength [lsearch -inline -regexp $msg {[Ee][Rr][Rr][Oo][Rr]}]]} { echo $msg }
			redirect -variable msg "connect_net_if_havent_already \[get_nets $net_name\] \[get_ports $port_name\]"; if {[llength [lsearch -inline -regexp $msg {[Ee][Rr][Rr][Oo][Rr]}]]} { echo $msg }
		}
	}
	current_design $original_top
	set return_array(port_array) [array get port_array]; set return_array(cell_array) [array get cell_array];set return_array(net_array) [array get net_array];set return_array(pin_array) [array get pin_array];
	return [array get return_array]
}
proc push_pins_up {specified_pins {arguments {}} } {
        set default_arguments [list [list new_ports_naming_command "string map {/ _pin_} %s"] [list target_design_naming_form "%s_modified"] ]
        lappend arguments {}
        initvars $default_arguments; initvars $arguments
	set names_of_specified_pins [get_object_name [get_pins $specified_pins]]; set original_top [current_design_name]; set target_design_name [format $target_design_naming_form $original_top]
	if {[sizeof_collection [get_designs -quiet $target_design_name]]} { remove_design $target_design_name }
	copy_design $original_top $target_design_name; current_design $target_design_name
	set foundpins [get_pins $names_of_specified_pins]; set msg {}; set sofar_successful 1
	foreach_in_collection pin $foundpins {
		set pin_name [get_object_name $pin]
		set direction [get_attribute $pin pin_direction]
		set new_port_name [eval [format $new_ports_naming_command $pin_name]]
		if {![sizeof_collection [set net [get_nets -of_objects $pin]]]} {
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "create_net_if_havent_already %s" $new_port_name] ] } else { return 0 }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net_if_havent_already \[get_nets %s\] \[get_pins %s\]" $new_port_name $pin_name] ] } else { return 0 }
			set net [get_nets -of_objects $pin]
		}
		if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "create_port_if_havent_already %s -direction %s" $new_port_name $direction] ] } else { return 0 }
		if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net_if_havent_already \[get_nets %s\] \[get_ports %s\]" $new_port_name $new_port_name] ] } else { return 0 }
	}
	if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error make_buses_for_design] } else { return 0 }
	current_design $original_top
	return $msg
}
proc quiet_eval_unless_error { eval_command {arguments {}} } {
	set default_args [list match_term {[Ee][Rr][Rr][Oo][Rr]} upvar_vars msg msg_varname msg]
	reset_array argument_array $default_args
	if {[is_array $arguments]} { array set argument_array $arguments } elseif {[string match *help $eval_command]} { puts "Usage: quiet_eval_unless_error <eval_command> <arguments>\nDefault arguments:\n"; echo_array argument_array; return 1 }
	foreach key [array names argument_array] { set $key $argument_array($key) }
	#set var_terms_in_command [concat $upvar_vars [foreach_regsub {^\$([^\(]+?).*} [lsearch -all -inline $eval_command {$*}] {\1} ]]; 
	foreach varname $upvar_vars { upvar $varname $varname }
	redirect -variable temp [format "set returncode \[$eval_command\]"]; set temp [split $temp "\n"]
	if {![llength $returncode]} { set returncode 0 } else { set returncode 1 }
	if {!$returncode} { echo [lsearch -all -inline -regexp $temp $match_term] }
	#if {[llength [set matched_msgs [lsearch -all -inline -regexp $temp $match_term]]]} { echo $matched_msgs; set returncode 0 } else { set returncode 1 }
	lappend $msg_varname ">>$eval_command"
	foreach line $temp {lappend $msg_varname $line}
	return $returncode
}
proc get_toplevel_cell_of { specified_cells } {
	set return_collection {}
	foreach_in_collection cell $specified_cells {
		set cellname [get_object_name $cell]
		if {[llength [set split_cell_name [split $cellname /] ] ] > 1} {
			append_to_collection -unique return_collection [get_cells [lindex $split_cell_name 0] -filter {@is_hierarchical==true}]
		} else {
			append_to_collection -unique return_collection $cell
		}
	}
	return $return_collection
}
proc get_leaf_cells_under { specified_cell {arguments {}} } {
	set default_arguments [list {max_depth 10} ]
	initvars $default_arguments
	lappend arguments {}
	foreach argument $arguments {
		if {[llength $argument] == 2} {
			set [lindex $argument 0] [lindex $argument 1]
		}
	}
	if {[sizeof_collection [filter [set foundcells [get_cells $specified_cell]] {@is_hierarchical==true}] ]} {
		set return_collection {}
		foreach_in_collection foundcell $foundcells {
			set cellname [get_object_name $foundcell]
			set sublevels_to_process [get_object_name [get_cells -quiet "$cellname/*" -filter {@is_hierarchical==true}]]
			append_to_collection -unique return_collection [get_cells -quiet "$cellname/*" -filter {@is_hierarchical==false}]
			while {[llength $sublevels_to_process]} {
				set current_sublevel [lindex $sublevels_to_process end]
				set sublevels_to_process [lrange $sublevels_to_process 0 end-1]
				set cells_under_thislevel [get_cells -quiet "$current_sublevel/*" -filter {@is_hierarchical==false}]
				set levels_under_thislevel [get_cells -quiet "$current_sublevel/*" -filter {@is_hierarchical==true}]
				lappend sublevels_to_process [get_object_name $levels_under_thislevel]
				append_to_collection -unique return_collection $cells_under_thislevel
			}
		}
	} else {
		puts "Give me a hierarchical cell.\n"
	}
	return $return_collection
}
proc fault_collection_to_array { fault_collection } {
	reset_array return_array {}
	foreach_in_collection fault $fault_collection {
		set pinpath [get_attribute $fault pinpath]
		set return_array($pinpath) [get_attribute $fault type]
	}
	return [array get return_array]
}
proc array_to_fault_collection { given_array } {
	if {[llength $given_array] == 1} { 
		upvar $given_array fault_array 
	} elseif {[is_array $given_array]} {
		reset_array fault_array $given_array
	} else {
		puts "Usage:\n\tarray_to_fault_collection (name_of_array|[array get name_of_array])\n"
	}
	set return_collection {}
	if {[array exists fault_array]} {
		foreach pinpath [array names fault_array] {
			append_to_collection -unique return_collection [get_faults -pinpath $pinpath -filter [format "@type==%s" $fault_array($pinpath)]]
		}
	}
	return $return_collection
}
proc remove_cell_from_current_level { cellname } {
	set infologs {};reset_array opposite_direction_of [list in out out in inout inout]
	set cell [get_cells $cellname]
	set cellpins [get_pins -of_objects $cell]
	foreach_in_collection pin $cellpins {
		set pindirection [get_attribute $pin pin_direction]; set portdirection $opposite_direction_of($pindirection)
		set net [get_nets -of_objects $pin]
		#if {![sizeof_collection [get_ports -of_objects $net -filter [format "@port_direction==%s" $pindirection]]]} {
			set newportname [string map [list "/" "_pin_" ] [get_object_name $pin]]
			redirect -variable temp {create_port $newportname  -direction $portdirection}; lappend infologs $temp
			redirect -variable temp {connect_net $net [get_ports $newportname]}; lappend infologs $temp
			redirect -variable temp {disconnect_net $net $pin}; lappend infologs $temp
		#}
	}
	echo $infologs > cmdlog
	if {[sizeof_collection [set left_nets [get_nets -of_objects $cellpins]]]} { puts "Error: Not all connections to specified cells are removed. These nets are:"; echo [join [get_object_name $left_nets] " \n"]}
	remove_cell $cell
}
proc reappend_cell_to_current_level { cellname ref_name } {
	set infologs {}
	create_cell $cellname $ref_name
	set ports [get_ports ${cellname}_pin_*]
	foreach_in_collection port $ports {
		set original_pinname [string map [list _pin_ /] [get_object_name $port]]
		set net [get_nets -of_objects $port]
		redirect -variable temp {disconnect_net $net $port}; lappend infologs $temp
		redirect -variable temp {connect_net $net [get_pins $original_pinname]}; lappend infologs $temp
		redirect -variable temp {remove_port $port}; lappend infologs $temp
	}
	echo $infologs > cmdlog
}
proc analyze_timing_reports { tmg_rpt } {
	set line_split [lsearch -all -inline -not [split $tmg_rpt "\n"] {}]
	reset_array returnarray {}; reset_array temparray {}
	set i 0; set path_id 0
	while {$i < [llength $line_split]} {
		set line [lindex $line_split $i]
		set wordlist [lsearch -all -inline -not [split $line { }] {}]
		set pinname [lindex $wordlist 0]
		if {[string match */* $pinname]} {
			if {[sizeof_collection [get_pins -quiet $pinname] ] } {
				set numbers [lsearch -all -inline -regexp $wordlist {^\d\.\d\d}]
				if {![llength $numbers]} { 
					incr i; set nextline [lindex $line_split $i]
					set wordlist [concat $wordlist [lsearch -all -inline -not [split $nextline { }] {}]]
					set numbers [lsearch -all -inline -regexp $wordlist {^\d\.\d\d}]
				}
				set temparray($pinname) [lrange $wordlist 1 end]
			}
		} elseif {[set stp_marker [lsearch -exact $wordlist {Startpoint:}]] != -1} {
			if {[info exists numbers]} {
				if {[llength $numbers] ==2 } { set temparray(total_slack) [lindex $numbers 1] }
				set returnarray($path_id) [array get temparray]
				lappend returnarray(slacks) $temparray(total_slack)
				reset_array temparray {}
				incr path_id
			}
			set temparray(start_point) [get_object_name [get_cells -quiet $wordlist] ] 
		} elseif {[set edp_marker [lsearch -exact $wordlist {Endpoint:}]] != -1} {
			set temparray(end_point) [get_object_name [get_cells -quiet $wordlist] ]
		}
		incr i
	}
	return [array get returnarray]
}
proc bypass_clk { {arguments {}} } {
        ## Define default arguments
	set default_arguments [list specified_clkpins {} bypass_control_net_name bypass_control replacement_additional_processing {}\
				mux_gate_model saed32rvt_tt1p05v25c/MUX21X1_RVT mux_gate_pinnames {bypassed A1 replacement A2 S S0 Z Y} target_design_nametag clkbypassed\
				replacement_net_name fastclk surrogate_net_naming_form %s_surrogate mux_naming_form %s_bypassmux]
	reset_array argument_array $default_arguments
	## Read-in arguments
	if {[is_array $arguments]} {array set argument_array $arguments} elseif {[expr ![llength $arguments] || [string match *help $arguments]]} {puts "Usage:\n\tbypass_pin <arguments\nDefault arguments:\n\n"; echo_array argument_array; return 1 }
	foreach key [array names argument_array] { set $key $argument_array($key) }
	reset_array mux_gate_pinname_hash $mux_gate_pinnames 
	set found_clkpins [filter [get_pins -quiet $specified_clkpins] -regexp {@full_name =~ ".*CLK"}]
	if {[sizeof_collection $found_clkpins]} {
		set clkpin_names [get_object_name $found_clkpins]
		set original_design_name [current_design_name]; set target_design_name [format "%s_%s" $original_design_name $target_design_nametag]
		create_sandbox_design $target_design_nametag
		set found_clkpins [filter [get_pins -quiet $clkpin_names] -regexp {@full_name =~ ".*CLK"}]
		set original_driving_nets [get_nets -of_objects $found_clkpins]
		set msg {}; set net_nr 0; set total_nr [sizeof_collection $original_driving_nets]
		quiet_eval_unless_error [format "create_net_if_havent_already %s" $bypass_control_net_name]; set sofar_successful [sizeof_collection [get_nets $bypass_control_net_name] ]
		quiet_eval_unless_error [format "create_net_if_havent_already %s" $replacement_net_name]; set sofar_successful [sizeof_collection [get_nets $replacement_net_name] ]
		while {$net_nr < $total_nr} {
			set net [index_collection $original_driving_nets $net_nr]; set net_name [get_object_name $net]; set surrogate_net_name [format $surrogate_net_naming_form [string map {/ _} $net_name]]
			set driven_cells [remove_from_collection -intersect [get_cells -of_objects [get_pins -of_objects $net -filter {@pin_direction==in}] -filter {@is_sequential==true}] \
						 [get_cells -of_objects $found_clkpins] ]
			set clkpins_driven_by_this_net [remove_from_collection -intersect [get_pins -of_objects $driven_cells -filter {@pin_direction==in}] $found_clkpins]
			quiet_eval_unless_error [format "create_net_if_havent_already %s" $surrogate_net_name]; set sofar_successful [sizeof_collection [get_nets $surrogate_net_name]]
			foreach_in_collection clkpin $clkpins_driven_by_this_net {
				if {$sofar_successful} {
					set sofar_successful [quiet_eval_unless_error [format "disconnect_net \[get_nets %s\] \[get_pins %s\]" $net_name [get_object_name $clkpin] ] ]
					set sofar_successful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\]" $surrogate_net_name [get_object_name $clkpin] ] ]
				} else {
					return 0
				}
			}
			set bypass_mux_name [format $mux_naming_form [string map {/ _} $net_name] ]
			if {$sofar_successful} { set sofar_successful [quiet_eval_unless_error [format "create_cell %s %s" $bypass_mux_name $mux_gate_model]] } else {return 0}
			if {$sofar_successful} { set sofar_successful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\]" $surrogate_net_name $bypass_mux_name/$mux_gate_pinname_hash(Z) ] ] } else {return 0}
			if {$sofar_successful} { set sofar_successful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\]" $net_name $bypass_mux_name/$mux_gate_pinname_hash(bypassed) ] ] } else {return 0}
			if {$sofar_successful} { set sofar_successful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\]" $replacement_net_name $bypass_mux_name/$mux_gate_pinname_hash(replacement) ] ] } else {return 0}
			if {$sofar_successful} { set sofar_successful [quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\]" $bypass_control_net_name $bypass_mux_name/$mux_gate_pinname_hash(S) ] ] } else {return 0}
			incr net_nr
		}
	} else {
		puts "No clk pins found among specified.\n"
	}
}
proc find_another_name_for { { arguments {}} } {
	set default_arguments [list specified_name {} object_class net alternative_namingtag [lincrement 1 10] other_param {}]
        reset_array argument_array $default_arguments
        if {[is_array $arguments]} {array set argument_array $arguments} elseif {[expr ![llength $arguments] || [string match *help $arguments]]} {puts "Usage:\n\tbypass_pin <arguments\nDefault arguments:\n\n"; echo_array argument_array; return 1 }
        foreach key [array names argument_array] { set $key $argument_array($key) }
	upvar name_to_try name_to_try; upvar msg msg
	set candidate_tried 0; set name_to_try $specified_name; set no_solution_found [sizeof_collection [get_${object_class}s -quiet $name_to_try]]
	while {$no_solution_found && $candidate_tried < [llength $alternative_namingtag]} {
		set name_to_try [format "%s_%s" $specified_name [lindex $alternative_namingtag $candidate_tried]]
		set no_solution_found [sizeof_collection [get_${object_class}s -quiet $name_to_try]]
		incr candidate_tried
	}
	if {$no_solution_found} {
		return {}
	} else {
		set returncode [quiet_eval_unless_error [join [concat "create_${object_class} $name_to_try" $other_param] " "]]
		return $returncode
	}
}
proc bypass_pin { {arguments {}} } {
	## Define default arguments
	set default_arguments [list specified_objects {} bypass_control_netname bypass_control replacement_additional_processing {}\
				mux_gate_model saed32rvt_tt1p05v25c/MUX21X1_RVT mux_gate_pinnames {bypassed A1 replacement A2 S S0 Z Y} target_design_naming_form %s_bypassed\
				bypass_with_net_array {}]
	reset_array argument_array $default_arguments
	## Read-in arguments
	if {[is_array $arguments]} {array set argument_array $arguments} elseif {[expr ![llength $arguments] || [string match *help $arguments]]} {puts "Usage:\n\tbypass_pin <arguments\nDefault arguments:\n\n"; echo_array argument_array; return 1 }
	foreach key [array names argument_array] { set $key $argument_array($key) }
	reset_array mux_gate_pinname_hash $mux_gate_pinnames; reset_array bypass_object_with $bypass_with_net_array
	set found_objects [add_to_collection [set pin_collection [get_pins -quiet $specified_objects]] [set port_collection [get_ports -quiet $specified_objects]] ]
	if {[sizeof_collection $found_objects]} { 
		set pin_names [get_object_name $pin_collection]; set port_names [get_object_name $port_collection]
		set original_design_name [current_design_name]; set target_design_name [format $target_design_naming_form $original_design_name]
		if {[sizeof_collection [get_designs -quiet $target_design_name]]} { remove_design $target_design_name }
		copy_design $original_design_name $target_design_name; current_design $target_design_name
		set found_objects [add_to_collection [set pin_collection [get_pins -quiet $pin_names]] [set port_collection [get_ports -quiet $port_names]] ]
		set nrof_processed_objects 0; set total_nrof_objects [sizeof_collection $found_objects]; set sofar_successful 1; set msg {}
		if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "create_net_if_havent_already %s" $bypass_control_netname] ] }
		while {$nrof_processed_objects < $total_nrof_objects} {
			set found_object [index_collection $found_objects $nrof_processed_objects]
			set net [get_nets -of_objects $found_object]; set sofar_successful 1
			set object_name [get_object_name $found_object]; set new_object_name [string map {/ _} $object_name]
			set bypassednetname [format "%s_bypassednet" $new_object_name]; 
			if {![info exists bypass_object_with($object_name)]} { set bypass_with_netname [format "%s_replacementnet" $new_object_name ] } else { set bypass_with_netname $bypass_object_with($object_name) }
			if {[expr [sizeof_collection [get_ports -quiet $found_object -filter {@port_direction==in}]] || [sizeof_collection [get_pins -quiet $found_object -filter {@pin_direction==out}]]]} { 
				set net_for_apin $bypassednetname; set net_for_zpin [get_object_name $net]
			} elseif {[expr [sizeof_collection [get_ports -quiet $found_object -filter {@port_direction==out}]] || [sizeof_collection [get_pins -quiet $found_object -filter {@pin_direction==in}]]]} { 
				set net_for_zpin $bypassednetname; set net_for_apin [get_object_name $net]
			}
			if {[sizeof_collection $net]} { set sofar_successful [quiet_eval_unless_error "disconnect_net $net $found_object"] } else { echo [format "Warning: No net connected to found_object %s; skipping it." [get_object_name $found_object]]; set sofar_successful 0 }
			if {$sofar_successful} { 
				set sofar_succsessful \
				[find_another_name_for [list specified_name [set muxname [format "%s_bypassmux" $new_object_name ] ] object_class cell other_param $mux_gate_model]]
				set muxname $name_to_try
			}
			if {$sofar_successful} { set sofar_successful [find_another_name_for [list specified_name $bypassednetname object_class net ] ]; set bypassednetname $name_to_try }
			if {$sofar_successful} { set sofar_succsessful [find_another_name_for [list specified_name $bypass_with_netname object_class net ] ]; set bypass_with_netname $name_to_try }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net_if_havent_already %s %s" $bypassednetname $found_object] ] }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net_if_havent_already \[get_nets %s\] \[get_pins %s\]" $net_for_apin $muxname/$mux_gate_pinname_hash(bypassed)]] }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net_if_havent_already \[get_nets %s\] \[get_pins %s\]" $net_for_zpin $muxname/$mux_gate_pinname_hash(Z)]] }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net_if_havent_already \[get_nets %s\] \[get_pins %s\]" $bypass_control_netname $muxname/$mux_gate_pinname_hash(S)]] }
			if {$sofar_successful} { set sofar_succsessful [quiet_eval_unless_error [format "connect_net_if_havent_already \[get_nets %s\] \[get_pins %s\]" $bypass_with_netname $muxname/$mux_gate_pinname_hash(replacement)]] }
			if {$sofar_successful} { append_to_collection -unique processed_objects $found_object; incr nrof_processed_objects } else { echo_list $msg > msg.inf; return 0 }
		}
		echo_list $msg > msg.inf
		return $processed_objects
	} else {
		puts "No specified objects found to bypass.\n"
		return {}
	}
}
proc lrepeat { {count 0} {string {}} {separator |}} {
	return [lrange [split [string repeat "$string$separator" $count] $separator] 0 end-1]
}
proc append_to_list_of_collections { listname list2append {uniqueness -notunique} } {
	upvar $listname targetlist
	if {[string match -unique $uniqueness]} {
		foreach element $list2append { 
			if {[search_collection $targetlist $element] != -1} { lappend targetlist $element }
		}
	} else {
		foreach element $list2append { 
			lappend targetlist $element
		}
	}
}
proc lit_combs {srclist { requested_depth 2 } } {
	set totallength [llength $srclist]
        if {[string is digit $requested_depth] && ($requested_depth <= $totallength)} { set depth $requested_depth } else {
                return -errorcode 1 -errorinfo "Error: second input=$requested_depth should be a number and no more than first input." }
	set returnlist {}; set indices [combinations $totallength $depth]; foreach index $indices { lappend returnlist [sublist_byindices $srclist $index] }
	return $returnlist
}
proc combinations { totallength { requested_depth 2 } } {
	if {[string is digit $requested_depth] && ($requested_depth <= $totallength)} { set depth $requested_depth } else { 
		return -errorcode 1 -errorinfo "Error: second input=$requested_depth should be a number and no more than first input." }
	set returnlist {}; set depth2list {}; if {$depth==2} { 
		foreach firstindex [lincrement 0 [expr $totallength-2]] { foreach secondindex [lincrement [expr $firstindex+1] [expr $totallength-1]] {
		lappend returnlist [list $firstindex $secondindex] } }
	} elseif {$depth>=2} { foreach firstindex [lincrement 0 [expr $totallength-$depth]] {
		foreach latterindices [lit_combs [lincrement [expr $firstindex+1] [expr $totallength-1]] [expr $depth-1]] {
		lappend returnlist [concat $firstindex $latterindices] }}
	} elseif {$depth==1} { set returnlist [lincrement 0 [expr $totallength-1]] }
	
	#set start [lincrement 0 [expr $depth-1]]; set returnlist {}
	#set countervector [lrepeat [expr $depth+1] 0]; set counterstops 0
	#set translatedvector $start
	#set end [lrange [concat [lincrement [expr $totallength-$depth] [expr $totallength-1]] [expr $totallength-1] ] 1 end]
	#while { !$counterstops  } {
	#	set incrementvector [concat [lrepeat $depth 0] 1]
	#	for { set j [expr $depth-1] } { $j >= 0 } { incr j -1 } {
	#		lset translatedvector $j [expr [lindex $start $j]+[lindex $countervector [expr $j+1]]]
	#		set afterincrement [expr [lindex $countervector [expr $j+1]]+[lindex $incrementvector [expr $j+1]] ]
	#		if {[lindex $end $j] <= $afterincrement} {
	#			lset countervector [expr $j+1] [expr [lindex $countervector $j]+1]
	#			lset incrementvector $j 1
	#		} else {
	#			lset countervector [expr $j+1] $afterincrement
	#		}
	#		if {![lindex $incrementvector $j] && ![lindex $countervector $j]} { break }
	#	}
	#	lappend returnlist [get_elements_by_indices $srclist $translatedvector]
	#	set counterstops [lindex $incrementvector 0]
	#}
	return $returnlist
}
proc all_selections_of {srclist} {
	set toplength [llength $srclist]; set sublengths [formatlist $srclist {llength {%s}} eval] 
	set longsubinds [lsearch -all -not -regexp $sublengths ^(0|1)$]; 
	set nroflongsubs [llength $longsubinds]; set prodsoflongsubs {}; foreach ind [lincrement 1 [expr $nroflongsubs-1]] { 
		lappend prodsoflongsubs [lprod [sublist_byindices $sublengths [lrange $longsubinds $ind end]]] }; lappend prodsoflongsubs 1
	set returnform $srclist; set returnlist {};set ind 0; set end [lprod [lsearch -all -not -inline $sublengths 0]]; while {$ind<$end} {
		set longsubselections {}; for {set subind 0} {$subind < [expr $nroflongsubs]} {incr subind} { 
			set thisprod [lindex $prodsoflongsubs $subind];set thislength [lindex $sublengths [lindex $longsubinds $subind]]
			lset returnform [lindex $longsubinds $subind] [lindex $srclist [lindex $longsubinds $subind] [expr int(double($ind)/$thisprod)%$thislength]] }
		lappend returnlist $returnform; incr ind
	}; return $returnlist
}
proc lprod {srclist} {	set result 1; foreach item $srclist { set result [expr $result*$item]}; return $result }

proc get_elements_by_indices { srclist indices } {
	set returnlist {}
	foreach index $indices { lappend returnlist [lindex $srclist $index ] }
	return $returnlist
}
proc lincrement { start end {increment 1} {abort_limit 10000000} } {
	set returnlist {}
	if {[string is digit $start] && [string is digit $end] && [llength $start] &&  [llength $end]} {
		if { $start > $end } { set increment [expr -1*abs($increment)];
			for {set i $start} { $i >= [get_extremes_of_list [list $end [expr $start+$abort_limit]] <]} { incr i $increment } { lappend returnlist $i } 
		} else { for {set i $start} { $i <= [get_extremes_of_list [list $end [expr $start+$abort_limit]] <]} { incr i $increment } { lappend returnlist $i }}
	}
	return $returnlist
}
proc linspace { start end {length {}} {abort_limit 100000000} } {
	set returnlist {}
	if {[string is digit $start] && [string is digit $end] && [llength $start] &&  [llength $end]} {
		if {![is_integer $length]} {set length [expr abs($end-$start)]}
		if {$length == 1} {set returnlist $end
		} else { set increment [expr abs($end-$start)*1.0/($length-1)];set length [lmin [list $length $abort_limit]] 
			if { $start > $end } { set increment [expr -1*$increment]; set start $end }
			for {set i 0} { $i < $length} { incr i } { lappend returnlist [expr $start+$i*$increment] }
		}
	}
	return $returnlist
}
proc search_collection { list_of_collections key_collection {var1 {}} {var2 {}} {var3 {}} } {
	set vars [list $var1 $var2 $var3]; set all_search 0; set inline_search {$i}; set not_search 0; set matching_code 0; set emptyresult -1
	foreach var $vars {
		if {[string match -all $var]} {
			set all_search 1
		} elseif {[string match -inline $var]} {
			set inline_search {$collection}; set emptyresult {}
		} elseif {[string match -not $var]} {
			set not_search 1; set matching_code -1
		}
	}
	set i 0; set continue_to_run 1; set returnlist {}
	if {[sizeof_collection $key_collection]} {
		while {($i < [llength $list_of_collections]) && $continue_to_run} {
			set collection [lindex $list_of_collections $i]
			if {[set match_result [compare_collections $collection $key_collection]] == $matching_code} {
				eval "lappend returnlist $inline_search"
				set continue_to_run $all_search
			}
			incr i
		}
	} else {
		puts "2nd input (collection search term) is not a collection.\n"
	}
	if {[llength $returnlist]} {
		return $returnlist
	} else {
		return $emptyresult
	}
}
proc find_longest_sublist {toplist {number_of_returns {}}} {
	set lengthlist [formatlist $toplist {llength %s}]
	set longestlength [get_extremes_of_list $lengthlist ]
	set longestelement_indices [lsearch -all -exact $lengthlist $longestlength]
	if {![string is digit $number_of_returns] || ![llength $number_of_returns] || [expr $number_of_returns >= [llength $longestelement_indices]]} { set number_of_returns end }
	set returnlist {}; foreach index [lrange $longestelement_indices 0 $number_of_returns] { lappend returnlist [lindex $toplist $index] }
	return $returnlist
}
proc initvars { varlist } {
	if {[llength $varlist]} {
		set nr_var_set 0
		foreach var $varlist {
			if {[llength $var] ==2} {
				upvar [lindex $var 0] varname
				set varname [lindex $var 1]
				incr nr_var_set
			}
		}
		return $nr_var_set
	}
}
proc get_rand_binary_vector { listlength } {
	set returnlist {}
	if {[string is digit $listlength]} { for {set i 0} {$i < $listlength} {incr i} { lappend returnlist [expr int(rand()*2)] } }
	return $returnlist 
}
proc is_port { name {filter {}} } {
	## shorthand for directions
	if {[string match in $filter] || [string match out $filter] || [string match inout $filter]} {
		 set filter [format "-filter {@port_direction==%s}" $filter]
	}
	return [eval "sizeof_collection \[get_ports -quiet $name $filter\]"]
}
proc dec2bin {i {width {}}} {
	set result {}
	while {$i>0} {
		set result [concat [expr {$i%2}] $result]
		set i [expr {$i/2}]
	}
	if {[llength [regexp -inline {^\d+$} $width]]} {
		if {$width > [llength $result]} {
			set result [concat [split [string repeat 0 [expr $width - [llength $result] ]] {}] $result]
		}
	}
	if {![llength $result]} { set result 0 }
	return $result
}
proc reset_array {arrayname value} {
	upvar $arrayname thearray
	if {[info exists thearray]} { unset thearray }
	array set thearray {}; array unset thearray
	if {[is_array $value]} { array set thearray $value } else { array set thearray {} }
	return [array get thearray]
}
proc remove_from_list {arg1 arg2 {arg3 {}}} {
	set intersecting 0
	#set motherlist {}; set objects2remove {}
	foreach arg [list $arg1 $arg2 $arg3] { if {[string match -intersect $arg]} { set intersecting 1 
		} elseif {![info exists motherlist]} { set motherlist $arg
		} elseif {![info exists objects2remove]} { set objects2remove $arg } }
	#if {$intersecting} { set matchingeval {string match %s %s} } else { set matchingeval {expr ![string match %s %s]} }
	if {$intersecting} { set matchingeval {string match %s %s} } else { set matchingeval {expr ![string match %s %s]} }
	if {[llength $motherlist] && [llength $objects2remove]} {
		#if {!$intersecting} { set joinbracket "\} {%s}\\\] && !\\\[string match \{"; set formatform "expr !\\\[string match {%s} {%s}\\\]"
		#} else { set joinbracket "\} {%s}\\\] || \\\[string match \{"; set formatform "expr \\\[string match {%s} {%s}\\\]" }
		##set objectsregexp [changeportnames_for_lsearch [format "^(%s)$" [join $objects2remove {|}]]]
		##if {$intersecting} { return [lsearch -regexp -all -inline -exact $motherlist $objectsregexp]
		##} else { return [lsearch -regexp -all -inline -not -exact $motherlist $objectsregexp]	}
		#set sub_list_form [format $formatform [join $objects2remove $joinbracket] %s]
		#return [sub_list $motherlist $sub_list_form]
		set result {{} {}};foreach a $motherlist {set ind 0;foreach b $objects2remove {set ind [expr $ind||[leq $a $b]]};llappend result $ind $a}
		return [lindex $result $intersecting]
	} elseif {[llength $motherlist] && ![llength $objects2remove]}  {
		if {$intersecting} {
			return {}
		} else {
			return $motherlist
		}
	} else {
		#puts "Warning: first list argument is empty.\n"
		return {}
	}
}
proc check_another_design {other_design_name} {
	set previous_design [current_design_name]
	current_design $other_design_name
	check_design
	current_design $previous_design
}
proc write_logs_to_file {log_var log_file_name} {
	puts "Overwriting $log_file_name.\n"
	echo "" > $log_file_name
	foreach log_entry $log_var {
		echo $log_entry >> $log_file_name
	}
	puts "Check $log_file_name for log content.\n"
}
proc flatten_list {original_list} {
	set interlist0 $original_list
	set interlist1 {}
	while {[expr [llength [lindex $interlist0 0]]>1]} {
		set interlist1 {}
		foreach elementlvl1 $interlist0 {
			foreach elementlvln $elementlvl1 {
				lappend interlist1 $elementlvln
			}
		}
		set interlist0 $interlist1
	}
	return $interlist0
}
proc unwrap_list {srclist} {
	set returnlist {}; foreach srccell $srclist { foreach subcell $srccell { lappend returnlist $subcell } }; return $returnlist
}
proc get_unique_list {original_list} {
	array set freq_hash {}; foreach element $original_list { set freq_hash($element) {} }
	return [array names freq_hash]
}
proc lsort_bysublistlength_increasing {a b} { return [expr [llength $a]>[llength $b]] }
proc lsort_bysublistlength_decreasing {a b} { return [expr [llength $b]>[llength $a]] }
proc get_frequency_array {original_list} {
        reset_array returnarray {}; set keylist {}
        foreach element $original_list {
                if {[expr [lsearch -exact $keylist $element] == -1]} {
                        set returnarray($element) 1
			set keylist [array names returnarray]
                } else {
			incr returnarray($element)
		}
        }
        return [array get returnarray]
}
proc get_nonempty_list {original_list} {
	set returnlist {}
	foreach element $original_list {
		if {[llength $element]} {
			lappend returnlist $element
		}
	}
	return $returnlist
}
proc formatlist {original_list {elementformat {%s}} {method {subst}} {upvarlist {}}} {
	if {[llength $upvarlist]} { foreach var2up $upvarlist { upvar $var2up $var2up } }
	set returnlist {}; set translatedform [string map {{%s} {$element}} $elementformat]
	foreach element $original_list { set thisform [subst $translatedform]; lappend returnlist [$method $thisform]	}
	return $returnlist
}
proc leq {a b} {expr {$a eq $b} }
proc createarrayfrom {keylist {elementformat {%s}} {method list} {upvarlist {}}} {
	if {[llength $upvarlist]} { foreach var2up $upvarlist { upvar $var2up $var2up } }
	array set returnarray {}
	foreach key $keylist { set returnarray($key) [$method [string map [list {%s} $key] $elementformat]] }
	return [array get returnarray]
}
proc formatarray {arraylist {elementformat {%value}} {method {}} {upvarlist {}}} {
	if {[llength $upvarlist]} { foreach var2up $upvarlist { upvar $var2up $var2up } }
	if {[llength $arraylist] == 1} {
		upvar $arraylist original_array
	} elseif {[is_array $arraylist]} {
		array set original_array $arraylist
	} else {
		puts "not an array. usage: format_array \[array get some_array|arrayname\] {elementformat e.g. %value} {method e.g. eval}\n"
		return 0
	}
	array set returnarray {}
	if {[llength $method]} { 
		foreach key [array names original_array] { 
			set returnarray($key) [$method [string map [list %value $original_array($key)] [string map [list {%key} $key] $elementformat]]]
		}
	} else {
		foreach key [array names original_array] {
			set returnarray($key) [string map [list %value $original_array($key)] [string map [list {%key} $key] $elementformat]]
		}
	}
	return [array get returnarray]
}
proc write_to_file {filecontentlist {targetfilename writtenbytcl.txt}} {
	puts "Writing to $targetfilename .\n"
	set FileID [open $targetfilename "w"]
	foreach filecontentline $filecontentlist {
		puts $FileID $filecontentline
	}
	close $FileID
}
## > means max, < means min
proc get_extremes_of_list {originallist {comparator >}} {
	set onlynumberlist [lsearch -all -regexp -inline $originallist {\d+}] 
	set returnelement [lindex $onlynumberlist 0]
	foreach element $onlynumberlist {
		if {[eval "expr $element $comparator $returnelement"]} {
			set returnelement $element
		}
	}
	return $returnelement
}
proc lmedian {originallist {percent 0.5}} {set onlynumberlist [lsearch -all -regexp -inline $originallist {[\d\.]+}];
	if {[is_integer $onlynumberlist]} { set sortedlist [lsort -integer $onlynumberlist]
	} else { set sortedlist [lsort -real $onlynumberlist] 	}
	return [lindex $sortedlist [expr int([llength $sortedlist]*$percent)] ]
}
proc lmean {originallist {round_method {}}} { if {[llength $round_method]} { 
            return [eval "expr int(${round_method}(double(\[lsum \$originallist])/\[llength \$originallist\]))"]
    } else { return [expr double([lsum $originallist])/[llength $originallist]]}
}
proc lmax {originallist} {	set onlynumberlist [lsearch -all -regexp -inline $originallist {\d+}];set returnelement [lindex $onlynumberlist 0]
	foreach element $onlynumberlist {if {[eval "expr $element > $returnelement"]} {	set returnelement $element}};return $returnelement}
proc lmin {originallist} {	set onlynumberlist [lsearch -all -regexp -inline $originallist {\d+}];set returnelement [lindex $onlynumberlist 0]
	foreach element $onlynumberlist {if {[eval "expr $element < $returnelement"]} {	set returnelement $element}};return $returnelement}
proc lincr {srclistname index {value 1}} {
	upvar $srclistname srclist;
	if {![info exists srclist]} {
		return -code error "Error using lincr: expected $srclistname as list name, it doesn't exist ."
	} else {
		if {![string is integer $index ]} { return -code error "Error using lincr: expected second argument to be index, it isn't a number."
		} elseif {[llength $srclist]<$index} { return -code error "Error using lincr: expected $srclistname to have ${index}th element, it doesn't."
	        } elseif {![string is double [lindex $srclist $index]]} { 
			return -code error "Error using lincr: expected \[lindex $srclistname $index\] to be number, it isn't."
		} else {
			lset srclist $index [expr [lindex $srclist $index]+$value]
			return 1
		}
	}
}
proc lconcat {srclistname index {newitem {}} } {
	upvar $srclistname srclist;
	if {![info exists srclist]} {
		return -code error "Error using lconcat: expected $srclistname as list name, it doesn't exist ."
	} else {
		if {![string is integer $index ]} { return -code error "Error using lconcat: expected second argument to be index, it isn't a number."
		} elseif {[llength $srclist]<$index} {return -code error "Error using lconcat: expected $srclistname to have ${index}th element, it doesn't."
		} else {
			lset srclist $index [concat [lindex $srclist $index] $newitem]
			return 1
		}
	}
}
proc llappend {srclistname index value} {
	if {![string is integer $index]} {
		return -code error "Error using llappend: expected index as second argument, got $index ."
	} else {
		upvar $srclistname srclist;
		if {($index < 0) || ($index > [llength $srclist])} {
			return -code error [format "Error using llappend: expected second argument in \[0,%s\], got $index ." [llength $srclist]]
		} else {
			set temp [lindex $srclist $index]; lappend temp $value; lset srclist $index $temp;
			return 1
		}
	}
}
proc read_from_file {file2read {splitchar "\n"} } {
	if {[file exist $file2read]} {
		set FileID [open $file2read r]
		set returnlist [read -nonewline $FileID]
		set returnlist [split $returnlist $splitchar]
		close $FileID
		return $returnlist
	} else {
		puts "No file found at $file2read .\n"
		return {}
	}
}
proc is_even {number} {
	if {[is_integer $number]} {
		if {[string is integer -strict $number]} {
			if {[expr ($number % 2) == 0]} {
				return 1
			} else {
				return 0
			}
		} else {
			return {}
		}
	} else {
		return {}
	}
}
proc is_number value {
    if {![catch {expr {abs($value)}}]} {
        return 1
    }
    set value [string trimleft $value 0]
    if {![catch {expr {abs($value)}}]} {
        return 1
    }
    return 0
}
proc is_integer {value} { regsub -all {\d} [join $value {}] {} nonintchar; return [expr ![llength $nonintchar] && [llength $value]] }
proc get_folders_from_path {filename_with_path} {
	if {[string match *\/* $filename_with_path]} {
		return [lindex [regexp -inline {^(.*)/} $filename_with_path] 1]
	} elseif {[string match *\\* $filename_with_path]} {
		return [lindex [regexp -inline {^(.*)\\} $filename_with_path] 1]
	}
}
proc insert_intolist_beforemarker {the_insertee thelist insertionmarker {options}} {
	if {![info exists thelist]} {
		return {}
	} elseif {![info exists the_insertee] || ![info exists insertionmarker]} {
		return $thelist
	} elseif {[llength [lsearch -inline $options $thelist [format "%s" $insertionmarker]]]} {
		set intermediate [join $the_insertee { }]
		set intermediate [split $intermediate {;}]
		set insertee {}
		foreach inter $intermediate { if {[llength $inter]} {lappend insertee [format "%s;" $inter]} }
		set insertion_site [lsearch $options $thelist [format "%s" $insertionmarker]]
		if {[expr $insertion_site > 0]} {
			return [concat [lrange $thelist 0 [expr $insertion_site-1]] $insertee [lrange $thelist $insertion_site end]]
		} else {
			return [concat $insertee $thelist]
		}
	} else {
		return $thelist
	}
}
proc insert_intolist_aftermarker {the_insertee thelist insertionmarker {options}} {
	if {![info exists thelist]} {
		return {}
	} elseif {![info exists the_insertee] || ![info exists insertionmarker]} {
		return $thelist
	} elseif {[llength [lsearch -inline $options $thelist [format "%s" $insertionmarker]]]} {
		set intermediate [join $the_insertee { }]
		set intermediate [split $intermediate {;}]
		set insertee {}
		foreach inter $intermediate { if {[llength $inter]} {lappend insertee [format "%s;" $inter]} }
		set insertion_site [lsearch $options $thelist [format "%s" $insertionmarker]]
		return [concat [lrange $thelist 0 $insertion_site] $insertee [lrange $thelist [expr $insertion_site+1] end]]
	} else {
		return $thelist
	}
}
proc mkdir_if_nonexist {dir_to_check} {upvar #0 msg msg;upvar myname myname;
	if {![llength $dir_to_check]} {lappend msg "$myname: tried to mkdir_if_nonexist with an empty name."; return 1
	} else {if {[file exists dir_to_check]} { return 2 } else { file mkdir $dir_to_check;return 0}}
}
proc grep {re args} {
    set files [eval glob -types f $args]
	set returnlist {}
    foreach file $files {
        set fp [open $file]
        while {[gets $fp line] >= 0} {
            if [regexp -- $re $line] {
                if {[llength $files] > 1} {puts -nonewline $file:}
                puts $line
				lappend returnlist $line
            }
        }
        close $fp
		return $returnlist
    }
}
proc standard_dc_write {modulename destfilename {command_and_options {write_file -format verilog -output}}} {
	mkdir_if_nonexist [get_folders_from_path $destfilename]
	return [eval $command_and_options $destfilename $modulename]
}
proc echo_list {srclist} {
	return [puts [join $srclist "\n"]]
}
## Not finished!

#proc sort_list {sourcelist {comparesymbol >}} {
#	set copylist $sourcelist
#	set returnlist [lindex $copylist 0]
#	foreach element $copylist {
#			set i 1
#			set returnlist [concat $element $returnlist]
#			set entrylength [llength $returnlist]
#			while {$i <= [llength $returnlist]} {
#				if {[expr $element $comparesymbol [lindex $returnlist $i]]} { 
#					set returnlist [concat [lrange $returnlist 0 [expr $i-1]] $element [lrange $returnlist [expr $i+1] end-1]]
#					break
#				}
#				incr i
#			}
#			if  {
#				puts "!!!!!!!!!!!!!Error: ran out of \$returnlist!!!!!!!!!!!!\n"
#				break
#			}
#	}
#	return $returnlist
#}
proc get_driving_pin {source_pins} {
	set returnlist {}
	foreach sourcepin $source_pins {
		if {[llength [get_object_name [get_pins $sourcepin]]} {
			lappend returnlist [get_object_name [get_pins -of_objects [get_nets -of_objects $sourcepin] -filter {@pin_direction==out}]]
		} else {
			puts "!! Error: $sourcepin is not a pin. !!\n"
			lappend returnlist {}
		}
	}
	return $returnlist
}
proc report_driving_pin {source_pins} {
	set returnlist {}
	foreach sourcepin $source_pins {
		if {[llength [get_object_name [get_pins $sourcepin]]} {
			redirect -variable netreport {report_net -connections []}
			lappend returnlist [get_object_name [get_pins -of_objects [get_nets -of_objects $sourcepin] -filter {@pin_direction==out}]]
		} else {
			puts "!! Error: $sourcepin is not a pin. !!\n"
			lappend returnlist {}
		}
	}
	return $returnlist
}
proc create_mut { {arguments {}} } {
	set msg {}
        set default_args [list sourcev {} tag2ports_hashlist [list din {} dout {} ] tag2direction_hashlist [list din in dout out] clkports clk rstports rst projname {} sessionid {} writepath {} timescale "1ns / 1ps" mutname {} sourcetoplevel {}]
        reset_array argument_array $default_args; foreach key [array names argument_array] { set $key $argument_array($key) }
        foreach key [array names argument_array] {
                if {[string match *list $key] && [is_array $argument_array($key)]} {
                        reset_array [string map { list array } $key] $argument_array($key)
                } else {
                        set $key $argument_array($key)
                }
        }
        if {[is_array $arguments]} { reset_array argument_array $arguments } elseif {[string match *help $arguments]} { puts "Usage:\n\t create_mut <arguments>\nDefault arguments:\n"; echo_array argument_array; return 1 } else { reset_array argument_array {}}
	upvar filename_hash filename_hash; upvar dftinfo_hash dftinfo_hash; upvar project_id project_id; upvar toplevelnetlist toplevelnetlist; upvar project_path project_path; upvar current_tool current_tool
        if {[info exists dftinfo_hash(clock_pin)]} { set clkports $dftinfo_hash(clock_pin); set sysclk [lindex $clkports 0] }
        if {[info exists dftinfo_hash(reset_pins)]} { set rstports $dftinfo_hash(reset_pins) }
	if {![llength $sessionid] && [info exists toplevelnetlist]} { set sessionid $toplevelnetlist}
	if {![llength $projname] && [info exists project_id]} { set projname $project_id }
	if {[info exists project_path] && [info exists project_id] && [info exists current_tool] && ![llength $writepath]} { set writepath [file normalize $project_path/$project_id/$current_tool] }
	mkdir_if_nonexist $writepath
	if {[llength $sourcetoplevel]} { 
		set necessary_conditions_true 1; set necessary_conditions {}; set necessary_files [list sourcev]; set necessary_lists [concat $necessary_files sourcetoplevel]
		foreach necessary_list $necessary_lists { lappend necessary_conditions [format "info exists %s" $necessary_list] }
		foreach necessary_file $necessary_files { lappend necessary_conditions [format "file exists %s" [join [subst [format "\$\{%s\}" $necessary_file]] ";file exists "] ] }
		foreach condition $necessary_conditions { if {![eval $condition]} { puts "Condition: $condition is not true.\n"; set necessary_conditions_true 0 } }
		if {[llength [get_object_name [get_designs -quiet $sourcetoplevel]]]} { remove_design $sourcetoplevel }
		foreach sourcefile $sourcev { read_file $sourcefile }
		if {[llength [get_object_name [get_designs -quiet $sourcetoplevel]]]} {
			current_design $sourcetoplevel
		} else { echo_list $msg > create_mut.msg; return 0 }
		if {!$necessary_conditions_true} { echo_list $msg > create_mut.msg; return 0 }
	} else {
		set sourcetoplevel [current_design_name] 
	}
	if {![llength $mutname]} { set mutname "mutof_${sourcetoplevel}_for_${sessionid}" }
	if {![llength $tag2ports_hasharray(din)]} { 
		set tag2ports_hasharray(din) [get_object_name [remove_from_collection [get_ports -filter {@port_direction==in}] [get_ports [concat $clkports $rstports] ] ] ]
	}
	if {![llength $tag2ports_hasharray(dout)]} { 
		set tag2ports_hasharray(dout) [get_object_name [get_ports -filter {@port_direction==out}]]
	}
        foreach key [array names argument_array] {
                if {[string match *list $key] && [is_array $argument_array($key)]} {
                        reset_array [string map { list array } $key] $argument_array($key)
                } else {
                        set $key $argument_array($key)
                }
        }
	puts "create_mut running parameters:\n"
	for { set i 0} {$i < [expr [llength $default_args]/2]} {incr i} { 
		if {[string match *list [lindex $default_args [expr $i*2]] ]} { 
			set key [string map {list array} [lindex $default_args [expr $i*2]] ] 
			echo [format "%s\t\t%s" $key [array get $key] ]
		} else { 
		set key [lindex $default_args [expr $i*2]]
		echo [format "%s\t\t%s" $key [subst [format "$%s" $key] ]] 
		}
	}
	#array set portswidth_hash {}
	if {[llength [get_object_name [get_designs -quiet $mutname]]]} {remove_design $mutname}
	create_design $mutname;	current_design $mutname; create_cell mut_inst $sourcetoplevel
	#array set ports_accountedfor_hash {}
	set mutinstports_accountedfor {}
	foreach tag [array names tag2ports_hasharray] {
		set direction $tag2direction_hasharray($tag)
		set portname $tag
		set portslist $tag2ports_hasharray($tag)
		#set portswidth_hash($portname) [llength $portslist] 
                if {[llength $portslist]} {
			for {set i 0} {$i < [llength $portslist]} {incr i} {
				set newport_name "${portname}\[$i\]"
                		quiet_eval_unless_error [format "create_port $newport_name -direction $direction"]
                                quiet_eval_unless_error [format "create_net $newport_name"]
		                quiet_eval_unless_error [format "connect_net \[get_nets $newport_name\] \[get_ports $newport_name\]"]
                		quiet_eval_unless_error [format "connect_net \[get_nets $newport_name\] \[get_pins mut_inst/%s\]" [lindex $portslist $i]]
                        }
		        create_bus [get_ports [format "%s\[*\]" $portname]] $portname -start [expr [llength $portslist]-1] -end 0
			set mutinstports_accountedfor [concat $mutinstports_accountedfor $portslist]
                }
	}
	set untagged_ports {}
	foreach mutinstport [get_object_name [get_pins -quiet mut_inst/*]] {
		set pushupport [string map {{mut_inst/} {}} $mutinstport]
		if {![llength [lsearch -inline $mutinstports_accountedfor [changeportnames_for_lsearch $pushupport]]]} {
			echo [format "\tPort:\t\t%s \n\tdoes not belong to any tag, will push them up to new mut level." $pushupport]
			create_port $pushupport -direction [get_attribute [get_pins -quiet $mutinstport] pin_direction]
			create_net $pushupport
			connect_net $pushupport [get_ports -quiet $pushupport]
			connect_net $pushupport [get_pins -quiet $mutinstport]
			lappend untagged_ports $pushupport
		}
	}
	set untagged_buses [get_unique_list [regsub -all {(\[|\]|\d)} $untagged_ports {}]]
	puts "Untagged buses: \n"
	echo_list $untagged_buses
	foreach potential_bus $untagged_buses {
		set ports_in_bus [lsearch -all -inline $untagged_ports [format "%s*" $potential_bus]]
		set buswidth [llength $ports_in_bus]
		if {[expr $buswidth > 1]} { create_bus [get_ports $ports_in_bus] $potential_bus -start [expr $buswidth-1] -end 0 }
	}
	if {[info exists filename_hash(dc_output)]} { mkdir_if_nonexist $filename_hash(dc_output); set targetfile $filename_hash(dc_output)/${mutname}.v } else { set targetfile ${mutname}.v }; puts "`timescale $timescale\n" > temp1.lis
	if {[file exists $targetfile]} { file delete $targetfile }
	if {[llength $sourcev]} {
		write_file -output temp.v -format verilog
		eval "sh cat temp1.lis temp.v [join $sourcev { }] >> $targetfile"
	} else {
		write_file -output temp.v -format verilog -hierarchy
		eval "sh cat temp1.lis temp.v >> $targetfile"
	}
	echo_list $msg > create_mut.msg
	current_design $sourcetoplevel; return $targetfile 
}
proc get_sublist_of {sourcelist {option {not_one_of}} taglist} {
	if {[llength $sourcelist]} {
		if {[string match not_one_of $option]} {
			return [lsearch -regexp -inline -all $sourcelist [format "(%s)" [join $taglist {|}]]]
		}
	} else {
		return {}
	}
}
proc echo_array {arraylist {redirection {}} {targetname {}} {sort_option {}} {sorted_key {}} } {
	upvar ansarray ansarray;reset_array ansarray {};
	if {[llength $arraylist] == 1} {
		upvar $arraylist thearray
	} elseif {[is_array $arraylist]} {
		array set thearray $arraylist
	} else {
		puts "not an array. usage: echo_array \[array get some_array\]\n"
		return 0
	}
	if {[llength [array names thearray]]} {
		array set ansarray [array get thearray];
		set maxkeylength 0
		foreach key [array names thearray] {
			set thiskeylength [llength [split $key {}]]
			set maxkeylength [expr ($maxkeylength>$thiskeylength)?$maxkeylength:$thiskeylength]
		}
		set spaces [string repeat { } [expr $maxkeylength-3]]
		if {[string match {file} $redirection] || [string match to_file $redirection] && [llength $targetname]} {
			## file|to for backward compatibility
			if {[file exists $targetname]} { file delete $targetname }
			set echoeval {echo $key$spaces\t$thearray($key) >> $targetname}
			set finaleval { puts "Written to $targetname .\n" }
		} elseif {[string match {variable} $redirection] || [string match {to_var} $redirection] && [llength $targetname]} {
			upvar $targetname $targetname; set temp {}
			set echoeval {lappend temp $key$spaces\t$thearray($key)}
			set finaleval {set $targetname [join [list [subst "\$$targetname"] [join $temp "\n"]] "\n"]};
		} elseif {[string match {*human*} $redirection]} {
			## normal redirect, but reduce too long values to their length
			set shortform {"-= a list with %s elements =-"}
			set echoeval {echo [format "%s%s\t%s" $key $spaces [expr {[set vlen [llength $thearray($key)]]>20?[format $shortform $vlen]:$thearray($key)}]]}
			set finaleval { puts "Done.\n" }
		} elseif {[llength $redirection] && ![llength $targetname]} {
			## assume $redirection is a form using %s
			set echoeval [format "echo \[format \"\$key\$spaces\t%s\" \[%s\]\]" {%s} [string map {%s $thearray($key)} $redirection]]
			set finaleval { puts "Done.\n" }
		} else {
			puts "Key$spaces\tValue\n"
			set echoeval {echo $key$spaces\t$thearray($key)}
			set finaleval { puts "Done.\n" }
		}
        set keys [array names thearray];if {[llength $sort_option]} {set keys [eval "lsort $sort_option {$keys}"]} elseif {[llength $sorted_key]} {set keys $sorted_key}
		foreach key $keys {
			set spaces [string repeat { } [expr $maxkeylength-[llength [split $key {}] ]  ]   ]
			eval $echoeval
		}
		eval $finaleval
		return 1
	} else {
		puts "Specified array is empty.\n"
		return 0
	}
}
proc echoarrays {arraylists {redirection {}} {targetname {}} } {
	set arraynames [formatlist [indsof $arraylists] {arr_%s} subst];set form {\$%s(%key)}
	for {set ind 0} {$ind<[llength $arraylists]} {incr ind} {set arraylist [lindex $arraylists $ind]
		if {[llength $arraylist] == 1} { upvar $arraylist [lindex $arraynames $ind];
		} elseif {[is_array $arraylist]} { array set [lindex $arraynames $ind] $arraylist
		} else { return -code error "Element $ind of first argument not an array. usage: echo_arrays \[list <arrayname|\[array get arrayname\]>\]\n"}
	}
	set keys {};foreach arrayname $arraynames { set keys [concat $keys [array names $arrayname]]}; set superkeyset [get_unique_list $keys]
	foreach key $superkeyset { foreach arrayname $arraynames {lappend ${arrayname}($key) {};
		set ${arrayname}($key) [lrange [subst "\$${arrayname}($key)"] 0 end-1]
		}
	};return [echo_array [createarrayfrom $superkeyset [format "\$%s%s" [join $arraynames {(%s) $}] (%s)] subst $arraynames]]
}
proc create_vcs_bash_for_coverage_analysis {{vc_or_verilog_list {}} {sim_library {~/lib/gtech_ver.v}} {procoption help} {cmoptions {}} {otherswitches {+v2k}} {outputfile {}} {reportfolder {}} {logfolder {}} } {
	if {[string match help $procoption]} { 
		puts "Usage:\n\tcreate_vcs_bash_for {\[filename of vc (source list)|source verilog list\]} \[verification_library\] \[help|using\] \[cm options: e.g., tgl+fsm+cond\] \[other switches, e.g. +define+SVA\] \[report folder to store reports\] \[log folder to store log\]\n\t(-sverilog will always be used) (-R will always be used) (not going to verify \"other switches\" for vcs)\n\t(Not going to check: whether vcs is ready to run on this system;)\n"
		return {}
	} else {
		set necessary_conditions_met 1
		if {([llength $vc_or_verilog_list] ==1) && [string match *.vc $vc_or_verilog_list] && [file exists $vc_or_verilog_list]} {
			set filelist "-f $vc_or_verilog_list"
		} elseif {[llength $vc_or_verilog_list]} {
			foreach verilog_file $vc_or_verilog_list {
				if {![file exists $verilog_file] || ![string match *.v $verilog_file]} {
					set necessary_conditions_met 0
					puts "$verilog_file is not a verilog file. If it is a vhdl file you need to use VCS MX.\n"
					break
				}
			}
			set filelist [join $vc_or_verilog_list { } ]
		}
		if {![file exists $sim_library] || ![string match *.v $sim_library]} { puts "$sim_library is an invalid simulation library.\n"; set necessary_conditions_met 0}
		if {$necessary_conditions_met} {
			#set other_vcs_switches [string map {+ { +}} [string map {- { -}} $otherswitches]]
			set other_vcs_switches $otherswitches
			set vcscmdform {vcs $osbitchoice -o $vcsoutputpath $filelist -v $sim_library -sverilog -R $other_vcs_switches -l $logfolder}
			set urgcmdform {urg $osbitchoice -dir $vcsoutputpath.vdb -metric $cmoptions -show legalonly -report $vcsoutputpath.rpt -format text }
			set dvecmdform {dve $osbitchoice -covdir $vcsoutputpath.vdb &}
			set supported_cmoptions [list tgl fsm line cond branch assert]
			set cmoptionslist [split $cmoptions {+-}]
			if {[llength [set cmoptionslist [join [lsearch -inline -all -regexp $cmoptionslist [format "(%s)" [join $supported_cmoptions {|} ]]] {+} ]]]} { 
					set other_vcs_switches [concat $other_vcs_switches -cm $cmoptionslist ] 
					set bash_commands [list $vcscmdform $urgcmdform $dvecmdform]
			} else {
					set bash_commands [list $vcscmdform]
				## if use concat, spaces within {...} will be interpreted as new elements in the list
			}
			mkdir_if_nonexist [get_folders_from_path $reportfolder]; mkdir_if_nonexist [get_folders_from_path $logfolder]
			set vcsoutputpath $reportfolder
		        if {[llength [regexp -inline x86_64 [sh uname -a]]]} { set osbitchoice -full64} else { set osbitchoice {} }
			if {![llength outputfile]} { set theoutputfile runvcs_for_coverage.bash } else { set theoutputfile $outputfile}
			if {[file exists $theoutputfile]} { file delete $theoutputfile }
			foreach bash_command $bash_commands {	echo [subst $bash_command] >> $theoutputfile } 
			puts "vcs commands written to $theoutputfile . Recommend running it outside tcl environment.\n"
			return $theoutputfile
		} else { return {} }
	}
}
proc get_ith_element_of_list {srclist {i 2} {offset 0} } {
	if {[llength $srclist]>=$i} {
		set returnlist {}
		for {set index 0} {$index <= [expr ceil([llength $srclist]/$i)]} {incr index} {
			lappend returnlist [lindex $srclist [expr ($index+1)*$i-1+$offset]]
		}
		return [get_nonempty_list $returnlist]
	} else {
		puts "Given list has less elements than \$i=$i.\n"
		return {}
	}
}
proc changeportnames_for_lsearch {portname} {
	return [string map {{[} {\[}} [string map {{]} {\]}} $portname ] ] 
}
proc read_listfile_into_hash {listfile {separatorchar { }}} {
	## assume the file is in a 1 2 3 4\nb 5 6 7 8\n... format
	## return a list in [list a {1 2 3 4} b {5 6 7 8}] format, which can then be used in array set arrayname $returnlist
	## does not perform: squeezing out zeros, changeportnames, removing colons, etc.
	if {[file exists $listfile]} {
		array set returnhash {}
		set filelines [read_from_file $listfile]
		foreach fileline $filelines {
			set currentline [split $fileline $separatorchar]
			if {[llength $currentline] > 1} {
				set returnhash([lindex $currentline 0]) [lrange $currentline 1 end]
			} else {
				set returnhash([lindex $currentline 0]) {}
			}
		}
		return [array get returnhash]
	} else {
		puts "$listfile doesn't exist.\n"
		return {}
	}
}
proc sync_files {{sync_toc ~/Dropbox/inf/toc.inf} {keep_empty remove_empty} {separatorchar { }}} {
	if {[file exists $sync_toc]} { puts "Syncing files according to ToC file $sync_toc :";
		set sync_toc_fullname [file normalize $sync_toc];
		set sync_toc_fullpath [file dirname $sync_toc_fullname];set sync_toc_ownname [file tail $sync_toc_fullname];
		array set toc_array [load_array_from $sync_toc $keep_empty $separatorchar];
		set return_var {}
		foreach file_ownname [array names toc_array] {if {[file exists [set file_fullname [file join a b $sync_toc_fullpath $file_ownname]]]} {
				mkdir_if_nonexist [file dirname $toc_array($file_ownname)]
				lappend return_var [catch [format "sh ln -s $file_fullname $toc_array($file_ownname)"]]
			} else {puts "\t$file_fullname does not exist; skip syncing it.";lappend return_var {}
			}
		}
		return $return_var
	} else {
		return -errorcode 0 -errorinfo "\$sync_toc= $sync_toc has to exist. Usage:\n\tsync_files <sync_toc=~/Dropbox/inf/toc.inf> <keep_empty=remove_empty> <separatorchar={ }>\n"
	}
}
proc save_array_to { arraylist {targetfilename savedarray.inf} {sync_toc do_not_sync} {keep_empty remove_empty} {separatorchar { }} } {
	set usageinfo [join [list "First parameter needs be array. Usage:" \
		"\tsave_array_to \[array get array_name\] <targetfilename=savedarray.inf> <sync_toc=~/Dropbox/inf/toc.inf> <separatorchar={ }>"] "\n"]
	if {[llength $arraylist] == 1} {upvar $arraylist thearray;
	} elseif {[is_array $arraylist]} { array set thearray $arraylist
	} else { return -errorcode 0 -errorinfo $usageinfo
	}
	file delete -force $targetfilename; puts "Now purge $targetfilename .\n";## file exists return 0 for invalid symlink; remove to be sure
	echo_array thearray to_file $targetfilename
	set return_var 1
	if {[file exists $sync_toc]} { set targetfile_fullname [file normalize $targetfilename];
		set targetfile_fullpath [file dirname $targetfile_fullname];set targetfile_ownname [file tail $targetfile_fullname];
		set sync_toc_fullname [file normalize $sync_toc];
		set sync_toc_fullpath [file dirname $sync_toc_fullname];set sync_toc_ownname [file tail $sync_toc_fullname];
		set targetfile_syncdirfullname [file join a b $sync_toc_fullpath $targetfile_ownname]
		file delete -force $targetfile_syncdirfullname;	puts "Now purge $targetfile_syncdirfullname (simlink in sync dir).\n"
		set uplink_success [catch [format "sh ln -s $targetfile_fullname $targetfile_syncdirfullname"]];
		lappend return_var $uplink_success
		array set toc_array [load_array_from $sync_toc $keep_empty $separatorchar];set toc_array($targetfile_ownname) $targetfile_fullpath
		set tocupdate_success [catch "save_array_to toc_array $sync_toc do_not_sync $keep_empty $separatorchar"];
		lappend return_var $tocupdate_success
	}
	return $return_var
}
proc load_array_from { {targetfilename savedarray.inf} { keep_empty remove_empty} {separatorchar {\s} } } {
	if {[file exists $targetfilename]} {
		if {[string match remove_empty $keep_empty]} { 
			set existingeval {set returnhash([lindex $currentline 0]) [lsearch -inline -regexp -all [lrange $currentline 1 end] {.+}]} 
		} else {
			set existingeval {set returnhash([lindex $currentline 0]) [lrange $currentline 1 end] }
		} 
		array set returnhash {}
		set filelines [read_from_file $targetfilename]
		set line_nr 0; set total_lines [llength $filelines]
		while {$line_nr < $total_lines} {
			set currentline [lindex $filelines $line_nr] 
			#set currentline [split [regsub -all $separatorchar [lindex $filelines $line_nr] ,] ,]
			if {[llength $currentline] > 1} {
				eval $existingeval
			} else {
				set returnhash([lindex $currentline 0]) {}
			}
			incr line_nr
		}
		return [array get returnhash]
	} else {
		puts "$targetfilename doesn't exist.\n"
		return {}
	}
}
proc which_pins_are_connected_to {pin_search_key {quiet {}} } {
	if {[llength [set centerpins [get_object_name [get_pins -quiet $pin_search_key]]]]} {
		set echolist {}
		set connected_cells {}
		foreach centerpin $centerpins {
				set centerpin_direction [get_attribute [get_pins $centerpin] pin_direction]
				if {[string match in $centerpin_direction]} {
					set pindirection2lookfor out
					set directionsymbols [list < -]
				} elseif {[string match out $centerpin_direction]} {
					set pindirection2lookfor in
					set directionsymbols [list - >]
				} elseif {[string match inout $centerpin_direction]} {
					set pindirection2lookfor inout
					set directionsymbols [list < >]
				} else {
					puts "direction of pin $centerpin is $centerpin_direction , which is unrecognizable.\n"
					return {}
				}
				set connectednet [get_object_name [get_nets -quiet -of_objects $centerpin]]
				##connected pins
				set connectedobjects [get_object_name [get_pins -quiet -of_objects [get_nets $connectednet] -filter "@pin_direction==$pindirection2lookfor"]]
				set connected_cells [concat $connected_cells $connectedobjects]
				##connected ports
				set connectedobjects [concat $connectedobjects [formatlist [get_object_name [get_ports -quiet -of_objects [get_nets $connectednet] -filter "@pin_direction==$centerpin_direction"]] "%s (port)"]]
				lappend echolist [format "$centerpin%s--$connectednet--%s%s" [lindex $directionsymbols 0] [lindex $directionsymbols 1] [lindex $connectedobjects 0]]
				if {[llength $connectedobjects] > 1 } {
					set connectedobjects [lrange $connectedobjects 1 end ]
					set tabspaces [regsub -all {.} "$centerpin---$connectednet" { }]
					foreach connectedobject $connectedobjects {
						lappend echolist [format "$tabspaces--%s%s" [lindex $directionsymbols 1] $connectedobject]
					}
				}
		}
		if {![string match *quiet $quiet]} {echo_list $echolist}
		return [get_unique_list $connected_cells]
	} else {
		echo [format "No cell found that matches $cellname in current design %s ." [current_design_name]]
	}
}
proc which_cells_are_connected_to {cell_search_key {direction {in out inout}} {quiet {}} } {
	if {[llength $quiet]} {set responsiveness {-quiet }} else { set responsiveness {}}
	set returnlist {}
	set cells [get_object_name [get_cells $cell_search_key]]
	foreach cell $cells {
		if {![string match *quiet $quiet]} {
			puts "---------------------------------------------------------\n"
			puts "For $cell:\n"
		}
		set pinslist_of_this_cell {}
		foreach possibledirection [list in out inout] { if {[llength [lsearch -exact -inline $direction $possibledirection]]} { set pinslist_of_this_cell [concat $pinslist_of_this_cell [get_object_name [get_pins -quiet -of_objects $cell -filter "@pin_direction==$possibledirection"]]] } }
		lappend returnlist [eval [format "get_object_name \[ get_cells $responsiveness-of_objects \[get_pins $responsiveness%s\]\]" [join [which_pins_are_connected_to $pinslist_of_this_cell $quiet] " "]]]
	}
	return [get_unique_list $returnlist]
}
proc foreach_regsub { {arg1 {}} {arg2 {}} {arg3 {}} {arg4 {}} {arg5 {}} {arg6 {}} {arg7 {}} {arg8 {}} {arg9 {}} {arg10 {}} {arg11 {}} } {
	set arguments [list $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8 $arg9 $arg10 $arg11]
	set switches -all
	set non_switch_arguments {}
	foreach argument $arguments {
		if {[llength [regexp -inline {^-(expanded|line|linestop|lineanchor|nocase|start|-)$} $argument]]} { 
			lappend switches $argument 
		} elseif {[llength [regexp -inline {^-all$} $argument]]} {
		} else { lappend non_switch_arguments $argument }
	}
	set returnlist {}
	if {[llength $non_switch_arguments] >= 3} {
		set search_pattern [lindex $non_switch_arguments 0]
		set sourcelist [lindex $non_switch_arguments 1]
		set replace_pattern [lindex $non_switch_arguments 2]
		foreach source_element $sourcelist {
			lappend returnlist [eval [format "regsub %s \{$search_pattern\} \$source_element \{$replace_pattern\}" $switches]]
		}
	}
	return $returnlist
}
proc get_reports_of_cell_buses {cell_search_key {quiet {}}} {
	set buses [get_unique_list [foreach_regsub -all {\[\d+\]} [get_object_name [get_pins -of_objects [get_cells -quiet $cell_search_key] ]  ] {}   ]    ]
	array set reports_of_buses {}
	set reports_of_buses(title) [list Direction "Width\t" "MSB\t" "LSB\t"]
	if {![string match *quiet $quiet]} {
		echo [format "%s\t\t\t%s" {Bus Name} [join $reports_of_buses(title) "\t"]]
		foreach bus $buses {
			set pins [get_object_name [get_pins $bus] ]
			set numbers [lrange [split [regsub -all {.+?\[(\d+)\]} [join $pins " "] { \1}] { }] 1 end]
			set reports_of_buses($bus) [get_unique_list [get_attribute [get_pins $pins] pin_direction ]  ]
			lappend reports_of_buses($bus) [llength $pins]
			lappend reports_of_buses($bus) [get_extremes_of_list $numbers >] 
			lappend reports_of_buses($bus) [get_extremes_of_list $numbers <] 
			## below: 24=3*8=number of spaces in 3 tabs; 8 is number of chars in "Bus Name"
			set spaces [string repeat " " [expr 24+8-[llength [split $bus {}] ]  ]   ]
			echo [format "%s%s%s" $bus $spaces [join $reports_of_buses($bus) "\t\t"]]
		}
	} else {
		                foreach bus $buses {
                        set pins [get_object_name [get_pins $bus] ]
                        set numbers [lrange [split [regsub -all {.+?\[(\d+)\]} [join $pins " "] { \1}] { }] 1 end]
			set reports_of_buses($bus) [get_unique_list [get_attribute [get_pins $pins] pin_direction ]  ]
                        lappend reports_of_buses($bus) [llength $pins]
                        lappend reports_of_buses($bus) [get_extremes_of_list $numbers >] 
                        lappend reports_of_buses($bus) [get_extremes_of_list $numbers <]
		}
	}
	return [array get reports_of_buses]
}
## A more generic version than [array names arrayname]. This accepts searches based on expressions.
proc get_keys_of_array {arraylist {searchmethod {satisfying expr 1 == 1}} } {
	array set source_array $arraylist
	set keys_pool [array names source_array]
	set returnlist {}
	if {[regexp {satisfying (.*)} $searchmethod {} expression]} {
		set expression [string map {%key "\$key_from_pool"} [string map {%value "\$source_array(\$key_from_pool)"} $expression]]
		foreach key_from_pool $keys_pool {
			if {[eval $expression]} { lappend returnlist $key_from_pool	}
		}
	}
	return $returnlist
}
proc get_values_of_array { arrayname {keylist {}} } {
	set returnlist {}
	if {[llength $arrayname] == 1} {
		upvar $arrayname thearray
	} elseif {[is_array $arrayname]} {
		array set thearray $arrayname
	}
	if {[array exists thearray]} {
		#Default: all values
		if {![llength $keylist]} { set keylist [array names thearray]}
		foreach key $keylist {
			if {[info exists thearray($key)]} { lappend returnlist $thearray($key) }
		}
	}
	return $returnlist
}
proc get_bus_of_pin {pin_search_key {get_pin_command get_pins} {lookfor {\[\d+\]}} {replacewith {}}} {
	if {[llength [set pinslist [get_object_name [$get_pin_command $pin_search_key] ]  ]    ]} {
		return [get_unique_list [foreach_regsub -all $lookfor $pinslist $replacewith] ]
	} else {
		return {}
	}
}
proc get_bus_of_port {pin_search_key {get_pin_command get_ports} {lookfor {\[\d+\]}} {replacewith {}}} {
	if {[llength [set pinslist [get_object_name [$get_pin_command $pin_search_key] ]  ]    ]} {
		return [get_unique_list [foreach_regsub -all $lookfor $pinslist $replacewith] ]
	} else {
		return {}
	}
}
#Not finished!
proc read_all_referenced_modules {topmodule_name source_search_path {logfilename sourcelocations.log} {readlog n} {sourcetype verilog}} {
	if {[file isdirectory $source_search_path]} {
		set no_source_unreadable 1
		set no_source_unreachable 1
		set module2find_list $topmodule_name
#		while {[llength $module2find_list] && $no_source_unreachable && $no_source_unreadable} {}
			set module2find [lindex $module2find_list 0]
#			set module2find_list [lrange $module2find_list 1 end]
#			if {[llength [set toprtl_successfully_read [read_file -format $sourcetype ]]]} {}
	} else {
		puts "2nd argument: $source_search_path is not a path.\nUsage: read_all_referenced_modules toplevel_module_name path_to_search_referenced_modules <logfile_for_restore_session> <read_logfile=y/n> <sourcetype=verilog/vhdl>\n"
	}
}
proc reverse_array {arraylist} {
	if {[llength $arraylist] == 1} {
		upvar $arraylist thearray
	} elseif {[is_array $arraylist]} {
		reset_array thearray $arraylist
	}
	if {[llength [array names thearray]]} {
		reset_array returnarray {}
		foreach key [array names thearray] { set values $thearray($key); foreach value $values { lappend returnarray($value) $key } }
		return [array get returnarray]
	} else {
		puts "Inappopriate input. Usage: reverse_array {arrayname|\[array get arrayname\]}\n"
		return {}
	}
}
proc get_flattened_list {inlist} {;## does not preserve list order
	set returnlist {}
	set tempstorage {}
	set inlist_index 0
	while {$inlist_index < [llength $inlist]} {
## If [lindex $inlist $inlist_index] is not processed yet
		set tempstorage [lindex $inlist $inlist_index]
		set tempstorage_index [expr [llength [lindex $inlist $inlist_index] ]-1]
		incr inlist_index
## If there are still fragments to be processed
		while {[llength $tempstorage]} {
## If current fragment is a list in itself
			if {[llength [lindex $tempstorage $tempstorage_index]]>1} {
				set lastbit [lindex [lindex $tempstorage $tempstorage_index] end]
				lset tempstorage $tempstorage_index [lrange [lindex $tempstorage $tempstorage_index] 0 end-1]
				lappend tempstorage $lastbit
				incr tempstorage_index
## Or if current fragment is only a string
			} else {
				## regsub to remove multi-level lists like {{{{1}}}}
				lappend returnlist [regsub {^{*([^{}]*)}*$} [lindex $tempstorage $tempstorage_index] {\1}]
## If [lindex $inlist $inlist_index] still has at least one fragment to process
				if {$tempstorage_index} {
					incr tempstorage_index -1
					set tempstorage [lrange $tempstorage 0 $tempstorage_index]
				} else {
					set tempstorage {}
				}
			}
		}
	}
	return $returnlist
}
proc is_array {arraylist} {
	if {[is_even [llength $arraylist] ] && [llength $arraylist] > 0} {
		return 1
	} else {
		return 0
	}
}
proc format_array_keys {arraylist {keyform %key} {method subst} {upvarlist {}}} {
	array set returnarray {};if {[llength $upvarlist]} { foreach var2up $upvarlist { upvar $var2up $var2up } }
        if {[llength $arraylist] == 1} { upvar $arraylist originalarray } elseif {[is_array $arraylist]} { array set originalarray $arraylist
	} else {puts "not an array. usage: format_array \[array get some_array|arrayname\] {elementformat e.g. %value} {method e.g. eval}\n";return 0 }
	if {[llength [array get originalarray]]} {
		set translatedkeyform [string map {%value $originalarray($key)} [string map {%key $key} $keyform] ]
		foreach key [array names originalarray] {
			set thisform [subst $translatedkeyform]
			set returnarray([$method $thisform]) $originalarray($key)
		}
	} else {
		puts "Inappopriate input. Usage: format_array_keys <array name|\[array get arrayname\]> <keyform:%key|%value> <method> <upvarlist>"
	}
	return [array get returnarray]
}
proc sub_list {sourcelist {keyform {%s}} {method eval} {upvarlist {}}} {
	foreach upvaritem $upvarlist { upvar $upvaritem $upvaritem }
	set translatedelementform [string map {%s $element} $keyform]; set returnlist {}
	foreach element $sourcelist { 
		set thiselementform [subst $translatedelementform]; 
		#set thiselementform $translatedelementform
	if {[$method $thiselementform]} { lappend returnlist $element }}
	return $returnlist 
}
proc sublist_byindices {sourcelist indices} {
	set returnlist {};set indices [get_nonempty_list $indices]
	foreach index $indices { 
		lappend returnlist [lindex $sourcelist $index]
	}
	return $returnlist
}
proc sbi {sourcelist indices} {return [sublist_byindices $sourcelist $indices]}
proc sub_array {arraylist {keyform {%key}} {method eval}} {
	if {[llength $arraylist] == 1} { upvar $arraylist originalarray
	} elseif {[is_array $arraylist]} { array set originalarray $arraylist
	} else { return -code error [format "$arraylist not an array. Usage: sub_array {\[array get arrayname\]|arrayname} <keyform:%s>" {%key|%value|\$originalarray(string)}]
	};array set returnarray {};set translatedkeyform [string map {%value $originalarray($key)} [string map {%key $key} $keyform] ]
	foreach key [array names originalarray] {set thiskeyform $translatedkeyform;if {[$method $thiskeyform]} {set returnarray($key) $originalarray($key)}
	}
	return [array get returnarray]
}
proc sub_array_v2 {arraylist {keyform {%key}} {method eval}} {
	if {[llength $arraylist] == 1} { upvar $arraylist originalarray
	} elseif {[is_array $arraylist]} { array set originalarray $arraylist
	} else { return -code error [format "$arraylist not an array. Usage: sub_array {\[array get arrayname\]|arrayname} <keyform:%s>" {%key|%value|\$originalarray(string)}]
	};array set returnarray {};set translatedkeyform [string map {%value $originalarray($key)} [string map {%key $key} $keyform] ]
	foreach key [array names originalarray] {set thiskeyform [subst $translatedkeyform];
        if {[$method $thiskeyform]} {set returnarray($key) $originalarray($key)}
	}
	return [array get returnarray]
}
proc subarray_by_keys { arraylist {keylist {}} {verify verifykey}} {
	if {[llength $arraylist] == 1} { upvar $arraylist original_array
	} elseif {[is_array $arraylist]} { array set original_array $arraylist
	} else { return -code error [format "$arraylist not an array. Usage: subarray_by_keys {\[array get arrayname\]|arrayname} \$keylist"]
	}
	array set returnarray {}; set availablekeys $keylist
    if {[string match verifykey $verify]} {set availablekeys [remove_from_list -intersect [array names original_array] $keylist]}
	foreach key $availablekeys {set returnarray($key) $original_array($key) }
	return [array get returnarray]
}
proc get_net2cell2net {netname {verbose no} {forcedirection {}} {go_offpath 0} {off_path_eval {expr ([regexp [format ".*%s.*" (DFF|FD)] $ref] && ![string match D $otherpin])} }  } {
	set returnlist {}
	if {[llength [get_object_name [get_nets $netname] ]  ]} {
		set pins [get_object_name [get_pins -of_objects $netname] ]
		if {[llength [set peculiarpins [lsearch -regexp -inline -all $pins {.*\/\*\*logic_[01]\*\*}] ]  ]} {
			set pins [lsearch -regexp -inline -all -not $pins {.*\/\*\*logic_[01]\*\*}]
			puts "$netname is found to connect to $peculiarpins . This might indicate lack of driver. Going to remove and suppress it.\n"
		}
		if {[string match verbose $verbose]} {	puts "Starting from $netname:\n" }
		foreach pin $pins {
			if {[llength $forcedirection]} {
				set direction $forcedirection
			} else {
				set direction [get_attribute [get_pins $pin] pin_direction ]
			}  
			set cell [get_object_name [get_cells -of_objects $pin] ]
			set ref [get_object_name [get_lib_cells -of_objects $cell] ]
			## for verbose message
			set pinspace [string repeat { } [llength [split "->$pin" {}] ]  ]

			if {[string match in $direction]} {
				set otherdirection out
			} elseif {[string match out $direction]} {
				set otherdirection in
			} elseif {[string match inout $direction]} {
				set otherdirection inout
			} else {
				puts "direction = $direction != (in|out|inout). Something went wrong for $pin .\n"
			}
			set otherpins [get_object_name [remove_from_collection [get_pins -of_objects $cell -filter "@pin_direction==$otherdirection"] [get_pins $pin] ]]
			set otherpin2othernet {}
			foreach otherpin $otherpins {
				if {$go_offpath || ![eval $off_path_eval]} {
					foreach foundnet [get_object_name [get_nets -of_objects $otherpin] ] { lappend returnlist $foundnet }
					lappend otherpin2othernet [format "$otherpin->%s" [get_object_name [get_nets -of_objects $otherpin] ] ]
				}
			}
			if {[string match verbose $verbose]} {
				echo [format "->$pin->%s" [join $otherpin2othernet "\n$pinspace->"] ]
			}
		}
	} else {
		echo [format "Erroneous input. Usage: get_net2cell2net netname <stopatdff=y|n> <dffpattern=(DFF|FD)>"]
	}
	return $returnlist
}
proc logcmdwith {varname cmd} {
	upvar $varname var
	redirect -variable temp $cmd
	lappend var $temp
}
proc create_port_if_havent_already {arg0 arg1 arg2 {otherargs2pass {}}} {
	set args [list $arg0 $arg1 $arg2] 
	for {set i 0} { $i < 2 } {incr i} {
		set arg [lindex $args $i]
		if {[string match -direction $arg]} {
			set direction [lindex $args [expr $i+1]]
			set portname [lindex $args [expr ($i==0)?2:0]]
		}
	}
	if {[info exists direction] && [info exists portname]} {
		if {![sizeof_collection [get_ports -quiet $portname]]} {
			eval [format "create_port -direction %s %s%s" $direction $portname $otherargs2pass]
		}
	}
}
proc create_net_if_havent_already {netname} {
	set successful 0
	if {![sizeof_collection [get_nets -quiet $netname]]} {
		set successful [create_net $netname]
	}
	return $successful
}
proc extract_partlogic_by_cells {cellnames {targetdesignname {}} {usethisdesign {}} {create_backup_port_for_internal_connections 0} {cmdlogfilename cmdlog}} {
	array set structure_array {}; array set return_array {};
        if {[llength $usethisdesign]} {
                if {[llength [get_object_name [get_designs $usethisdesign] ]]} {
                        current_design $usethisdesign
                } else {
                        puts "Error in input. Usage: propagate_tags_basedon_name \[array get array_of_tags_indexed_by_pinnames\] <{}=use current design|{design_name}=use design specified with design_name>\n"
                        return {}
                }
        } else {
                echo [format "Assuming current design (%s) is the design to propagate to. User should make sure this is true." [current_design_name] ]
        }
	foreach_in_collection foundcell [get_cells $cellnames] { lappend structure_array([get_object_name $foundcell]) [format "if {!\[sizeof_collection \[get_cells -quiet %s\]\]} {create_cell %s %s}" [get_object_name $foundcell] [get_object_name $foundcell] [get_attribute $foundcell ref_name] ] }
	set foundcells [get_cells $cellnames]
	set foundcellnames [get_object_name $foundcells]
	set regexp_foundcellnames [format "(%s)" [join $foundcellnames {|}]]
	echo [format "Requested cells:\t%s\nFound cells:\t%s\n" [join $cellnames {,}] [join $foundcellnames {,}] ]
	set pins [get_pins -of_objects $foundcells -filter {@pin_direction==out}]
	append_to_collection -unique pins [get_pins -of_objects $foundcells -filter {@pin_direction==inout}]
	append_to_collection -unique pins [get_pins -of_objects $foundcells -filter {@pin_direction==in}]
	foreach_in_collection pin $pins {
		set connected_net [get_nets -of_objects $pin]
		## If either 1) exists externally connected pin; or 2) told to create regardless, create pushup ports
		if {$create_backup_port_for_internal_connections || [sizeof_collection [remove_from_collection $pins_on_the_other_side $internal_pins] ]} {
			set pushupportname [string map [list [format "%s/" [get_object_name $foundcell]] [format "%s_" [get_object_name $foundcell]] ] [get_object_name $pin]]
			set return_array($targetdesignname/$pushupportname) [get_object_name $pin]
			if {[sizeof_collection $connected_net]} {
				set thisnetname [get_object_name $connected_net]
				lappend structure_array([get_object_name $pin]) [format "if \{!\[sizeof_collection \[get_ports -quiet -of_objects \[get_nets -quiet %s\] \]\]\} \{create_port -direction %s %s; connect_net \[get_nets %s\] \[get_ports %s\]\}" $thisnetname [get_attribute $pin pin_direction] $pushupportname $thisnetname $pushupportname]
			} else {
				set thisnetname $pushupportname
				lappend structure_array([get_object_name $pin]) [format "create_net_if_havent_already %s" $thisnetname]
				lappend structure_array([get_object_name $pin]) [format "create_port_if_havent_already -direction %s %s" [get_attribute $pin pin_direction] $pushupportname]
				lappend structure_array([get_object_name $pin]) [format "connect_net_if_havent_already \[get_nets %s\] \[get_pins %s\]" $thisnetname [get_object_name $pin]]
				lappend structure_array([get_object_name $pin]) [format "connect_net_if_havent_already \[get_nets %s\] \[get_ports %s\]" $thisnetname $pushupportname]
			}
		}
		## Connect found net to this pin of this cell
		if {[sizeof_collection $connected_net]} {
			append_to_collection -unique nets $connected_net
			if {![info exists structure_array([get_object_name $connected_net])]} {
				lappend structure_array([get_object_name $connected_net]) [format "create_net_if_havent_already %s" $thisnetname] 
			}
			lappend structure_array([get_object_name $pin]) [format "connect_net_if_havent_already \[get_nets %s\] \[get_pins %s\]" [get_object_name $connected_net] [get_object_name $pin]]
		}
	}
	## If constant driver exists on the connected net
	foreach_in_collection net $nets {
		set const_drivers [get_const_drivers_of_netname [get_object_name $net]]
		foreach_in_collection const_driver $const_drivers { 
			lappend structure_array([get_object_name $net]) [format "create_cell %s -logic %s" [get_object_name $const_drivers] [get_attribute $const_driver ref_name] ] 
			lappend structure_array([get_object_name $net]) [format "connect_net_if_havent_already \[get_nets %s\] \[get_pins -of_objects %s\]" [get_object_name $net] [get_object_name $const_driver] ]
		}
	}
	if {[llength $targetdesignname]} {
		if {[sizeof_collection [get_designs -quiet $targetdesignname]]} { remove_design $targetdesignname }
		create_design $targetdesignname
		set netsname [get_object_name $nets]
		set pinsname [get_object_name $pins]
		current_design $targetdesignname
		set creationlog {}
		foreach cellname $foundcellnames { foreach value $structure_array($cellname) { redirect -variable temp {eval $value}; lappend creationlog [format "\t----%s----" $value]; lappend creationlog $temp } }
		foreach netname $netsname { foreach value $structure_array($netname) { redirect -variable temp {eval $value}; lappend creationlog [format "\t----%s----" $value]; lappend creationlog $temp } }
		foreach pinname $pinsname { foreach value $structure_array($pinname) { redirect -variable temp {eval $value}; lappend creationlog [format "\t----%s----" $value]; lappend creationlog $temp } }
		echo_list $creationlog > $cmdlogfilename
		puts "Check $cmdlogfilename for cell creation details.\n"
		save_array_to [array get structure_array] $targetdesignname.inf
	} else {
		puts "No target design name specified. To have it created, eval each element in values of each key in returned array.\n"
		save_array_to [array get structure_array] extract_partlogic_by_cells.inf
	}
	return [array get return_array]
}
proc get_const_drivers_of_netname {{netname {}}} {
	if {[sizeof_collection [get_nets $netname]]} {
		return [filter [get_cells -of_objects [get_pins -of_objects [get_nets $netname]]] -regexp {@ref_name =~ "\*\*logic_[01]\*\*"}]
	} else {
		return [filter [get_cells] -regexp {@ref_name =~ "\*\*logic_[01]\*\*"}]
	}
}
proc get_leaf_name {entityname} {
	return [lindex [split $entityname {/}] end]
}
proc connect_net_if_havent_already {netinput pin_or_portinput} {
	set connected 0
	if {[sizeof_collection [get_nets -quiet $netinput] ] && ([sizeof_collection [get_pins -quiet $pin_or_portinput]] || [sizeof_collection [get_ports -quiet $pin_or_portinput]])} {
		set net [get_nets -quiet $netinput]
		set all_provided_candidates [add_to_collection [get_pins -quiet $pin_or_portinput] [get_ports -quiet $pin_or_portinput]]
		set yet_to_connect_pin_or_port [remove_from_collection $pin_or_portinput [all_connected $net]]
		if {[sizeof_collection $yet_to_connect_pin_or_port]} {  set connected [connect_net $net $yet_to_connect_pin_or_port] } else {echo [format "%s already connected to %s, skipping." [get_object_name $net] [join [get_object_name $pin_or_portinput] { AND }] ]; set connected 1 }
	} else {
		echo [format "Error in input: %s needs be net, or %s needs be pin or port collection." [get_object_name $netinput] [get_object_name $pin_or_portinput] ]
	}
	return $connected
}
## Only checks if second collection has only one element
proc belongs_to_collection {collection_a collection_b} {
	if {([sizeof_collection $collection_b]==1)} {
		set returnvalue 0
		if {[sizeof_collection $collection_a] == [sizeof_collection [remove_from_collection $collection_a $collection_b]]} { return 0} else {return 1}
	} else {
		return -1
	}
}
proc number_of_alist_element_of_blist {alist blist} {
	set returnnumber 0
	foreach element $alist {
		if {[llength [lsearch -inline -exact $blist $element]]} { incr returnnumber }
	}
	return $returnnumber
}
proc get_these_elements_from_list {elementlist sourcelist {options qualifying}} {
	set regexpform [changeportnames_for_lsearch [format "(%s)" [join $elementlist {|}]]]
	if {[string match *all* $options]} {
		set regexpform [format ".*%s.*" $regexpform]
	}	
	if {[string match *qualifying* $options]} {
		return [lsearch -all -inline -regexp $sourcelist $regexpform]
	} elseif {[string match *inverse* $options]} {
		return [lsearch -all -inline -not -regexp $sourcelist $regexpform]
	} else {
		puts "Usage: get_these_elements_from_list \$list_of_elements \$list_to_search_from <options>=qualifying(return elements that matches)|inverse(return elements that does not match)\n"
		return {}
	}
	
}
proc make_buses_for_design {{usethisdesign {}} {sortmethod descending} {infologfile cmdlog}} {
	set infologs {}
        if {[llength $usethisdesign]} {
                if {[llength [get_object_name [get_designs $usethisdesign] ]]} {
                        current_design $usethisdesign
                } else {
                        puts "Error in input. Usage: propagate_tags_basedon_name \[array get array_of_tags_indexed_by_pinnames\] <{}=use current design|{design_name}=use design specified with design_name>\n"
                        return {}
                }
        } else {
                echo [format "Assuming current design (%s) is the design to make bus for. User should make sure this is true." [current_design_name] ]
        }
	redirect -variable temp {report_bus}; 
	if {![llength [lsearch -exact -inline [split $temp "\n"] {This design has no bussed ports.} ]] || ![llength [lsearch -exact -inline [split $temp "\n"] {This design has no bussed nets.}]]} {
		puts "Existing buses are removed.\n"; remove_bus *
	}
	set potentialportbuses [get_unique_list [foreach_regsub -all {(.*?)\[\d+\](.*?)} [get_object_name [get_ports *[*]*]] {\1<number>\2}]]
	set potentialnetbuses [remove_from_list [get_unique_list [foreach_regsub -all {(.*?)\[\d+\](.*?)} [get_object_name [get_nets *[*]*]] {\1<number>\2}]] $potentialportbuses]
	array set bustype_hash {}; foreach potentialportbus $potentialportbuses { set bustype_hash($potentialportbus) port }; foreach potentialnetbus $potentialnetbuses { set bustype_hash($potentialnetbus) net }
	if {[string match ascending $sortmethod]} { set sortcmd {-start [lindex $numbers 0] -end [lindex $numbers end] -sort} } else { set sortcmd {-start [lindex $numbers end] -end [lindex $numbers 0]}}
	set unbused_ports [get_ports]; set unbused_nets [get_nets]
	foreach potentialbus [array names bustype_hash] {
		if {[string match port $bustype_hash($potentialbus)]} {
			set regexp_terms [regexp -all -inline [string map {<number> {\[\d+\]}} $potentialbus] [get_object_name $unbused_ports]]
			set potentialobjects [get_ports $regexp_terms]
			set evalitems [format "\[get_ports \[list %s\]\]" [get_object_name $potentialobjects]]
			set unbused_ports [remove_from_collection $unbused_ports $potentialobjects]
		} else {
			set regexp_terms [regexp -all -inline [string map {<number> {\[\d+\]}} $potentialbus] [get_object_name $unbused_nets]]
			set potentialobjects [get_nets $regexp_terms]
			set evalitems [format "\[get_nets \[list %s\]\]" [get_object_name $potentialobjects]]
			set unbused_nets [remove_from_collection $unbused_nets $potentialobjects]
		}
		if {[sizeof_collection $potentialobjects]} {
			set numbers {}; foreach portname [get_object_name $potentialobjects] {regexp {.*\[(\d+?)\].*} $portname {} temp; lappend numbers $temp}
			set numbers [lsort -integer $numbers]
			set thiseval [format "create_bus $evalitems %s %s" [string map {<number> {}} $potentialbus] [subst $sortcmd]]
			redirect -variable temp $thiseval; lappend infologs [format "%s:" $potentialbus]; lappend infologs $thiseval; lappend infologs $temp
		} else {
			lappend infologs "Warning: Search term $potentialbus did not produce any result."
		}
	}
	echo_list $infologs > $infologfile
	puts "Check $infologfile for information.\n"
	return 1
}
proc build_ref_library { {arguments {}} } {
	upvar filename_hash filename_hash
	set defaultargs [list array_name ref2file_array source_search_path [sh pwd] inf_filename {} ]
	reset_array argument_array $defaultargs
	if {[is_array $arguments]} { array set argument_array $arguments } elseif {[expr ![llength $arguments] || [string match help $arguments]]} { puts "Usage:\n\tbuild_ref_library <arguments>\nDefault values of arguments:\n"; echo_array argument_array; return 1  }
	foreach key [array names argument_array]  { set $key $argument_array($key)}
	if {![llength $inf_filename]} { set inf_filename $filename_hash(dc_output)/${array_name}.inf }
	upvar $array_name $array_name
	reset_array $array_name {}
	set allvfiles_from_source [sh find $source_search_path -name "\"*.v\""]
        if {[expr [llength [array names $array_name]]<100]} {
		foreach vfile $allvfiles_from_source {
			set vfile_contents [split [join [read_from_file $vfile] {}] {;}]
			set found_module_lines [lsearch -regexp -inline -all $vfile_contents {module\s+([\w\d]+)\s*\(}]
        		#set found_module_lines [split [join [grep {module\s+([\w\d]+)\s*\(} $vfile] { }] {;}]
                	if {[llength $found_module_lines]} {
				set found_modules [regexp -all -inline {module\s+([\w\d]+)[^\w\d]} $found_module_lines]
	                        for {set i 1} {$i < [llength $found_modules]} {incr i 2} { lappend ${array_name}([lindex $found_modules $i]) $vfile }
		        }
	  	}
	}
	return $array_name
}
## Not fully debugged
proc resolve_references { {arguments {}} } {
	set defaultargs [list {topleveldesign {}} {source_search_path {}} {break_limit 100} {ref_array_name {}}]
	initvars $defaultargs
	if {[is_array $arguments]} { reset_array arguments_array $arguments }
	foreach key [array names arguments_array] { set $key $arguments_array($key) }
	if {![llength $topleveldesign]} { set topleveldesign [current_design_name] }
	if {![llength $source_search_path]} { set source_search_path [pwd]}
	set exitcode 0
	if {![llength $arguments]} {
		puts "Usage: resolve_references {topleveldesign <design_name_in_memory>} {source_search_path <path>}\n"
	} else {
		set refs_need_resolving {}; set refs_unable_to_resolve {}; set repeat_counter 0; set refs_left_unresolved 1
		set modules2lookfor {}; set oldmodules2lookfor {}
		if {![llength $ref_array_name]} { 
			upvar filename_hash filename_hash; set ref_array_name [build_ref_library [list [list source_search_path $source_search_path]] ]
		} else { upvar $ref_array_name $ref_array_name}
		while {$refs_left_unresolved && ($repeat_counter < $break_limit) } {
			set sourcefiles2read {}
			foreach ref2resolve $refs_need_resolving {
				if {[info exists ${ref_array_name}($ref2resolve)]} {
					lappend sourcefiles2read [subst [format "\$%s(%s)" $ref_array_name $ref2resolve ]]
				} else {
					lappend refs_unable_to_resolve $ref2resolve
				}
			}
			foreach sourcefile $sourcefiles2read {
				read_verilog $sourcefile
				analyze -format verilog $sourcefile
			}
			current_design $topleveldesign
			set refs_unresolvable {}; set refs_yet_to_resolve {}
			redirect -variable temp {link}
			set line_split_message [split $temp "\n"]
			set index 0; set message_length [llength $line_split_message];set oldmodules2lookfor [concat $oldmodules2lookfor $refs_need_resolving]; set modules2lookfor {}
			while {$index < $message_length} {
				## " char need removal because tcl can't handle list with . char after element in quotes
				set thisline [string map [list "\"" {}] [lindex $line_split_message $index]]
				if {[llength [set keyword_locations [lsearch -all -regexp $thisline {^(Warning:|Unable|to|resolve|reference)$}]]] == 5} {
					lappend modules2lookfor [string map {' {}} [lindex $thisline [expr [lindex $keyword_locations end]+1]]]
				}
				incr index
			}
			## Because link messages may refer to same module multiple times
			set modules2lookfor [get_unique_list $modules2lookfor]
			set modulesfailed2read [remove_from_list -intersect $modules2lookfor $oldmodules2lookfor]; set refs_unable_to_resolve [concat $refs_unable_to_resolve $modulesfailed2read]
			set refs_need_resolving [remove_from_list $modules2lookfor $refs_unable_to_resolve]; set refs_left_unresolved [llength $refs_need_resolving]
			incr repeat_counter
		}
		if {[llength $refs_unable_to_resolve]} { echo [format "These following modules are un-resolvable:\n\t%s" [join $refs_unable_to_resolve "\n\t"]]; set exitcode [llength $refs_unable_to_resolve] } else {
			set exitcode 1
		}
	}
	return $exitcode
}
## Not finished and probably won't as sh_command wouldn't work in dc_shell
proc find_reference_for { {arguments {}} } {
	set defaultargs [list {modulename {}} {source_search_path {}} ]
	initvars $defaultargs
	foreach argument $arguments { 	if {[llength $argument] == 2} {	initvar $argument } }
	if {![llength $source_search_path]} { set source_search_path [sh pwd] }
	if {![llength $modulename]} {
		puts "No modulename specified. Need to specify module to look for.\n"
		set returnlist {}
	} else {
		set grepterm [format "\'module %s\'" $modulename]
		set sh_command [format "sh grep %s `find %s -name \"*.v\"`" $grepterm $source_search_path]
		set command_return [eval $sh_command]
	}
}
proc find_scancells_by_si { test_si_port } {
	set current_net [get_nets -of_objects [get_ports $test_si_port]]; set return_collection {}
	while {[sizeof_collection $current_net]} {
		set next_si_pin [filter [all_fanout -from $current_net -flat -endpoints_only] -regexp {@full_name =~ ".*SI$"}]
		append_to_collection return_collection [set next_cell_in_chain [get_cells -quiet -of_objects $next_si_pin -filter {@is_sequential==true}]]
		set current_net [get_nets -of_objects [get_pins -of_objects $next_cell_in_chain -filter {@pin_direction==out}]]
	}
	return $return_collection
}
proc make_lfsr_of { {arguments {}} } {
	upvar filename_hash filename_hash; upvar project_path project_path
        set default_args [list xor_model saed32rvt_tt1p05v25c/XOR3X1_RVT xor_pin_map [list A1 A1 A2 A2 A3 A3 Z Y ] specified_chains {} tap_array_file {} bypass_control_netname bypass_control intermediate_net_name interim target_design_tag lfsred tap_xor_naming_form chain_%s_tap_xor_%s lfsr_enable lfsr_enable ext_ctrl_sig_name ext_ctrl_sig]
	reset_array argument_array $default_args; if {[is_array $arguments]} { array set argument_array $arguments } elseif {[string match *help $arguments]} { puts "Usage:\n\t make_lfsr_of <arguments>\nDefault arguments:\n"; echo_array argument_array; return 1 }
        foreach key [array names argument_array] { set $key $argument_array($key) }
	reset_array tag_array {};reset_array pin_name_of $xor_pin_map
	if {![llength $tap_array_file]} {
		if {[info exists filename_hash(lfsr_tap_array)]} { set tap_array_file $filename_hash(lfsr_tap_array) } else { set tap_array_file $project_path/logs/lfsr_tap_array.inf }
		mkdir_if_nonexist [join [lrange [split $tap_array_file /] 0 end-1] /]
	}
	if {[file exists $tap_array_file]} {
		reset_array tag_array [load_array_from $tap_array_file]
	}
	foreach specified_chain $specified_chains {
		if {[sizeof_collection [get_ports [format "test_si%s" $specified_chain]]]} {
			lappend found_chains $specified_chain
		}
	}
	#set found_chains [remove_from_list -intersect [get_scan_chains_by_name] $specified_chains]
	reset_array cell_array {}; set chain_lengths {};
	foreach chain $found_chains {
		set cell_array($chain) [get_object_name [find_scancells_by_si [get_ports [format "test_si%s" $chain] ]]]
		lappend chain_lengths [llength $cell_array($chain)]
	}
	set chain_lengths [get_unique_list $chain_lengths]
	foreach length $chain_lengths {
		if {![info exists tag_array($length)]} {
			puts "please enter tap locations of LFSR length $length:\n"
			set tag_array($length) [gets stdin]
		}
	}
	echo_array tag_array to_file $tap_array_file
	reset_array tap_nets_of_chain {}
	foreach chain $found_chains {
		set tap_positions $tag_array([llength $cell_array($chain)]) 
		set tap_nets_of_this_chain [get_nets -of_objects [filter [get_pins -of_objects [get_cells $cell_array($chain)]] -regexp {@full_name =~ ".*Q$"}]]
		set source_tap_nets {}
		foreach tap_position $tap_positions {
			append_to_collection source_tap_nets [index_collection $tap_nets_of_this_chain $tap_position]
		}
		set tap_nets_of_chain($chain) [get_object_name $source_tap_nets]
	}
	create_sandbox_design $target_design_tag; set msg {}; set sofar_successful 1; set tap0_sis {}; reset_array tap0_bypass_array {}
	foreach chain $found_chains {
		set tap_xor_count 0
		set nets_to_read_from [concat $tap_nets_of_chain($chain) {placebo}]
		while {[llength $nets_to_read_from]>2} {
			if {$sofar_successful} {
				set xor_gate_name [format $tap_xor_naming_form $chain $tap_xor_count]; set new_intermediate_net_name [format "%s_%s_%s" $intermediate_net_name $chain $tap_xor_count]
				quiet_eval_unless_error [format "create_cell %s $xor_model" $xor_gate_name]
				quiet_eval_unless_error [format "create_net  %s" $new_intermediate_net_name]
				quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\]" $new_intermediate_net_name $xor_gate_name/$pin_name_of(Z)]
				quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\]" [lindex $nets_to_read_from 0] $xor_gate_name/$pin_name_of(A1)]
				quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\]" [lindex $nets_to_read_from 1] $xor_gate_name/$pin_name_of(A2)]
				if {[info exists pin_name_of(A3)]} { 
					set ext_ctrl_sig [format "%s_%s_%s" $ext_ctrl_sig_name $chain $tap_xor_count]
					quiet_eval_unless_error [format "create_net  %s" $ext_ctrl_sig]
					quiet_eval_unless_error [format "connect_net \[get_nets %s\] \[get_pins %s\]" $ext_ctrl_sig $xor_gate_name/$pin_name_of(A3)]
				}
				set nets_to_read_from [concat $new_intermediate_net_name [lrange $new_intermediate_net_name 2 end] ]
			} else {
				echo_list $msg > make_lfsr_of.msg
				return 0
			}
			incr tap_xor_count
		}
		set this_tap0_si [filter [get_pins -of_objects [get_cells [lindex $cell_array($chain) 0]]] -regexp {@full_name =~ ".*SI$"}]
		set tap0_bypass_array([get_object_name $this_tap0_si]) [lindex $nets_to_read_from 0]
	}
	echo_list $msg > make_lfsr_of.msg
	bypass_pin [list specified_objects [get_pins [array names tap0_bypass_array]] bypass_control $lfsr_enable bypass_with_net_array [array get tap0_bypass_array] target_design_naming_form %s_fb_bypassed]
}
proc get_chains_of_scan_cells { cell_collection } {
	set chains [get_scan_chains_by_name]
	reset_array found_chains {};
	foreach chain $chains {
		set cells_of_this_chain [get_scan_cells_of_chain -chain $chain]
		if {[sizeof_collection $cells_of_this_chain]} { set found_chains($chain) $cells_of_this_chain }
	}
	echo_array found_chains
	return [array get found_chains]
}
proc make_iocl { {arguments {}} } {
	set default_args [list startend_ports {} direction in keepreg 0 target_design_name extracted_iocl register_io 1 clock_name clk newport_bus_names [list mlff_inport mcff_outport]]
	reset_array argument_array $default_args; if {[is_array $arguments]} { array set argument_array $arguments } elseif {[string match *help $arguments]} { puts "Usage:\n\t make_iocl <arguments>\nDefault arguments:\n"; echo_array argument_array; return 1 }
	puts "Parameters of this run:\n\n";
	foreach key [array names argument_array] { set $key $argument_array($key); puts "$key =\t$argument_array($key)\n" }
	set startend_port_names [get_object_name [get_ports $startend_ports]]
	if {[string match in $direction]} { 
		set pins_command [format "get_pins -quiet \[all_fanin -to \[get_ports \$startend_port_names\] -flat\]"] 
		set endstart_ports_command [format "get_ports -quiet \[all_fanin -to \[get_ports \$startend_port_names\] -flat -startpoints_only\]"] 
		set ff_pins_command [format "get_nets -of_objects \[get_pins -of_objects \[get_cells -of_objects \[%s -startpoints_only\] -filter {@is_sequential==true}\] -filter {@pin_direction==out}\]" "all_fanin -to \[get_ports \$startend_port_names\] -flat" ]
		set newbus_name [lindex $newport_bus_names 0]
	} else { 
		set pins_command [format "get_pins -quiet \[all_fanout -from \[get_ports \$startend_port_names\] -flat\]"] 
		set endstart_ports_command [format "get_ports -quiet \[all_fanout -from \[get_ports \$startend_port_names\] -flat -endpoints_only\]"] 
		set ff_pins_command [format "get_nets -of_objects \[get_pins -of_objects \[get_cells -of_objects \[%s -endpoints_only\] -filter {@is_sequential==true}\] -filter {@pin_direction==out}\]" "all_fanout -from \[get_ports \$startend_port_names\] -flat" ]
		set newbus_name [lindex $newport_bus_names 1]
	}
	if {!$keepreg} { set pins_command [format "remove_from_collection \[%s\] \[get_pins -of_objects \[get_cells -of_objects \[%s\] -filter {@is_sequential==true}\]\]" $pins_command $pins_command] }
	set pins [get_pins [eval $pins_command]]; set endstart_nets  [eval $ff_pins_command]; set endstart_ports [eval $endstart_ports_command]
	reset_array net_of {}; set net_names {}; reset_array port_of {}; reset_array direction_of {}; reset_array new_name_of_port {}; reset_array new_port_of_net {}; set recorded_ports {}
	set pin_names [get_object_name $pins]
	set cells [get_cells -of_objects $pins]; reset_array ref_of {}; set cell_names [string map {/ _} [get_object_name $cells]]
	foreach_in_collection cell $cells {
		set updated_cell_name [string map {/ _} [get_object_name $cell]]
		set ref_name [get_object_name [get_lib_cells -of_objects $cell]]
		if {![llength $ref_name]} { set ref_name [get_attribute [get_object_name $cell] ref_name] }
		if {[regexp {\*\*logic_([01])\*\*} $ref_name trash logic_value]} { set ref_name "-logic $logic_value"}
		set ref_of($updated_cell_name) $ref_name
	}
	set newport_count 0;set updated_pin_names {}
	foreach pin_name $pin_names {
		set exploded_pin_name [split $pin_name /]
		lappend updated_pin_names [set updated_pin_name [join [concat [join [lrange $exploded_pin_name 0 end-1] {_}] [lindex $exploded_pin_name end] ] /]]
		set updated_net_name [string map {/ _} [set net_name [get_object_name [get_nets -of_objects [get_pins $pin_name] ]]]]
		set net_of($updated_pin_name) $updated_net_name
		if {[sizeof_collection [set ports [get_ports -of_objects [get_nets $net_name ] -filter "@port_direction==$direction" ]] ]} {
			set ports [remove_from_collection $ports $recorded_ports]
			foreach_in_collection port $ports { 
				set port_name [get_object_name $port]
				set newport_name [format "%s_\[%s\]" $newbus_name $newport_count]
				append_to_collection -unique recorded_ports $port
				append_to_collection -unique recorded_ports [get_nets -of_objects $port]
				set new_name_of_port($port_name) $newport_name
				set port_of($updated_net_name) $newport_name
				set direction_of($newport_name) $direction
				incr newport_count
				##puts "$newport_count:$port_name:$updated_net_name:$pin_name\n"
			}
		}
	}
	foreach_in_collection net $endstart_nets {
		if {![sizeof_collection [remove_from_collection -intersect $recorded_ports $net]]} {
			set updated_net_name [string map {/ _} [get_object_name $net]]
			set newport_name [format "%s_\[%s\]" $newbus_name $newport_count]
			set new_port_of_net([get_object_name $net]) $newport_name
			append_to_collection -unique recorded_ports $net
			#set newport_name [format "${direction}port_of_%s" $updated_net_name]
			lappend port_of($updated_net_name) $newport_name
			set direction_of($newport_name) $direction
			incr newport_count 
		}
	}
	foreach_in_collection port $endstart_ports {
		if {![sizeof_collection [remove_from_collection -intersect $recorded_ports $port]]} {
			set updated_net_name [string map {/ _} [get_object_name [get_nets -of_objects $port]]]
			set newport_name [format "%s_\[%s\]" $newbus_name $newport_count]
			append_to_collection -unique recorded_ports $port
			set new_name_of_port([get_object_name $port]) $newport_name
			lappend port_of($updated_net_name) $newport_name
			set direction_of($newport_name) $direction
			incr newport_count 
		}
	}
	foreach_in_collection port [get_ports $startend_ports] {
		set net_name [string map {/ _} [get_object_name [get_nets -of_objects $port]]]
		set port_name [get_object_name $port]
		lappend port_of($net_name) $port_name
		set direction_of($port_name) [get_attribute $port port_direction]
	}
	set net_names [lsearch -not -inline -all [get_unique_list [concat [array names port_of] [string map {/ _} [get_object_name [get_nets -of_objects $pins]]] ]] {}]
	set original_design [current_design_name]
	if {[sizeof_collection [get_designs -quiet $target_design_name]]} { remove_design $target_design_name }
	create_design $target_design_name; current_design $target_design_name
	set sofar_successful 1; set msg {}
	foreach cell_name $cell_names { quiet_eval_unless_error [format "create_cell %s %s" $cell_name $ref_of($cell_name)]] }
	foreach port_name [array names direction_of] { quiet_eval_unless_error [format "create_port %s -direction %s" $port_name $direction_of($port_name)] ] }
	foreach net_name $net_names { 
		lappend msg ">>Now processing net: $net_name"
		quiet_eval_unless_error [format "create_net $net_name"] 
		if {[info exists port_of($net_name)]} {
			foreach port_name $port_of($net_name) {
				quiet_eval_unless_error [format "connect_net_if_havent_already \[get_nets $net_name\] \[get_ports $port_name\]"]
			}
		}
	}
	foreach pin_name $updated_pin_names { quiet_eval_unless_error [format " connect_net \[get_nets %s\] \[get_pins $pin_name\]" $net_of($pin_name)]}
	if {$register_io} { register_io_ports [list clock_name $clock_name] }
	current_design $original_design
	echo_list $msg > make_iocl.msg
	return [list net [array get new_port_of_net] port [array get new_name_of_port]]
}
proc replicate_circuits_by_pins { pin_objects {target_design_name ocl} } {
	reset_array net_of {}; set net_names {}; set pins [get_pins $pin_objects]
	set pin_names [get_object_name $pins]
	set cells [get_cells -of_objects $pins]; reset_array ref_of {}; set cell_names [string map {/ _} [get_object_name $cells]]
	foreach_in_collection cell $cells {
		set ref_of([string map {/ _} [get_object_name $cell]]) [get_object_name [get_lib_cells -of_objects $cell]]
	}
	set updated_pin_names {}
	foreach pin_name $pin_names {
		set exploded_pin_name [split $pin_name /]
		lappend updated_pin_names [set updated_pin_name [join [concat [join [lrange $exploded_pin_name 0 end-1] {_}] [lindex $exploded_pin_name end] ] /]]
		set net_of($updated_pin_name) [set net_name [string map {/ _} [get_object_name [get_nets -of_objects [get_pins $pin_name] ]]]]
	}
	set net_names [string map {/ _} [get_object_name [get_nets -of_objects $pins] ] ]
	set original_design [current_design_name]
	if {[sizeof_collection [get_designs -quiet $target_design_name]]} { remove_design $target_design_name }
	create_design $target_design_name; current_design $target_design_name
	foreach cell_name $cell_names { create_cell $cell_name $ref_of($cell_name) }
	foreach net_name $net_names { create_net $net_name }
	foreach pin_name $updated_pin_names { connect_net [get_nets $net_of($pin_name)] [get_pins $pin_name] }
	current_design $original_design
}
## Not finished!
proc simplified_retrieve { starting_pins {arguments {}} } {
	set defaultarguments [list {keep_sequential 1}]
	initvars $defaultarguments
	lappend arguments {}
	initvars $arguments 
	reset_array return_array {}
	set pins_collection {}
	append_to_collection -unique pins_collection 
	set pins_in_fanin_cones [all_fanin -to [filter $starting_pins {@pin_direction==in}] -flat ]
	set pins_in_fanin_cones [all_fanin -to [filter $starting_pins {@pin_direction==in}] -flat ]
	if {!$keep_sequential} { set pins_in_fanin_cones [remove_from_collection $pins_in_fanin_cones [all_fanin -to [filter $starting_pins {@pin_direction==in}] -flat -startpoints_only] ] }
	set pins_in_fanout_cones [all_fanout -from [filter $starting_pins {@pin_direction==in}] -flat ]
	if {!$keep_sequential} { set pins_in_fanout_cones [remove_from_collection $pins_in_fanout_cones [all_fanout -from [filter $starting_pins {@pin_direction==out}] -flat -startpoints_only] ] }
	set onpath_pins [add_to_collection -unique $pins_in_fanin_cones $pins_in_fanout_cones]
	set all_cells [get_cells -hierarchcal -of_objects $onpath_pins]; set return_array(all_cells) [get_object_name $all_cells]
	set all_pins [get_pins -of_objects $all_cells]; 
	set all_nets [get_nets -hierarchical -of_objects $all_pins]
	set all_ports [get_ports -hierarchical -of_objects $all_nets]
	set found_hierarchies {}
	foreach_in_collection cell $all_cells {
		set cellname [get_object_name $cell]
		if {[llength [set splitcellname [split $cellname /]]] > 1]} {
			set potential_hierarchies [lrange $splitcellname 0 end-1]
			for { set level 0 } { $level < [llength $splitcellname] } { incr level } {
				append_to_collection -unique found_hierarchies [get_cells [join [lrange $potential_hierarchies 0 $level] /] -filter {@is_hierarchical==true}]
			}
		}
		set return_array($cellname) [get_object_name [get_lib_cells -of_objects $cell]]
	}
	set current_toplevel [current_design_name]; set extracted_design_naming_form "%s_extracted"; set extracted_toplevel_name [format $extracted_design_naming_form $current_toplevel]
	if {[sizeof_collection [get_designs -quiet $extracted_toplevel_name]]} {
		remove_design $extracted_toplevel_name
	}
	copy_design $current_toplevel $extracted_toplevel_name
	foreach_in_collection hierarchy_cell $found_hierarchies {
		set cellname [get_object_name $hierarchy_cell]
	}
}
proc retrieve_logic_cone {direction_by_startername_hashlist {use_another_design {}} {target_design_name {}} {tagarraylist {}} {infologfile cmdlog} {include_stopcell no} {stopcelleval {filter $cells {@is_sequential == true}}} }  {
	array set direction_of_theother_pin [list in out out in inout inout {in out} {in out}]
	array set structurearray {}
	array set tagarray {}
	set necessary_conditions_met 1
	set failure_reason {}
	set infolog {}
	## Use the specified design
	if {[llength $use_another_design]} {
		if {[llength [set design2moveto [get_object_name [get_designs $use_another_design] ]  ]   ]} {
			current_design $design2moveto
		} else {
			puts "Erroneous input for \$use_another_design = $use_another_design . No design can be found with that search term.\n"
			return {}
		}
	} else {
		echo [format "Use current_design = %s by default. User is advised to take note." [current_design_name] ]
	}
## Takes in given starternames; make sure their given directions are legal; get nets these names represent; create starters from found nets.
	if {[is_array $direction_by_startername_hashlist]} {
		set starters {}
		array set direction_by_startername_hash $direction_by_startername_hashlist
		## Associate given direction with found items
		foreach givenstartername [array names direction_by_startername_hash] {
			## if given direction is not legal; add support to "all" in future.
			if {![string match in $direction_by_startername_hash($givenstartername)] && ![string match out $direction_by_startername_hash($givenstartername)] && ![string match inout $direction_by_startername_hash($givenstartername)]} { 
				set necessary_conditions_met 0
				lappend failure_reason "In array by first argument, value $direction_by_startername_hash(startername) of key $startername is not in, not out, nor inout."
				break 
			} else {
				## Starters only consider nets; if cannot found by get_nets, consider get_nets -of
				if {![sizeof_collection [set foundstartersbyname [get_nets -quiet $givenstartername]] ]} {
					if {![sizeof_collection [set foundstartersbyname [get_nets -quiet -of_objects $givenstartername]] ]} {
						if {![sizeof_collection [set foundstartersbyname [get_nets -quiet -of_objects [get_ports -quiet $givenstartername]]] ]} {
							lappend infolog "$givenstartername in given array does not specify any net. Skipping."
							set foundstartersbyname {}
						}
					}
				}
				foreach_in_collection foundstarterbyname $foundstartersbyname { 
					## This is necessary when starters are given as ports, because starters are defined as nets
					set direction_by_startername_hash([get_object_name $foundstarterbyname]) $direction_by_startername_hash($givenstartername)
					append_to_collection -unique starters $foundstarterbyname
				}
			}
		}
	} else {
		if {[string match help $direction_by_startername_hashlist]} {
			puts "Usage:\n\t (array set dest_array_for_structural_info) \[retrieve_logic_cone \[array get array_of_direction_by_starters\] source_design_name \[array get tagarray\] dest_infolog_filename|default=cmdlog target_design_name include_stopcell:default=no|include stopcellpattern:default={.*(DFF|FD).*}\]\n"
			return {}
		} else {
			 set necessary_conditions_met 0
			lappend failure_reason "First argument is not list of array."
		}
	}
	## Setup tag array if specified, use starter names if not
	if {[is_array $tagarraylist]} {	array set tagarray $tagarraylist } else { foreach startername [get_object_name $starters] { set tagarray($startername) $startername } }
	## Catch errors or begin main processing
	if {!$necessary_conditions_met} { echo [format "Error: Found unmet necessary conditions.\n\t" [join $failure_reason "\n\t"]] } else {
		## all starters are nets; this collection changes constantly
		array set nets_of_newports { in {} out {} inout {} {in out} {} }
		set originalstarters [copy_collection $starters]
		set processednets {}; set currenttag {}; set netcollection {}; set cellcollection {}; set portcollection {}; set pincollection {}
		## default $currentdirection is out
		set currentdirection out
		while {[sizeof_collection $starters]} {
			set currentstarter [index_collection $starters [expr [sizeof_collection $starters]-1] ]
			set starters [remove_from_collection $starters $currentstarter]
			if {[info exists direction_by_startername_hash([get_object_name $currentstarter])]} {
				set currentdirection $direction_by_startername_hash([get_object_name $currentstarter])
			} else {
				lappend infolog [format "Warning: no direction record found for %s . Assuming previous direction %s in following processing." [get_object_name $currentstarter] $currentdirection]
				set currentdirection out
			}
			set theotherdirection $direction_of_theother_pin($currentdirection)
			## If currentstarter is one of originalstarters, update tags
			if {[sizeof_collection [remove_from_collection -intersect $originalstarters $currentstarter]]} { 
				if {[info exists tagarray([get_object_name $currentstarter])]} { set currenttag tagarray([get_object_name $currentstarter]) } else { set currenttag [get_object_name $currentstarter]; set tagarray([get_object_name $currentstarter]) $currenttag }
			}
			if {![info exists tagarray($currenttag)]} {set tagarray($currenttag) {}}
			set tagarray($currenttag) [get_unique_list [concat $tagarray($currenttag) [get_object_name $currentstarter] ] ]
			## Skipping already processed nets
			if {[sizeof_collection [remove_from_collection -intersect $processednets $currentstarter]]} {
				lappend infolog [format "Info: %s is already processed. Skipped." [get_object_name $currentstarter]]
			} else {
## net/cell/portcollection are collections to be converted to namelists to be stored in structurearray, so as to make sure user can create cells/ports/nets before attempting to connect them. One 
## exception on portcollection: because of necessity to create new ports, portcollection is not sufficient; however, new ports can be indexed using existing newport counters.
				append_to_collection -unique netcollection $currentstarter
				## Cells/Pins on a different direction than current retrieving direction is not recorded
				set pins_of_correct_direction [get_pins -of_objects $currentstarter -filter [format "@pin_direction==%s" $currentdirection]]
				## On the other hand, all ports are recorded
				set ports [get_ports -of_objects $currentstarter]
				append_to_collection -unique portcollection $ports
				set cells [get_cells -of_objects $pins_of_correct_direction]
				set stoppat_cells [eval $stopcelleval]
				set normal_cells [remove_from_collection $cells $stoppat_cells]
## In "include stopcell" mode, stop cells (e.g., DFFs) are recorded, new ports are created on its "other direction" ports;
## In "skip stopcell" mode, stop cells (e.g., DFFs) are not recorded, new port is created on current net
				if {[sizeof_collection $stoppat_cells]} {
					if {[string match include* $include_stopcell]} {
						## This option hasn't been vetted.
						append_to_collection -unique pincollection $pins_of_correct_direction
						append_to_collection -unique pincollection [get_pins -of_objects $stoppat_cells]
						append_to_collection -unique netcollection [get_nets -of_objects [get_pins -of_objects $stoppat_cells]]
						append_to_collection -unique nets_of_newports($theotherdirection) [get_nets -of_objects [get_pins -of_objects $stoppat_cells]]
					} else {
						append_to_collection -unique pincollection [remove_from_collection $pins_of_correct_direction [get_pins -of_objects $stoppat_cells]]
						append_to_collection -unique nets_of_newports($theotherdirection) $currentstarter
					}
				} else {
					append_to_collection -unique pincollection $pins_of_correct_direction
				}
				## Append newly found nets of normal cells to starters
				set normalcell_pins_of_theother_direction [get_pins -of_objects $normal_cells -filter [format "@pin_direction==%s" $theotherdirection]]
				set newly_found_nets [remove_from_collection [get_nets -of_objects $normalcell_pins_of_theother_direction ] $processednets]
				append_to_collection -unique pincollection $normalcell_pins_of_theother_direction
				append_to_collection -unique starters $newly_found_nets 
				## Add direction for these newly found nets
				foreach_in_collection newly_found_net $newly_found_nets { set direction_by_startername_hash([get_object_name $newly_found_net]) $currentdirection }
				## Update processed nets collection
				append_to_collection -unique processednets $currentstarter
			}
		}
		lappend infolog [format "Processed nets:\n\t%s" [get_object_name $processednets]]
		redirect -variable temp {echo_array [array get direction_by_startername_hash]}; lappend infolog $temp
		## Convert cell/net/portcollection to lists and store them in structurearray
		set nets_w_pin [get_nets -of_objects $pincollection]
		set nets_w_pin_or_port $nets_w_pin
		append_to_collection -unique nets_w_pin_or_port [get_nets -of_objects $portcollection]
		foreach direction [array names nets_of_newports] { append_to_collection -unique nets_w_pin_or_port $nets_of_newports($direction) }
		set structurearray(netnames) [get_object_name $nets_w_pin_or_port ]
		if {[sizeof_collection [set nets_wo_pin_or_port [remove_from_collection $netcollection $nets_w_pin_or_port]]]} {
			lappend infolog [format "Warning: These nets are recorded but no pin or net is connected to them: \n\t%s" [join [get_object_name $nets_wo_pin_or_port] {,}] ]
		}
		set processedports {}
		foreach_in_collection net $nets_w_pin_or_port {
			set netname [get_object_name $net];set structurearray($netname) {};lappend structurearray($netname) "create_net_if_havent_already $netname"
			set pinnames [get_object_name [remove_from_collection -intersect [get_pins -of_objects $net] $pincollection] ]
			set ports [remove_from_collection -intersect [get_ports -of_objects $net] $portcollection]
			append_to_collection -unique processedports $ports
			set portnames [get_object_name $ports]
			foreach_in_collection port $ports { set portname [get_object_name $port]; lappend structurearray($portname) [format "create_port $portname -direction %s" [get_attribute $port port_direction] ] }
			lappend structurearray($netname) "connect_net_if_havent_already \[get_nets $netname\] \[get_pins \[list $pinnames\]\]"
			if {[sizeof_collection $ports]} {lappend structurearray($netname) "connect_net_if_havent_already \[get_nets $netname\] \[get_ports \[list $portnames\]\]"}
		}
		set structurearray(cellnames) [get_object_name [get_cells -of_objects $pincollection] ]
		foreach_in_collection cell [get_cells -of_objects $pincollection] {
			set cellname [get_object_name $cell]
			if {[get_attribute $cell is_hierarchical]} {
				lappend structurearray($cellname) [format "create_cell $cellname %s" [get_attribute $cell ref_name] ]
			} else {
				lappend structurearray($cellname) [format "create_cell $cellname %s" [get_object_name [get_lib_cells -of_objects $cell]] ]
			}
		}
		foreach direction [array names nets_of_newports] {
			set i 0
			set structurearray(new${direction}port_width) [sizeof_collection $nets_of_newports($direction)]
			foreach_in_collection net $nets_of_newports($direction) {
				set netname [get_object_name $net]; lappend structurearray($netname) "connect_net_if_havent_already \[get_nets $netname\] \[get_ports new${direction}port\[$i\] \]"
				incr i
			}
		}
		set structurearray(portnames) [get_object_name $portcollection]
		foreach_in_collection port [remove_from_collection $portcollection $processedports] {
			lappend infolog [format "Of net %s, port %s is/are connected but not recorded in \$portcollection. THis is not expected." [get_object_name [get_nets -of_objects $port]] [get_object_name $port] ]
			set portname [get_object_name $port]; lappend structurearray($portname) [format "create_port $portname -direction %s" [get_attribute $port port_direction] ]
		}
		set structurearray(tagarray) [array get tagarray]
		## also store count of newports
		foreach direction [array names newportcounter] { set structurearray(number_of_new${direction}ports) $newportcounter($direction) }
		## Now create list of nets, cells, ports, and append them to structurearray as well as counter
		if {[string match nowrite $target_design_name]} {
			puts "User chose not to write retrieve result. Retrieved architecture is returned.\n"
		} else {
			write_extracted_logiccone [array get structurearray] $target_design_name $infologfile
		}
		echo_list $infolog > $infologfile 
		echo [get_object_name $pincollection] >> $infologfile
	}
	return [array get structurearray]
}
proc compare_lists { lista listb {order_sensitivity integer} } {
	if {[string match order_sensitive $order_sensitivity]} {
		set match [string match [join $lista {}] [join $listb {}] ]
		set list_a $lista; set list_b $listb
	} elseif {[regexp {(ascii|dictionary|integer|real|increasing|decreasing|indices|nocase|unique)} $order_sensitivity regexpresult]} {
		set list_a [lsort -$order_sensitivity $lista]
		set list_b [lsort -$order_sensitivity $listb]
		set match [string match [join $list_a {}] [join $list_b {}] ]
	}
	if {!$match} {
		if {[expr ([llength $list_a]>[llength $list_b])]} { set longer_list $list_a; set shorter_list $list_b } else { set shorter_list $list_a; set longer_list $list_b }
		set returnlist_a {}; set returnlist_b {};
		foreach element $shorter_list { if {[lsearch -exact $longer_list $element] == -1} { lappend returnlist_a $element } }
		foreach element $longer_list { if {[lsearch -exact $shorter_list $element] == -1} { lappend returnlist_b $element } }
		return [list $returnlist_a $returnlist_b]
	} else {
		return $match
	}
}

##
if {[info exists current_tool]} {
	switch $current_tool {
		tmax	{ source $dropboxpath/tcl/procs_tetramax.tcl }
		dc	{ source $dropboxpath/tcl/procs_design_compiler.tcl }
		icc	{ source $dropboxpath/tcl/procs_ic_compiler.tcl }
		pt	{ source $dropboxpath/tcl/procs_prime_time.tcl }
	}
	alias reload_debugprocs {source $dropboxpath/tcl/procs_under_debug.tcl};## under_debug means finished writing, needs debugging before trusting
	alias testagain {source $dropboxpath/tcl/test.tcl};## test means currently testing different behaviors
	source $dropboxpath/tcl/setup_source_defs.tcl
	source $dropboxpath/tcl/aliases.tcl;source $dropboxpath/tcl/syntdetect_aliases.tcl
	source $dropboxpath/tcl/setup_library_paths.tcl
}
