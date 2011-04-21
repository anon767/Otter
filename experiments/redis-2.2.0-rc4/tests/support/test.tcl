set ::num_tests 0
set ::num_passed 0
set ::num_failed 0
set ::tests_failed {}

proc assert {condition} {
    if {![uplevel 1 expr $condition]} {
        error "assertion:Expected '$value' to be true"
    }
}

proc assert_match {pattern value} {
    if {![string match $pattern $value]} {
        error "assertion:Expected '$value' to match '$pattern'"
    }
}

proc assert_equal {expected value} {
    if {$expected ne $value} {
        error "assertion:Expected '$value' to be equal to '$expected'"
    }
}

proc assert_error {pattern code} {
    if {[catch {uplevel 1 $code} error]} {
        assert_match $pattern $error
    } else {
        error "assertion:Expected an error but nothing was catched"
    }
}

proc assert_encoding {enc key} {
    # Swapped out values don't have an encoding, so make sure that
    # the value is swapped in before checking the encoding.
    set dbg [r debug object $key]
    while {[string match "* swapped at:*" $dbg]} {
        r debug swapin $key
        set dbg [r debug object $key]
    }
    assert_match "* encoding:$enc *" $dbg
}

proc assert_type {type key} {
    assert_equal $type [r type $key]
}

# This is called before starting the test
proc announce_test {s} {
    if {[info exists ::env(TERM)] && [string match $::env(TERM) xterm]} {
        puts -nonewline "$s\033\[0K"
        flush stdout
        set ::backward_count [string length $s]
    }
}

# This is called after the test finished
proc colored_dot {tags passed} {
    if {[info exists ::env(TERM)] && [string match $::env(TERM) xterm]} {
        # Go backward and delete what announc_test function printed.
        puts -nonewline "\033\[${::backward_count}D\033\[0K\033\[J"

        # Print a coloured char, accordingly to test outcome and tags.
        if {[lsearch $tags list] != -1} {
            set colorcode {31}
            set ch L
        } elseif {[lsearch $tags hash] != -1} {
            set colorcode {32}
            set ch H
        } elseif {[lsearch $tags set] != -1} {
            set colorcode {33}
            set ch S
        } elseif {[lsearch $tags zset] != -1} {
            set colorcode {34}
            set ch Z
        } elseif {[lsearch $tags basic] != -1} {
            set colorcode {35}
            set ch B
        } else {
            set colorcode {37}
            set ch .
        }
        if {$colorcode ne {}} {
            if {$passed} {
                puts -nonewline "\033\[0;${colorcode};40m"
            } else {
                puts -nonewline "\033\[7;${colorcode};40m"
            }
            puts -nonewline $ch
            puts -nonewline "\033\[0m"
            flush stdout
        }
    } else {
        if {$passed} {
            puts -nonewline .
        } else {
            puts -nonewline F
        }
    }
}

proc test {name code {okpattern undefined}} {
    # abort if tagged with a tag to deny
    foreach tag $::denytags {
        if {[lsearch $::tags $tag] >= 0} {
            return
        }
    }

    # check if tagged with at least 1 tag to allow when there *is* a list
    # of tags to allow, because default policy is to run everything
    if {[llength $::allowtags] > 0} {
        set matched 0
        foreach tag $::allowtags {
            if {[lsearch $::tags $tag] >= 0} {
                incr matched
            }
        }
        if {$matched < 1} {
            return
        }
    }

    incr ::num_tests
    set details {}
    lappend details $::curfile
    lappend details $::tags
    lappend details $name

    if {$::verbose} {
        puts -nonewline [format "#%03d %-68s " $::num_tests $name]
        flush stdout
    } else {
        announce_test $name
    }

    if {[catch {set retval [uplevel 1 $code]} error]} {
        if {[string match "assertion:*" $error]} {
            set msg [string range $error 10 end]
            lappend details $msg
            lappend ::tests_failed $details

            incr ::num_failed
            if {$::verbose} {
                puts "FAILED"
                puts "$msg\n"
            } else {
                colored_dot $::tags 0
            }
        } else {
            # Re-raise, let handler up the stack take care of this.
            error $error $::errorInfo
        }
    } else {
        if {$okpattern eq "undefined" || $okpattern eq $retval || [string match $okpattern $retval]} {
            incr ::num_passed
            if {$::verbose} {
                puts "PASSED"
            } else {
                colored_dot $::tags 1
            }
        } else {
            set msg "Expected '$okpattern' to equal or match '$retval'"
            lappend details $msg
            lappend ::tests_failed $details

            incr ::num_failed
            if {$::verbose} {
                puts "FAILED"
                puts "$msg\n"
            } else {
                colored_dot $::tags 0
            }
        }
    }
    flush stdout

    if {$::traceleaks} {
        set output [exec leaks redis-server]
        if {![string match {*0 leaks*} $output]} {
            puts "--- Test \"$name\" leaked! ---"
            puts $output
            exit 1
        }
    }
}

# Shadow a bunch of helper functions, and provide a few extra
proc test {name code {okpattern undefined}} {
    puts -nonewline "void "
    puts -nonewline [string map {" " "_" "(" "lparen" ")" "rparen" "-" "dash" "/" "slash" "." "dot" "," "comma" ":" "colon"} $name]
    puts "(void) {\n    redisReply *reply = NULL;"
    global env
    if {$env(OTTER_SYMBOLIC) == 1} {
        puts "    char *helper_string;"
    }
    uplevel $code
    if {$okpattern ne "undefined"} {
        if {[string length $okpattern] > 100} {
            puts "//truncated expected value"
        } else {
            assert_equal $okpattern "ignored"
            puts "    freeReplyObject(reply);"
        }
    }
    puts "}\n"
}

proc print_command {str} {
    global env
    if {$env(OTTER_SYMBOLIC) == 1} {
        puts [regsub {(.*), (".*")\);} $str "    helper_string = make_symbolic(\\2);\n\\1, helper_string);\n    free(helper_string);"]
    } else {
        puts $str
    }
}

proc otter_expect {expected reply} {
    if {[llength $expected] == 0} {
        puts "    expect_nil($reply);"
    } elseif {[llength $expected] == 1} {
        print_command "    expect($reply, \"[transform $expected]\");"
    } else {
        puts "    __ASSERT($reply->type == REDIS_REPLY_ARRAY);"
        puts "    __ASSERT($reply->elements == [llength $expected]);"
        for {set i 0} {$i < [llength $expected]} {incr i} {
            otter_expect [lindex $expected $i] "$reply->element\[$i]"
        }
    }
}

proc assert_equal {expected value} {
    return [otter_expect $expected "reply"]
}

proc assert_error {pattern code} {
    uplevel 1 $code
    puts "    expect_error(reply, \"[string map {{*} {.*}} $pattern]\");"
}

proc assert_encoding {enc key} {
    print_command "    expect_encoding(\"$enc\", \"[transform $key]\");"
}

proc map {f xs} {
    set result {}
    foreach x $xs {
        lappend result [$f $x]
    }
    return $result
}

set transformed_keys [dict create\
                          blist1      "\\x01\\x01"\
                          blist2      "\\x01\\x02"\
                          blist       "\\x01\\x03"\
                          dstlist     "\\x01\\x04"\
                          list1       "\\x01\\x05"\
                          list2       "\\x01\\x06"\
                          list3       "\\x01\\x07"\
                          mylist1     "\\x01\\x08"\
                          mylist2     "\\x01\\x09"\
                          mylist      "\\x01\\x0a"\
                          myotherlist "\\x01\\x0b"\
                          myziplist1  "\\x01\\x0c"\
                          myziplist2  "\\x01\\x0d"\
                          newlist     "\\x01\\x0e"\
                          nolist      "\\x01\\x0f"\
                          notalist    "\\x01\\x10"\
                          srclist     "\\x01\\x11"\
                          xlist       "\\x01\\x12"\
                          l           "\\x01\\x13"\
                         ]

set transformed_vals [dict create\
                          7        "\\x02\\x08"\
                          8        "\\x02\\x09"\
                          95       "\\x02\\x0a"\
                          96       "\\x02\\x0b"\
                          98       "\\x02\\x0c"\
                          99       "\\x02\\x0d"\
                          9        "\\x02\\x0e"\
                          a        "\\x02\\x0f"\
                          bar      "\\x02\\x10"\
                          b        "\\x02\\x11"\
                          c        "\\x02\\x12"\
                          d        "\\x02\\x13"\
                          foobared "\\x02\\x14"\
                          foobar   "\\x02\\x15"\
                          foo      "\\x02\\x16"\
                          f        "\\x02\\x17"\
                          test     "\\x02\\x18"\
                          zap      "\\x02\\x19"\
                          yy       "\\x02\\x1a"\
                          zz       "\\x02\\x1b"\
                          aa       "\\x02\\x1c"\
                          ddd      "\\x02\\x1d"\
                          dd       "\\x02\\x1e"\
                          bad      "\\x02\\x1f"\
                          aaa      "\\x02\\x20"\
                          42       "\\x02\\x21"
                     ]
# It would be nice to set the values 0 to 6 as well, but I can't (given the way
# I'm doing things) because they would conflict with numbers that are used
# concretely in the tests (e.g., lengths and indices of lists, numbers of
# elements removed)

#                      0        "\\x02\\x01"\
#                      1        "\\x02\\x02"\
#                      2        "\\x02\\x03"\
#                      3        "\\x02\\x04"\
#                      4        "\\x02\\x05"\
#                      5        "\\x02\\x06"\
#                      6        "\\x02\\x07"\

proc make_key_symbolic {str} {
    global transformed_keys
    if {[dict exists $transformed_keys $str]} {
        return [dict get $transformed_keys $str]
    }    
    return $str
}

proc make_val_symbolic {str} {
    global transformed_vals
    if {[dict exists $transformed_vals $str]} {
        return [dict get $transformed_vals $str]
    }    
    return $str
}

proc escape_backslashes str { return [string map {"\\" "\\\\"} $str] }

proc transform {args} {
    global env
    set args [map escape_backslashes $args]
    if {$env(OTTER_SYMBOLIC) == 1} {
        # Convert keys to symbolic
        set args [map make_key_symbolic $args]
        # Convert values to symbolic
        set args [map make_val_symbolic $args]
    }
    return [join $args]
}
