fn f() {
                                    gate_modifier!( "bundle" => native_link_modifiers_bundle
                                    "verbatim" => native_link_modifiers_verbatim
                                    "whole-archive" => native_link_modifiers_whole_archive
                                    "as-needed" => native_link_modifiers_as_needed
                                );
}
