#ifndef BIF_H
#define BIF_H

#include "proc.h"
#include "term.h"
#include "atom.h"

#define result(r)	proc_bif_result(ctx, (r))

// a generic function pointer to avoid warnings
typedef term_t (*bifN_t)(process_t *ctx);

typedef term_t (*bif0_t)(process_t *ctx);
typedef term_t (*bif1_t)(term_t a, process_t *ctx);
typedef term_t (*bif2_t)(term_t a, term_t b, process_t *ctx);
typedef term_t (*bif3_t)(term_t a, term_t b, term_t c, process_t *ctx);
typedef term_t (*bif4_t)(term_t a, term_t b, term_t c, term_t d, process_t *ctx);

// os:type/0 [0]
term_t bif_type0(process_t *ctx);
// crypto:rc4_update/2 [1]
term_t bif_rc4_update2(term_t Text, term_t Opaque, process_t *ctx);
// crypto:rc4_init/1 [2]
term_t bif_rc4_init1(term_t Key, process_t *ctx);
// crypto:sha1/1 [3]
term_t bif_sha1_1(term_t Data, process_t *ctx);
// crypto:sha1_final/1 [4]
term_t bif_sha1_final1(term_t Context, process_t *ctx);
// crypto:sha1_update/2 [5]
term_t bif_sha1_update2(term_t Data, term_t Context, process_t *ctx);
// crypto:sha1_init/0 [6]
term_t bif_sha1_init0(process_t *ctx);
// crypto:md5/1 [7]
term_t bif_md5_1(term_t Data, process_t *ctx);
// crypto:md5_final/1 [8]
term_t bif_md5_final1(term_t Context, process_t *ctx);
// crypto:md5_update/2 [9]
term_t bif_md5_update2(term_t Data, term_t Context, process_t *ctx);
// crypto:md5_init/0 [10]
term_t bif_md5_init0(process_t *ctx);
// gen_udp:sendto/4 [11]
term_t bif_sendto4(term_t Sock, term_t RemIP, term_t RemPort, term_t Packet, process_t *ctx);
// gen_udp:open_socket/2 [12]
term_t bif_open_socket2(term_t LocIP, term_t LocPort, process_t *ctx);
// gen_tcp:close/1 [13]
term_t bif_close1(term_t Sock, process_t *ctx);
// gen_tcp:controlling_process/2 [14]
term_t bif_controlling_process2(term_t Sock, term_t Pid, process_t *ctx);
// gen_tcp:listen_socket/2 [15]
term_t bif_listen_socket2(term_t LocIP, term_t LocPort, process_t *ctx);
// gen_tcp:connect_socket/4 [16]
term_t bif_connect_socket4(term_t RemIP, term_t RemPort, term_t LocIP, term_t LocPort, process_t *ctx);
// inet:getaddrs0/2 [17]
term_t bif_getaddrs0_2(term_t Addr, term_t Family, process_t *ctx);
// file:read_file_info0/1 [18]
term_t bif_read_file_info0_1(term_t Filename, process_t *ctx);
// file:list_dir3_0/1 [19]
term_t bif_list_dir3_0_1(term_t Dir, process_t *ctx);
// file:list_dir2/1 [20]
term_t bif_list_dir2_1(term_t Dir, process_t *ctx);
// file:list_dir/1 [21]
term_t bif_list_dir1(term_t Dir, process_t *ctx);
// file:del_dir/1 [22]
term_t bif_del_dir1(term_t Dir, process_t *ctx);
// file:make_dir/1 [23]
term_t bif_make_dir1(term_t Dir, process_t *ctx);
// file:get_cwd/0 [24]
term_t bif_get_cwd0(process_t *ctx);
// file:set_cwd/1 [25]
term_t bif_set_cwd1(term_t Dir, process_t *ctx);
// file:rename/2 [26]
term_t bif_rename2(term_t Src, term_t Dst, process_t *ctx);
// file:delete/1 [27]
term_t bif_delete1(term_t Path, process_t *ctx);
// file:write0/2 [28]
term_t bif_write0_2(term_t Port, term_t Bin, process_t *ctx);
// file:read0/2 [29]
term_t bif_read0_2(term_t Port, term_t Len, process_t *ctx);
// file:open0/3 [30]
term_t bif_open0_3(term_t FileName, term_t Mode, term_t Perms, process_t *ctx);
// erlang:daemonize/0 [31]
term_t bif_daemonize0(process_t *ctx);
// erlang:make_port/3 [32]
term_t bif_make_port3(term_t Node, term_t Serial, term_t Creation, process_t *ctx);
// erlang:make_pid/3 [33]
term_t bif_make_pid3(term_t Node, term_t Serial, term_t Creation, process_t *ctx);
// erlang:make_ref/3 [34]
term_t bif_make_ref3(term_t Node, term_t Serial, term_t Creation, process_t *ctx);
// erlang:make_ref/0 [35]
term_t bif_make_ref0(process_t *ctx);
// erlang:put/1 [36]
term_t bif_put1(term_t Dict, process_t *ctx);
// erlang:get/0 [37]
term_t bif_get0(process_t *ctx);
// erlang:universaltime/0 [38]
term_t bif_universaltime0(process_t *ctx);
// erlang:localtime/0 [39]
term_t bif_localtime0(process_t *ctx);
// erlang:time/0 [40]
term_t bif_time0(process_t *ctx);
// erlang:date/0 [41]
term_t bif_date0(process_t *ctx);
// erlang:now/0 [42]
term_t bif_now0(process_t *ctx);
// erlang:get_locals/1 [43]
term_t bif_get_locals1(term_t Pid, process_t *ctx);
// erlang:get_stacktrace0/1 [44]
term_t bif_get_stacktrace0_1(term_t Pid, process_t *ctx);
// erlang:garbage_collect/1 [45]
term_t bif_garbage_collect1(term_t Pid, process_t *ctx);
// erlang:garbage_collect/0 [46]
term_t bif_garbage_collect0(process_t *ctx);
// erlang:process_flag/3 [47]
term_t bif_process_flag3(term_t Pid, term_t What, term_t Value, process_t *ctx);
// erlang:port_info/2 [48]
term_t bif_port_info2(term_t Port, term_t What, process_t *ctx);
// erlang:process_info/2 [49]
term_t bif_process_info2(term_t Pid, term_t What, process_t *ctx);
// erlang:close_port/1 [50]
term_t bif_close_port1(term_t Port, process_t *ctx);
// erlang:ports/0 [51]
term_t bif_ports0(process_t *ctx);
// erlang:set_port_option/3 [52]
term_t bif_set_port_option3(term_t Port, term_t Opt, term_t Value, process_t *ctx);
// erlang:open_port/2 [53]
term_t bif_open_port2(term_t Term, term_t Options, process_t *ctx);
// erlang:is_local_node/1 [54]
term_t bif_is_local_node1(term_t Node, process_t *ctx);
// erlang:set_node/1 [55]
term_t bif_set_node1(term_t Node, process_t *ctx);
// erlang:node/0 [56]
term_t bif_node0(process_t *ctx);
// erlang:node/1 [57]
term_t bif_node1(term_t PidRefPort, process_t *ctx);
// erlang:registered/0 [58]
term_t bif_registered0(process_t *ctx);
// erlang:whereis/1 [59]
term_t bif_whereis1(term_t RegName, process_t *ctx);
// erlang:unregister/1 [60]
term_t bif_unregister1(term_t RegName, process_t *ctx);
// erlang:register/2 [61]
term_t bif_register2(term_t RegName, term_t Pid, process_t *ctx);
// erlang:phash/2 [62]
term_t bif_phash2(term_t Term, term_t Range, process_t *ctx);
// erlang:prp_triple/1 [63]
term_t bif_prp_triple1(term_t PidRefPort, process_t *ctx);
// erlang:append_element/2 [64]
term_t bif_append_element2(term_t Tuple, term_t Elem, process_t *ctx);
// erlang:make_tuple/2 [65]
term_t bif_make_tuple2(term_t N, term_t InitVal, process_t *ctx);
// erlang:list_to_float/1 [66]
term_t bif_list_to_float1(term_t Chars, process_t *ctx);
// erlang:float_to_list/1 [67]
term_t bif_float_to_list1(term_t N, process_t *ctx);
// erlang:list_to_tuple/1 [68]
term_t bif_list_to_tuple1(term_t List, process_t *ctx);
// erlang:tuple_to_list/1 [69]
term_t bif_tuple_to_list1(term_t Tuple, process_t *ctx);
// erlang:list_to_binary/1 [70]
term_t bif_list_to_binary1(term_t Bin, process_t *ctx);
// erlang:binary_to_list/3 [71]
term_t bif_binary_to_list3(term_t Bin, term_t Beg, term_t End, process_t *ctx);
// erlang:term_to_binary/1 [72]
term_t bif_term_to_binary1(term_t Bin, process_t *ctx);
// erlang:binary_to_term/1 [73]
term_t bif_binary_to_term1(term_t Bin, process_t *ctx);
// erlang:split_binary/2 [74]
term_t bif_split_binary2(term_t Bin, term_t N, process_t *ctx);
// erlang:list_to_atom/1 [75]
term_t bif_list_to_atom1(term_t Atom, process_t *ctx);
// erlang:atom_to_list/1 [76]
term_t bif_atom_to_list1(term_t Atom, process_t *ctx);
// erlang:trunc/1 [77]
term_t bif_trunc1(term_t Number, process_t *ctx);
// erlang:round/1 [78]
term_t bif_round1(term_t Number, process_t *ctx);
// erlang:float/1 [79]
term_t bif_float1(term_t Number, process_t *ctx);
// erlang:hd/1 [80]
term_t bif_hd1(term_t List, process_t *ctx);
// erlang:tl/1 [81]
term_t bif_tl1(term_t List, process_t *ctx);
// erlang:length/1 [82]
term_t bif_length1(term_t List, process_t *ctx);
// erlang:setelement/3 [83]
term_t bif_setelement3(term_t N, term_t Tuple, term_t Value, process_t *ctx);
// erlang:element/2 [84]
term_t bif_element2(term_t N, term_t Tuple, process_t *ctx);
// erlang:bit_size/1 [85]
term_t bif_bit_size1(term_t Bin, process_t *ctx);
// erlang:size/1 [86]
term_t bif_size1(term_t TupleOrBin, process_t *ctx);
// erlang:fun_info/2 [87]
term_t bif_fun_info2(term_t Fun, term_t What, process_t *ctx);
// erlang:fun_info/1 [88]
term_t bif_fun_info1(term_t Fun, process_t *ctx);
// erlang:make_fun/3 [89]
term_t bif_make_fun3(term_t Name, term_t Arity, term_t FreeVars, process_t *ctx);
// erlang:send_msg0/2 [90]
term_t bif_send_msg0_2(term_t Rcpt, term_t Msg, process_t *ctx);
// erlang:display/1 [91]
term_t bif_display1(term_t Term, process_t *ctx);
// io:print_iolist/1 [92]
term_t bif_print_iolist1(term_t IOList, process_t *ctx);
// code:soft_purge/1 [93]
term_t bif_soft_purge1(term_t Mod, process_t *ctx);
// code:purge/1 [94]
term_t bif_purge1(term_t Mod, process_t *ctx);
// code:delete/1 [95]
term_t bif_delete_module1(term_t Mod, process_t *ctx);
// code:poll_ports/1 [96]
term_t bif_poll_ports1(term_t Time, process_t *ctx);
// code:destroy_process/1 [97]
term_t bif_destroy_process1(term_t Pid, process_t *ctx);
// code:run_slice/2 [98]
term_t bif_run_slice2(term_t Pid, term_t Reductions, process_t *ctx);
// code:spawn0/1 [99]
term_t bif_spawn0_1(term_t F, process_t *ctx);
// code:spawn0/3 [100]
term_t bif_spawn0_3(term_t Mod, term_t Fun, term_t Args, process_t *ctx);
// code:all_loaded/0 [101]
term_t bif_all_loaded0(process_t *ctx);
// code:is_loaded/1 [102]
term_t bif_is_loaded1(term_t Mod, process_t *ctx);
// code:load_module0/3 [103]
term_t bif_load_module0_3(term_t Mod, term_t Exports, term_t Preloaded, process_t *ctx);
// code:list_embedded/0 [104]
term_t bif_list_embedded0(process_t *ctx);
// code:embedded_module/1 [105]
term_t bif_embedded_module1(term_t Mod, process_t *ctx);
// code:undefined_builtin/0 [106]
term_t bif_undefined_builtin0(process_t *ctx);
// lists:member/2 [107]
term_t bif_member2(term_t Elem, term_t List, process_t *ctx);
// lists:last/1 [108]
term_t bif_last1(term_t List, process_t *ctx);
// lists:keysearch/3 [109]
term_t bif_keysearch3(term_t Key, term_t N, term_t TupleList, process_t *ctx);
// lists:keymember/3 [110]
term_t bif_keymember3(term_t Key, term_t N, term_t TupleList, process_t *ctx);
// lists:reverse/2 [111]
term_t bif_reverse2(term_t List, term_t Tail, process_t *ctx);

#endif
