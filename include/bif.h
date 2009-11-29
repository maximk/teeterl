#ifndef BIF_H
#define BIF_H

#include "proc.h"
#include "term.h"

// file:write0/2 [0]
term_t bif_write0_2(term_t File, term_t Bin, proc_t *proc);
// file:read0/2 [1]
term_t bif_read0_2(term_t File, term_t Len, proc_t *proc);
// file:open0/3 [2]
term_t bif_open0_3(term_t FileName, term_t Mode, term_t Perms, proc_t *proc);
// gen_tcp:listen_socket/2 [3]
term_t bif_listen_socket2(term_t LocIP, term_t LocPort, proc_t *proc);
// gen_tcp:connect_socket/4 [4]
term_t bif_connect_socket4(term_t RemIP, term_t RemPort, term_t LocIP, term_t LocPort, proc_t *proc);
// gen_tcp:controlling_process/2 [5]
term_t bif_controlling_process2(term_t Sock, term_t Pid, proc_t *proc);
// inet:getaddrs0/2 [6]
term_t bif_getaddrs0_2(term_t Addr, term_t Family, proc_t *proc);
// code:module_offset/3 [7]
term_t bif_module_offset3(term_t Module, term_t File, term_t Line, proc_t *proc);
// code:source_line/2 [8]
term_t bif_source_line2(term_t Module, term_t Offset, proc_t *proc);
// code:load_module/1 [9]
term_t bif_load_module1(term_t Where, proc_t *proc);
// code:is_loaded/1 [10]
term_t bif_is_loaded1(term_t Module, proc_t *proc);
// erlang:daemonize/0 [11]
term_t bif_daemonize0(proc_t *proc);
// erlang:display/1 [12]
term_t bif_display1(term_t Iolist, proc_t *proc);
// erlang:statistics/1 [13]
term_t bif_statistics1(term_t What, proc_t *proc);
// erlang:close/1 [14]
term_t bif_close1(term_t Oid, proc_t *proc);
// erlang:property/3 [15]
term_t bif_property3(term_t Term, term_t Opt, term_t Val, proc_t *proc);
// erlang:property/2 [16]
term_t bif_property2(term_t Term, term_t Opt, proc_t *proc);
// erlang:universaltime/0 [17]
term_t bif_universaltime0(proc_t *proc);
// erlang:localtime/0 [18]
term_t bif_localtime0(proc_t *proc);
// erlang:time/0 [19]
term_t bif_time0(proc_t *proc);
// erlang:date/0 [20]
term_t bif_date0(proc_t *proc);
// erlang:now/0 [21]
term_t bif_now0(proc_t *proc);
// erlang:get/1 [22]
term_t bif_get1(term_t Key, proc_t *proc);
// erlang:get/0 [23]
term_t bif_get0(proc_t *proc);
// erlang:erase/1 [24]
term_t bif_erase1(term_t Key, proc_t *proc);
// erlang:erase/0 [25]
term_t bif_erase0(proc_t *proc);
// erlang:put/2 [26]
term_t bif_put2(term_t Key, term_t Value, proc_t *proc);
// erlang:exit/2 [27]
term_t bif_exit2(term_t Pid, term_t Reason, proc_t *proc);
// erlang:exit/1 [28]
term_t bif_exit1(term_t Reason, proc_t *proc);
// erlang:throw/1 [29]
term_t bif_throw1(term_t Reason, proc_t *proc);
// erlang:error/2 [30]
term_t bif_error2(term_t Reason, term_t Args, proc_t *proc);
// erlang:error/1 [31]
term_t bif_error1(term_t Reason, proc_t *proc);
// erlang:'!'/2 [32]
term_t bif_send_msg2(term_t Pid, term_t Msg, proc_t *proc);
// erlang:spawn/1 [33]
term_t bif_spawn1(term_t Fun, proc_t *proc);
// erlang:spawn/3 [34]
term_t bif_spawn3(term_t Module, term_t Function, term_t Args, proc_t *proc);
// erlang:whereis/1 [35]
term_t bif_whereis1(term_t RegName, proc_t *proc);
// erlang:unregister/1 [36]
term_t bif_unregister1(term_t RegName, proc_t *proc);
// erlang:register/2 [37]
term_t bif_register2(term_t RegName, term_t PidOid, proc_t *proc);
// erlang:float_to_list/1 [38]
term_t bif_float_to_list1(term_t Float, proc_t *proc);
// erlang:atom_to_list/1 [39]
term_t bif_atom_to_list1(term_t Atom, proc_t *proc);
// erlang:list_to_binary/1 [40]
term_t bif_list_to_binary1(term_t List, proc_t *proc);
// erlang:binary_to_list/3 [41]
term_t bif_binary_to_list3(term_t Bin, term_t Start, term_t Stop, proc_t *proc);
// erlang:binary_to_list/1 [42]
term_t bif_binary_to_list1(term_t Bin, proc_t *proc);
// erlang:term_to_binary/1 [43]
term_t bif_term_to_binary1(term_t Term, proc_t *proc);
// erlang:binary_to_term/1 [44]
term_t bif_binary_to_term1(term_t Bin, proc_t *proc);
// erlang:list_to_tuple/1 [45]
term_t bif_list_to_tuple1(term_t List, proc_t *proc);
// erlang:tuple_to_list/1 [46]
term_t bif_tuple_to_list1(term_t Tuple, proc_t *proc);
// erlang:append_element/2 [47]
term_t bif_append_element2(term_t Tuple, term_t Elem, proc_t *proc);
// erlang:setelement/3 [48]
term_t bif_setelement3(term_t N, term_t Tuple, term_t Value, proc_t *proc);
// erlang:make_tuple/3 [49]
term_t bif_make_tuple3(term_t N, term_t DefValue, term_t InitList, proc_t *proc);
// erlang:make_tuple/2 [50]
term_t bif_make_tuple2(term_t N, term_t InitValue, proc_t *proc);
// erlang:'--'/2 [51]
term_t bif_minusminus2(term_t List1, term_t List2, proc_t *proc);
// erlang:'++'/2 [52]
term_t bif_plusplus2(term_t List1, term_t List2, proc_t *proc);
// lists:reverse/2 [53]
term_t bif_reverse2(term_t Ls, term_t Hs, proc_t *proc);
// lists:member/2 [54]
term_t bif_member2(term_t Elem, term_t List, proc_t *proc);
// lists:keyfind/3 [55]
term_t bif_keyfind3(term_t Key, term_t N, term_t TupleList, proc_t *proc);
// lists:keysearch/3 [56]
term_t bif_keysearch3(term_t Key, term_t N, term_t TupleList, proc_t *proc);
// lists:keymember/3 [57]
term_t bif_keymember3(term_t Key, term_t N, term_t TupleList, proc_t *proc);

#endif
