-module(ops).
-export([asm/1,disasm/1,switch/0]).

asm(is_integer) -> {0,none};
asm(is_bignum) -> {1,none};
asm(is_atom) -> {2,none};
asm(is_boolean) -> {3,none};
asm(is_float) -> {4,none};
asm(is_function) -> {5,none};
asm(is_function2) -> {6,none};
asm(is_number) -> {7,none};
asm(is_pid) -> {8,none};
asm(is_port) -> {9,none};
asm(is_record) -> {10,none};
asm(is_reference) -> {11,none};
asm(is_cons) -> {12,none};
asm(is_nil) -> {13,none};
asm(is_list) -> {14,none};
asm(is_tuple) -> {15,none};
asm(is_tuple_of_arity) -> {16,n};
asm(is_binary) -> {17,none};
asm(dup) -> {18,none};
asm(swap) -> {19,none};
asm(drop) -> {20,n};
asm(reset_msgs) -> {21,none};
asm(get_msg) -> {22,label};
asm(drop_msg) -> {23,none};
asm(send_msg) -> {24,none};
asm(get_arg) -> {25,n};
asm(clear_arg) -> {26,n};
asm(set_arg) -> {27,n};
asm(push_args) -> {28,n};
asm(set_args) -> {29,n};
asm(get_var) -> {30,n};
asm(clear_var) -> {31,n};
asm(set_var) -> {32,n};
asm(jump_if_not) -> {33,label};
asm(jump_if) -> {34,label};
asm(jump) -> {35,label};
asm('catch') -> {36,label};
asm(drop_catch) -> {37,none};
asm(raise) -> {38,none};
asm(lit) -> {39,term};
asm(car) -> {40,none};
asm(cdr) -> {41,none};
asm(make_cons) -> {42,none};
asm(make_cons_nil) -> {43,none};
asm(list_copy) -> {44,none};
asm(list_append) -> {45,none};
asm(list_subtract) -> {46,none};
asm(pack_tuple) -> {47,n};
asm(unpack_tuple) -> {48,n};
asm(tuple_elem) -> {49,n};
asm(beg_bin) -> {50,none};
asm(end_bin) -> {51,none};
asm(bin_add_i_b) -> {52,n};
asm(bin_add_i_l) -> {53,n};
asm(bin_add_i_n) -> {54,n};
asm(bin_add_f_b) -> {55,n};
asm(bin_add_f_l) -> {56,n};
asm(bin_add_f_n) -> {57,n};
asm(bin_add_b) -> {58,n};
asm(bin_fetch_start) -> {59,none};
asm(is_bin_consumed) -> {60,none};
asm(bin_get_context) -> {61,none};
asm(bin_size_is) -> {62,none};
asm(bin_size_is_at_least) -> {63,none};
asm(bin_fetch_i_u_b) -> {64,n};
asm(bin_fetch_i_u_l) -> {65,n};
asm(bin_fetch_i_u_n) -> {66,n};
asm(bin_fetch_i_s_b) -> {67,n};
asm(bin_fetch_i_s_l) -> {68,n};
asm(bin_fetch_i_s_n) -> {69,n};
asm(bin_fetch_f_b) -> {70,n};
asm(bin_fetch_f_l) -> {71,n};
asm(bin_fetch_f_n) -> {72,n};
asm(bin_fetch_bin) -> {73,n};
asm(self) -> {74,none};
asm(enter) -> {75,n};
asm(leave) -> {76,none};
asm(ret) -> {77,n};
asm(addi) -> {78,n};
asm(subi) -> {79,n};
asm(add) -> {80,none};
asm(sub) -> {81,none};
asm('or') -> {82,none};
asm('bor') -> {83,none};
asm('xor') -> {84,none};
asm('bxor') -> {85,none};
asm('bsl') -> {86,none};
asm('bsr') -> {87,none};
asm(mult) -> {88,none};
asm('div') -> {89,none};
asm(idiv) -> {90,none};
asm('rem') -> {91,none};
asm('and') -> {92,none};
asm('band') -> {93,none};
asm(equal) -> {94,none};
asm(neq) -> {95,none};
asm(lesseq) -> {96,none};
asm(less) -> {97,none};
asm(moreeq) -> {98,none};
asm(more) -> {99,none};
asm(exeq) -> {100,none};
asm(nexeq) -> {101,none};
asm('not') -> {102,none};
asm('bnot') -> {103,none};
asm(negate) -> {104,none};
asm(equal_to) -> {105,term};
asm(neq_to) -> {106,term};
asm(lesseq_than) -> {107,term};
asm(less_than) -> {108,term};
asm(moreeq_than) -> {109,term};
asm(more_than) -> {110,term};
asm(bif_call0) -> {111,e0};
asm(bif_call1) -> {112,e1};
asm(bif_call2) -> {113,e2};
asm(bif_call3) -> {114,e3};
asm(bif_call4) -> {115,e4};
asm(call) -> {116,label};
asm(tail_call) -> {117,label};
asm(call_far) -> {118,mfn};
asm(tail_call_far) -> {119,mfn};
asm(apply) -> {120,none};
asm(apply2) -> {121,none};
asm(apply_fun) -> {122,none};
asm(level) -> {123,n};
asm(rlevel) -> {124,n};
asm(nop) -> {125,none};
asm(break) -> {126,none};
asm(Op) -> exit({op,Op}).

disasm(0) -> {is_integer,none,{1,1,1}};
disasm(1) -> {is_bignum,none,{1,1,1}};
disasm(2) -> {is_atom,none,{1,1,1}};
disasm(3) -> {is_boolean,none,{1,1,1}};
disasm(4) -> {is_float,none,{1,1,1}};
disasm(5) -> {is_function,none,{1,1,1}};
disasm(6) -> {is_function2,none,{2,1,1}};
disasm(7) -> {is_number,none,{1,1,1}};
disasm(8) -> {is_pid,none,{1,1,1}};
disasm(9) -> {is_port,none,{1,1,1}};
disasm(10) -> {is_record,none,{3,1,1}};
disasm(11) -> {is_reference,none,{1,1,1}};
disasm(12) -> {is_cons,none,{1,1,1}};
disasm(13) -> {is_nil,none,{1,1,1}};
disasm(14) -> {is_list,none,{1,1,1}};
disasm(15) -> {is_tuple,none,{1,1,1}};
disasm(16) -> {is_tuple_of_arity,n,{1,1,1}};
disasm(17) -> {is_binary,none,{1,1,1}};
disasm(18) -> {dup,none,{1,2,2}};
disasm(19) -> {swap,none,{2,2,2}};
disasm(20) -> {drop,n,{arg,0,0}};
disasm(21) -> {reset_msgs,none,{1,0,0}};
disasm(22) -> {get_msg,label,{0,1,0}};
disasm(23) -> {drop_msg,none,{0,0,0}};
disasm(24) -> {send_msg,none,{2,1,1}};
disasm(25) -> {get_arg,n,{0,1,1}};
disasm(26) -> {clear_arg,n,{0,1,1}};
disasm(27) -> {set_arg,n,{1,0,0}};
disasm(28) -> {push_args,n,{arg,0,0}};
disasm(29) -> {set_args,n,{arg,0,0}};
disasm(30) -> {get_var,n,{0,1,1}};
disasm(31) -> {clear_var,n,{0,1,1}};
disasm(32) -> {set_var,n,{1,0,0}};
disasm(33) -> {jump_if_not,label,{1,0,0}};
disasm(34) -> {jump_if,label,{1,0,0}};
disasm(35) -> {jump,label,{0,noret,0}};
disasm(36) -> {'catch',label,{0,0,2}};
disasm(37) -> {drop_catch,none,{0,0,0}};
disasm(38) -> {raise,none,{2,noret,noret}};
disasm(39) -> {lit,term,{0,1,1}};
disasm(40) -> {car,none,{1,1,1}};
disasm(41) -> {cdr,none,{1,1,1}};
disasm(42) -> {make_cons,none,{2,1,1}};
disasm(43) -> {make_cons_nil,none,{1,1,1}};
disasm(44) -> {list_copy,none,{1,1,1}};
disasm(45) -> {list_append,none,{2,1,1}};
disasm(46) -> {list_subtract,none,{2,1,1}};
disasm(47) -> {pack_tuple,n,{arg,1,1}};
disasm(48) -> {unpack_tuple,n,{1,arg,arg}};
disasm(49) -> {tuple_elem,n,{1,1,1}};
disasm(50) -> {beg_bin,none,{0,0,0}};
disasm(51) -> {end_bin,none,{0,1,1}};
disasm(52) -> {bin_add_i_b,n,{2,0,0}};
disasm(53) -> {bin_add_i_l,n,{2,0,0}};
disasm(54) -> {bin_add_i_n,n,{2,0,0}};
disasm(55) -> {bin_add_f_b,n,{2,0,0}};
disasm(56) -> {bin_add_f_l,n,{2,0,0}};
disasm(57) -> {bin_add_f_n,n,{2,0,0}};
disasm(58) -> {bin_add_b,n,{2,0,0}};
disasm(59) -> {bin_fetch_start,none,{1,0,0}};
disasm(60) -> {is_bin_consumed,none,{0,1,1}};
disasm(61) -> {bin_get_context,none,{0,1,1}};
disasm(62) -> {bin_size_is,none,{2,1,1}};
disasm(63) -> {bin_size_is_at_least,none,{2,1,1}};
disasm(64) -> {bin_fetch_i_u_b,n,{1,1,1}};
disasm(65) -> {bin_fetch_i_u_l,n,{1,1,1}};
disasm(66) -> {bin_fetch_i_u_n,n,{1,1,1}};
disasm(67) -> {bin_fetch_i_s_b,n,{1,1,1}};
disasm(68) -> {bin_fetch_i_s_l,n,{1,1,1}};
disasm(69) -> {bin_fetch_i_s_n,n,{1,1,1}};
disasm(70) -> {bin_fetch_f_b,n,{1,1,1}};
disasm(71) -> {bin_fetch_f_l,n,{1,1,1}};
disasm(72) -> {bin_fetch_f_n,n,{1,1,1}};
disasm(73) -> {bin_fetch_bin,n,{1,1,1}};
disasm(74) -> {self,none,{0,1,1}};
disasm(75) -> {enter,n,{0,0,0}};
disasm(76) -> {leave,none,{0,0,0}};
disasm(77) -> {ret,n,{1,noret,noret}};
disasm(78) -> {addi,n,{1,1,1}};
disasm(79) -> {subi,n,{1,1,1}};
disasm(80) -> {add,none,{2,1,1}};
disasm(81) -> {sub,none,{2,1,1}};
disasm(82) -> {'or',none,{2,1,1}};
disasm(83) -> {'bor',none,{2,1,1}};
disasm(84) -> {'xor',none,{2,1,1}};
disasm(85) -> {'bxor',none,{2,1,1}};
disasm(86) -> {'bsl',none,{2,1,1}};
disasm(87) -> {'bsr',none,{2,1,1}};
disasm(88) -> {mult,none,{2,1,1}};
disasm(89) -> {'div',none,{2,1,1}};
disasm(90) -> {idiv,none,{2,1,1}};
disasm(91) -> {'rem',none,{2,1,1}};
disasm(92) -> {'and',none,{2,1,1}};
disasm(93) -> {'band',none,{2,1,1}};
disasm(94) -> {equal,none,{2,1,1}};
disasm(95) -> {neq,none,{2,1,1}};
disasm(96) -> {lesseq,none,{2,1,1}};
disasm(97) -> {less,none,{2,1,1}};
disasm(98) -> {moreeq,none,{2,1,1}};
disasm(99) -> {more,none,{2,1,1}};
disasm(100) -> {exeq,none,{2,1,1}};
disasm(101) -> {nexeq,none,{2,1,1}};
disasm(102) -> {'not',none,{1,1,1}};
disasm(103) -> {'bnot',none,{1,1,1}};
disasm(104) -> {negate,none,{1,1,1}};
disasm(105) -> {equal_to,term,{1,1,1}};
disasm(106) -> {neq_to,term,{1,1,1}};
disasm(107) -> {lesseq_than,term,{1,1,1}};
disasm(108) -> {less_than,term,{1,1,1}};
disasm(109) -> {moreeq_than,term,{1,1,1}};
disasm(110) -> {more_than,term,{1,1,1}};
disasm(111) -> {bif_call0,e0,{0,1,1}};
disasm(112) -> {bif_call1,e1,{1,1,1}};
disasm(113) -> {bif_call2,e2,{2,1,1}};
disasm(114) -> {bif_call3,e3,{3,1,1}};
disasm(115) -> {bif_call4,e4,{4,1,1}};
disasm(116) -> {call,label,{0,1,0}};
disasm(117) -> {tail_call,label,{0,noret,0}};
disasm(118) -> {call_far,mfn,{0,1,1}};
disasm(119) -> {tail_call_far,mfn,{0,noret,noret}};
disasm(120) -> {apply,none,{3,1,1}};
disasm(121) -> {apply2,none,{3,1,1}};
disasm(122) -> {apply_fun,none,{2,1,1}};
disasm(123) -> {level,n,{0,0,0}};
disasm(124) -> {rlevel,n,{0,0,0}};
disasm(125) -> {nop,none,{0,0,0}};
disasm(126) -> {break,none,{0,noret,noret}};
disasm(Op) -> exit({op,Op}).

switch() ->
[{0,is_integer},
 {1,is_bignum},
 {2,is_atom},
 {3,is_boolean},
 {4,is_float},
 {5,is_function},
 {6,is_function2},
 {7,is_number},
 {8,is_pid},
 {9,is_port},
 {10,is_record},
 {11,is_reference},
 {12,is_cons},
 {13,is_nil},
 {14,is_list},
 {15,is_tuple},
 {16,{is_tuple_of_arity,[arity],n}},
 {17,is_binary},
 {18,dup},
 {19,swap},
 {20,{drop,[n],n}},
 {21,reset_msgs},
 {22,{get_msg,[expired],label}},
 {23,drop_msg},
 {24,send_msg},
 {25,{get_arg,[ano],n}},
 {26,{clear_arg,[ano],n}},
 {27,{set_arg,[ano],n}},
 {28,{push_args,[n],n}},
 {29,{set_args,[n],n}},
 {30,{get_var,[vno],n}},
 {31,{clear_var,[vno],n}},
 {32,{set_var,[vno],n}},
 {33,{jump_if_not,[l],label}},
 {34,{jump_if,[l],label}},
 {35,{jump,[l],label}},
 {36,{'catch',[l],label}},
 {37,drop_catch},
 {38,raise},
 {39,{lit,[t],term}},
 {40,car},
 {41,cdr},
 {42,make_cons},
 {43,make_cons_nil},
 {44,list_copy},
 {45,list_append},
 {46,list_subtract},
 {47,{pack_tuple,[arity],n}},
 {48,{unpack_tuple,[arity],n}},
 {49,{tuple_elem,[index],n}},
 {50,beg_bin},
 {51,end_bin},
 {52,{bin_add_i_b,[unit],n}},
 {53,{bin_add_i_l,[unit],n}},
 {54,{bin_add_i_n,[unit],n}},
 {55,{bin_add_f_b,[unit],n}},
 {56,{bin_add_f_l,[unit],n}},
 {57,{bin_add_f_n,[unit],n}},
 {58,{bin_add_b,[unit],n}},
 {59,bin_fetch_start},
 {60,is_bin_consumed},
 {61,bin_get_context},
 {62,bin_size_is},
 {63,bin_size_is_at_least},
 {64,{bin_fetch_i_u_b,[unit],n}},
 {65,{bin_fetch_i_u_l,[unit],n}},
 {66,{bin_fetch_i_u_n,[unit],n}},
 {67,{bin_fetch_i_s_b,[unit],n}},
 {68,{bin_fetch_i_s_l,[unit],n}},
 {69,{bin_fetch_i_s_n,[unit],n}},
 {70,{bin_fetch_f_b,[unit],n}},
 {71,{bin_fetch_f_l,[unit],n}},
 {72,{bin_fetch_f_n,[unit],n}},
 {73,{bin_fetch_bin,[unit],n}},
 {74,self},
 {75,{enter,[n],n}},
 {76,leave},
 {77,{ret,[n],n}},
 {78,{addi,[i],n}},
 {79,{subi,[i],n}},
 {80,add},
 {81,sub},
 {82,'or'},
 {83,'bor'},
 {84,'xor'},
 {85,'bxor'},
 {86,'bsl'},
 {87,'bsr'},
 {88,mult},
 {89,'div'},
 {90,idiv},
 {91,'rem'},
 {92,'and'},
 {93,'band'},
 {94,equal},
 {95,neq},
 {96,lesseq},
 {97,less},
 {98,moreeq},
 {99,more},
 {100,exeq},
 {101,nexeq},
 {102,'not'},
 {103,'bnot'},
 {104,negate},
 {105,{equal_to,[t],term}},
 {106,{neq_to,[t],term}},
 {107,{lesseq_than,[t],term}},
 {108,{less_than,[t],term}},
 {109,{moreeq_than,[t],term}},
 {110,{more_than,[t],term}},
 {111,{bif_call0,[entry],e0}},
 {112,{bif_call1,[entry],e1}},
 {113,{bif_call2,[entry],e2}},
 {114,{bif_call3,[entry],e3}},
 {115,{bif_call4,[entry],e4}},
 {116,{call,[l],label}},
 {117,{tail_call,[l],label}},
 {118,{call_far,[amod,afun,n],mfn}},
 {119,{tail_call_far,[amod,afun,n],mfn}},
 {120,apply},
 {121,apply2},
 {122,apply_fun},
 {123,{level,[n],n}},
 {124,{rlevel,[n],n}},
 {125,nop},
 {126,break}].

%% EOF
