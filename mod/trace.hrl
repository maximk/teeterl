%
%
%

% update code in scheduler.c when the record changes

-record(postmortem, {pid,			% 1
					 reg_name,		% 2
					 module,		% 3
					 offset,		% 4
					 source,		% 5
					 call_stack,	% 6
					 registers,		% 7
					 slots,			% 8
					 heap_size,		% 9
 					 result,		% 10
					 reason}).		% 11

-define(SLICE_RESULT_YIELD, 100).
-define(SLICE_RESULT_WAIT, 101).
-define(SLICE_RESULT_DONE, 102).
-define(SLICE_RESULT_EXIT, 103).
-define(SLICE_RESULT_ERROR, 104).
-define(SLICE_RESULT_THROW, 105).

%EOF
