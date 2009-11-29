//
//      Stack manipulation
//

/// dup
        term_t t = top();
        push(t);

/// swap
        term_t a, b;
        a = top();
        b = top1();
        top() = b;
        top1() = a;

/// drop(uint n)
        ds->nelts -= n;

/// level(uint n)
        if (ds->nelts != n)
                exception(A_ERROR, A_STACK_LEVEL);

/// rlevel(uint n)
        if (cs->nelts != n)
                exception(A_ERROR, A_STACK_LEVEL);

/// nop
        // do nothing

/// self
		// self is known to be local
		push(localpid(proc->serial));
        
		//term_t pid = make_pid(my_node, proc->serial, 0, proc->gc_cur);
        //push(pid);
