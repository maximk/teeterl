//
//      Recognizers
//

/// is_integer
        term_t t = top();
        top() = bool(is_int(t) || is_bignum(t));

/// is_bignum
        term_t t = top();
        top() = bool(is_bignum(t));

/// is_atom
        term_t t = top();
        top() = bool(is_atom(t));

/// is_boolean
        term_t t = top();
        top() = bool(t == A_FALSE || t == A_TRUE);

/// is_float
        term_t t = top();
        top() = bool(is_float(t));

/// is_function
        term_t t = top();
        top() = bool(is_fun(t));

/// is_function2
        term_t n = pop();
        term_t fun = top();

        if (!is_int(n))
                bad_arg();

        if (!is_fun(fun))
                top() = A_FALSE;
        else
        {
                //TODO: will be gone when fridge becomes list
                term_t fridge = fun_fridge(fun);
                int nfree = (is_tuple(fridge))
                        ?int_value(tup_size(fun_fridge(fun)))
                        :0;
                int arity1 = int_value(fun_arity(fun)) - nfree;
                top() = bool(int_value(n) == arity1);
        }

/// is_literal
        term_t t = pop();
        push(bool(is_literal(t)));

/// is_number
        term_t t = pop();
        push(bool(is_number(t)));

/// is_pid
        term_t t = pop();
        push(bool(is_pid(t)));

/// is_port
        term_t t = pop();
        push(bool(is_port(t)));

/// is_record
        term_t n = pop();
        term_t a = pop();
        term_t t = top();
        if (!is_int(n) || !is_atom(a))
                bad_arg();
        top() = bool(is_tuple(t) && tup_elts(t)[0] == a && tup_size(t) == n);

/// is_reference
        term_t t = pop();
        push(bool(is_ref(t)));

/// is_cons
        term_t t = pop();
        push(bool(is_cons(t)));

/// is_nil
        term_t t = top();
        top() = bool(is_nil(t));

/// is_list
        term_t t = top();
        top() = bool(is_list(t));

/// is_tuple
        term_t t = top();
        top() = bool(is_tuple(t));

/// is_tuple_of_arity(uint arity)
        term_t t = top();
        int ok = is_tuple(t) && (int_value(tup_size(t)) == arity);
        top() = bool(ok);

/// is_binary
        term_t t = top();
        top() = bool(is_binary(t));

