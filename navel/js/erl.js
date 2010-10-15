//
//
//
//	Number -> Number
//		12	->	12
//		1.2	->	1.2
//		12345678901234 -> {n:'12345678901234'}
//
//	Atom -> String
//		abc	->	'abc'
//
//	Binaries -> Object
//		<<1,2,3>>	->	{b:'010203'}
//		<<1,2,1:3>>	->	{b:{wb:'0102',tr:1,ts:3}}
//
//	Fun -> Object
//		#fun	->	{f:''}
//
//  Oid -> Object
//		#Outlet<local.2.0>	->	{o:2}
//		#Outlet<remote.2.3>	->	{o:{id:2,nd:'remote',cn:3}}
//
//  Pid -> Object
//		<local.2.0>	->	{p:2}
//		<remote.2.3>	->	{p:{id:2,nd:'remote',cn:3}}
//
//	Tuple -> Object
//		{1,2,3}	->	{t:[1,2,3]}
//
//	List -> Array
//		[1,2,3]	->	[1,2,3]
//		[]		->	[]
//
//	String	->	Object
//		"hello"	->	{s:'hello'}
//
//	partials ->	Object
//		<<1,2,3,...>>	{x:{pf:{b:'010203'},ts:1024}}
//		{1,2,3,...}		{x:{pf:{t:[1,2,3]},ts:1024}}
//		[1,2,3,...]		{x:{pf:[1,2,3],ts:1024}}
//		"hello..."		{x:{pf:{s:"hello"},ts:1024}}
//

function binary()
{
	var h = $.map(arguments, function(b)
	{
		var s = b.toString(16);
		return (s.length == 2) ?s :'0'+s;
	});
	return {b:h.join('')};
}

function fun()
{
	return {f:''}
}

function oid(id, nd, cn)
{
	if (arguments.length == 1)
		return {o:id};
	else
		return {o:{id:id, nd:nd, cn:cn}};
}

function pid(id, nd, cn)
{
	if (arguments.length == 1)
		return {p:id};
	else
		return {p:{id:id, nd:nd, cn:cn}};
}

function tuple()
{
	return {t:arguments};
}

function string(s)
{
	return {s:s};
}

function partial(prefix, total)
{
	return {x:{pf:prefix, ts:total}};
}

///////////////////////////////////////////////////////////////////////////////

function pp(t)
{
	if (typeof(t) == 'number')
	{
		return t.toString();	// small integer or float
	}
	else if (typeof(t) == 'string')
	{
		return t;	// atom
	}
	else if (typeof(t) == 'object' && typeof(t.length) != 'undefined')
	{
		var es = $.map(t, function(e)
		{
			if (typeof(e) == 'undefined')
				console.log(t);
			return pp(e);
		});
		return '['+es.join(',')+']';
	}
	else if (typeof(t) == 'object')
	{
		var selector;
		for(var key in t)
		{
			if(t.hasOwnProperty(key))
			{
				selector = key;
				break;
			}
		}
		
		switch (selector)
		{
		case 'n':	// bignum
			return '#bignum';
		case 'b':	// binary
			return '#binary';
		case 'f':	// fun
			return '#fun';
		case 'o':	// oid
			return '#oid';
		case 'p':	// pid
			return '#pid';
		case 't':	// tuple
			var es = $.map(t['t'], function(e)
			{
				return pp(e);
			});
			return '{'+es.join(',')+'}';
		case 's':	// string
			return '"'+t['s']+'"';
		case 'x':	// partial
			return '#partial';
		default:
			console.log(selector);
			throw new Error();
		}
	}
	else
	{
		console.log(t);
		throw new Error();
	}
}

//EOF
